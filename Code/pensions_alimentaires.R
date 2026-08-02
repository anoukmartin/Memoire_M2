# Pensions alimentaires 

# ============================================================
# 1. RÉCUPÉRATION DES VERSEMENTS DES EX-CONJOINT-E-S
# ============================================================

menages <- readRDS("Data_output/data_recode/menages_ageminmax.Rds")


# Sélection des variables relatives aux versements
aide_reg <- menages %>%  
  select(
    IDENT_MEN,
    starts_with("EXRECR"),
    starts_with("MRECR"),
    starts_with("OBLRECR"),
    starts_with("PERRECR"),
    starts_with("PRORECR")
  ) %>% 
  
  # Recodage des valeurs manquantes des montants
  mutate_at(
    .vars = vars(starts_with("MRECR")),
    .funs = function(x) {
      if_else(x %in% c(999999, 999998), NA, x)
    }
  ) %>% 
  
  # Suppression des anciennes variables MRECR
  select(-c(paste0("MRECR", 1:3))) %>% 
  
  # Renommage des variables de montant
  rename(
    MRECR1 = MRECR1_D,
    MRECR2 = MRECR2_D,
    MRECR3 = MRECR3_D
  )


# Variables à mettre au format long
variables <- c(
  "MRECR",
  "OBLRECR",
  "PERRECR",
  "PRORECR"
)


# Mise au format long de chaque série de variables
tabs <- lapply(
  variables,
  function(x) {
    
    tab <- pivot_longer(
      aide_reg,
      cols = starts_with(x),
      names_to = "id_vers",
      values_to = x,
      values_drop_na = FALSE
    ) %>%
      mutate(
        id_vers = str_remove_all(id_vers, x)
      ) %>%
      select(
        IDENT_MEN,
        id_vers,
        all_of(x)
      )
    
    return(tab)
  }
)

names(tabs) <- variables


# Fusion des différentes informations sur les versements
aide_reg2 <- tabs$MRECR %>%
  left_join(
    tabs$OBLRECR,
    by = c("IDENT_MEN", "id_vers")
  ) %>%
  left_join(
    tabs$PERRECR,
    by = c("IDENT_MEN", "id_vers")
  ) %>%
  left_join(
    tabs$PRORECR,
    by = c("IDENT_MEN", "id_vers")
  )


# ============================================================
# 2. SÉLECTION DES VERSEMENTS PROVENANT DES EX-CONJOINT-E-S
# ============================================================

aide_exconj <- aide_reg2 %>%
  
  # PRORECR == 4 : versement provenant d'un ex-conjoint
  filter(PRORECR == "4") %>%
  
  # Calcul du montant mensuel
  mutate(
    MRECR_mens = MRECR / PERRECR
  ) %>%
  
  # Séparation selon le caractère obligatoire ou non du versement
  mutate(
    MRECE_mens_oblig = if_else(
      OBLRECR == "1",
      MRECR_mens,
      NA
    ),
    
    MRECE_mens_nonoblig = if_else(
      OBLRECR == "2",
      MRECR_mens,
      NA
    )
  )


# Agrégation au niveau du ménage
aide_exconjsum <- aide_exconj %>%
  group_by(IDENT_MEN) %>%
  summarise(
    montant_vers = sum(
      MRECR_mens,
      na.rm = TRUE
    ),
    
    montant_oblig = sum(
      MRECE_mens_oblig,
      na.rm = TRUE
    ),
    
    montant_nonoblig = sum(
      MRECE_mens_nonoblig,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


# ============================================================
# 3. RÉCUPÉRATION DES INFORMATIONS SUR LES MÉNAGES
# ============================================================





revenus_comp <- menages %>%
  select(
    TDM8,
    TDM8_SEXE,
    TAF,
    PONDMEN,
    IDENT_MEN,
    NIVIE,
    REVDISP,
    n_FractionClasse
  ) %>%
  
  left_join(
    aide_exconjsum,
    by = "IDENT_MEN"
  ) %>%
  
  # Identification des ménages recevant un versement
  mutate(
    pens_exconj = if_else(
      is.na(montant_vers),
      "Non",
      "Oui"
    )
  )


# Ajout du nombre d'enfants issus d'unions anterieure ####
# dans le ménage 

indiv <- readRDS("Data_output/data_recode/indiv_in_menagesAge.Rds")
adultes <- indiv %>%
  filter(ENFANT == "2") %>%
  mutate(NENFANTS_PROPRES = NENFANTS - NENFANTS_COMMUNS)
freq(adultes$NENFANTS_PROPRES)
Nenfants_propres <- adultes %>%
  group_by(IDENT_MEN) %>%
  summarise(NENFANTS_UNION_ANT = sum(NENFANTS_PROPRES))
freq(Nenfants_propres$NENFANTS_UNION_ANT)

revenus_comp <- revenus_comp %>% left_join(Nenfants_propres)
aide_exconjsum <- aide_exconjsum %>%
  left_join(menages) %>%
  left_join(Nenfants_propres) %>%
  # Création des groupes de facettes
  mutate(
    ENFANTS_DE = case_when(
      str_detect(
        TDM8_SEXE,
        "mère"
      ) ~ "Mère",
      
      str_detect(
        TDM8_SEXE,
        "père"
      ) ~ "Père",
      
      str_detect(
        TDM8_SEXE,
        "chacun des membres"
      ) ~ "Les deux",
      
      TRUE ~ NA_character_
    )
  )

# ============================================================
# 4. PART DES MÉNAGES RECEVANT UNE PENSION
# ============================================================

pens_comp <- revenus_comp

pens_comp <- lprop(
  wtd.table(
    x = pens_comp$TDM8_SEXE,
    y = pens_comp$pens_exconj,
    weights = pens_comp$PONDMEN
  )
) %>%
  as.data.frame() %>%
  filter(
    Var2 == "Oui",
    Var1 != "All"
  )
pens_comp
barplot(pens_comp$Freq)

# ============================================================
# 5. PART DES MÉNAGES RECEVANT UNE PENSION
#    DANS LES FAMILLES RECOMPOSÉES ET MONOPARENTALES
# ============================================================

pens_comp2 <- revenus_comp %>%
  filter(
    str_detect(
      TAF,
      "recomposée|monoparentale"
    )
  )

pens_comp2 <- lprop(
  wtd.table(
    x = pens_comp2$TDM8_SEXE,
    y = pens_comp2$pens_exconj,
    weights = pens_comp2$PONDMEN
  )
) %>%
  as.data.frame() %>%
  filter(
    Var2 == "Oui",
    Var1 != "All"
  )

pens_comp2
barplot(pens_comp2$Freq)
# ============================================================
# 6. PRÉPARATION DES MONTANTS DE VERSEMENTS
# ============================================================

revenus_comp2 <- revenus_comp %>%
  
  # Pour les ménages recevant une pension :
  # absence de montant dans une catégorie = 0
  mutate(
    montant_oblig = if_else(
      is.na(montant_oblig),
      0,
      montant_oblig
    ),
    
    montant_nonoblig = if_else(
      is.na(montant_nonoblig),
      0,
      montant_nonoblig
    )
  ) %>%
  
  # On conserve uniquement les ménages recevant un versement
  # filter(
  #   pens_exconj == "Oui"
  # ) %>%
  
  # Champ : familles recomposées et monoparentales
  filter(
    str_detect(
      TAF,
      "recomposée|monoparentale"
    )
  ) %>%
  mutate(
    montant_vers = montant_vers / NENFANTS_UNION_ANT, 
    montant_oblig = montant_oblig / NENFANTS_UNION_ANT,
    montant_nonoblig = montant_nonoblig / NENFANTS_UNION_ANT
  ) %>%
  # Passage au format long
  pivot_longer(
    cols = c(
      montant_oblig,
      montant_nonoblig
    ),
    names_to = "name",
    values_to = "value"
  )

freq(revenus_comp2$NENFANTS_UNION_ANT)
# ============================================================
# 7. CALCUL DES MOYENNES PONDÉRÉES
# ============================================================

revenus_comp2 <- revenus_comp2 %>%
  group_by(
    TDM8_SEXE,
    name
  ) %>%
  summarise(
    value_mean = weighted.mean(
      x = value,
      w = PONDMEN,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# ============================================================
# 8. RECODAGE DES CATÉGORIES DE VERSEMENT
# ============================================================

revenus_comp2 <- revenus_comp2 %>%
  
  # Ordre d'affichage des barres
  mutate(
    name = fct_relevel(
      name,
      "montant_nonoblig"
    )
  ) 
  
 


# ============================================================
# 9. CRÉATION D'UNE VARIABLE POUR L'AFFICHAGE DANS LES FACETTES
# ============================================================

# On conserve uniquement les modalités de TDM8_SEXE
# correspondant au groupe de la facette.
#
# Ici, chaque ligne appartient déjà à la facette correspondant
# à sa propre modalité de TDM8_SEXE.
#
# On transforme ensuite TDM8_SEXE en facteur dans chaque groupe
# afin que les niveaux inutilisés soient effectivement supprimés.

revenus_comp2 <- revenus_comp2 %>%
  group_by(ENFANTS_DE) %>%
  mutate(
    TDM8_SEXE_facette = factor(
      TDM8_SEXE,
      levels = unique(TDM8_SEXE)
    )
  ) %>%
  ungroup()


# Vérification
freq(revenus_comp2$ENFANTS_DE)

# ============================================================
# 9. GRAPHIQUE 1
# ============================================================

names(pens_comp2)[1] <- c("TDM8_SEXE")

pens_comp2 <- pens_comp2 %>%
  mutate(
    ENFANTS_DE = case_when(
      str_detect(
        TDM8_SEXE,
        "mère"
      ) ~ "Mère",
      
      str_detect(
        TDM8_SEXE,
        "père"
      ) ~ "Père",
      
      str_detect(
        TDM8_SEXE,
        "chacun des membres"
      ) ~ "Les deux",
      
      TRUE ~ NA_character_
    )
  )


pens_comp2 <- pens_comp2 %>%
  group_by(ENFANTS_DE) %>%
  mutate(
    TDM8_SEXE = factor(
      TDM8_SEXE,
      levels = unique(TDM8_SEXE)
    )
  ) %>%
  ungroup()


# Vérification
freq(revenus_comp2$ENFANTS_DE)


levels(pens_comp2$TDM8_SEXE) <- sapply(levels(pens_comp2$TDM8_SEXE), function(x){insert_line_breaks(x, 30)})
ggplot(
  data = pens_comp2,
  aes(
    x = TDM8_SEXE,
    fill = ENFANTS_DE,
    y = Freq,
  )
) +
  
  geom_col(color = "black") +
  
  geom_text(
    aes(
      label = paste0(round(
        Freq,
        0
      ), "%")
    ),
    position = position_stack(
      vjust = 0.5
    ),
    size = 3,
    family = "serif"
  ) +
  
  geom_hline(
    yintercept = 0
  ) +
  
  labs(
    x = "Configuration familiale du ménage",
    y = "Perception de versement par un-e ex-conjoint-e"
  ) +
  scale_fill_manual(
    values = c(
      "#1b9e77",
      "#d95f02",
      "#7570b3"),
    name = "Enfants issus d'une\nprécédante union"
  ) +
  
  # facet_wrap(
  #   ~ ENFANTS_DE,
  #   nrow = 3,
  #   ncol = 1,
  #   drop = TRUE,
  #   scales = "free_x"
  # ) +
  
  theme(
    axis.line.y = element_blank(),
    axis.text.y = element_text()
  ) +
  theme_tufte() +
  coord_flip()


# ============================================================
# 10. GRAPHIQUE 2
# ============================================================
levels(revenus_comp2$TDM8_SEXE_facette) <- sapply(levels(revenus_comp2$TDM8_SEXE_facette), function(x){insert_line_breaks(x, 30)})

ggplot(
  data = revenus_comp2,
  aes(
    x = TDM8_SEXE_facette,
    y = value_mean,
    fill = ENFANTS_DE,
    alpha = name
  )
) +
  
  geom_col(color = "black") +
  
  geom_text(
    aes(
      label = round(
        value_mean,
        1
      )
    ),
    position = position_stack(
      vjust = 0.5
    ),
    size = 3,
    family = "serif"
  ) +
  
  geom_hline(
    yintercept = 0
  ) +
  
  labs(
    x = "Configuration familiale du ménage",
    y = "Montant mensuel moyen par enfant issu\nd'une union anterieure (en euros)"
  ) +
  scale_alpha_manual(
    values = c(
      "montant_oblig" = 1,
      "montant_nonoblig" = 0.5
    ),
    labels = c(
      "montant_oblig" = "qui doit les verser\nobligatoirement",
      "montant_nonoblig" = "qui les verse\nlibrement"
    ),
    name = "Sommes reçues\nrégulièrement\nd'ex-conjoint-e-s :"
  ) +
  scale_fill_manual(
    values = c(
          "#1b9e77",
          "#d95f02",
          "#7570b3"),
    name = "Enfants issus d'une\nprécédante union"
  ) +
  
  # facet_wrap(
  #   ~ ENFANTS_DE,
  #   nrow = 3,
  #   ncol = 1,
  #   drop = TRUE,
  #   scales = "free_x"
  # ) +
  
  theme(
    axis.line.y = element_blank(),
    axis.text.y = element_text()
  ) +
  theme_tufte() +
  coord_flip()

###############################################################################.
# Regression sur le montant des pensions par enfant ####
###############################################################################.

aide_exconjsum <- aide_exconjsum %>%
  filter(str_detect(TAF, "recomposée|monoparentale")) %>%
  mutate(
    TDM8 = TDM8 %>%
      str_remove(" \\(mère \\+\\n?enfant\\(s\\)\\)") %>%
      str_remove(" \\(père \\+\\n?enfant\\(s\\)\\)") %>%
           as_factor() %>%
           relevel(ref = "Famille monoparentale"), 
         ENFANTS_DE = ENFANTS_DE %>%
      as.factor() %>%
           relevel("Mère"), 
    NIVIE = NIVIE/1200, 
    n_FractionClasse = n_FractionClasse %>% relevel(ref = "'Petits-moyens' [C3]"))
  
summary(aide_exconjsum$montant_vers)

aide_exconjsum$ENFANTS_DE
aide_exconjsum$montant_vers
aide_exconjsum$NENFANTS_UNION_ANT
modele <- lm(
  log(montant_vers +1) ~
    NIVIE + n_FractionClasse +
    NENFANTS_UNION_ANT +
    ENFANTS_DE + TDM8,
  weights = PONDMEN,
  data = aide_exconjsum
)

modele <- glm(
  montant_vers ~
    NIVIE + n_FractionClasse +
    NENFANTS_UNION_ANT +
    ENFANTS_DE + TDM8,
  weights = PONDMEN,
  data = aide_exconjsum
)

tbl_regression(modele) %>%
  bold_p(t = 0.1) %>%
  add_glance_source_note()


aide_exconjsum$n_RevenusContribF