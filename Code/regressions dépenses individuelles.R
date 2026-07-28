################################################################################.
# RÉGRESSION DES DÉPENSES INDIVIDUELLES DE VÊTEMENTS DES ENFANTS ###############
################################################################################.

################################################################################.
# 1. Chargement des données ----
################################################################################.

# Données individuelles
indiv <- readRDS("Data_output/data_recode/indiv_in_menagesAge.Rds")

# Dépenses individuelles
dep_ind <- readRDS("Data_output/DepIndiv.Rds") |>
  pad_2digits("NOI")

freq(dep_ind$NOI)

# Ajout des dépenses de vêtements et de réparation de vêtements
indiv <- indiv |>
  left_join(
    dep_ind |>
      mutate(
        MLOISIRCUL_D = case_when(
          if_all(matches("^SM.*_D$"), is.na) ~ NA_real_,
          TRUE ~ rowSums(
            pick(matches("^SM.*_D$")),
            na.rm = TRUE
          )
        )
      ) %>%
      select(IDENT_MEN, NOI, MVETEMENTS_D, MLOISIRCUL_D, MREPET_D, MCOIFFUR_D)
  )
summary(indiv$MVETEMENTS_D)
freq.na(indiv$MVETEMENTS_D)
summary(indiv$MLOISIRCUL_D)
freq.na(indiv$MLOISIRCUL_D)
summary(indiv$MCOIFFUR_D)
freq.na(indiv$MCOIFFUR_D)

# Revenus du ménage
menages <- readRDS("Data_output/data_recode/menages_ageminmax.Rds")

indiv <- indiv |>
  left_join(
    menages |>
      select(IDENT_MEN, REVTOT, REVDISP, COEFFUC, NENFANTS_MENAGE = NENFANTS)
  )

################################################################################
# DEPENSES DES ENFANTS #########################################################
################################################################################

################################################################################.
# 2. Sélection de la population étudiée ----
################################################################################.

# Restriction aux enfants
enfants <- indiv |>
  filter(ENFANT == "1")

################################################################################.
# 3. Exploration de la variable dépendante ----
################################################################################.

summary(enfants$AGE)

summary(enfants$MVETEMENTS_D)
boxplot(enfants$MVETEMENTS_D)

# Environ 30 % des dépenses sont manquantes
freq.na(enfants$MVETEMENTS_D)
freq.na(enfants$MLOISIRCUL_D)
# Une valeur extrêmement élevée (17 520 €) apparaît comme un outlier.
# Elle est remplacée par NA afin qu'elle ne domine pas l'estimation.
enfants$MVETEMENTS_D[
  enfants$MVETEMENTS_D == 17520
] <- NA

# Distribution brute
plot(density(enfants$MVETEMENTS_D, na.rm = TRUE))

# Distribution après transformation logarithmique
ggplot(enfants) +
  geom_density(aes(x = log(MVETEMENTS_D + 1)))

# Importance des dépenses nulles
freq(enfants$MVETEMENTS_D == 0)
mean(enfants$MVETEMENTS_D, na.rm = TRUE)

################################################################################.
# 4. Préparation des données pour les régressions ----
################################################################################.
freq(enfants$TAF)

data <- enfants |>
  filter(TAF != "Autre ménage (complexe)") |>
  
  # Suppression des observations sans variable dépendante
  filter(!is.na(MVETEMENTS_D)) |>
  
  # Mise à l'échelle des variables monétaires
  mutate(
    NIVIE = NIVIE / 1200,
    n_REVENUS_indiv_H = n_REVENUS_indiv_H / 1200,
    n_REVENUS_indiv_F = n_REVENUS_indiv_F / 1200,
    n_REVENUS_indiv_PERE = n_REVENUS_indiv_PERE / 1200,
    n_REVENUS_indiv_MERE = n_REVENUS_indiv_MERE / 1200,
    n_REVENUS_indiv_BEAUPARENT = n_REVENUS_indiv_BEAUPARENT / 1200,
    REVDISP = REVDISP/1200
  ) |>
  mutate(SEXE = case_when(
    SEXE == "1" ~ "Masculin", 
    SEXE == "2" ~ "Féminin"
  )) |>
  # Définition des modalités de référence
  mutate(
    
    n_FractionClasse =
      fct_relevel(n_FractionClasse, "'Petits-moyens' [C3]"),
    
    TDM8_SEXE =
      fct_relevel(
        TDM8_SEXE,
        "Couple avec uniquement enfant(s) du couple"
      ) |> droplevels(),
    
    TDM8 =
      fct_relevel(
        TDM8,
        "Couple avec uniquement enfant(s) du couple"
      ) |> droplevels(),
    
    TAF =
      fct_relevel(
        TAF,
        "Famille traditionnelle"
      ) |> droplevels(),
    MOCO_DET2 = case_when(
      MOCO_DET == "Enfant d’un seul des adultes de la famille recomposée"
      & TAF == "Famille recomposée (sans enfant du couple)" ~ "Enfant d’un seul des adultes de la famille recomposée sans enfants communs", 
      MOCO_DET == "Enfant d’un seul des adultes de la famille recomposée"
      & TAF == "Famille recomposée (avec enfant(s) du couple)" ~ "Enfant d’un seul des adultes de la famille recomposée avec enfants communs",
      TRUE ~ MOCO_DET
    ),
    MOCO_DET2 =
      fct_relevel(
        MOCO_DET2,
        "Enfant d’une famille traditionnelle"
      ) |> droplevels(), 
    
    MOCO_DET_SEXE2 = case_when(
      MOCO_DET_SEXE == "Enfant du père de famille recomposée"
      & TAF == "Famille recomposée (sans enfant du couple)" ~ "Enfant du père de famille recomposée sans enfants communs", 
      MOCO_DET_SEXE == "Enfant de la mère de famille recomposée"
      & TAF == "Famille recomposée (sans enfant du couple)" ~ "Enfant de la mère de famille recomposée sans enfants communs", 
      MOCO_DET_SEXE == "Enfant du père de famille recomposée"
      & TAF == "Famille recomposée (avec enfant(s) du couple)" ~ "Enfant du père de famille recomposée avec enfants communs",
      MOCO_DET_SEXE == "Enfant de la mère de famille recomposée"
      & TAF == "Famille recomposée (avec enfant(s) du couple)" ~ "Enfant de la mère de famille recomposée avec enfants communs", 
      TRUE ~ MOCO_DET_SEXE
    ),
    MOCO_DET_SEXE2 =
      fct_relevel(
        MOCO_DET_SEXE2,
        "Enfant d’une famille traditionnelle"
      ) |> droplevels(), 
    MOCO_DET_SEXE =
      fct_relevel(
        MOCO_DET_SEXE,
        "Enfant d’une famille traditionnelle"
      ) |> droplevels(),
    
    MOCO_DET =
      fct_relevel(
        MOCO_DET,
        "Enfant d’une famille traditionnelle"
      ) |> droplevels()
  ) |>
  #rec_NENFANTS(Var = "N_FRERES_SOEURS_TOUS")|>
  # Construction des classes d'âge
  mutate(
    
    AGE_categories = case_when(
      AGE < 3  ~ "Nourrisson/bambin [0;3[",
      AGE < 7  ~ "Jeune enfant [3;7[",
      AGE < 11 ~ "Enfant [7;11[",
      AGE < 15 ~ "Jeune adolescent [11;15[",
      AGE < 18 ~ "Adolescent [15;18[",
      AGE < 25 ~ "Jeune adulte [18;25[",
      TRUE     ~ "Adulte [+25]"
    ),
    
    AGE_categories = factor(
      AGE_categories,
      levels = c(
        "Nourrisson/bambin [0;3[",
        "Jeune enfant [3;7[",
        "Enfant [7;11[",
        "Jeune adolescent [11;15[",
        "Adolescent [15;18[",
        "Jeune adulte [18;25[",
        "Adulte [+25]"
      )
    ),
    
    AGE_categories =
      relevel(AGE_categories, ref = "Enfant [7;11["), 
    POSITION_FRATERIE = 
      as.factor(POSITION_FRATERIE) %>%
      relevel(POSITION_FRATERIE, ref = "Aîné-e")) 

################################################################################.
# 5. Modèle principal ----
################################################################################.

# tris a plat 


design <- data %>%
  as_survey_design(
    ids = IDENT_MEN,
    weights = PONDIND
  )

tableau <- design %>%
  group_by(MOCO_DET, n_FractionClasse) %>%
  summarise(
    moyenne_depenses = survey_mean( vartype = "ci",
      MVETEMENTS_D,
      na.rm = TRUE
    )
  ) %>%
  as_tibble()

marges_moco <- design %>%
  group_by(MOCO_DET) %>%
  summarise(
    moyenne_depenses = survey_mean(
      MVETEMENTS_D,
      vartype = "ci",
      na.rm = TRUE
    )
  ) %>%
  mutate(n_FractionClasse = "Ensemble")

marges_FracClass <- design %>%
  group_by(n_FractionClasse) %>%
  summarise(
    moyenne_depenses = survey_mean(
      MVETEMENTS_D,
      vartype = "ci",
      na.rm = TRUE
    )
  ) %>%
  mutate(MOCO_DET = "Ensemble")

marges_tous<- design %>%
  summarise(
    moyenne_depenses = survey_mean(
      MVETEMENTS_D,
      vartype = "ci",
      na.rm = TRUE
    )
  ) %>%
  mutate(MOCO_DET = "Ensemble", 
         n_FractionClasse = "Ensemble")

tableau <- bind_rows(tableau, marges_moco)
tableau <- bind_rows(tableau, marges_FracClass) 
tableau <- bind_rows(tableau, marges_tous) 

tableau <- tableau %>%
  mutate(
    across(
      c(moyenne_depenses, moyenne_depenses_low, moyenne_depenses_upp),
      ~ round(.x, 0)
    )) %>%
  mutate(moyenne = str_glue("{moyenne_depenses} \n [{moyenne_depenses_low} – {moyenne_depenses_upp}]"))



tableau %>%
  pivot_wider(id_cols = n_FractionClasse, 
              names_from = MOCO_DET, 
              values_from = moyenne)  |>
  flextable() |>
  set_header_labels(
    n_FractionClasse = "Position sociale du ménage"
  ) |>
  add_header_row(
    values = c("", "Position de l'enfant dans le ménage"),
    colwidths = c(1, 5)
  ) |>
  font(fontname = "Garamond", part = "all") |>
  fontsize(size = 10, part = "all") |>
  align(align = "center", part = "all", j = 2:6) |>
  autofit()

head(tableau)
names(tableau)
# GG plot 

library(ggplot2)

ggplot(tableau, 
       aes(x = n_FractionClasse, 
           y = moyenne_depenses, 
           fill = MOCO_DET)) +
  
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  
  geom_errorbar(
    aes(
      ymin = moyenne_depenses_low,
      ymax = moyenne_depenses_upp
    ),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  
  labs(
    x = "Position sociale du ménage",
    y = "Dépenses moyennes d'habillement (€)",
    fill = "Position de l'enfant dans le ménage"
  ) +
  
  theme_minimal() +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    text = element_text(family = "Garamond")
  )

# linéaire 
modele <- lm(
  MVETEMENTS_D ~
    NIVIE + n_FractionClasse +
    POSITION_FRATERIE +
    SEXE + RANG_ENFANT_SEXE +
    AGE_categories +
    MOCO_DET,
  weights = PONDIND,
  data = data
)
library(fixest)


modele <- lm(
  log(MVETEMENTS_D + 1) ~
    NIVIE + n_FractionClasse +
    POSITION_FRATERIE +
    SEXE + RANG_ENFANT_SEXE +
    AGE_categories +
    MOCO_DET,
  
  weights = PONDIND,
  #cluster = IDENT_MEN,
  data = data
)

modele_fix <- feols(
  log(MVETEMENTS_D +1) ~
    NIVIE + n_FractionClasse +
    POSITION_FRATERIE +
    SEXE + RANG_ENFANT_SEXE +
    AGE_categories +
    MOCO_DET,

  weights = ~PONDIND,
  cluster = ~IDENT_MEN,
  data = data
)



summary(modele)
summary(modele_fix)

tbl_regression(modele)|> 
  add_glance_table()  |>
  bold_p(t = 0.1) %>%
  as_flex_table() |>
  font(fontname = "Garamond", part = "all") |>
  fontsize(size = 10, part = "all") |>
  autofit()

library(officer)

doc <- read_docx()

doc <- doc |>
  body_add_flextable(
    tbl_regression(modele) |>
      add_glance_source_note() |>
      bold_p(t = 0.1) |>
      as_flex_table() |>
      font(fontname = "Garamond", part = "all") |>
      fontsize(size = 10, part = "all") |>
      border_remove() |>
      hline_top(border = fp_border(width = 1)) |>
      hline_bottom(border = fp_border(width = 1)) |>
      autofit()
  )

print(doc, target = "Resultats/resultats_regression-depenses_vetements_enfants.docx")
  
# add_significance_stars(
#     thresholds = c(.001, .01, .05, .10),
#     hide_ci = FALSE,
#     hide_se = TRUE,
#     hide_p = FALSE
#   )

plot(modele)

################################################################################.
# 6. Test de robustesse : exclusion du 1 % supérieur des dépenses ----
################################################################################.

quantile(data$MVETEMENTS_D, .99)

modele_log_sans_top <- lm(
  
  log(MVETEMENTS_D + 1) ~
    n_FractionClasse +
    SEXE +
    AGE_categories +
    N_FRERES_SOEURS_TOUS +
    RANG_ENFANT +
    NIVIE*MOCO_DET,
  weights = PONDIND,
  
  data = subset(
    data,
    MVETEMENTS_D <= quantile(data$MVETEMENTS_D, .99)
  )
)

tbl_regression(modele_log_sans_top) %>%
  bold_p(t = 0.1) %>%
  add_glance_source_note()

# Les coefficients d'intérêt restent très proches :
# les résultats semblent peu sensibles aux très fortes dépenses.



################################################################################.
# 7. Variante : prise en compte du statut légal des enfants ----
################################################################################.

# On remplace MOCO_DET par MOCO_DET_SEXE afin de distinguer
# les enfants du père, de la mère ou du couple.

modele <- lm(
  log(MVETEMENTS_D + 1) ~
    
    n_FractionClasse +
    SEXE +
    AGE_categories +
    N_FRERES_SOEURS_TOUS +
    RANG_ENFANT +
    NIVIE*MOCO_DET_SEXE,
  
  weights = PONDIND,
  data = data
)

tbl_regression(modele) |>
  add_glance_source_note() |>
  bold_p(t = 0.10) 


################################################################################.
# 8. Les revenus des adultes expliquent-ils les dépenses ? ####
################################################################################.

## Revenus des hommes et des femmes

modele <- lm(
  log(MVETEMENTS_D + 1) ~
    NIVIE +
    n_FractionClasse +
    SEXE +
    AGE +
    N_FRERES_SOEURS_TOUS +
    RANG_ENFANT +
    n_REVENUS_indiv_F +
    n_REVENUS_indiv_H,
  
  weights = PONDIND,
  data = data
)



## Revenus du père et de la mère

modele <- lm(
  log(MVETEMENTS_D + 1) ~
    NIVIE +
    n_FractionClasse +
    SEXE +
    AGE +
    N_FRERES_SOEURS_TOUS +
    RANG_ENFANT +
    n_REVENUS_indiv_MERE +
    n_REVENUS_indiv_PERE,
  
  weights = PONDIND,
  data = data
)


## Interactions avec la configuration familiale

modele <- lm(
  log(MVETEMENTS_D + 1) ~
    NIVIE +
    n_FractionClasse +
    SEXE +
    AGE +
    N_FRERES_SOEURS_TOUS +
    RANG_ENFANT +
    TDM8_SEXE:n_REVENUS_indiv_F +
    TDM8_SEXE:n_REVENUS_indiv_H,
  
  weights = PONDIND,
  data = data
)



################################################################################.
# 9. Analyse restreinte aux familles recomposées avec effets fixe ####
################################################################################.

data_recomp <- data |>
  filter(str_detect(MOCO_DET, "recomposée")) %>%
  mutate(MOCO_DET = MOCO_DET %>% as.factor() %>%
           droplevels() %>%
           relevel("Enfant du couple dans une famille recomposée"))

freq(data_recomp$MOCO_DET)

# Dans les familles recomposées, on cherche à déterminer
# si les dépenses sont davantage associées au revenu
# du parent biologique ou de celui du beau-parent.

data_recomp2 <- data_recomp %>%
  group_by(IDENT_MEN) %>%
  filter(n_distinct(MOCO_DET, na.rm = TRUE) > 1) %>%
  ungroup()

modele <- feols(
  
  log(MVETEMENTS_D + 1) ~
    SEXE +
    AGE_categories +
    POSITION_FRATERIE +
    RANG_ENFANT_SEXE +
    MOCO_DET |  
    
    IDENT_MEN,
  
  weights = ~PONDIND,
  cluster = ~IDENT_MEN,
  data = data_recomp2
)

tbl_regression(modele) |>
  bold_p(t = .10) |>
  bold_labels() |>
  #italicize_levels() |>
  add_glance_source_note() |>
  as_flex_table() |>
  font(fontname = "Garamond", part = "all") |>
  fontsize(size = 10, part = "all") |>
  autofit()



summary(modele)
summary(data_recomp$N_FRERES_SOEURS_TOUS)


# Stat des pondérés des adelphies dans les familles recomposées : ----


design_recomp <- svydesign(
  ids = ~IDENT_MEN,
  weights = ~PONDIND,
  data = data_recomp2
)

svyquantile(
  ~N_FRERES_SOEURS_TOUS,
  design = design_recomp,
  quantiles = c(0.25, 0.5, 0.75),
  na.rm = TRUE
)

svymean(
  ~N_FRERES_SOEURS_TOUS,
  design = design_recomp,
  na.rm = TRUE
)

# Enfants hors familles recomposées
data_notrecomp2 <- data %>%
  filter(!(IDENT_IND %in% data_recomp2$IDENT_IND)) %>%
  group_by(IDENT_MEN) %>%
  filter(n() >1) %>%
  ungroup()


modele <- feols(
  
  log(MVETEMENTS_D + 1) ~
    SEXE +
    AGE_categories +
    POSITION_FRATERIE +
    RANG_ENFANT_SEXE +
    MOCO_DET_SEXE |  
    
    IDENT_MEN,
  
  weights = ~PONDIND,
  cluster = ~IDENT_MEN,
  data = data_notrecomp2
)

tbl_regression(modele) |>
  bold_p(t = .10) |>
  bold_labels() |>
  #italicize_levels() |>
  add_glance_source_note() |> 
  as_flex_table() |>
  font(fontname = "Garamond", part = "all") |>
  fontsize(size = 10, part = "all") |>
  autofit()



############################################.
# DEPENSES DES ADULTES #####################
############################################.
################################################################################
# 2. Sélection de la population étudiée ----
################################################################################

# Restriction aux enfants
adultes <- indiv |>
  filter(ENFANT == "2") 

################################################################################
# 3. Exploration de la variable dépendante ----
################################################################################

summary(adultes$AGE)

summary(adultes$MVETEMENTS_D)
boxplot(adultes$MVETEMENTS_D)

# Environ 35 % des dépenses sont manquantes
freq.na(adultes$MVETEMENTS_D)
freq.na(adultes$MLOISIRCUL_D)

# Une valeur extrêmement élevée (17 520 €) apparaît comme un outlier.
# Elle est remplacée par NA afin qu'elle ne domine pas l'estimation.
adultes$MVETEMENTS_D[
  adultes$MVETEMENTS_D == 7692.0
] <- NA
adultes$MVETEMENTS_D[
  adultes$MVETEMENTS_D == 6633.0
] <- NA


# Distribution brute
plot(density(adultes$MVETEMENTS_D, na.rm = TRUE))

# Distribution après transformation logarithmique
ggplot(adultes) +
  geom_density(aes(x = log(MVETEMENTS_D + 1)))

# Importance des dépenses nulles
freq(adultes$MVETEMENTS_D == 0)
mean(enfants$MVETEMENTS_D, na.rm = TRUE)

################################################################################
# 4. Préparation des données pour les régressions ----
################################################################################
freq(adultes$TAF)

data <- adultes |>
  filter(TAF != "Autre ménage (complexe)") |>
  
  # Suppression des observations sans variable dépendante
  filter(!is.na(MVETEMENTS_D)) |>
  
  # Mise à l'échelle des variables monétaires
  mutate(
    NIVIE = NIVIE / 1200,
    n_REVENUS_indiv = n_REVENUS_indiv/1200,
    n_REVENUS_indiv_H = n_REVENUS_indiv_H / 1200,
    n_REVENUS_indiv_F = n_REVENUS_indiv_F / 1200,
    # n_REVENUS_indiv_PERE = n_REVENUS_indiv_PERE / 1200,
    # n_REVENUS_indiv_MERE = n_REVENUS_indiv_MERE / 1200,
    # n_REVENUS_indiv_BEAUPARENT = n_REVENUS_indiv_BEAUPARENT / 1200,
    REVDISP = REVDISP/1200
  ) |>
  mutate(SEXE = case_when(
    SEXE == "1" ~ "Masculin", 
    SEXE == "2" ~ "Féminin"
  )) |>
  # Définition des modalités de référence
  mutate(
    
    n_FractionClasse =
      fct_relevel(n_FractionClasse, "'Petits-moyens' [C3]"),
    
    TDM8_SEXE =
      fct_relevel(
        TDM8_SEXE,
        "Couple avec uniquement enfant(s) du couple"
      ) |> droplevels(),
    
    TDM8 =
      fct_relevel(
        TDM8,
        "Couple avec uniquement enfant(s) du couple"
      ) |> droplevels(),
    
    TAF =
      fct_relevel(
        TAF,
        "Famille traditionnelle"
      ) |> droplevels(),
    
    MOCO_DET_SEXE =
      fct_relevel(
        MOCO_DET_SEXE,
        "Homme d’une famille traditionnelle"
      ) |> droplevels(),
    
    MOCO_DET =
      fct_relevel(
        MOCO_DET,
        "Adulte célibataire et sans enfant"
      ) |> droplevels()
  )
  #rec_NENFANTS(Var = "N_FRERES_SOEURS_TOUS")|>
  # Construction des classes d'âge

################################################################################
# 5. Modèle principal ----
################################################################################

# tris a plat 

design <- data %>%
  as_survey_design(
    ids = IDENT_MEN,
    weights = PONDIND
  )

modalites <- unique(na.omit(data$MOCO_DET))

resultats <- map_dfr(modalites, function(modalite) {
  
  # Sous-échantillon
  design_sous <- subset(
    design,
    TAF == modalite
  )
  
  # Moyenne pondérée
  moyenne <- svymean(
    ~MVETEMENTS_D,
    design = design_sous,
    na.rm = TRUE
  )
  
  # Quantiles pondérés
  quantiles <- svyquantile(
    ~MVETEMENTS_D,
    design = design_sous,
    quantiles = c(0.25, 0.50, 0.75),
    na.rm = TRUE,
    ci = FALSE
  )
  
  tibble(
    TAF = modalite,
    moyenne = as.numeric(coef(moyenne)),
    Q1 = as.numeric(quantiles[[1]][1]),
    mediane = as.numeric(quantiles[[1]][2]),
    Q3 = as.numeric(quantiles[[1]][3])
  )
})

# linéaire 
modele <- lm(
  log(MVETEMENTS_D + 1) ~
    n_FractionClasse + AGE + NENFANTS + NIVIE +
    MOCO_DET+n_REVENUS_indiv,
  weights = PONDIND,
  data = data
)
summary(modele)

tbl_regression(modele)|> 
  add_glance_source_note()  |>
  bold_p(t = 0.1) %>%
  as_flex_table() |>
  font(fontname = "Garamond", part = "all") |>
  fontsize(size = 10, part = "all") |>
  autofit()


modeleh <- lm(
  log(MVETEMENTS_D + 1) ~
    n_FractionClasse+ AGE + NENFANTS + MOCO_DET + NIVIE +
    MOCO_DET:n_REVENUS_indiv,
  weights = PONDIND,
  data = data,
  subset = SEXE == "Masculin"
)
summary(modeleh)

tbl_regression(modeleh)|> 
  add_glance_source_note()  |>
  bold_p(t = 0.1) %>%
  as_flex_table() |>
  font(fontname = "Garamond", part = "all") |>
  fontsize(size = 10, part = "all") |>
  autofit()

modelef <- lm(
  log(MVETEMENTS_D + 1) ~
    n_FractionClasse+ AGE + NENFANTS + MOCO_DET + NIVIE +
    MOCO_DET:n_REVENUS_indiv,
  weights = PONDIND,
  data = data,
  subset = SEXE == "Féminin"
)
summary(modelef)

tbl_regression(modelef)|> 
  add_glance_source_note()  |>
  bold_p(t = 0.1) %>%
  as_flex_table() |>
  font(fontname = "Garamond", part = "all") |>
  fontsize(size = 10, part = "all") |>
  autofit()









library(officer)
doc <- read_docx()

doc <- doc |>
  body_add_flextable(
    tbl_regression(modele) |>
      add_glance_source_note() |>
      bold_p(t = 0.1) |>
      as_flex_table() |>
      font(fontname = "Garamond", part = "all") |>
      fontsize(size = 10, part = "all") |>
      border_remove() |>
      hline_top(border = fp_border(width = 1)) |>
      hline_bottom(border = fp_border(width = 1)) |>
      autofit()
  )

print(doc, target = "resultats_regression.docx")

# add_significance_stars(
#     thresholds = c(.001, .01, .05, .10),
#     hide_ci = FALSE,
#     hide_se = TRUE,
#     hide_p = FALSE
#   )

plot(modele)

################################################################################
# 6. Test de robustesse : exclusion du 1 % supérieur des dépenses ----
################################################################################

quantile(data$MVETEMENTS_D, .99)

modele_log_sans_top <- lm(
  
  log(MVETEMENTS_D + 1) ~
    n_FractionClasse +
    SEXE +
    AGE_categories +
    N_FRERES_SOEURS_TOUS +
    RANG_ENFANT +
    NIVIE*MOCO_DET,
  weights = PONDIND,
  
  data = subset(
    data,
    MVETEMENTS_D <= quantile(data$MVETEMENTS_D, .99)
  )
)

tbl_regression(modele_log_sans_top) %>%
  bold_p(t = 0.1) %>%
  add_glance_source_note()

# Les coefficients d'intérêt restent très proches :
# les résultats semblent peu sensibles aux très fortes dépenses.

