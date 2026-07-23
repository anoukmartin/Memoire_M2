################################################################################
# RÉGRESSION DES DÉPENSES INDIVIDUELLES DE VÊTEMENTS DES ENFANTS ###############
################################################################################

################################################################################
# 1. Chargement des données ----
################################################################################

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
# 2. Sélection de la population étudiée ----
################################################################################

# Restriction aux enfants
enfants <- indiv |>
  filter(ENFANT == "1")

################################################################################
# 3. Exploration de la variable dépendante ----
################################################################################

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
mean(enfants$MVETEMENTS_D == 0, na.rm = TRUE)

################################################################################
# 4. Préparation des données pour les régressions ----
################################################################################
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

################################################################################
# 5. Modèle principal ----
################################################################################

modele <- lm(
  MVETEMENTS_D ~
    NIVIE +
    n_FractionClasse +
    SEXE +
    AGE_categories +
    N_FRERES_SOEURS_TOUS +
    RANG_ENFANT +
    NIVIE+
    MOCO_DET,
  weights = PONDIND,
  data = data
)

modele <- lm(
  log(MVETEMENTS_D + 1) ~
    NIVIE + n_FractionClasse +
    POSITION_FRATERIE +
    SEXE + RANG_ENFANT_SEXE +
    AGE_categories +
    MOCO_DET,
  
  weights = PONDIND,
  data = data
)

summary(modele)

tbl_regression(modele)|> 
  add_glance_source_note() |>
  bold_p(t = 0.1)
  
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

################################################################################
# 7. Variante : prise en compte du statut légal des enfants ----
################################################################################

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


################################################################################
# 8. Les revenus des adultes expliquent-ils les dépenses ?
################################################################################

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

...

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

...

################################################################################
# 9. Analyse restreinte aux familles recomposées
################################################################################

data_recomp <- data |>
  filter(str_detect(MOCO_DET, "recomposée"))

freq(data_recomp$MOCO_DET)

# Dans les familles recomposées, on cherche à déterminer
# si les dépenses sont davantage associées au revenu
# du parent biologique ou de celui du beau-parent.

modele <- lm(
  
  log(MVETEMENTS_D + 1) ~
    n_FractionClasse +
    SEXE +
    AGE_categories +
    N_FRERES_SOEURS_TOUS +
    RANG_ENFANT +
    NIVIE +
    MOCO_DET_SEXE:n_REVENUS_indiv_PERE +
    MOCO_DET_SEXE:n_REVENUS_indiv_BEAUPARENT,
  
  weights = PONDIND,
  data = data_recomp
)

tbl_regression(modele) |>
  bold_p(t = .10) |>
  bold_labels() |>
  italicize_levels() |>
  add_glance_source_note()