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
      ) |>
      select(IDENT_MEN, NOI, VAG, MVETEMENTS_D, MLOISIRCUL_D, MREPET_D, MCOIFFUR_D)
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
  rec_TYPOLOG() %>%
  mutate(
    NIVIE = NIVIE / 1200,
    n_REVENUS_indiv_H = n_REVENUS_indiv_H / 1200,
    n_REVENUS_indiv_F = n_REVENUS_indiv_F / 1200,
    n_REVENUS_indiv_PERE = n_REVENUS_indiv_PERE / 1200,
    n_REVENUS_indiv_MERE = n_REVENUS_indiv_MERE / 1200,
    n_REVENUS_indiv_BEAUPARENT = n_REVENUS_indiv_BEAUPARENT / 1200,
    REVDISP = REVDISP/1200,
    ANNEE_BDF = as.factor(ANNEE_BDF)
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
      relevel(POSITION_FRATERIE, ref = "Aîné-e"), 
    TYPOLOG = 
      as.factor(TYPOLOG) %>%
      relevel(TYPOLOG, ref = "Toute l'année ou presque")) 


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



tab <- tableau %>%
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
  autofit() |> 
  width(j = 1, width = 2) |>
  width(j = 2:6, width = 1.0)

tab

head(tableau)
names(tableau)

# Print word : 
library(officer)

doc <- read_docx()

doc <- doc |>
  body_add_flextable(
    tab
  )

doc



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

#### MODELES #####

var_label(data$NIVIE) <- "Niveau de vie mensuel (en centaine d'euros)"
var_label(data$n_FractionClasse) <- "Position sociale du ménage"
var_label(data$MOCO_DET) <- "Situation familiale"
var_label(data$ANNEE_BDF) <- "Année d'enquête"
var_label(data$POSITION_FRATERIE) <- "Place dans l'adelphie"
var_label(data$SEXE) <- "Sexe"
var_label(data$RANG_ENFANT_SEXE) <- "Rang sexué dans l'adelphie"
var_label(data$AGE_categories) <- "Age"
var_label(data$TYPOLOG) <- "Temporalité de présence"

data <- data %>%
  mutate(VAG = str_glue("V{VAG}") %>% 
           as.character())
freq(data$VAG)
var_label(data$VAG) <- "Vague"
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


modele <- lm(
  log(MVETEMENTS_D + 1) ~
    NIVIE + n_FractionClasse +
    POSITION_FRATERIE +
    SEXE + RANG_ENFANT_SEXE +
    AGE_categories +
    TYPOLOG +
    MOCO_DET + 
    ANNEE_BDF + 
    VAG:ANNEE_BDF,
  
  weights = PONDIND,
  #cluster = IDENT_MEN,
  data = data
)




summary(modele)


reg <- tbl_regression(modele, conf.int = F)|> 
  add_glance_source_note()  |>
  bold_p(t = 0.1) %>%
  as_flex_table() |>
  font(fontname = "Garamond", part = "all") |>
  fontsize(size = 10, part = "all") |>
  autofit()
reg


doc <- doc |>
  body_add_break() |>
  body_add_flextable(
    reg |>
      border_remove() |>
      hline_top(border = fp_border(width = 1)) |>
      hline_bottom(border = fp_border(width = 1)) |>
      autofit()
  )


# add_significance_stars(
#     thresholds = c(.001, .01, .05, .10),
#     hide_ci = FALSE,
#     hide_se = TRUE,
#     hide_p = FALSE
#   )

# plot(modele)

################################################################################.
# 6. Test de robustesse : exclusion du 1 % supérieur des dépenses ----
################################################################################.

quantile(data$MVETEMENTS_D, .99)

modele_log_sans_top <- lm(
  
  log(MVETEMENTS_D + 1) ~
    NIVIE + n_FractionClasse +
    POSITION_FRATERIE +
    SEXE + RANG_ENFANT_SEXE +
    AGE_categories +
    TYPOLOG +
    MOCO_DET + 
    ANNEE_BDF + 
    VAG:ANNEE_BDF,
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
# 9. Analyse restreinte aux familles recomposées avec effets fixe ####
################################################################################.

data_recomp <- data |>
  filter(str_detect(MOCO_DET, "recomposée")) %>%
  mutate(MOCO_DET = MOCO_DET %>% as.factor() %>%
           droplevels() %>%
           relevel("Enfant du couple dans une famille recomposée"))

freq(data_recomp$MOCO_DET)
freq(data_recomp$TYPOLOG)

data_recomp2 <- data %>%
  group_by(IDENT_MEN) %>%
  filter(n_distinct(MOCO_DET, na.rm = TRUE) > 1) %>%
  mutate(MOCO_DET = MOCO_DET %>%
           as.character() %>%
           str_remove_all(" de la famille recomposée") %>% 
           str_remove_all(" dans une famille recomposée") %>% 
           as.factor()) %>%
  mutate(MOCO_DET = MOCO_DET %>%
           relevel("Enfant du couple"))


freq(data_recomp2$MOCO_DET)
var_label(data_recomp2$MOCO_DET) <- "Position dans la famille"
modele <- feols(
  
  log(MVETEMENTS_D + 1) ~
    SEXE +
    AGE_categories +
    POSITION_FRATERIE +
    RANG_ENFANT_SEXE +
    TYPOLOG + 
    MOCO_DET |  
    
    IDENT_MEN,
  
  weights = ~PONDIND,
  cluster = ~IDENT_MEN,
  data = data_recomp2
)

reg2 <- tbl_regression(modele, conf.int = F) |>
  bold_p(t = .10) |>
  bold_labels() |>
  #italicize_levels() |>
  add_glance_source_note() |>
  as_flex_table() |>
  font(fontname = "Garamond", part = "all") |>
  fontsize(size = 10, part = "all") |>
  autofit()

reg2


doc <- doc |>
  body_add_break() |>
  body_add_flextable(
    reg2 |>
      border_remove() |>
      hline_top(border = fp_border(width = 1)) |>
      hline_bottom(border = fp_border(width = 1)) |>
      autofit()
  )

print(doc, target = "Resultats/resultats_regression-depenses_vetements_enfants.docx")

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
# data_notrecomp2 <- data %>%
#   filter(!(IDENT_IND %in% data_recomp2$IDENT_IND)) %>%
#   group_by(IDENT_MEN) %>%
#   filter(n() >1) %>%
#   ungroup()


data_not_recomp2 <- data %>%
  group_by(IDENT_MEN) %>%
  filter(n() > 1) %>%
  filter(n_distinct(MOCO_DET, na.rm = TRUE) == 1) %>%
  ungroup()


modele <- feols(
  log(MVETEMENTS_D + 1) ~
    SEXE +
    AGE_categories +
    POSITION_FRATERIE +
    RANG_ENFANT_SEXE +
    TYPOLOG |  
    
    IDENT_MEN,
  
  weights = ~PONDIND,
  cluster = ~IDENT_MEN,
  data = data_not_recomp2
)

reg3 <- tbl_regression(modele, conf.int = F) |>
  bold_p(t = .10) |>
  bold_labels() |>
  #italicize_levels() |>
  add_glance_source_note() |> 
  as_flex_table() |>
  font(fontname = "Garamond", part = "all") |>
  fontsize(size = 10, part = "all") |>
  autofit()

reg3

doc <- doc |>
  body_add_break() |>
  body_add_flextable(
    reg3 |>
      border_remove() |>
      hline_top(border = fp_border(width = 1)) |>
      hline_bottom(border = fp_border(width = 1)) |>
      autofit()
  )

print(doc, target = "Resultats/resultats_regression-depenses_vetements_enfants.docx")
