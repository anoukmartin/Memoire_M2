

infosBDF <- readRDS("Data_output/infosBDF.Rds")


familles <- readRDS("Data_output/data_recode/menages_ageminmax.Rds") %>%
  #select(IDENT_MEN, AISE, NIVIE, SEXEREP) %>%
  rec_SEXE(Var = "SEXEREP")
menages$SEXEREP
familles <- readRDS("Data_output/familles_parents.Rds") %>%
  left_join(menages, 
            by = "IDENT_MEN") 

familles$DNIVIE2 <- as.factor(familles$DNIVIE2)


freq(familles$n_TYPMEN_new)
data <- familles %>%
  mutate(
    SEXEREP = case_when(
      SEXEREP == "Femme" ~ 1, 
      SEXEREP == "Homme" ~ 0),
    NIVIE = NIVIE/1200) %>%
    #n_FractionClasse = relevel(n_FractionClasse, "Classes moyennes superieures [C4]")) %>%
  #filter(hetero == "Hetero") %>%
  filter(COUPLE_SEXE == "Couple de sexes différents") %>%
  mutate(TDM8 = droplevels(TDM8), 
         TDM8_SEXE = droplevels(TDM8_SEXE)) 

var_label(data$NIVIE) <- "Niveau de vie mensuel (en centaine d'euros)"
var_label(data$n_FractionClasse) <- "Fraction de classe"
var_label(data$TDM8_SEXE) <- "Configuration parentale"
var_label(data$TDM8) <- "Configuration familiale"
var_label(data$DNIVIE2) <- "Décile de niveau de vie"

data$PONDMEN <- data$PONDMEN/mean(data$PONDMEN)

tab <- data %>%
  mutate(Ensemble = "1") %>%
  as_survey_design(weights = PONDMEN) %>%
  tbl_svysummary(include = c("NIVIE", "n_FractionClasse", "TDM8", "TDM8_SEXE", "SEXEREP", "Ensemble"), 
                 by = SEXEREP, 
                 percent = "row")  %>%
  add_p()
tab

saveTableau(tab, type = "tab", 
            label = "desSexeRep",
            description = "Sexe du répondant en fonction de la classe sociale et de la configuration familiale du ménage", 
            ponderation = T, 
            champ = paste0(infosBDF$champ, " dont la personne de référence ou le conjoint est un adulte agé de 21 à 60 ans"), 
            n = nrow(data))
            
# data <- data %>%
#   subset(!(n_TYPMEN_new %in% c("Complexe", "Monoparentale", "Personne seule")))

library(forcats)

data <- data %>%
  mutate(
    n_FractionClasse = fct_relevel(
      n_FractionClasse,
      "'Petits-moyens' [C3]"
    ),
    TDM8_SEXE = fct_relevel(
      TDM8_SEXE,
      "Couple avec uniquement enfant(s) du couple"
    )
  )

reg <- glm(
  formula = SEXEREP ~ NIVIE + n_FractionClasse + TDM8_SEXE,
  data = data,
  weights = PONDMEN,
  family = "quasibinomial"
)

summary(reg)


tblreg <- tbl_regression(reg, exponentiate = T, 
                         label = list(NIVIE ~ "Niveau de vie mensuel (en centaine d'euros)", 
                                      n_FractionClasse ~ "Fraction de classe", 
                                      TDM8_SEXE~ "Configuration parentale du couple")) %>%
  bold_labels() %>%
  add_glance_source_note()
tblreg

saveTableau(tblreg, type = "reg", 
            label = "SexeRep",
            description = "Regression sur le sexe du répondant dans les couples enquétés", 
            ponderation = T, 
            champ = paste0(infosBDF$champ, " dont la personne de référence ou le conjoint est un adulte agé de 25 à 65 ans et vivant en couple (hors configuration familiale complexe)"), 
            n = nrow(data))

