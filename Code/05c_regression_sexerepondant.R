

infosBDF <- readRDS("Data_output/infosBDF.Rds")


familles <- readRDS("Data_output/data_recode/menages_ageminmax.Rds") %>%
  #select(IDENT_MEN, AISE, NIVIE, SEXEREP) %>%
  rec_SEXE(Var = "SEXEREP")





data <- familles %>%
  mutate(
    SEXEREP = case_when(
      SEXEREP == "Femme" ~ 1, 
      SEXEREP == "Homme" ~ 0),
    NIVIE = NIVIE/1200) %>%
  filter(TDM8_SEXE != "Autre ménage (complexe)")%>%
  filter(COUPLE_SEXE == "Couple de sexes différents")%>%
  mutate(TDM8 = droplevels(TDM8), 
         TDM8_SEXE = droplevels(TDM8_SEXE)) 

var_label(data$NIVIE) <- "Niveau de vie mensuel (en centaine d'euros)"
var_label(data$n_FractionClasse) <- "Fraction de classe"
var_label(data$TDM8_SEXE) <- "Configuration parentale"
var_label(data$TDM8) <- "Configuration familiale"
var_label(data$DNIVIE2) <- "Décile de niveau de vie"

data$PONDMEN <- data$PONDMEN/mean(data$PONDMEN)

tab <- data %>%
  mutate(Ensemble = T) %>%
  as_survey_design(weights = PONDMEN) %>%
  tbl_svysummary(include = c("NIVIE", "n_FractionClasse", "TDM8_SEXE", "SEXEREP", "Ensemble"), 
                 by = SEXEREP, 
                 percent = "row")  %>%
  add_p()
tab

saveTableau(tab, type = "tab", 
            label = "desSexeRep",
            description = "Sexe du répondant en fonction de la classe sociale et de la configuration familiale du ménage", 
            ponderation = T, 
            champ = paste0(infosBDF$champ, "formé par deux individus de sexe différent et dont au moins l'un d'entre eux est agé de 21 à 60 ans"), 
            n = nrow(data))


tab <- data %>%
  mutate(Ensemble = "1") %>%
  filter(TDM8_SEXE != "Autre ménage (complexe)")%>%
  filter(COUPLE_SEXE == "Couple de sexes différents")%>%
  as_survey_design(weights = PONDMEN) %>%
  tbl_svysummary(include = c("NIVIE", "n_FractionClasse",  "n_RevenusContribF", "Ensemble"), 
                 by = n_RevenusContribF, 
                 percent = "row")  %>%
  add_p()
tab
            
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
    ),
    n_RevenusContribF = fct_relevel(
      n_RevenusContribF,
      "Entre 40 et 50%"
    )
  )
freq(data$n_RevenusContribF)
reg <- glm(
  formula = SEXEREP ~ NIVIE + n_FractionClasse + n_RevenusContribF + TDM8_SEXE ,
  data = data,
  weights = PONDMEN,
  family = "quasibinomial"
)

summary(reg)

agemax
tblreg <- tbl_regression(reg, exponentiate = T, 
                         label = list(NIVIE ~ "Niveau de vie mensuel (en centaine d'euros)", 
                                      n_FractionClasse ~ "Fraction de classe", 
                                      TDM8_SEXE~ "Configuration parentale du couple")) %>%
  bold_p(t = 0.1)%>%
  bold_labels() %>%
  add_glance_source_note()
tblreg

saveTableau(tblreg, type = "reg", 
            label = "SexeRep",
            description = "Regression sur le sexe du répondant dans les couples enquétés", 
            ponderation = T, 
            champ = paste0(infosBDF$champ, " dont la personne de référence ou le conjoint est un adulte agé de 25 à 65 ans et vivant en couple (hors configuration familiale complexe)"), 
            n = nrow(data))

tbl
