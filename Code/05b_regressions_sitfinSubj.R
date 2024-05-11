################################################################################-
########## Tests régression situation financière ressentie #####################
################################################################################-

infosBDF <- readRDS("Data_output/infosBDF.Rds")

familles <- readRDS("Data_output/famillesToutes.Rds")
dic_fam<- look_for(familles)

# Recodages sur familles 
familles <- familles |> 
  mutate(
    n_configSynth = n_configSynth |> 
      factor(levels = unique(familles$n_configSynth)) |> 
      fct_infreq(w = familles$PONDMEN), 
    n_configFam = n_configFam |> 
      factor(levels = unique(familles$n_configFam)) |> 
      fct_infreq(w = familles$PONDMEN), 
    n_configFamTemp = n_configFamTemp |> 
      factor(levels = unique(familles$n_configFamTemp)) |> 
      fct_infreq(w = familles$PONDMEN),  
    n_ParentsMenage = n_ParentsMenage |>
      factor(levels = unique(familles$n_ParentsMenage)) |> 
      fct_infreq(w = familles$PONDMEN), 
    n_ParentsHorsDom = n_ParentsHorsDom  |>
      factor(levels = unique(familles$n_ParentsHorsDom )) |> 
      fct_infreq(w = familles$PONDMEN), 
    n_EnfantsHD = case_when(n_NEnfantsHD >= 1 ~ 1, 
                            TRUE ~0),
    NIVIE = labelled(as.numeric(NIVIE)/1000, 
                     label = "Niveau de vie du ménage (en miliers d'euros)"))

var_label(familles$n_configSynth) <- "Configuration familiale" 
var_label(familles$n_configFam) <- "Configuration familiale"
var_label(familles$n_configFamTemp) <- "Configuration familiale temporaire"
var_label(familles$n_configFamSynth) <- "Configuration familiale"
var_label(familles$n_ParentsMenage) <- "Parents des enfants du ménage"
var_label(familles$n_ParentsHorsDom) <- "Parents des enfants vivants hors domicile"
var_label(familles$n_NEnfantsHD) <- "Enfants vivants hors domicile"
var_label(familles$n_configMen) <- "Configuration"

# 1.1. Régression sur l'aisance budgétaire ressentie ###################

## Construction base de donnée sur laquelle on va travailler ###################
names(familles)
familles$n_EnfantsHD
familles$AGEPR
data <- familles |>
  mutate(AISE = if_else(AISE %in% c("", "8", "9"), NA, AISE) %>%
           as.factor() %>%
           fct_relevel("5", "4", "3", "2", "1")) %>%
  # mutate(config = case_when(
  #   n_ParentsMenage != "Sans enfants" ~ n_ParentsMenage, 
  #   TRUE ~ TYPMEN5)) 
  mutate(
    CSMEN6 = CSMEN6 %>% fct_relevel(
      "Professions intermédiaires", "CPIS", "Employés", "Ouvriers",
      "ACCE", "Agriculteurs")) 
  # mutate(n_configMenage = n_configMenage %>%
  #          fct_relevel("Parents en couple", "Mère en couple", "Père en couple",
  #                      "Mère célibataire", "Père célibataire", "Couple sans enfant",
  #                      "Femme seule", "Homme seul", "Autre type de ménage (ménage complexe)"))


data <- data %>%
  mutate(PONDFAM = PONDMEN/mean(data$PONDMEN)) # On centre la variable de pondération
class(data$NIVIE)
## Régression pondérée #########################################################

library(ordinal)

summary(data$NIVIE)
plot(data$AISE, data$NIVIE)
chisq.test(data$AISE, data$n_configMen)
chisq.test(data$AISE, data$n_EnfantsHD)



names(data)
reg <- clm(AISE ~ NIVIE + n_configMenage + n_EnfantsHD + n_ageMoyEnfMen + AGEPR + CSMEN6,
           data = data, 
           weights = PONDFAM)
tblreg3 <- tbl_regression(reg, intercept = F, exponentiate = T, 
                          label = list(n_configMenage  ~ "Configuration dans le ménage", 
                                        n_EnfantsHD ~ "Enfants vivants hors domicile", 
                                       n_ageMoyEnfMen ~ "Age moyen des enfants du ménage", 
                                       CSMEN6 ~ "CSP de la PR"))
tblreg3 

## Enregistrement des résultats ################################################
saveTableau(tblreg3, 
            type = "Reg",
            label = "aisance", 
            description = "Regression sur l'aisance budgétaire ressentie", 
            champ = paste0(infosBDF$champ, " déclarant au moins un enfant à charge"), 
            ponderation = TRUE, 
            n = reg$n)
            





