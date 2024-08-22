################################################################################-
########## Tests régression situation financière ressentie #####################
################################################################################-

infosBDF <- readRDS("Data_output/infosBDF.Rds")


menages <- readRDS("Data_output/menages.Rds") %>%
  select(IDENT_MEN, AISE, NIVIE, SEXEREP) %>%
  rec_SEXE(Var = "SEXEREP")
menages$SEXEREP
familles <- readRDS("Data_output/familles_parents.Rds") %>%
  left_join(menages, 
            by = "IDENT_MEN") 


dic_fam<- look_for(familles)
# 
# # Recodages sur familles 
# familles <- familles |> 
#   mutate(
#     n_configSynth = n_configSynth |> 
#       factor(levels = unique(familles$n_configSynth)) |> 
#       fct_infreq(w = familles$PONDMEN), 
#     n_configFam = n_configFam |> 
#       factor(levels = unique(familles$n_configFam)) |> 
#       fct_infreq(w = familles$PONDMEN), 
#     n_configFamTemp = n_configFamTemp |> 
#       factor(levels = unique(familles$n_configFamTemp)) |> 
#       fct_infreq(w = familles$PONDMEN),  
#     n_ParentsMenage = n_ParentsMenage |>
#       factor(levels = unique(familles$n_ParentsMenage)) |> 
#       fct_infreq(w = familles$PONDMEN), 
#     n_ParentsHorsDom = n_ParentsHorsDom  |>
#       factor(levels = unique(familles$n_ParentsHorsDom )) |> 
#       fct_infreq(w = familles$PONDMEN), 
#     n_EnfantsHD = case_when(n_NEnfantsHD >= 1 ~ 1, 
#                             TRUE ~0),
#     NIVIE = labelled(as.numeric(NIVIE)/1000, 
#                      label = "Niveau de vie du ménage (en miliers d'euros)"))
# 
# var_label(familles$n_configSynth) <- "Configuration familiale" 
# var_label(familles$n_configFam) <- "Configuration familiale"
# var_label(familles$n_configFamTemp) <- "Configuration familiale temporaire"
# var_label(familles$n_configFamSynth) <- "Configuration familiale"
# var_label(familles$n_ParentsMenage) <- "Parents des enfants du ménage"
# var_label(familles$n_ParentsHorsDom) <- "Parents des enfants vivants hors domicile"
# var_label(familles$n_NEnfantsHD) <- "Enfants vivants hors domicile"
# var_label(familles$n_configMen) <- "Configuration"

# 1.1. Régression sur l'aisance budgétaire ressentie ###################

## Construction base de donnée sur laquelle on va travailler ###################
names(familles)

data <- familles |>
  mutate(AISE = if_else(AISE %in% c("", "8", "9"), NA, AISE) %>%
           as.factor() %>%
           fct_relevel("5", "4", "3", "2", "1")) %>%
  mutate(n_NEnfantsMenage = if_else(is.na(n_NEnfantsMenage), 0, n_NEnfantsMenage)) %>%
  mutate(n_NEnfantsMenage13 = if_else(is.na(n_NEnfantsMenage13), 0, n_NEnfantsMenage13)) %>%
  #mutate(n_NEnfantsMenage = n_NEnfantsMenage - n_NEnfantsMenage13) %>%
  mutate(n_NEnfantsHD = if_else(is.na(n_NEnfantsHD), 0, n_NEnfantsHD)) %>%
  #rec_NENFANTS(Var = "n_NEnfantsMenage") %>%
  # rec_NENFANTS(Var = "n_NEnfantsHD") %>%
  mutate(NIVIE = NIVIE/1200) 
freq(data$n_NEnfantsMenage13)
freq(data$n_NEnfantsMenage)
freq(data$n_NEnfantsHD)

# mutate(n_configMenage = n_configMenage %>%
  #          fct_relevel("Parents en couple", "Mère en couple", "Père en couple",
  #                      "Mère célibataire", "Père célibataire", "Couple sans enfant",
  #                      "Femme seule", "Homme seul", "Autre type de ménage (ménage complexe)"))

drop
data <- data %>%
  subset(!(n_TYPMEN_sexe %in% c("Homme et femme en couple", "Homme célibataire", "Femme célibataire"))) %>%
  mutate(n_TYPMEN_sexe = droplevels(n_TYPMEN_sexe)) %>%
  mutate(PONDFAM = PONDMEN/mean(data$PONDMEN)) # On centre la variable de pondération
class(data$NIVIE)
freq(data$n_TYPMEN_sexe)

## Régression pondérée #########################################################

library(ordinal)

var_label(data$NIVIE) <- "Niveau de vie mensuel (en centaine d'euros)"
data$n_FractionClasse <- relevel(data$n_FractionClasse, "Classes moyennes superieures [C4]")
var_label(data$n_FractionClasse) <- "Fraction de classe"
var_label(data$n_NEnfantsMenage) <- "Nombre d'enfants"
var_label(data$n_AgeEnfantsMenage) <- "Age moyen des enfants"
var_label(data$n_TYPMEN_sexe) <- "Configuration parentale"
summary(data$NIVIE)
plot(data$AISE, data$NIVIE)
freq(data$n_TYPMEN_sexe)
chisq.test(data$AISE, data$n_TYPMEN_sexe)
plot(data$AISE, data$n_AgeEnfantsMenage)
freq(data$n_FractionClasse)
plot(data$n_FractionClasse, data$AISE)
data$NIVIEsq <- data$NIVIE*data$NIVIE
data$AISE
names(data)



reg <- clm(AISE ~ NIVIE + n_FractionClasse + n_NEnfantsMenage + n_AgeEnfantsMenage + n_TYPMEN_sexe,
           data = data, 
           weights = PONDFAM)
#step(reg)
summary(reg)
freq(data$n_NEnfantsMenage)
freq(data$n_TYPMEN_sexe)
tblreg3 <- tbl_regression(reg, intercept = F, exponentiate = T)
tblreg3 

## Enregistrement des résultats ################################################
saveTableau(tblreg3, 
            type = "Reg",
            label = "aisance", 
            description = "Regression sur l'aisance budgétaire ressentie", 
            champ = paste0(infosBDF$champ, " déclarant au moins un enfant à charge"), 
            ponderation = TRUE, 
            n = reg$n)
            





