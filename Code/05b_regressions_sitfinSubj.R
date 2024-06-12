################################################################################-
########## Tests régression situation financière ressentie #####################
################################################################################-

infosBDF <- readRDS("Data_output/infosBDF.Rds")


menages <- readRDS("Data_output/menages.Rds") %>%
  select(IDENT_MEN, AISE, NIVIE)
 
familles <- readRDS("Data_output/familles_parents.Rds") %>%
  left_join(menages, 
            by = "IDENT_MEN") 

familles$n_NEnfantsMenage

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
  mutate(NIVIE = NIVIE/1200) %>%
  filter(n_NEnfantsMenage >0)
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
freq(data$n_TYPMEN_sexe)
chisq.test(data$AISE, data$n_TYPMEN_sexe)
plot(data$AISE, data$n_AgeEnfantsMenage)
freq(data$n_FractionClasse)
plot(data$n_FractionClasse, data$AISE)

data$AISE
names(data)

reg <- clm(AISE ~ NIVIE + n_FractionClasse + n_NEnfantsMenage + n_AgeEnfantsMenage + n_TYPMEN_sexe,
           data = data, 
           weights = PONDFAM)
#step(reg)
summary(reg)

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
            





