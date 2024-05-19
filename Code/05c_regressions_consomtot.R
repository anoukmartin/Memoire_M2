################################################################################-
########## Tests régression situation financière ressentie #####################
################################################################################-

infosBDF <- readRDS("Data_output/infosBDF.Rds")

familles <- readRDS("Data_output/familles_parents.Rds")
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


## Construction base de donnée sur laquelle on va travailler ###################
names(familles)

conso <- readRDS("Data_output/conso.Rds")
dic_conso <- look_for(conso)
menages <- readRDS("Data_output/menages.Rds")
data <- left_join(familles, conso[, c("IDENT_MEN", "CTOT")])
data <- left_join(data, 
                  menages %>%
                    select(NIVIE, IDENT_MEN, 
                           REVDISP, REVTOT))
data <- data %>%
  mutate(REVTOT = REVTOT/100, 
         CTOT = CTOT/100, 
         REVDISP = REVDISP/100, 
         NIVIE/100)

  filter(!is.na(CTOT) & !is.na(REVDISP) 
         & !is.na(n_ParentsMenage)
         & !is.na(n_EnfantsHD)) %>%
  filter(n_ParentsMenage != "Sans enfants")

var_label(data$REVTOT) <- "Revenus totaux (en milliers d'euros)"
var_label(data$CTOT) <- "Consommation totale (en milliers d'euros)"
var_label(data$REVDISP) <- "Revenus disponible (en milliers d'euros)"

data <- data %>%
  mutate(PONDFAM = PONDMEN/mean(data$PONDMEN)) # On centre la variable de pondération

## Régression pondérée #########################################################

summary(data$REVTOT)
plot(data$CTOT, data$REVTOT)
chisq.test(data$CTOT, data$n_TYPMEN_sexe)
chisq.test(data$CTOT, data$n_EnfantsHD)

reg <- lm(CTOT ~ NIVIE + n_TYPMEN_sexe,
          data = data, 
          weights = PONDFAM)

tblreg1 <- tbl_regression(reg, intercept = T)
tblreg1

## Enregistrement des résultats ################################################
saveTableau(tblreg3, 
            type = "Reg",
            label = "aisance", 
            description = "Regression sur l'aisance budgétaire ressentie", 
            champ = paste0(infosBDF$champ, " déclarant au moins un enfant à charge"), 
            ponderation = TRUE, 
            n = reg$n)
