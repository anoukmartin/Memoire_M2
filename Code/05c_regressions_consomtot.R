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
familles <- readRDS("Data_output/data_recode/menages_ageminmax.Rds")

data <- left_join(familles, conso[, c("IDENT_MEN", "CTOT")])

data <- data %>%
  mutate(REVTOT = REVTOT/12, 
         CTOT = CTOT/12, 
         REVDISP = REVDISP/12, 
         NIVIE = NIVIE/1200, 
         n_REVENUS_indiv_F = n_REVENUS_indiv_F/1200, 
         n_REVENUS_indiv_H = n_REVENUS_indiv_H/1200) %>%
  filter(!is.na(CTOT) & !is.na(REVDISP)) %>%
  filter(COUPLE_SEXE == "Couple de sexes différents") %>%
  mutate(TDM8_SEXE = fct_relevel(TDM8_SEXE, "Couple avec uniquement enfant(s) du couple"))

var_label(data$REVTOT) <- "Revenus totaux mensuels"
var_label(data$CTOT) <- "Consommation totale mensuelle"
var_label(data$REVDISP) <- "Revenus disponible mensuelle"
var_label(data$n_REVENUS_indiv_H) <- "Revenus masculin mensuel (en centaine)"
var_label(data$n_REVENUS_indiv_F) <- "Revenus féminin mensuel (en centaine)"
var_label(data$TDM8_SEXE) <- "Configuration du ménage"
var_label(data$NENFANTS) <- "Nombre d'enfants dans le ménage"
var_label(data$n_FractionClasse) <- "Position sociale du ménage"



data <- data %>%
  mutate(PONDFAM = PONDMEN/mean(data$PONDMEN)) # On centre la variable de pondération

## Régression pondérée #########################################################

summary(data$REVTOT)
plot(data$CTOT, data$REVTOT)
chisq.test(data$CTOT, data$TDM8_SEXE)
chisq.test(data$CTOT, data$n_EnfantsHD)
boxplot(data$CTOT)
reg <- lm(log(CTOT) ~ n_FractionClasse + NENFANTS + TDM8_SEXE*n_REVENUS_indiv_H + TDM8_SEXE*n_REVENUS_indiv_F,
          data = data, 
          weights = PONDFAM)
summary(reg)

tblreg1 <- tbl_regression(reg, intercept = T) %>%
  add_glance_source_note()  |>
  bold_p(t = 0.1) %>%
  as_flex_table() |>
  font(fontname = "Garamond", part = "all") |>
  fontsize(size = 10, part = "all") |>
  autofit()
  
tblreg1


## Enregistrement des résultats ################################################
saveTableau(tblreg3, 
            type = "Reg",
            label = "aisance", 
            description = "Regression sur l'aisance budgétaire ressentie", 
            champ = paste0(infosBDF$champ, " déclarant au moins un enfant à charge"), 
            ponderation = TRUE, 
            n = reg$n)
