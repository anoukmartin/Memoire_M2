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
            

# 1.2. Régression vêtements et chaussures pour enfants #########################

## calcul du montant de dépenses globale en lien avec la scolarité #############
depVetmMen <- conso |> 
  filter(IDENT_MEN %in% familles$IDENT_MEN) |> 
  select(c("IDENT_MEN"), starts_with("C03123"), starts_with("C03213"))

sum <-apply(depVetmMen[, -1],1,sum,na.rm=TRUE)
familles <- bind_cols(familles, n_DepVetement = sum)
var_label(familles$n_DepVetement) <-  "Montant des dépenses d'habillement pour enfants"
var_label(familles$n_DepVetement)
rm(depVetmMen, sum)


## Construction base de donnée sur laquelle on va travailler ################### 
dicFam <- look_for(familles)
data <- familles |>
  mutate(
    n_DepVetementParEnf = labelled(n_DepVetement/n_NEnfantsTous, 
                                   label = "Montant des dépenses d'habillement pour enfants (moyenne par enfant)"), 
    n_DepVetementPart = labelled((n_DepVetement/REVTOT)*100, 
                            label = "Part des dépenses d'habillement pour enfants des les revenus totaux du ménage"), 
    NIVIE = labelled(NIVIE/100, 
                     label = "Niveau de vie du ménage (en centaines d'euros)"), 
    REVTOT = labelled(REVTOT/100, 
                      label = "Revenus totaux du ménage (en centaines d'euros)"), 
    REVDISP = labelled(REVDISP/100, 
                      label = "Revenu disponible du ménage (en centaines d'euros)"))|>
  mutate(PONDFAM = PONDMEN/mean(familles$PONDMEN)) |>  # On centre la variable de pondération
  as_survey_design(weights = PONDFAM, ids = c(IDENT_MEN)) 

## Régression pondérée #########################################################

reg <- svyglm(n_DepVetementParEnf ~ REVDISP + n_NEnfantsTous + n_ageMoyEnfTous + CSMEN + DIP14PR + n_configSynth,
              design = data)

tblreg2 <- tbl_regression(reg, intercept = F) |>
  add_glance_source_note() |>
  add_global_p(keep = TRUE) 
tblreg2

## Enregistrement des résultats ################################################
saveTableau(tblreg2, 
            type = "Reg",
            label = "DepHabillement", 
            description = "Regression sur les dépenses d'habillement pour enfants", 
            champ = paste0(infosBDF$champ, " déclarant au moins un enfant à charge"), 
            ponderation = TRUE, 
            n = dim(familles)[1])
