################################################################################-
##################  Tests régression dépenses ##################################
################################################################################-

infosBDF <- readRDS("Data_output/infosBDF.Rds")

familles <- readRDS("Data_output/famillesToutes.Rds")
dic_fam<- look_for(familles)

# Recodages sur familles 
familles <- familles |> 
  mutate(
    n_configSynth = n_configSynth |> 
      factor(levels = unique(familles$n_configSynth)) |> 
      fct_infreq(),
    n_configFam = n_configFam |> 
      factor(levels = unique(familles$n_configFam)) |> 
      fct_infreq(), 
    n_configFamTemp = n_configFamTemp |> 
      factor(levels = unique(familles$n_configFamTemp)) |> 
      fct_infreq())
var_label(familles$n_configSynth) <- "Configuration familiale" 
var_label(familles$n_configFam) <- "Configuration familiale"
var_label(familles$n_configFamTemp) <- "Configuration familiale temporaire"
var_label(familles$n_configFamSynth) <- "Configuration familiale"

conso <- readRDS("Data_output/conso.Rds")
dic_conso <- look_for(conso)

# 1.1. Régression sur le montant des dépenses d'enseignement ###################

## calcul du montant de dépenses globale en lien avec la scolarité #############
depScolMen <- conso |> 
  filter(IDENT_MEN %in% familles$IDENT_MEN) |> 
  select(c("IDENT_MEN"), starts_with("C10")) |> 
  select(-starts_with("C1015"))

sum <-apply(depScolMen[, -1],1,sum,na.rm=TRUE)
familles <- bind_cols(familles, n_DepEns = sum)
var_label(familles$n_DepEns) <- "Dépenses d'enseignement"
var_label(familles$n_DepEns)
rm(depScolMen, sum)

## Construction base de donnée sur laquelle on va travailler ###################
names(familles)
familles$n_NEnfantsTous
data <- familles |>
  mutate(n_configMenage =   fct_relevel(n_configMenage,
    "Parents en couple", "Mère en couple", "Père en couple",
    "Mère célibataire", "Père célibataire", "Couple sans enfant",
    "Femme seule", "Homme seul", "Autre type de ménage (ménage complexe)"
  )) |>
  mutate(PONDFAM = PONDMEN/mean(familles$PONDMEN)) %>% # On centre la variable de pondération
  as_survey_design(weights = PONDFAM) |> 
  mutate(
    NIVIE = labelled(NIVIE/1000, 
                     label = "Niveau de vie du ménage (en miliers d'euros)"), 
    n_DepEnsPart = labelled(n_DepEns/REVTOT, 
                              label = "Part des dépenses d'enseignement des les revenus totaux du ménage"), 
    n_DepEnsEnfant = labelled(n_DepEns/n_NEnfantsTous, 
                              label = "Montant moyen des dépenses d'enseignement par enfant"), 
    REVTOT = labelled(REVTOT/1000, 
                            label = "Revenus totaux du ménage (en miliers d'euros)")) %>%
  mutate(
    CSMEN6 = CSMEN6 %>% fct_relevel(
    "Professions intermédiaires", "CPIS", "Employés", "Ouvriers",
    "ACCE", "Agriculteurs"))

iorder(data$n_configMenage)
names(familles)

iorder(data$CSMEN6)
## Régression pondérée #########################################################
reg <- svyglm(n_DepEns ~ NIVIE + n_NEnfantsTous + n_configMenage,
              design = data)
tblreg1 <- tbl_regression(reg, intercept = F) |>
  add_glance_source_note() |>
  add_global_p(keep = TRUE)

tblreg1 

## Enregistrement des résultats ################################################
saveTableau(tblreg1, 
            type = "Reg",
            label = "DepEnseignement", 
            description = "Regression sur les dépenses d'enseignement du ménage", 
            champ = paste0(infosBDF$champ, " déclarant au moins un enfant à charge"), 
            ponderation = TRUE, 
            n = dim(familles)[1])
            

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
