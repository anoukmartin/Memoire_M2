################################################################################-
##################  Tests régression dépenses ##################################
################################################################################-

infosBDF <- readRDS("Data_output/infosBDF.Rds")

familles <- readRDS("Data_output/familles.Rds")
dic_fam<- look_for(familles)

# Recodages sur familles 
familles <- familles |> 
  mutate(
    n_config = n_config |> 
      factor(levels = unique(familles$n_config)) |> 
      fct_infreq(),
    n_configFam = n_configFam |> 
      factor(levels = unique(familles$n_configFam)) |> 
      fct_infreq())
var_label(familles$n_config) <- "Configuration familiale" 
var_label(familles$n_configFam) <- "Configuration familiale"

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
data <- familles |>
  as_survey_design(weights = PONDMEN) |> 
  mutate(
    NIVIE = labelled(NIVIE/1000, 
                     label = "Niveau de vie du ménage (en miliers d'euros)"))

## Régression pondérée #########################################################
reg <- svyglm(n_DepEns ~ NIVIE + COEFFUC + n_config,
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
var_label(familles$n_DepVetement) <-  "Dépenses d'habillement pour enfants"
var_label(familles$n_DepVetement)
rm(depVetmMen, sum)

## Construction base de donnée sur laquelle on va travailler ################### 
names(familles)
data <- familles |>
  as_survey_design(weights = PONDMEN) |> 
  mutate(
    NIVIE = labelled(NIVIE/1000, 
                     label = "Niveau de vie du ménage (en miliers d'euros)"))

## Régression pondérée #########################################################
reg <- svyglm(n_DepVetement ~ NIVIE + COEFFUC + n_config,
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
