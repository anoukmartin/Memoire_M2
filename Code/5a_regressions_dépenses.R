################################################################################-
##################  Tests régression dépenses ##################################
################################################################################-

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
var_label(data$variables$n_config) <- "Configuration familiale" 
var_label(data$variables$n_configFam) <- "Configuration familiale"

conso <- readRDS("Data_output/conso.Rds")
dic_conso <- look_for(conso)

# 1.1. régression sur le montant des dépenses d'enseignement ###################

# calcul du montant de dépenses globale en lien avec la scolarité 
depScolMen <- conso |> 
  filter(IDENT_MEN %in% familles$IDENT_MEN) |> 
  select(c("IDENT_MEN"), starts_with("C10")) |> 
  select(-starts_with("C1015"))

sum <-apply(depScolMen[, -1],1,sum,na.rm=TRUE)
familles <- bind_cols(familles, n_DepEns = sum)
var_label(familles$n_DepEns) <- "Dépenses d'enseignement"
var_label(familles$n_DepEns)
rm(depScolMen, sum)

# Construction base de donnée sur laquelle on va travailler 
names(familles)
vars <- c("PONDMEN", "n_genreFam", "n_enfantNewUnion", "n_config", "n_configFam", "NIVIE", "NENFANTS", "NPERS", "CSMEN", "COEFFUC")
data <- familles |>
  select(n_DepEns, all_of(vars)) |> 
  as_survey_design(weights = PONDMEN) |> 
  mutate(
    NIVIE = labelled(NIVIE/1000, 
                     label = "Niveau de vie du ménage (en miliers d'euros)"))

var_label(data$variables$n_config) <- "Configuration familiale" 
var_label(data$variables$n_configFam) <- "Configuration familiale"

# régression pondérée 
reg <- svyglm(n_DepEns ~ NIVIE + COEFFUC + n_config,
              design = data)
tblreg1 <- tbl_regression(reg, intercept = F) |>
  add_glance_source_note() |>
  add_global_p(keep = TRUE) 
tblreg1

# 1.2. Régression vêtements et chaussures pour enfants ##########################

# calcul du montant de dépenses globale en lien avec la scolarité 
depVetmMen <- conso |> 
  filter(IDENT_MEN %in% familles$IDENT_MEN) |> 
  select(c("IDENT_MEN"), starts_with("C03123"), starts_with("C03213"))

sum <-apply(depVetmMen[, -1],1,sum,na.rm=TRUE)
familles <- bind_cols(familles, n_DepVetement = sum)
var_label(familles$n_DepVetement) <-  "Dépenses d'habillement pour enfants"
var_label(familles$n_DepVetement)
rm(depVetmMen, sum)

# Construction base de donnée sur laquelle on va travailler 
names(familles)
vars <- c("PONDMEN", "n_genreFam", "n_enfantNewUnion", "n_config", "n_configFam", "NIVIE", "NENFANTS", "NPERS", "CSMEN", "COEFFUC")
data <- familles |>
  select(n_DepVetement, all_of(vars)) |> 
  as_survey_design(weights = PONDMEN) |> 
  mutate(
    NIVIE = labelled(NIVIE/1000, 
                     label = "Niveau de vie du ménage (en miliers d'euros)"))

var_label(data$variables$n_config) <- "Configuration familiale" 
var_label(data$variables$n_configFam) <- "Configuration familiale"

# régression pondérée 
reg <- svyglm(n_DepVetement ~ NIVIE + COEFFUC + n_config,
              design = data)
tblreg2 <- tbl_regression(reg, intercept = F) |>
  add_glance_source_note() |>
  add_global_p(keep = TRUE) 



regConso <- tbl_merge(
  list(tblreg1, tblreg2), 
  tab_spanner = c("**Dépenses d'enseignement**", "**Dépenses d'habillement pour enfant**")
)
regConso
