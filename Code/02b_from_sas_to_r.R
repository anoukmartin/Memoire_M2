

### From SAS to R formats #####




################################################################################
# Import dans l'environnement ##################################################
################################################################################

## Tables info individuelles ###################################################

### Table des ménages ##########################################################

menages <- read_sas("Data_input/BDF_bind/MENAGE.sas7bdat")

names(menages) <- str_to_upper(names(menages))

summary(menages$PONDMEN)
sum(menages$PONDMEN)

menages$PONDMEN <- menages$PONDMEN / mean(menages$PONDMEN)

summary(menages$PONDMEN)
sum(menages$PONDMEN)

saveData(menages, "menages")

################################################################################

### Table des individus ########################################################

indiv <- read_sas("Data_input/BDF_bind/INDIVIDU.sas7bdat")

names(indiv) <- str_to_upper(names(indiv))

# Correction TYPEMPLOI
indiv[indiv$TRAVAIL == "1" & indiv$TYPEMPLOI == "", ]$TYPEMPLOI <- "7"

# Pondérations
indiv <- left_join(
  indiv,
  menages[, c("IDENT_MEN", "PONDMEN")]
)

summary(indiv$PONDMEN)
sum(indiv$PONDMEN)

indiv$PONDIND <- indiv$PONDMEN / mean(indiv$PONDMEN)

summary(indiv$PONDIND)
sum(indiv$PONDIND)

saveData(indiv, "indiv")

################################################################################

### Table enfants hors ménage ##################################################

enfHD <- read_sas("Data_input/BDF_bind/ENFANTHORSDOM.sas7bdat")

names(enfHD) <- str_to_upper(names(enfHD))

enfHD <- left_join(
  enfHD,
  menages[, c("IDENT_MEN", "PONDMEN")]
)

summary(enfHD$PONDMEN)

enfHD$PONDIND <- enfHD$PONDMEN / mean(enfHD$PONDMEN)

summary(enfHD$PONDIND)

saveData(enfHD, "enfHD")

################################################################################
# Tables budgets ###############################################################
################################################################################

### Dépenses ménages ###########################################################

DepMenages <- read_sas(
  "Data_input/BDF_bind/DEPMEN.sas7bdat"
)

names(DepMenages) <- str_to_upper(names(DepMenages))

saveData(DepMenages, "DepMenages")

################################################################################

### Dépenses individus #########################################################

DepIndiv <- read_sas(
  "Data_input/BDF_bind/DEPINDIV.sas7bdat"
)

names(DepIndiv) <- str_to_upper(names(DepIndiv))

saveData(DepIndiv, "DepIndiv")

################################################################################

### Consommation ###############################################################

conso <- read_sas(
  "Data_input/BDF_bind/C05.sas7bdat"
)

names(conso) <- str_to_upper(names(conso))

saveData(conso, "conso")

################################################################################
# Métadonnées ##################################################################
################################################################################

infosBDF <- NULL

infosBDF$des <- "Métadonnées de l'enquête budget de famille"
infosBDF$nom <- "Budget de famille"
infosBDF$champ <- "ménages ordinaires résidant en France"
infosBDF$vague <- "2017 et 2011"

saveData(infosBDF, label = "infosBDF")

################################################################################
# Nettoyage ####################################################################
################################################################################

rm(
  enfHD,
  indiv,
  menages,
  DepIndiv,
  DepMenages,
  conso,
  infosBDF
)

log_message("Importation terminée avec succès")
