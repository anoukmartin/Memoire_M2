################################################################################
############################### IMPORT #########################################
################################################################################

# Packages nécessaires #########################################################
library(httr)
library(tidyverse)
library(haven)
library(janitor)

################################################################################
# Fonction de logging ##########################################################
################################################################################

log_message <- function(message, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(sprintf("[%s] [%s] %s\n", timestamp, level, message))
}

################################################################################
# Configuration WebDAV #########################################################
################################################################################

config <- list(
  
  # URL racine WebDAV
  base_url = "https://sdrive.cnrs.fr/remote.php/dav/files/1426185",
  
  # Dossier distant contenant les données
  remote_path = "BDF",
  
  # Fichiers SAS à récupérer
  sas_files = c(
    "INDIVIDU.sas7bdat",
    "ENFANTHORSDOM.sas7bdat",
    "MENAGE.sas7bdat",
    "DEPMEN.sas7bdat",
    "DEPINDIV.sas7bdat",
    "C05.sas7bdat"
  ),
  
  # Noms locaux des fichiers
  local_sas_files = c(
    "individu.sas7bdat",
    "enfantHD.sas7bdat",
    "menages.sas7bdat",
    "depenses_menages.sas7bdat",
    "depenses_individus.sas7bdat",
    "consommation.sas7bdat"
  ),
  
  # Documentation
  doc_files = c(
    "Dictionnaire.pdf",
    "BDF 2016_ Questionnaire Visite1.pdf"
  ),
  
  local_doc_files = c(
    "Dictionnaire.pdf",
    "Questionnaire.pdf"
  )
)

################################################################################
# Création des dossiers ########################################################
################################################################################

dir.create("Data_input", showWarnings = FALSE)
dir.create("Data_input/sas", recursive = TRUE, showWarnings = FALSE)

dir.create("Documentation", showWarnings = FALSE)

################################################################################
# Fonction téléchargement WebDAV ##############################################
################################################################################

download_webdav_file <- function(url, user, password, destfile) {
  
  tryCatch({
    
    log_message(paste("Téléchargement :", basename(destfile)))
    
    res <- GET(
      url,
      authenticate(user, password)
    )
    
    stop_for_status(res)
    
    writeBin(
      content(res, "raw"),
      destfile
    )
    
    log_message(paste("Téléchargement réussi :", basename(destfile)))
    
  }, error = function(e) {
    
    log_message(
      paste("Erreur téléchargement", basename(destfile), ":", e$message),
      "ERROR"
    )
    
    stop(e)
  })
}

################################################################################
# Téléchargement des fichiers SAS #############################################
################################################################################

for (i in seq_along(config$sas_files)) {
  
  remote_url <- paste0(
    config$base_url,
    "/",
    config$remote_path,
    "/",
    config$sas_files[i]
  )
  
  local_path <- file.path(
    "Data_input/sas",
    config$local_sas_files[i]
  )
  
  download_webdav_file(
    url = remote_url,
    user = Sys.getenv("USER_SSPCLOUD"),
    password = Sys.getenv("MDP_SSPCLOUD"),
    destfile = local_path
  )
}

################################################################################
# Téléchargement documentation #################################################
################################################################################

for (i in seq_along(config$doc_files)) {
  
  remote_url <- paste0(
    config$base_url,
    "/",
    config$remote_path,
    "/",
    config$doc_files[i]
  )
  
  local_path <- file.path(
    "Documentation",
    config$local_doc_files[i]
  )
  
  download_webdav_file(
    url = remote_url,
    user = Sys.getenv("USER_SSPCLOUD"),
    password = Sys.getenv("MDP_SSPCLOUD"),
    destfile = local_path
  )
}

################################################################################
# Import dans l'environnement ##################################################
################################################################################

## Tables info individuelles ###################################################

### Table des ménages ##########################################################

menages <- read_sas("Data_input/sas/menages.sas7bdat")

names(menages) <- str_to_upper(names(menages))

summary(menages$PONDMEN)

menages$PONDMEN <- menages$PONDMEN / mean(menages$PONDMEN)

summary(menages$PONDMEN)

saveData(menages, "menages")

################################################################################

### Table des individus ########################################################

indiv <- read_sas("Data_input/sas/individu.sas7bdat")

names(indiv) <- str_to_upper(names(indiv))

# Correction TYPEMPLOI
indiv[indiv$TRAVAIL == "1" & indiv$TYPEMPLOI == "", ]$TYPEMPLOI <- "7"

# Pondérations
indiv <- left_join(
  indiv,
  menages[, c("IDENT_MEN", "PONDMEN")]
)

summary(indiv$PONDMEN)

indiv$PONDIND <- indiv$PONDMEN / mean(indiv$PONDMEN)

summary(indiv$PONDIND)

saveData(indiv, "indiv")

################################################################################

### Table enfants hors ménage ##################################################

enfHD <- read_sas("Data_input/sas/enfantHD.sas7bdat")

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
  "Data_input/sas/depenses_menages.sas7bdat"
)

names(DepMenages) <- str_to_upper(names(DepMenages))

saveData(DepMenages, "DepMenages")

################################################################################

### Dépenses individus #########################################################

DepIndiv <- read_sas(
  "Data_input/sas/depenses_individus.sas7bdat"
)

names(DepIndiv) <- str_to_upper(names(DepIndiv))

saveData(DepIndiv, "DepIndiv")

################################################################################

### Consommation ###############################################################

conso <- read_sas(
  "Data_input/sas/consommation.sas7bdat"
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
infosBDF$vague <- 2017

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