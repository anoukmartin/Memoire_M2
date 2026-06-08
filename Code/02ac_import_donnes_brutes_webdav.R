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
  remote_path = "Thèse/Progédo/BDF_2017",
  
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
    config$remote_path, "/Donnees_SAS/",
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
    config$remote_path, "/Documentation/",
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





