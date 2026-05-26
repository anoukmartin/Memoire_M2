
################################################################################
############################### IMPORT #########################################
################################################################################

library(httr)
library(tidyverse)
library(haven)
library(janitor)

################################################################################
# Logging ######################################################################
################################################################################

log_message <- function(message, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(sprintf("[%s] [%s] %s\n", timestamp, level, message))
}

################################################################################
# Configuration ################################################################
################################################################################

config <- list(
  
  base_url = "https://sdrive.cnrs.fr/remote.php/dav/files/1426185",
  
  years = c("2011", "2017"),
  
  remote_root = "Thèse/Progédo"
)

################################################################################
# Dossiers locaux ##############################################################
################################################################################

dir.create("Data_input", showWarnings = FALSE)
dir.create("Documentation", showWarnings = FALSE)

################################################################################
# Fonction : lister fichiers WebDAV ###########################################
################################################################################

list_webdav_files <- function(url, user, password) {
  
  res <- VERB(
    "PROPFIND",
    url,
    authenticate(user, password),
    add_headers(Depth = 1)
  )
  
  stop_for_status(res)
  
  xml <- content(res, as = "text", encoding = "UTF-8")
  
  # extraction simple des href (robuste mais minimaliste)
  hrefs <- stringr::str_extract_all(xml, "(?<=<d:href>).*?(?=</d:href>)")[[1]]
  
  hrefs <- hrefs[!grepl("/$|^$|\\.pdf$|\\.sas7bdat$", hrefs) | grepl("\\.sas7bdat$", hrefs)]
  
  return(hrefs)
}

################################################################################
# Fonction téléchargement ######################################################
################################################################################

download_webdav_file <- function(url, user, password, destfile) {
  
  tryCatch({
    
    log_message(paste("Téléchargement :", basename(destfile)))
    
    res <- GET(url, authenticate(user, password))
    stop_for_status(res)
    
    writeBin(content(res, "raw"), destfile)
    
    log_message(paste("OK :", basename(destfile)))
    
  }, error = function(e) {
    log_message(paste("Erreur :", basename(destfile), ":", e$message), "ERROR")
  })
}

################################################################################
# Boucle sur les années ########################################################
################################################################################

user <- Sys.getenv("USER_SSPCLOUD")
password <- Sys.getenv("MDP_SSPCLOUD")

for (year in config$years) {
  
  log_message(paste("Année :", year))
  
  remote_dir <- paste0(
    config$base_url, "/",
    config$remote_root, "/BDF_", year, "/Donnees_SAS/"
  )
  
  local_dir <- file.path("Data_input", paste0("BDF_", year))
  dir.create(local_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Liste dynamique des fichiers
  files <- list_webdav_files(remote_dir, user, password)
  
  sas_files <- files[grepl("\\.sas7bdat$", files)]
  
  for (file_url in sas_files) {
    
    file_name <- basename(file_url)
    
    remote_url <- paste0(remote_dir, file_name)
    local_path <- file.path(local_dir, file_name)
    
    download_webdav_file(remote_url, user, password, local_path)
  }
}



################################################################################
# Documentation (si identique entre années) ####################################
################################################################################

doc_dir <- paste0(config$base_url, "/", config$remote_root, "/Documentation/")

doc_local <- file.path("Documentation")

doc_files <- list_webdav_files(doc_dir, user, password)

for (file_url in doc_files) {
  
  file_name <- basename(file_url)
  
  remote_url <- paste0(doc_dir, file_name)
  local_path <- file.path(doc_local, file_name)
  
  download_webdav_file(remote_url, user, password, local_path)
}

