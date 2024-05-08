

################################################################################-
############################ DES FONCTIONS #####################################
################################################################################-

################################################################################-
# I. Des fonctions de gestion ##################################################
################################################################################-


#' Enregistrer un tableau de données avec toutes les infos nécessaires 
#'
#' @param tableau 
#' @param label 
#' @param description 
#' @param champ 
#' @param n 
#' @param ponderation 
#' @param resPath 
#'
#' @return
#' @export
#'
#' @examples
saveTableau <- function(tableau, type, label, description, champ, n, ponderation, resPath = "Resultats"){
  if(!dir.exists(resPath)) {dir.create(resPath)}
  tab <- NULL
  tab$label <- label
  tab$descrption <- description
  tab$tableau <- tableau
  tab$champ <- champ
  tab$n <- n
  tab$ponderation <- ponderation
  save(tab, 
       file = paste0(resPath, "/", type, "_", label, ".Rdata"))
}

saveData <- function(data, label, dataPath = "Data_output"){
  if(!dir.exists(dataPath)) {dir.create(dataPath)}
  saveRDS(data, 
          file = paste0(dataPath, "/", label, ".Rds"))
}

loadData <- function(data = "all", dataPath = "Data_output"){
  listData <- list.files(dataPath) %>%
    str_remove_all(".Rds")
  if(data == "all") {
    data <- listData
  } else {
    if (any(!(data %in% listData))) {
      missing <- data[!(data %in% listData)]
      warningCondition(paste0(combine_words(data), " n'existe(nt) pas dans le dossier ", dataPath, "/"))
    } 
  }
  listData <- listData[listData %in% data]
  temp <- sapply(X = listData, 
                 FUN = function(x){
                   readRDS(file = paste0(dataPath, "/", x, ".Rds"))
                   }
  )
  return(temp)
}

loadResultats <- function(resultats = "all", resPath = "Resultats"){
  listRes <- list.files(resPath) %>%
    str_remove_all(".Rds")
  if(resultats == "all") {
    resultats <- listRes
  } else {
    if (any(!(resultats %in% listRes))) {
      missing <- resultats[!(resultats %in% listRes)]
      warningCondition(paste0(combine_words(missing), " n'existe(nt) pas dans le dossier ", resPath, "/"))
    } 
  }
  listRes <- listRes[listRes %in% resultats]
  temp <- sapply(X = listRes, FUN = function(x){
    readRDS(file = paste0(resPath, "/", x, ".Rds"))
  }
  )
  return(temp)
}


################################################################################-
# II. Des fonctions de recodages ###############################################
################################################################################-


#' Créé un identifiant individuel 
#'
#' @param data un tableau de données
#' @param IdentIndiv la variable d'identification individuelle au sein du ménage
#' @param IdentMenage la variable d'identification du méange
#' @param NewVarName le nom que l'on veut donner à la nouvelle variable
#'
#' @return data (le tableau de données) avec une nouvelle variable d'identification (nommée avec NewVarName construite par combinaison de IdentIndiv et IdentMenage 
#' @export
#'
#' @examples
#' 
#' 
var_IDENTIFIANT <- function(data, IdentIndiv, IdentMenage, NewVarName){
  data$tempIndiv <- NA
  data[, "tempIndiv"] <- data[, IdentIndiv] 
  data$tempMenage <- NA
  data[, "tempMenage"] <- data[, IdentMenage]
  data <- data %>%
    mutate(tempVar = case_when(
      str_length(tempIndiv) == 1 ~ paste0(tempMenage, "0", tempIndiv), 
      str_length(tempIndiv) == 2 ~ paste0(tempMenage, tempIndiv))) 
  names(data)[names(data) == "tempVar"] <- NewVarName
  data$tempIndiv <- NULL
  data$tempMenage <- NULL
  return(data)
}

#' Recodage de la variable couple
#'
#' @param data 
#' @param NewVar 
#'
#' @return
#' @export
#'
#' @examples
rec_COUPLE <- function(data, NewVar = FALSE) {
  data <- data %>%
    mutate(temp = case_when(
      COUPLE == "1" ~ "Oui, avec une personne qui vit dans le logement", 
      COUPLE == "2" ~ "Oui, avec une personne qui ne vit pas dans le logement", 
      COUPLE == "3" ~ "Non"))
  if(isFALSE(NewVar)){
    data$COUPLE <- data$temp 
    data$temp <- NULL
  } else { 
    names(data)[names(data) == "temp"] <- NewVar
    }
  return(data)
}

rec_TYPMEN5 <- function(data, NewVar = FALSE) {
  data <- data %>%
    mutate(temp = case_when(
      TYPMEN5 == "1" ~ "Personne seule", 
      TYPMEN5 == "2" ~ "Famille monoparentale", 
      TYPMEN5 == "3" ~ "Couple sans enfant", 
      TYPMEN5 == "4" ~ "Couple avec au moins un enfant",
      TYPMEN5 == "5" ~ "Autre type de ménage (ménage complexe)"))
  if(isFALSE(NewVar)){
    data$TYPMEN5 <- data$temp 
    data$temp <- NULL
  } else { 
    names(data)[names(data) == "temp"] <- NewVar
  }
  return(data)
}

rec_CSP6 <- function(data, Var, NewVar = FALSE) {
  data$temp <- NULL
  data[, "temp"] <- data[, Var]
  data$temp
  data <- data %>%
    mutate(temp = str_sub(temp, 1, 1)) %>%
    mutate(temp = fct_recode(temp,
                             NULL = "",
                             NULL = "0",
                             "Agriculteurs" = "1",
                             "ACCE" = "2",
                             "CPIS" = "3",
                             "Professions intermédiaires" = "4",
                             "Employés" = "5",
                             "Ouvriers" = "6",
                             "Retraités" = "7", 
                             "Autres inactifs" = "8", 
                             NULL = "H", 
                             NULL = "h"
    ))
  if(isFALSE(NewVar)){
    data[, Var] <- data[, "temp"]
    data$temp <- NULL
  } else { 
    names(data)[names(data) == "temp"] <- NewVar
  }
  return(data)
}


rec_DIP <- function(data, Var, NewVar = FALSE) {
  data$temp <- NULL
  data[, "temp"] <- data[, Var]
  data$temp
  data <- data %>%
    mutate(temp = fct_recode(temp,
      NULL = "",
      "Bac+5 et plus" = "10",
      "Bac+5 et plus" = "12",
      "Bac+5 et plus" = "20",
      "Bac+1 à Bac+3" = "30",
      "Bac+1 à Bac+3" = "31",
      "Bac+1 à Bac+3" = "33",
      "Bac et équivalents" = "41",
      "Bac et équivalents" = "42",
      "Bac et équivalents" = "43",
      "Bac et équivalents" = "44",
      "CAP, BEP et équivalents" = "50",
      "BEPC, Brevet ou en dessous" = "60",
      "BEPC, Brevet ou en dessous" = "70",
      "BEPC, Brevet ou en dessous" = "71"
    ))
  if(isFALSE(NewVar)){
    data[, Var] <- data[, "temp"]
    data$temp <- NULL
  } else { 
    names(data)[names(data) == "temp"] <- NewVar
  }
  return(data)
}


rec_AG6 <- function(data, Var = "AG6", NewVar = FALSE) {
  data$temp <- NULL
  data[, "temp"] <- data[, Var]
  data$temp
  data <- data %>%
    mutate(temp = fct_recode(temp,
                             "de 0 à 14 ans" = "00",
                             "de 15 à 29 ans" = "15",
                             "de 30 à 39 ans" = "30",
                             "de 40 à 49 ans" = "40",
                             "de 50 à 59 ans" = "50",
                             "60 ans et plus" = "60"
                             ))
  if(isFALSE(NewVar)){
    data[, Var] <- data[, "temp"]
    data$temp <- NULL
  } else { 
    names(data)[names(data) == "temp"] <- NewVar
  }
  return(data)
}
