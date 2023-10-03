
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
saveTableau <- function(tableau, label, description, champ, n, ponderation, resPath = "Resultats"){
  if(!dir.exists(resPath)) {dir.create(resPath)}
  tab <- NULL
  tab$label <- label
  tab$descrption <- description
  tab$tableau <- tableau
  tab$champ <- champ
  tab$n <- n
  tab$ponderation <- ponderation
  save(tab, 
       file = paste0(resPath, "/tab_", label, ".rds"))
}
