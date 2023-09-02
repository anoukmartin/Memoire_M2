
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

rec_COUPLE <- function(data, NewVar = FALSE) {
  data <- data %>%
    mutate(temp = fct_recode(COUPLE, 
      "Oui, avec une personne qui vit dans le logement" = "1", 
      "Oui, avec une personne qui ne vit pas dans le logement" = "2", 
      "Non" = "3"))
  if(isFALSE(NewVar)){
    data$COUPLE <- data$temp 
    data$temp <- NULL
  } else { 
    names(data)[names(data) == "temp"] <- NewVar
    }
  return(data)
}
