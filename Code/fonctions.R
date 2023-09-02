
#' Title
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