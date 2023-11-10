

# Table des scripts qu'on veut exécuter dans l'ordre : 
listScript <- list.files("Code") 
listScript
rm(listScript)

# Exectuion 
execute <- function(part, path = "Code"){
  listScript <- list.files(path) 
  to_execute <- listScript[str_sub(listScript, 0, 2) == part]
  to_execute
  for (script in to_execute) {
    print(paste0("Execution du script : ", script))
    source(file.path(path, script), verbose = T)
  }
}


# Au démarrage #################################################################
# On exécute les scripts commençant par 00 et 01
source(file = "Code/00_requirements.R")
source(file = "Code/01a_packages.R")
source(file = "Code/01b_fonctions.R")

# Réimporter les données brutes ################################################
# On exécute les scripts commençant par 02
execute(part = "02")

# Repérages sous populations ###################################################
# On exécute les scripts commençant par 03
execute(part = "03")



