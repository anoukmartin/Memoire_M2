

# Table des scripts qu'on veut exécuter dans l'ordre : 
listScript <- list.files("Code") 
listScript


# Exectuion 
execute <- function(part, path = "Code"){
  listScript <- list.files(path) 
  to_execute <- listScript[str_starts(str_split(listScript, "_", simplify = T)[, 1], part)]
  to_execute
  for (script in to_execute) {
    print(paste0("Execution du script : ", script))
    source(file.path(path, script), verbose = T, max.deparse.length = 20000)
  }
}

# Au démarrage #################################################################
# On exécute les scripts de setup commençant par 00 et 01
source(file = "Code/01a_packages.R") # packages et options par défaut des fonctions
source(file = "Code/01b_fonctions.R") # des nouvelles fonctions utiles ici


# Réimporter les données brutes ################################################
# On exécute les scripts commençant par 02
execute(part = "02a")

# Repérages sous populations ###################################################
# On exécute les scripts commençant par 03
execute(part = "03a")
execute(part = "03b")
execute(part = "03c")
execute(part = "03d")
execute(part = "03e")

# Résultats statistiques #######################################################                                   
## Statistiques descriptives ###################################################
execute(part = "04b1")
execute(part = "04b2")
execute(part = "04c")
execute(part = "06a")
execute(part = "06b")

## Regressions #################################################################
execute(part = "04e")
execute(part = "04f")
execute(part = "05a")
execute(part = "05b")


