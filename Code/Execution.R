
# Chemin des scripts R 
codePath <- "Code"

# Table des scripts qu'on veut exécuter dans l'ordre : 
listScript <- list.files(codePath) 
listScript <- listScript[1:3]
listScript

for (script in listScript) {
  print(paste0("Execution du script : ", script))
  source(file.path(codePath, script))
}


