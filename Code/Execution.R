

# Table des scripts qu'on veut exécuter dans l'ordre : 
listScript <- list.files("Code") 
listScript
rm(listScript)

# Au démarrage #################################################################
# On exécute les scripts commençant par 01
execute(part = "01")

# Réimporter les données brutes ################################################
# On exécute les scripts commençant par 02
execute(part = "02")

# Repérages sous populations ###################################################
## Situations familiales des enfants ####
# On exécute les scripts commençant par 03
execute(part = "03")



