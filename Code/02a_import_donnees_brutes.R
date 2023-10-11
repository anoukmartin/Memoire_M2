

################################################################################-
############################### IMPORT #########################################
################################################################################-


################################################################################-
# Import depuis le stockage SSP Cloud ##########################################
################################################################################-
# Création du fichier de données 
dir.create("Data_input")
bucket <- "anoukmartin"
objects <- get_bucket(bucket, region = "")
objects

# Import des données
dir.create("Data_input/sas")
save_object("BDF/INDIVIDU.sas7bdat", bucket, region = "", 
            file = "Data_input/sas/individu.sas7bdat")
save_object("BDF/ENFANTHORSDOM.sas7bdat", bucket, region = "", 
            file = "Data_input/sas/enfantHD.sas7bdat")
save_object("BDF/MENAGE.sas7bdat", bucket, region = "", 
            file = "Data_input/sas/menages.sas7bdat")


################################################################################-
# Import dans l'envrionnement de travail #######################################
################################################################################-

## Tables info individuelles ###################################################

### Table des ménages ####
menages <- read_sas(data_file = "Data_input/sas/menages.sas7bdat")
names(menages) <- str_to_upper(names(menages))
saveData(menages, "menages") 

### Table des individus ####
indiv <- read_sas(data_file = "Data_input/sas/individu.sas7bdat")
names(indiv) <- str_to_upper(names(indiv))
saveData(indiv, "indiv")

### Table des enfants hors ménages ####
enfHD <- read_sas(data_file = "Data_input/sas/enfantHD.sas7bdat")
names(enfHD) <- str_to_upper(names(enfHD))
saveData(enfHD, "enfHD")


## Tables infos budgets  #######################################################

rm(objects)

################################################################################-
# Métadonnées de l'enquête #####################################################
################################################################################-
infosBDF <- NULL
infosBDF$des <- "Métadonnées de l'enquête budget de famille"
infosBDF$nom <- "Budget de famille"
infosBDF$champ <- "ménages ordinaires résidant en France"
infosBDF$vague <- 2017
saveData(infosBDF, label = "infosBDF")

# du ménage ###################################################################
rm(enfHD, indiv, menages, infosBDF, bucket)
