

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
save_object("BDF/DEPMEN.sas7bdat", bucket, region = "", 
            file = "Data_input/sas/depenses_menages.sas7bdat")
save_object("BDF/DEPINDIV.sas7bdat", bucket, region = "", 
            file = "Data_input/sas/depenses_individus.sas7bdat")
save_object("BDF/C05.sas7bdat", bucket, region = "", 
            file = "Data_input/sas/consommation.sas7bdat")

# Import de la doc 
dir.create("Documentation")
save_object("BDF/Dictionnaire.pdf", bucket, region = "", 
            file = "Documentation/Dictionnaire.pdf")
save_object("BDF/BDF 2016_ Questionnaire Visite1.pdf", bucket, region = "", 
            file = "Documentation/Questionnaire.pdf")

################################################################################-
# Import dans l'envrionnement de travail #######################################
################################################################################-

## Tables info individuelles ###################################################

### Table des ménages ####
# Import 
menages <- read_sas(data_file = "Data_input/sas/menages.sas7bdat")
# Noms de variables en majuscule 
names(menages) <- str_to_upper(names(menages))
# On centre la variable de pondération 
summary(menages$PONDMEN)
menages$PONDMEN <- menages$PONDMEN/(mean(menages$PONDMEN))
summary(menages$PONDMEN)
# On enregistre ces données
saveData(menages, "menages") 

### Table des individus ####
# Idem
indiv <- read_sas(data_file = "Data_input/sas/individu.sas7bdat")
names(indiv) <- str_to_upper(names(indiv))
# On centre la variable de pondération 
# Comme tous les individus d'un ménage sont enquêté, ils ont tous une probabilité de 1 d'être enquêté, donc leur poids est le même que celui du ménage
indiv <- left_join(indiv, menages[, c("IDENT_MEN", "PONDMEN")])
summary(indiv$PONDMEN)
indiv$PONDIND <- indiv$PONDMEN/(mean(indiv$PONDMEN))
summary(indiv$PONDIND)
saveData(indiv, "indiv")

### Table des enfants hors ménages ####
enfHD <- read_sas(data_file = "Data_input/sas/enfantHD.sas7bdat")
names(enfHD) <- str_to_upper(names(enfHD))
enfHD <- left_join(enfHD, menages[, c("IDENT_MEN", "PONDMEN")])
summary(enfHD$PONDMEN)
enfHD$PONDIND <- enfHD$PONDMEN/(mean(enfHD$PONDMEN))
summary(enfHD$PONDIND)
saveData(enfHD, "enfHD")


## Tables infos budgets  #######################################################

### Table dépenses du ménage ####
DepMenages <- read_sas(data_file = "Data_input/sas/depenses_menages.sas7bdat")
names(DepMenages) <- str_to_upper(names(DepMenages))
saveData(DepMenages, "DepMenages")

### Table dépenses des individus ####
DepIndiv <- read_sas(data_file = "Data_input/sas/depenses_individus.sas7bdat")
names(DepIndiv) <- str_to_upper(names(DepIndiv))
saveData(DepIndiv, "DepIndiv")

### Table des dépenses de consommatuon ####
conso <- read_sas(data_file = "Data_input/sas/consommation.sas7bdat")
names(conso) <- str_to_upper(names(conso))
saveData(conso, "conso")


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
rm(enfHD, indiv, menages, DepIndiv, DepMenages, conso, infosBDF, bucket)

