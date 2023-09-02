

################################################################################
############################### Import #########################################
################################################################################

# Creation du fichier de données 
dir.create("Data_input")
bucket <- "anoukmartin"
objects <- get_bucket(bucket, region = "")
objects

# Import des données ------------------------------------------------------
dir.create("Data_input/sas")
save_object("BDF/INDIVIDU.sas7bdat", bucket, region = "", 
            file = "Data_input/sas/individu.sas7bdat")
save_object("BDF/ENFANTHORSDOM.sas7bdat", bucket, region = "", 
            file = "Data_input/sas/enfantHD.sas7bdat")
save_object("BDF/MENAGE.sas7bdat", bucket, region = "", 
            file = "Data_input/sas/menages.sas7bdat")


#### Tables info indiv ####

## Table des ménages 
menages <- read_sas(data_file = "Data_input/sas/menages.sas7bdat")
names(menages) <- str_to_upper(names(menages))

## Table des individus
indiv <- read_sas(data_file = "Data_input/sas/individu.sas7bdat")
names(indiv) <- str_to_upper(names(indiv))

## Table des enfants hors ménages 
enfHD <- read_sas(data_file = "Data_input/sas/enfantHD.sas7bdat")
names(enfHD) <- str_to_upper(names(enfHD))


#### Tables infos budgets  ####

