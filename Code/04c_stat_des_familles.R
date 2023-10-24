################################################################################-
######################  Situations menages   ###################################
################################################################################-

familles <- readRDS("Data_output/familles.Rds")
dictionnaire <- look_for(familles)


################################################################################-
# 1. Des tableaux croisés ######################################################
################################################################################-

## 1.1. Tableau qui résume la situation des menages ############################

### a) construction du tableau ####

tab <- familles %>%
  select(NPERS, NENFANTS, n_enfantNewUnion, NCOUPLES, AGPR, APART, n_config, n_genreFam) %>%
  tbl_summary(by = n_config) %>%
  add_overall(last = T)
tab # voir le tableau

### b) enregistrement du tableau ####
saveTableau(tableau = tab,
            label = "descriptionCompositionFamilles",
            description = "Situations familiales des enfants (simplifiée)",
            champ = paste0("Enfants (au sens du TCM) d'individus appartenant à des ", infosBDF$champ), 
            n = dim(enfantTous)[1], 
            ponderation = F)

rm(tab) # un peu de ménage

load("Resultats/tab_situationsFamEnfantsSimplifiée.rds")
tab
rm(tab)
