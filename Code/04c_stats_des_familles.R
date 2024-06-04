# ################################################################################-
# ######################  Situations menages   ###################################
# ################################################################################-
# 
# familles <- readRDS("Data_output/familles.Rds")
# dictionnaire <- look_for(familles)
# infosBDF <- readRDS("Data_output/infosBDF.Rds")
# famillesToutes <- readRDS("Data_output/famillesToutes.Rds")
# 
# 
# ################################################################################-
# # 1. Des tableaux croisés ######################################################
# ################################################################################-
# 
# ## 1.1. Tableau qui résume la situation des menages ############################
# 
# ### a) construction du tableau ####
# 
# tab <- familles %>%
#   select(NPERS, NENFANTS, n_enfantNewUnion, NCOUPLES, AGPR, APART, n_config, n_genreFam) %>%
#   tbl_summary(by = n_config) %>%
#   add_overall(last = T)
# tab # voir le tableau
# 
# ### b) enregistrement du tableau ####
# saveTableau(tableau = tab,
#             type = "tab",
#             label = "descriptionCompositionFamilles",
#             description = "Composition des ménages en fonction de la configuration familiale principale (simplifiée)",
#             champ = paste0(infosBDF$champ, " comptant au moins un membre déclarant la présence d’au moins un enfant (selon le TCM), résidant dans le logement"), 
#             n = dim(familles)[1], 
#             ponderation = F)
# 
# 
# rm(tab) # un peu de ménage
# 
# 
# tab
# rm(tab)
# 
# 
# ## 1.2. Tableau situation familiale principale et temporaire ###################
# 
# 
# ### a) construction du tableau ####
# tab <- famillesToutes %>%
#   select(NPERS, NENFANTS, n_enfantNewUnion, n_enfantNewUnionHD, NCOUPLES, AGPR, APART, n_configSynth, n_genreFam, n_genreFamTemp) %>%
#   tbl_summary(by = n_configSynth) %>%
#   add_overall(last = T)
# tab # voir le tableau
# 
# ### b) enregistrement du tableau ####
# saveTableau(tableau = tab,
#             type = "tab",
#             label = "descriptionCompositionFamillesSynth",
#             description = "Composition des ménages en fonction de la configuration familiale principale et temporaire",
#             champ = paste0(infosBDF$champ, " comptant au moins un membre déclarant la présence d’au moins un enfant (selon le TCM), que ce dernier réside dans le même logement ou non"), 
#             n = dim(famillesToutes)[1], 
#             ponderation = F)
# 
# 
# rm(tab) # un peu de ménage


## Tableau position sociale des ménages recomposés. #### 

familles <- readRDS("Data_output/familles_parents.Rds")

tab <- familles %>%
  mutate(n_FractionClasse = as.factor(n_FractionClasse)) %>%
  mutate(Ensemble = T, 
         Effectifs = 1) %>%
  as_survey_design(weights = PONDMEN) %>%
  tbl_svysummary(by = n_TYPMEN_new, 
                 include = c("n_FractionClasse", "Ensemble"), 
                 statistic = list(all_categorical() ~ "{p}")
                 )  %>%
  add_overall(last = T) %>%
  modify_header(all_stat_cols() ~ "**{level}**")
tab

### b) enregistrement du tableau ####
saveTableau(tableau = tab,
            type = "tab",
            label = "positionTypeFamiliaux",
            description = "position sociale des types familiales (fractions de classe) ",
            champ = paste0(infosBDF$champ, "dont la personne de référence ou son/sa conjoint-e est un adulte agé de 25 à 65 ans"), 
            n = dim(familles)[1], 
            ponderation = T)


rm(tab) # un peu de ménage
