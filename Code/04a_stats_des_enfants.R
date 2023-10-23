################################################################################-
######################  Situations enfants  ###################################
################################################################################-

enfantTous <- readRDS("Data_output/enfantsTous.Rds")
################################################################################-
# 1. Des tableaux croisés ######################################################
################################################################################-

## 1.1. Tableau qui résume la situation des enfants ############################

# On distingue selon que leurs parents cohabitent ou non : 

### a) construction du tableau ####
# cas des parents cohabitants
tab1 <- enfantTous %>%
  filter(n_ParentsCohab2 == unique(enfantTous$n_ParentsCohab2)[1]) %>%
  select(n_ResidParents, n_CouplePere, n_CoupleMere, n_statutResid) %>%
  tbl_summary(by = n_ResidParents) %>%
  add_overall(last = T)
# cas des parents non-cohabitants
tab2 <- enfantTous %>%
  filter(n_ParentsCohab2 == unique(enfantTous$n_ParentsCohab2)[2]) %>%
  select(n_ResidParents, n_CouplePere, n_CoupleMere, n_statutResid) %>%
  tbl_summary(by = n_ResidParents) %>%
  add_overall(last = T)
# On merge les deux tableaux 
tab <- tbl_merge(tbls = list(tab1, tab2), 
                 tab_spanner = unique(enfantTous$n_ParentsCohab2)[1:2]) 
tab # visualiser le tableau

### b) enregistrement du tableau ####
saveTableau(tableau = tab,
            label = "situationsFamEnfants",
            description = "Situations familiales des enfants",
            champ = paste0("Enfants (au sens du TCM) d'individus appartenant à des ", infosBDF$champ), 
            n = dim(enfantTous)[1], 
            ponderation = F)

rm(tab, tab1, tab2) # un peu de ménage 

## 1.2. Tableau qui résume la situation des enfants (simplifié) ################

# On distingue selon que leurs parents cohabitent ou non : 

### a) construction du tableau ####

tab <- enfantTous %>%
  select(n_situationMere, n_situationPere, n_ResidParents) %>%
  tbl_summary(by = n_ResidParents) %>%
  bold_labels() %>%
  add_overall(last = T)
tab # voir le tableau

### b) enregistrement du tableau ####
saveTableau(tableau = tab,
            label = "situationsFamEnfantsSimplifiée",
            description = "Situations familiales des enfants (simplifiée)",
            champ = paste0("Enfants (au sens du TCM) d'individus appartenant à des ", infosBDF$champ), 
            n = dim(enfantTous)[1], 
            ponderation = F)

rm(tab) # un peu de ménage

load("Resultats/tab_situationsFamEnfantsSimplifiée.rds")
tab
rm(tab)


## 1.3. Tableau des configuration familiales primaires #########################

### a) construction du tableau ####

tab <- enfantTous %>%
  select(n_configFamEnfantsP, n_statutResid) %>%
  tbl_summary(by = n_statutResid) %>%
  bold_labels() %>%
  add_overall(last = T)
tab # voir le tableau

### b) enregistrement du tableau ####
saveTableau(tableau = tab,
            label = "configPFamEnfants",
            description = "Configuration familiale principale des enfants",
            champ = paste0("Enfants (au sens du TCM) d'individus appartenant à des ", infosBDF$champ), 
            n = dim(enfantTous)[1], 
            ponderation = F)

rm(tab) # un peu de ménage

## 1.4. Tableau des configuration familiales secondaires #######################

### a) construction du tableau ####

tab <- enfantTous %>%
  select(n_configFamEnfantsS, n_statutResid) %>%
  tbl_summary(by = n_statutResid) %>%
  bold_labels() %>%
  add_overall(last = T)
tab # voir le tableau

### b) enregistrement du tableau ####
saveTableau(tableau = tab,
            label = "configSFamEnfants",
            description = "Configuration familiale secondaire des enfants",
            champ = paste0("Enfants (au sens du TCM) d'individus appartenant à des ", infosBDF$champ), 
            n = dim(enfantTous)[1], 
            ponderation = F)

rm(tab) # un peu de ménage