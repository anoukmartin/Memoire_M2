


################################################################################- 
#########################  REPERAGE SOUS-POP  ##################################
################################################################################- 

# On adopte une approche par les enfants, parce que c'est ce qui est usuel dans 
# dans la quantification du nombre de familles recomposées


################################################################################-
# 1. Enfants enquêtés dans le ménage ###########################################
################################################################################-
# On va regarder avec qui vivent les enfants appartenant à l'enquête pour 
# voir si il s'agit de configuration familiales recomposées

names(indiv)
indiv$IDENT_IND

## 1.1 Les enfants : au sens du TCM et - de 25 ans #############################
# On définit qui est considéré comme des enfants 
indiv$ENFRP # Variable enfant au sens du recensement
indiv$ENFANT # Variable au sens du TCM (critère du budget commun en plus)
tbl_cross(indiv, ENFANT, ENFRP) # On regarde si elles se recoupent 
temp <- indiv %>%
  filter(ENFANT == "2" & ENFRP == "1")
# Donc on a trois individus assez âgés qui sont considérés comme enfant au sens 
# du RP mais pas du TCM, de toutes façon on va mettre un age limite pour être 
# considéré comme enfant, car on peut faire l'hypothèse que un enfant est à 
# charge économique d'au moins un adulte, ce qui va dans le sens de prendre le 
# critère du budget commun. Propositon du seuil de 25 ans car ouverture de bcp 
# de droits sociaux (RSA...)
rm(temp)
# Donc limite d'âge 
enfants <- indiv %>%
  filter(AG <= 25)  %>% # on peut changer en le seuil, 25 ans me paraît bien seuil d'ouverture du RSA 
  filter(ENFANT == "1") # puis on ne prend que ceux qui sont considérés comme enfant au sens du TCM (idem recensement car on virer les individus agés)

# Ensuite on va regarder avec qui vivent ces enfants
enfants <- enfants %>%
  mutate(n_NPARENTS = case_when(
    PER1E == "1" & MER1E == "1" ~ "les deux",
    PER1E == "1" & MER1E != "1" ~ "le père uniquement",
    PER1E != "1" & MER1E == "1" ~ "la mère uniquement",
    PER1E != "1" & MER1E != "1" ~ "aucun des deux"))
tbl_summary(enfants, 
            include = n_NPARENTS)
# Aucun enfants ne vit avec aucun de ses parents,
# enfant vivant avec uniquement leur mère sur représentés, parce que familles 
# mono sont plus souvent des mères isolées que des pères, + sur-échantillon 
# dans l’enquête BDF

# On récupère l'id individuel du père, de la mère et de l'enfant
tbl_summary(enfants, 
            include = c("NOI", "MER2E", "PER2E"), 
            type = list(everything() ~ "categorical"))
# Comme les identifiants individuels peuvent avoir 1 ou plusieurs chiffres, on ajoute des "0"
# On utilise cette fonction pour faire ces identifiants individuels
enfants <- enfants %>%
  var_IDENTIFIANT(IdentIndiv = "NOI", IdentMenage = "IDENT_MEN", NewVarName = "n_IdentIndiv") %>%
  var_IDENTIFIANT(IdentIndiv = "PER2E", IdentMenage = "IDENT_MEN", NewVarName = "n_IdentPere") %>%
  var_IDENTIFIANT(IdentIndiv = "MER2E", IdentMenage = "IDENT_MEN", NewVarName = "n_IdentMere")

## 1.2. Les parents de ces enfants #############################################
# On crée une table pour les parents de ces enfants 
parents <- indiv %>%
  var_IDENTIFIANT(IdentIndiv = "NOI", IdentMenage = "IDENT_MEN" , NewVarName = "n_IdentParent") %>%
  select(n_IdentParent, COUPLE, SEXE)
# on regarde s'ils sont en couple
parents %>%
  select(COUPLE) %>%
  rec_COUPLE() %>%
  tbl_summary()
  
## 1.3. Situation conjugale des parents des enfants ###########################
# On joint la table des parents sur les identifiants des pères et des mères 
enfants2 <- enfants %>%
  left_join(parents %>% rec_COUPLE(NewVar = "n_CouplePere") %>% select(-COUPLE, -SEXE),
            by = c("n_IdentPere" = "n_IdentParent")) %>%
  left_join(parents %>% rec_COUPLE(NewVar = "n_CoupleMere") %>% select(-COUPLE, -SEXE),
            by = c("n_IdentMere" = "n_IdentParent"))
enfants <- enfants2
rm(enfants2)

# On regarde la situation conjugale des parents en fonction d'avec qui l'enfant vit
enfants %>%
  select(n_NPARENTS, n_CouplePere, n_CoupleMere) %>%
  tbl_summary(by = "n_NPARENTS") 
  

################################################################################-
# 2. Les enfants hors-domicile #################################################
################################################################################-
# Enfants hors domicile, qui ne sont pas enquêtés dans le ménage, mais on a des 
# infos sur eux remplis par leur parents (présents dans les ménages enquêtés)
# cela permet de savoir qui vit "virtuellement" dans des familles recomposées, 
# par exemple enfant qui vit chez sa mère mais dont le père est remis en couple 
# on peut se dire qu'il s'agit d'un ménage 

## 2.1. Les enfants : moins de 25 ans ##########################################
# Donc comme pour les enfants appartenant aux ménages enquêtés, on applique une 
# limite d'âge.
enfantHD <- enfHD %>%
  mutate(AG = 2017 - HODAN) %>% # Calcul de l'age au 31 décembre
  filter(AG <= 25) %>% # on peut changer en le seuil, 25 ans me paraît bien seuil d'ouverture du RSA 
  var_IDENTIFIANT(IdentIndiv = "NUMORDRE", IdentMenage = "IDENT_MEN", NewVarName = "n_IdentIndiv")
names(enfantHD)

## 2.2. Les parents de ces enfants ##############################################
temp <- enfantHD %>%
  # On fait une ligne par parent d'enfant hors domicile auquel on donne un identifiant
  select(starts_with("HODLN"), c("IDENT_MEN", "n_IdentIndiv")) %>%
  pivot_longer(cols = starts_with("HODLN")) %>%
  filter(value == "1") %>%
  mutate(name = str_remove(name, "HODLN")) %>%
  select(-value) %>% 
  var_IDENTIFIANT(IdentIndiv = "name", IdentMenage = "IDENT_MEN", NewVarName = "n_IdentParent") %>%
  # On récupère les infos sur les parents
  left_join(parents) %>%
  mutate(SEXE = fct_recode(SEXE, 
                           "pere" = "1", 
                           "mere" = "2")) %>%
  # On repasse sur un format une ligne par enfant avec l'info sur leurs parents
  pivot_wider(id_cols = n_IdentIndiv, 
              values_from = n_IdentParent,
              names_from = SEXE) %>%
  rename(n_IdentPere = pere, 
         n_IdentMere = mere) %>%
  filter(!(n_IdentIndiv %in% c("0486201", "0486202"))) %>% # On vire le cas de deux enfants qui ont deux pères à voir ce qu'on fait des familles homoparentales
  mutate(n_IdentPere = as.character(n_IdentPere), 
         n_IdentMere = as.character(n_IdentMere)) %>%
  mutate(n_IdentPere = if_else(n_IdentPere == "NULL", NA, n_IdentPere), 
         n_IdentMere = if_else(n_IdentMere == "NULL", NA, n_IdentMere)) %>%
  left_join(parents %>% rec_COUPLE(NewVar = "n_CouplePere") %>% select(-COUPLE, -SEXE),
            by = c("n_IdentPere" = "n_IdentParent")) %>%
  left_join(parents %>% rec_COUPLE(NewVar = "n_CoupleMere") %>% select(-COUPLE, -SEXE),
            by = c("n_IdentMere" = "n_IdentParent")) %>%
  mutate(n_ParentsCohab = if_else(
    str_sub(n_IdentMere, 1, 5) == str_sub(n_IdentPere, 1, 5), "Oui", "Non", missing = "Non")) %>%
  # On récupère des infos sur où vivent les enfants 
  left_join(enfantHD %>% select(n_IdentIndiv, HODCO)) %>%
  mutate(n_NPARENTS = case_when(
    HODCO == "3" & !is.na(n_IdentPere) ~ "Enfant résidant chez sa mère", 
    HODCO == "3" & !is.na(n_IdentMere) ~ "Enfant résidant chez son père", 
    HODCO != "3" ~ "Enfant résidant hors domicile(s) des parents"))
head(temp)

## 2.3. Situation conjugale des parents de enfants ############################# 
enfantHD <- enfantHD %>%
  left_join(temp)
names(enfantHD)
rm(temp)
enfantHD %>%
  select(n_ParentsCohab, n_CouplePere, n_CoupleMere) %>%
  tbl_summary(by = "n_ParentsCohab")


################################################################################-
# 3. Tous les enfants ##########################################################
################################################################################-

## 3.1. Un seul tableau ########################################################

# On combine les deux tableaux de données 
enfantTous <- bind_rows(
  enfants %>% 
    select(starts_with("n_")) %>% 
    mutate(n_statutResid = "Enfant du ménage (au sens du TCM)"), 
  enfantHD %>% 
    select(starts_with("n_")) %>% 
    mutate(n_statutResid = "Enfant résidant hors domicile"))
names(enfantTous)

# On harmonise certaines variables
enfantTous <- enfantTous %>%
  # Avec qui réside l'enfant 
  mutate(n_ResidParents = case_when(
    n_statutResid == "Enfant résidant hors domicile" ~ n_NPARENTS,
    n_NPARENTS == "la mère uniquement" ~ "Enfant résidant chez sa mère", 
    n_NPARENTS == "le père uniquement" ~ "Enfant résidant chez son père", 
    n_NPARENTS == "les deux" ~ "Enfant résidant chez ses deux parents")) %>%
  # Est ce que les parents cohabitent ? 
  mutate(n_ParentsCohab = case_when(
    n_statutResid == "Enfant résidant hors domicile" ~ n_ParentsCohab, 
    n_NPARENTS == "la mère uniquement" ~ "Non", 
    n_NPARENTS == "le père uniquement" ~ "Non", 
    n_NPARENTS == "les deux" ~ "Oui")) %>%
  mutate(n_ParentsCohab2 = case_when(
    n_ParentsCohab == "Oui" ~ "Parents cohabitants", 
    n_ParentsCohab == "Non" ~ "Parents non-cohabitants")) %>%
  # On crée aussi un recodage qui simplifie la situation conjugale des parents
    # On ne va considérer que les cas ou un nouveau couple cohabite
    # On néglige les deux cas ou les parents cohabitent mais ne se déclarent pas en couple 
  mutate(
    n_situationMere = case_when(
      n_ParentsCohab == "Oui" ~ "En couple avec l'autre parent", 
      n_ParentsCohab == "Non" & n_CoupleMere ==  "Oui, avec une personne qui vit dans le logement" ~ "En couple avec une autre personne", 
      n_ParentsCohab == "Non" & n_CoupleMere !=  "Oui, avec une personne qui vit dans le logement" ~ "Célibataire ou en couple non-cohabitant"), 
    n_situationPere = case_when(
      n_ParentsCohab == "Oui" ~ "En couple avec l'autre parent", 
      n_ParentsCohab == "Non" & n_CouplePere ==  "Oui, avec une personne qui vit dans le logement" ~ "En couple avec une autre personne", 
      n_ParentsCohab == "Non" & n_CouplePere !=  "Oui, avec une personne qui vit dans le logement" ~ "Célibataire ou en couple non-cohabitant")) %>%
  # On labellise les colonnes 
  mutate(n_ResidParents = labelled(n_ResidParents, label = "Enfant résidant avec"), 
         n_NPARENTS = labelled(n_ResidParents, label = "Enfant résidant avec"), 
         n_statutResid = labelled(n_statutResid, label = "Statut résidentiel dans l'enquête"), 
         n_ParentsCohab = labelled(n_ParentsCohab, label = "Cohabitation des parents"), 
         n_ParentsCohab2 = labelled(n_ParentsCohab2, label = "Cohabitation des parents"), 
         n_CoupleMere = labelled(n_CoupleMere, label = "Mère en couple"), 
         n_CouplePere = labelled(n_CouplePere, label = "Père en couple"), 
         n_situationMere = labelled(n_situationMere, 
                                    label = "Situation conjugale de la mère"), 
         n_situationPere = labelled(n_situationPere, 
                                    label = "Situation conjugale du père"))
  

names(enfantTous)

  
## 3.2. Tableau qui résume la situation des enfants ############################

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

## 3.3. Tableau qui résume la situation des enfants (simplifié) ############################

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


enfantTous <- enfantTous %>%
  mutate(
    n_configFamEnfants = case_when(
      n_ResidParents == "Enfant résidant chez ses deux parents" ~ "Configuration traditionelle", 
      (n_ResidParents == "Enfant résidant chez sa mère" & n_situationMere == "En couple avec une autre personne") 
      | (n_ResidParents == "Enfant résidant chez son père" & n_situationPere == "En couple avec une autre personne") ~ "Configuration recomposée",
      (n_ResidParents == "Enfant résidant chez sa mère" & n_situationMere == "Célibataire ou en couple non-cohabitant") 
      | (n_ResidParents == "Enfant résidant chez son père" & n_situationPere == "Célibataire ou en couple non-cohabitant") ~ "Configuration monoparentale", 
      n_ResidParents == "Enfant résidant hors domicile(s) des parents" ~ "Non cohabitant avec ses parents"))


enfantTous %>%
  select(n_configFamEnfants, n_statutResid) %>%
  tbl_summary(by = n_statutResid)

## 3.3. Identifications des configurations familiales ##########################

# Donc garce à ce tableau, on peut identifier les configuration familiales
load("Resultats/tab_situationsFamEnfants.rds")
tab$tableau


  



# 4. Ménages concernés par ces situations ######################################



