

## Approche par les enfants ####

names(indiv)

indiv$IDENT_IND

# On définit qui sont considéré comme des enfants 
indiv$ENFRP # Variable enfant au sens du recensement
indiv$ENFANT # Variable au sens du TCM (budget commun en plus)
tbl_cross(indiv, ENFANT, ENFRP) # On regarde si elles se recoupent 
temp <- indiv %>%
  filter(ENFANT == "2" & ENFRP == "1")
# Donc on a trois individus assez agés qui sont considérés comme enfant au sens 
# du RP mais pas du TCM
rm(temp)

# On va définir une limite d'âge 
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


# On crée une table pour les parents de ces enfants et on regarde si ils sont en couple
parents <- indiv %>%
  var_IDENTIFIANT(IdentIndiv = "NOI", IdentMenage = "IDENT_MEN" , NewVarName = "n_IdentParent") %>%
  select(n_IdentParent, COUPLE, SEXE)
parents %>%
  select(COUPLE) %>%
  rec_COUPLE() %>%
  tbl_summary()
  

# On joint la table des parents sur les identifiants des pères et des mères 
enfants2 <- enfants %>%
  left_join(parents %>% rec_COUPLE(NewVar = "n_CouplePere") %>% select(-COUPLE, -SEXE),
            by = c("n_IdentPere" = "n_IdentParent")) %>%
  left_join(parents %>% rec_COUPLE(NewVar = "n_CoupleMere") %>% select(-COUPLE, -SEXE),
            by = c("n_IdentMere" = "n_IdentParent"))
enfants <- enfants2
rm(enfants2)

enfants %>%
  select(n_NPARENTS, n_CouplePere, n_CoupleMere) %>%
  tbl_summary(by = "n_NPARENTS") 
  




# On ajoute la situation des enfants hors domicile ###################################""
# Un seuil d'âge
enfantHD <- enfHD %>%
  mutate(AG = 2017 - HODAN) %>% # Calcul de l'age au 31 décembre
  filter(AG <= 25) %>% # on peut changer en le seuil, 25 ans me paraît bien seuil d'ouverture du RSA 
  var_IDENTIFIANT(IdentIndiv = "NUMORDRE", IdentMenage = "IDENT_MEN", NewVarName = "n_IdentIndiv")
names(enfantHD)

# On va cherche des infos sur leurs parents 
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
  # On récupère des infos sur ou vivent les enfants 
  left_join(enfantHD %>% select(n_IdentIndiv, HODCO)) %>%
  mutate(n_NPARENTS = case_when(
    HODCO == "3" & !is.na(n_IdentPere) ~ "Enfant résidant chez sa mère", 
    HODCO == "3" & !is.na(n_IdentMere) ~ "Enfant résidant chez son père", 
    HODCO != "3" ~ "Enfant résidant hors domicile(s) des parents"))

names(enfantHD)

enfantHD <- enfantHD %>%
  left_join(temp)

enfantHD %>%
  select(n_ParentsCohab, n_CouplePere, n_CoupleMere) %>%
  tbl_summary(by = "n_ParentsCohab")



names(enfants)
names(enfantHD)
enfantTous <- bind_rows(
  enfants %>% 
    select(starts_with("n_")) %>% 
    mutate(n_statutResid = "Enfant du ménage (au sens du TCM)"), 
  enfantHD %>% 
    select(starts_with("n_")) %>% 
    mutate(n_statutResid = "Enfant résidant hors domicile"))
names(enfantTous)

enfantTous <- enfantTous %>%
  mutate(n_ResidParents = case_when(
    n_statutResid == "Enfant résidant hors domicile" ~ n_NPARENTS,
    n_NPARENTS == "la mère uniquement" ~ "Enfant résidant chez sa mère", 
    n_NPARENTS == "le père uniquement" ~ "Enfant résidant chez son père", 
    n_NPARENTS == "les deux" ~ "Enfant résidant chez ses deux parents")) %>%
  mutate(n_ParentsCohab = case_when(
    n_statutResid == "Enfant résidant hors domicile" ~ n_ParentsCohab, 
    n_NPARENTS == "la mère uniquement" ~ "Non", 
    n_NPARENTS == "le père uniquement" ~ "Non", 
    n_NPARENTS == "les deux" ~ "Oui")) %>%
  mutate(n_ParentsCohab2 = case_when(
    n_ParentsCohab == "Oui" ~ "Parents cohabitants", 
    n_ParentsCohab == "Non" ~ "Parents non-cohabitants")) %>%
  # On labelise les colones 
  mutate(n_ResidParents = labelled(n_ResidParents, label = "Enfant résidant avec"), 
         n_NPARENTS = labelled(n_ResidParents, label = "Enfant résidant avec"), 
         n_statutResid = labelled(n_statutResid, label = "Statut résidentiel dans l'enquête"), 
         n_ParentsCohab = labelled(n_ParentsCohab, label = "Cohabitation des parents"), 
         n_ParentsCohab2 = labelled(n_ParentsCohab, label = "Cohabitation des parents"), 
         n_CoupleMere = labelled(n_CoupleMere, label = "Mère en couple"), 
         n_CouplePere = labelled(n_CouplePere, label = "Père en couple"))
  
names(enfantTous)



tableaux <- NULL
names(enfantTous)

  

tab1 <- enfantTous %>%
  filter(n_ParentsCohab2 == "Parents cohabitants") %>%
  select(n_ResidParents, n_CouplePere, n_CoupleMere, n_statutResid) %>%
  tbl_summary(by = n_ResidParents, 
              label = list(
                
              ))%>%
  add_overall(last = T)
tab1


          enfantTous %>%
            filter(n_ParentsCohab2 == "Parents non-cohabitants") %>%
            select(n_ResidParents, n_CouplePere, n_CoupleMere, n_statutResid) %>%
            tbl_summary(by = n_ResidParents)))

tableaux$StructureFam_viaEnfants <- 

enfant




