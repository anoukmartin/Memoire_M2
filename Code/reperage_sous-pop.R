

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
# enfnant vivant avec uniquement leur mère surreprésenté, parce que famille 
# mono sont plus souvent des mères isolées que des pères, + sur-échantillon 
# dans l'enquète BDF

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

# On ajoute la situation des enfants hors domicile 
# Un seuil d'âge
enfantHD <- enfHD %>%
  mutate(AG = 2017 - HODAN) %>% # Calcul de l'age au 31 décembre
  filter(AG <= 25) %>% # on peut changer en le seuil, 25 ans me paraît bien seuil d'ouverture du RSA 
  var_IDENTIFIANT(IdentIndiv = "NUMORDRE", IdentMenage = "IDENT_MEN", NewVarName = "n_IdentIndiv")
names(enfDH)

# On va cherche l'identifiant de leurs parents 
temp <- enfantHD %>%
  select(starts_with("HODLN"), c("IDENT_MEN", "n_IdentIndiv")) %>%
  pivot_longer(cols = starts_with("HODLN")) %>%
  filter(value == "1") %>%
  mutate(name = str_remove(name, "HODLN")) %>%
  select(-value) %>% 
  var_IDENTIFIANT(IdentIndiv = "name", IdentMenage = "IDENT_MEN", NewVarName = "n_IdentParent") %>%
  left_join(parents)

  pivot_wider(id_cols = n_IdentIndiv, 
              values_from = name, 
              names_from = c("parent1", "parent2"))


  





