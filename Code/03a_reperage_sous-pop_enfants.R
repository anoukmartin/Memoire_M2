################################################################################- 
#########################  REPERAGE SOUS-POP  ##################################
################################################################################- 

# On adopte une approche par les enfants, parce que c'est ce qui est usuel dans 
# dans la quantification du nombre de familles recomposées
age_limite <- 25

################################################################################-
# 1. Enfants enquêtés dans le ménage ###########################################
################################################################################-
# On va regarder avec qui vivent les enfants appartenant à l'enquête pour 
# voir si il s'agit de configuration familiales recomposées

indiv <- readRDS("Data_output/indiv.Rds")
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
  filter(AG < age_limite)  %>% # on peut changer en le seuil, 25 ans me paraît bien seuil d'ouverture du RSA 
  filter(ENFANT == "1") # puis on ne prend que ceux qui sont considérés comme enfant au sens du TCM (idem recensement car on virer les individus agés)

summary(enfants$AGE)
boxplot(enfants$AGE)

# Ensuite on va regarder avec qui vivent ces enfants
enfants <- enfants %>%
  mutate(n_NPARENTS = case_when(
    PER1E == "1" & MER1E == "1" ~ "les deux",
    PER1E == "1" & MER1E != "1" ~ "le père uniquement",
    PER1E != "1" & MER1E == "1" ~ "la mère uniquement",
    PER1E != "1" & MER1E != "1" ~ "aucun des deux")) %>%
  mutate(n_AutreParent = case_when(
    PER1E == "1" & MER1E == "2" ~ "autre parent vivant ailleurs",
    PER1E == "1" & MER1E %in% c("3", "4") ~ "autre parent inconnu ou décédé",
    MER1E == "1" & PER1E == "2" ~ "autre parent vivant ailleurs",
    MER1E == "1" & PER1E %in% c("3", "4") ~ "autre parent inconnu ou décédé"))

tbl_summary(enfants, 
            include = c("n_NPARENTS", "n_AutreParent"))

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
freq(indiv$CONJOINT)
indiv$CONJOINT <- indiv$CONJOINT %>%
  str_trim()%>%
  as.numeric()

parents <- indiv %>%
  var_IDENTIFIANT(IdentIndiv = "NOI", 
                  IdentMenage = "IDENT_MEN" , 
                  NewVarName = "n_IdentParent") %>%
  var_IDENTIFIANT(NewVarName = "n_IdentConjoint", 
                  IdentMenage = "IDENT_MEN", 
                  IdentIndiv = "CONJOINT") %>%
  select(n_IdentParent, COUPLE, SEXE, n_IdentConjoint)

# on regarde s'ils sont en couple
parents %>%
  select(COUPLE) %>%
  rec_COUPLE() %>%
  tbl_summary()
  
## 1.3. Situation conjugale des parents des enfants ###########################
# On joint la table des parents sur les identifiants des pères et des mères 
enfants2 <- enfants %>%
  left_join(parents %>% 
              rec_COUPLE(NewVar = "n_CouplePere") %>% 
              select(-COUPLE, -SEXE) %>%
              rename(n_IdentConjointPere = n_IdentConjoint), 
            by = c("n_IdentPere" = "n_IdentParent")) %>%
  left_join(parents %>% 
              rec_COUPLE(NewVar = "n_CoupleMere") %>% 
              select(-COUPLE, -SEXE) %>%
              rename(n_IdentConjointMere = n_IdentConjoint),
            by = c("n_IdentMere" = "n_IdentParent"))
names(enfants2)
enfants <- enfants2
rm(enfants2)

# On regarde la situation conjugale des parents en fonction d'avec qui l'enfant vit
enfants %>%
  select(n_NPARENTS, n_CouplePere, n_CoupleMere) %>%
  tbl_summary(by = "n_NPARENTS") 


# On regarde si ils vivent avec des beaux parents 
enfants <- enfants %>%
  mutate(n_ConjPere = case_when(
    n_IdentConjointPere != n_IdentMere ~ "Beau-parent", 
    !is.na(n_IdentConjointPere) & is.na(n_IdentMere) ~ "Beau-parent",
    n_IdentConjointPere == n_IdentMere ~ "Parent",
    is.na(n_IdentConjointPere) ~ NA_character_)) %>%
  mutate(n_ConjMere = case_when(
    n_IdentConjointMere != n_IdentPere ~ "Beau-parent", 
    !is.na(n_IdentConjointMere) & is.na(n_IdentPere) ~ "Beau-parent", 
    n_IdentConjointMere == n_IdentPere ~ "Parent", 
    is.na(n_IdentConjointMere) ~ NA_character_))
freq(enfants$n_ConjMere)
freq(enfants$n_ConjPere)
table(enfants$n_ConjMere, enfants$n_ConjPere, useNA = "ifany")

# On centre la variable de pondération 
enfants$PONDIND <- enfants$PONDMEN/mean(enfants$PONDMEN)
summary(enfants$PONDIND)

saveData(enfants, label = "enfantsDuMenage")

################################################################################-
# 2. Les enfants hors-domicile #################################################
################################################################################-
# Enfants hors domicile, qui ne sont pas enquêtés dans le ménage, mais on a des 
# infos sur eux remplis par leur parents (présents dans les ménages enquêtés)
# cela permet de savoir qui vit "virtuellement" dans des familles recomposées, 
# par exemple enfant qui vit chez sa mère mais dont le père est remis en couple 
# on peut se dire qu'il s'agit d'un ménage 

## 2.1. Les enfants : moins de 25 ans ##########################################

enfHD <- readRDS("Data_output/enfHD.Rds")
infosBDF <- readRDS("Data_output/infosBDF.Rds")

# Pour les enfants hors domicile on prend les mêmes critères sauf résidence : 
# - ne pas etre en couple cohabitant et ne pas avoir soi même d'enfant 
names(enfHD)
enfantHD <- enfHD %>%
  mutate(AG = infosBDF$vague - HODAN) %>% # Calcul de l'age au 31 décembre
  filter(HODENF == 0 & HODMAT != "1") %>%
  filter(AG < age_limite) %>%
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
  left_join(parents %>% 
              rec_COUPLE(NewVar = "n_CouplePere") %>% 
              select(-COUPLE, -SEXE) %>%
              rename(n_IdentConjointPere = n_IdentConjoint),
            by = c("n_IdentPere" = "n_IdentParent")) %>%
  left_join(parents %>% 
              rec_COUPLE(NewVar = "n_CoupleMere") %>% 
              select(-COUPLE, -SEXE) %>%
              rename(n_IdentConjointMere = n_IdentConjoint),
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

## 2.4. ont des beaux-parents ##################################################
# On regarde si ils vivent avec des beaux parents 
enfantHD <- enfantHD %>%
  mutate(n_ConjPere = case_when(
    n_IdentConjointPere != n_IdentMere ~ "Beau-parent", 
    !is.na(n_IdentConjointPere) & is.na(n_IdentMere) ~ "Beau-parent",
    n_IdentConjointPere == n_IdentMere ~ "Parent",
    is.na(n_IdentConjointPere) ~ NA_character_)) %>%
  mutate(n_ConjMere = case_when(
    n_IdentConjointMere != n_IdentPere ~ "Beau-parent", 
    !is.na(n_IdentConjointMere) & is.na(n_IdentPere) ~ "Beau-parent", 
    n_IdentConjointMere == n_IdentPere ~ "Parent", 
    is.na(n_IdentConjointMere) ~ NA_character_))
freq(enfantHD$n_ConjMere)
freq(enfantHD$n_ConjPere)
table(enfantHD$n_ConjMere, enfantHD$n_ConjPere, useNA = "ifany")

saveData(enfantHD, "enfantsHorsDom")


################################################################################-
# 3. Tous les enfants ##########################################################
################################################################################-


## 3.1. Jointure ###############################################################
# On combine les deux tableaux de données 
names(indiv)

enfantTous <- bind_rows(
  enfants %>% 
    select(IDENT_MEN, PER1E, MER1E, starts_with("n_")) %>% 
    mutate(n_statutResid = "Enfant du ménage (au sens du TCM)"), 
  enfantHD %>% 
    select(IDENT_MEN, starts_with("n_")) %>% 
    mutate(n_statutResid = "Enfant résidant hors domicile"))
names(enfantTous)


## 3.2. Harmonisation des variables ############################################
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
    n_ParentsCohab == "Non" ~ "Parents non-cohabitants"))


## 3.3. De nouvelles variables #################################################
# On crée aussi un recodage qui simplifie la situation conjugale des parents
  # On ne va considérer que les cas ou un nouveau couple cohabite
  # On néglige les deux cas ou les parents cohabitent mais ne se déclarent pas en couple 
enfantTous <- enfantTous %>%
  mutate(
    n_situationMere = case_when(
      n_ParentsCohab == "Oui" ~ "En couple avec l'autre parent", 
      n_ParentsCohab == "Non" & n_CoupleMere ==  "Oui, avec une personne qui vit dans le logement" ~ "En couple avec une autre personne", 
      n_ParentsCohab == "Non" & n_CoupleMere !=  "Oui, avec une personne qui vit dans le logement" ~ "Célibataire ou en couple non-cohabitant"), 
    n_situationPere = case_when(
      n_ParentsCohab == "Oui" ~ "En couple avec l'autre parent", 
      n_ParentsCohab == "Non" & n_CouplePere ==  "Oui, avec une personne qui vit dans le logement" ~ "En couple avec une autre personne", 
      n_ParentsCohab == "Non" & n_CouplePere !=  "Oui, avec une personne qui vit dans le logement" ~ "Célibataire ou en couple non-cohabitant")) %>%
  # Configuration familiale principale : configuration familiale du ménage ou l'enfant réside
  mutate(
    n_configFamEnfantsP = case_when(
      n_ResidParents == "Enfant résidant chez ses deux parents" ~ "Configuration traditionnelle", 
      (n_ResidParents == "Enfant résidant chez sa mère" & is.na(n_situationMere)) 
      | (n_ResidParents == "Enfant résidant chez son père" & is.na(n_situationPere)) ~ "Configuration monoparentale ou recomposée", 
      (n_ResidParents == "Enfant résidant chez sa mère" & n_situationMere == "En couple avec une autre personne") 
      | (n_ResidParents == "Enfant résidant chez son père" & n_situationPere == "En couple avec une autre personne") ~ "Configuration recomposée",
      (n_ResidParents == "Enfant résidant chez sa mère" & n_situationMere == "Célibataire ou en couple non-cohabitant") 
      | (n_ResidParents == "Enfant résidant chez son père" & n_situationPere == "Célibataire ou en couple non-cohabitant") ~ "Configuration monoparentale",
      n_ResidParents == "Enfant résidant hors domicile(s) des parents" ~ "Non cohabitant avec ses parents")) %>%
  mutate(
    n_configFamEnfantsPSexe = case_when(
      (n_ResidParents == "Enfant résidant chez ses deux parents") ~ "Configuration traditionnelle", 
      (n_ResidParents == "Enfant résidant chez sa mère" & is.na(n_situationMere)) ~ "Configuration monoparentale ou recomposée maternelle",
      (n_ResidParents == "Enfant résidant chez son père" & is.na(n_situationPere)) ~ "Configuration monoparentale ou recomposée paternelle", 
      (n_ResidParents == "Enfant résidant chez sa mère" & n_situationMere == "En couple avec une autre personne") ~ "Configuration recomposée maternelle",
      (n_ResidParents == "Enfant résidant chez son père" & n_situationPere == "En couple avec une autre personne") ~ "Configuration recomposée paternelle",
      (n_ResidParents == "Enfant résidant chez sa mère" & n_situationMere == "Célibataire ou en couple non-cohabitant") ~ "Configuration monoparentale maternelle",
      (n_ResidParents == "Enfant résidant chez son père" & n_situationPere == "Célibataire ou en couple non-cohabitant") ~ "Configuration monoparentale paternelle",
      (n_ResidParents == "Enfant résidant hors domicile(s) des parents") ~ "Non cohabitant avec ses parents")) %>%
  
  # Configuration familiale secondaire : configuration familiale du ménage ou réside l'autre parent
  mutate(
    n_configFamEnfantsS = case_when(
      n_ResidParents == "Enfant résidant chez ses deux parents" ~ "Pas de configuration secondaire", 
      (n_ResidParents == "Enfant résidant chez sa mère" & n_situationPere == "En couple avec une autre personne") 
      | (n_ResidParents == "Enfant résidant chez son père" & n_situationMere == "En couple avec une autre personne") ~ "Configuration recomposée",
      (n_ResidParents == "Enfant résidant chez sa mère" & n_situationPere == "Célibataire ou en couple non-cohabitant") 
      | (n_ResidParents == "Enfant résidant chez son père" & n_situationMere == "Célibataire ou en couple non-cohabitant") ~ "Configuration monoparentale", 
      n_ResidParents == "Enfant résidant hors domicile(s) des parents" & 
        (n_situationPere == "En couple avec l'autre parent" | n_situationMere == "En couple avec l'autre parent") ~ "Configuration traditionnelle", 
      n_ResidParents == "Enfant résidant hors domicile(s) des parents" & 
        ((n_situationPere == "Célibataire ou en couple non-cohabitant" & n_situationMere == "Célibataire ou en couple non-cohabitant")
         |(n_situationPere == "Célibataire ou en couple non-cohabitant" & is.na(n_situationMere))
         |(n_situationMere == "Célibataire ou en couple non-cohabitant" & is.na(n_situationPere))) ~ "Configuration monoparentale", 
      n_ResidParents == "Enfant résidant hors domicile(s) des parents" & 
        ((n_situationPere == "En couple avec une autre personne" & n_situationMere == "En couple avec une autre personne")
         |(n_situationPere == "En couple avec une autre personne" & is.na(n_situationMere))
         |(n_situationMere == "En couple avec une autre personne" & is.na(n_situationPere))) ~ "Configuration recomposée")) %>%
  mutate(
    n_configFamEnfantsS = case_when(
      !is.na(n_configFamEnfantsS) ~ n_configFamEnfantsS,
      is.na(n_configFamEnfantsS) & n_ResidParents == "Enfant résidant chez son père" & MER1E %in% c("3", "4") ~ "Pas de configuration secondaire", 
      is.na(n_configFamEnfantsS) & n_ResidParents == "Enfant résidant chez sa mère" & PER1E %in% c("3", "4") ~ "Pas de configuration secondaire", 
      is.na(n_configFamEnfantsS) & n_ResidParents == "Enfant résidant chez son père" & MER1E == "2" ~ "Configuration inconnue", 
      is.na(n_configFamEnfantsS) & n_ResidParents == "Enfant résidant chez sa mère" & PER1E == "2"  ~ "Configuration inconnue"
    )
  ) %>%
  mutate(
    n_configFamEnfantsSSexe = case_when(
      (n_ResidParents == "Enfant résidant chez ses deux parents") ~ "Pas de configuration secondaire", 
      (n_ResidParents == "Enfant résidant chez sa mère" & n_situationPere == "En couple avec une autre personne") ~ "Configuration recomposée paternelle",
      (n_ResidParents == "Enfant résidant chez son père" & n_situationMere == "En couple avec une autre personne") ~ "Configuration recomposée maternelle",
      (n_ResidParents == "Enfant résidant chez sa mère" & n_situationPere == "Célibataire ou en couple non-cohabitant")  ~ "Configuration monoparentale paternelle", 
      (n_ResidParents == "Enfant résidant chez son père" & n_situationMere == "Célibataire ou en couple non-cohabitant") ~ "Configuration monoparentale maternelle", 
      (n_ResidParents == "Enfant résidant hors domicile(s) des parents" & 
        (n_situationPere == "En couple avec l'autre parent" | n_situationMere == "En couple avec l'autre parent")) ~ "Configuration traditionnelle", 
      (n_ResidParents == "Enfant résidant hors domicile(s) des parents" 
       & n_situationPere == "Célibataire ou en couple non-cohabitant" 
       & n_situationMere == "Célibataire ou en couple non-cohabitant") ~ "Configuration monoparentale maternelle et paternelle", 
      (n_ResidParents == "Enfant résidant hors domicile(s) des parents" 
       & n_situationPere == "Célibataire ou en couple non-cohabitant" & is.na(n_situationMere)) ~ "Configuration monoparentale paternelle", 
      (n_ResidParents == "Enfant résidant hors domicile(s) des parents"  
       & n_situationMere == "Célibataire ou en couple non-cohabitant" & is.na(n_situationPere)) ~ "Configuration monoparentale maternelle", 
      (n_ResidParents == "Enfant résidant hors domicile(s) des parents" 
       & n_situationPere == "En couple avec une autre personne" 
       & n_situationMere == "En couple avec une autre personne") ~ "Configuration recomposée maternelle et paternelle", 
      (n_ResidParents == "Enfant résidant hors domicile(s) des parents" 
       & n_situationPere == "En couple avec une autre personne" 
       & is.na(n_situationMere)) ~ "Configuration recomposée paternelle", 
      (n_ResidParents == "Enfant résidant hors domicile(s) des parents" 
       & n_situationMere == "En couple avec une autre personne" 
       & is.na(n_situationPere)) ~ "Configuration recomposée maternelle")) %>%
  mutate(n_configFamEnfantsSSexe = case_when(
    !is.na(n_configFamEnfantsSSexe) ~ n_configFamEnfantsSSexe,
    is.na(n_configFamEnfantsSSexe) & n_ResidParents == "Enfant résidant chez son père" & MER1E %in% c("3", "4") ~ "Pas de configuration secondaire", 
    is.na(n_configFamEnfantsSSexe) & n_ResidParents == "Enfant résidant chez sa mère" & PER1E %in% c("3", "4") ~ "Pas de configuration secondaire", 
    is.na(n_configFamEnfantsSSexe) & n_ResidParents == "Enfant résidant chez son père" & MER1E == "2" ~ "Configuration maternelle inconnue", 
    is.na(n_configFamEnfantsSSexe) & n_ResidParents == "Enfant résidant chez sa mère" & PER1E == "2"  ~ "Configuration paternelle inconnue"
  )
)

  # On labellise les colonnes 
enfantTous <- enfantTous %>%
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
                                    label = "Situation conjugale du père"), 
         n_configFamEnfantsP = labelled(n_configFamEnfantsP, 
                                        label = "Configuration familiale principale"), 
         n_configFamEnfantsPSexe = labelled(n_configFamEnfantsPSexe, 
                                            label = "Configuration familiale principale paternelle ou maternelle"),
         n_configFamEnfantsS = labelled(n_configFamEnfantsS, 
                                        label = "Configuration familiale secondaire"), 
         n_configFamEnfantsSSexe = labelled(n_configFamEnfantsSSexe, 
                                        label = "Configuration familiale secondaire paternelle ou maternelle"))


names(enfantTous)

# On enregistre ce tableau de données
saveData(enfantTous, label = "enfantsTous")

# On ajoute ces données sur celles des enfants du ménages et des enfants hors domicile 
enfants <- readRDS("Data_output/enfantsDuMenage.Rds") 
data <- enfantTous[, !(names(enfantTous) %in% names(enfants))]
data$n_IdentIndiv <- enfantTous$n_IdentIndiv
enfants <- enfants %>%
  left_join(data,
            by = "n_IdentIndiv")
names(enfants)
saveRDS(enfants, "Data_output/enfantsDuMenage.Rds")

enfantHD <- readRDS("Data_output/enfantsHorsDom.Rds")
data <- enfantTous[, !(names(enfantTous) %in% names(enfantHD))]
data$n_IdentIndiv <- enfantTous$n_IdentIndiv
enfantHD <- enfantHD %>%
  left_join(data,
            by = "n_IdentIndiv")
names(enfantHD)  
saveRDS(enfantHD, "Data_output/enfantsHorsDom.Rds")

# Un peu de ménage
rm(enfantHD, enfants, enfantTous, enfHD, indiv, parents, infosBDF)





