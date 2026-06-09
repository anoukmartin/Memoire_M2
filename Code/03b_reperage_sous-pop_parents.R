
################################################################################- 
#########################  REPERAGE SOUS-POP  ##################################
################################################################################- 

# On adopte une approche par les enfants, parce que c'est ce qui est usuel dans
# dans la quantification du nombre de familles recomposées, dans cette partie on
# identifie les situations des parents

indiv <- readRDS("Data_output/indiv.Rds") %>%
  var_IDENTIFIANT(NewVarName = "n_IdentIndiv", 
                  IdentIndiv = "NOI", 
                  IdentMenage = "IDENT_MEN") %>%
  var_IDENTIFIANT(NewVarName = "n_IdentConjoint", 
                  IdentMenage = "IDENT_MEN", 
                  IdentIndiv = "CONJOINT")
freq(indiv$COUPLE)


## Information sur le conjoint ##################################################

## Sexe du conjoint ----

conj <- indiv %>%
  filter(n_IdentIndiv %in% unique(indiv$n_IdentConjoint)) %>%
  select(n_IdentConjoint = n_IdentIndiv, n_SEXE_conj = SEXE) 

indiv <- left_join(indiv, conj)
freq(indiv$SEXE)
freq(indiv$n_SEXE_conj)

str(indiv[, c("SEXE", "COUPLE", "n_SEXE_conj")])

## Variable statut conjugal genrée ----
indiv <- indiv %>%
  mutate(n_statutConjugalSexe = case_when(
         SEXE == "1" & COUPLE %in% c("2", "3") ~ "Homme célibataire", 
         SEXE == "2" & COUPLE %in% c("2", "3") ~ "Femme célibataire",
         SEXE == "1" & COUPLE == "1" & n_SEXE_conj == "1" ~ "Homme en couple avec un homme",
         SEXE == "2" & COUPLE == "1" & n_SEXE_conj == "1" ~ "Femme en couple avec un homme",
         SEXE == "2" & COUPLE == "1" & n_SEXE_conj == "2" ~ "Femme en couple avec une femme",
         SEXE == "1" & COUPLE == "1" & n_SEXE_conj == "2" ~ "Homme en couple avec une femme"))

freq(indiv$n_statutConjugalSexe)

## Variables d'infos sur les enfants (enfants du ménage ou hors domicile, beaux-enfants...) ###########

enfantsMenage <- readRDS("Data_output/enfantsDuMenage.Rds") 

list_parents <- unique(c(enfantsMenage$n_IdentMere, 
                       enfantsMenage$n_IdentPere))

list_parents <- list_parents[!is.na(list_parents)]

list_beauparents <- c(
  enfantsMenage[
    enfantsMenage$n_ConjPere == "Beau-parent"
    & !is.na(enfantsMenage$n_ConjPere), ]$n_IdentConjointPere,
  enfantsMenage[
    enfantsMenage$n_ConjMere == "Beau-parent"
    & !is.na(enfantsMenage$n_ConjMere), ]$n_IdentConjointMere
  ) %>%
  unique()

list_parentremisencouple <-  c(
  enfantsMenage[
    enfantsMenage$n_ConjPere == "Beau-parent"
    & !is.na(enfantsMenage$n_ConjPere), ]$n_IdentPere,
  enfantsMenage[
    enfantsMenage$n_ConjMere == "Beau-parent"
    & !is.na(enfantsMenage$n_ConjMere), ]$n_IdentMere
) %>%
  unique()

indiv <- indiv %>%
  mutate(n_EnfantsMen = case_when(
    n_IdentIndiv %in% list_parents ~ TRUE, 
    IDENT_MEN %in% unique(enfantsMenage$IDENT_MEN) 
    & !(n_IdentIndiv %in% enfantsMenage$n_IdentIndiv) ~ FALSE, 
    TRUE ~ NA)) %>%
  mutate(n_BeauxEnfantsMen = case_when(
    n_IdentIndiv %in% list_beauparents ~ TRUE, 
    IDENT_MEN %in% unique(enfantsMenage$IDENT_MEN) & !(n_IdentIndiv %in% enfantsMenage$n_IdentIndiv) ~ FALSE, 
    TRUE ~ NA)) %>%
  mutate(n_RemisEnCoupleEnfantsMen = case_when(
    n_IdentIndiv %in% list_parentremisencouple ~ TRUE, 
    IDENT_MEN %in% unique(enfantsMenage$IDENT_MEN) 
    & !(n_IdentIndiv %in% enfantsMenage$n_IdentIndiv) & n_EnfantsMen ~ FALSE, 
    TRUE ~ NA))

freq(indiv$n_EnfantsMen)
freq(indiv$n_BeauxEnfantsMen)
freq(indiv$n_RemisEnCoupleEnfantsMen)


#infos sur les enfants 
infos_enfantsMenage <- enfantsMenage %>%
  pivot_longer(cols = c("n_IdentMere", "n_IdentPere"), 
               values_to = "n_IdentParent",
               values_drop_na = T) %>%
  group_by(n_IdentParent)%>%
  summarise(n_NEnfantsMen = n(), 
            n_AgeEnfantsMen = mean(AG))
infos_enfantsMenage  
indiv <- left_join(indiv, infos_enfantsMenage, 
                   by = c("n_IdentIndiv" = "n_IdentParent"))

#infos sur les beau-enfants
infos_enfantsMenage <- enfantsMenage %>%
  filter(!is.na(n_IdentBeauParent)) %>%
  group_by(n_IdentBeauParent) %>%
  summarise(n_NBeauxEnfantsMen = n(), 
            n_AgeBeauxEnfantsMen = mean(AG))
  
indiv <- left_join(indiv, infos_enfantsMenage, 
                   by = c("n_IdentIndiv" = "n_IdentBeauParent"))

#infos enfants du couple 
infos_enfantsMenage <- enfantsMenage %>%
  filter(n_ConjMere == "Parent") %>%
  pivot_longer(cols = c("n_IdentMere"), 
               values_to = "n_IdentParent",
               values_drop_na = T) %>%
  group_by(n_IdentParent)%>%
  summarise(n_NEnfantsCouple = n(), 
            n_AgeEnfantsCouple = mean(AG))
infos_enfantsMenage2 <- enfantsMenage %>%
  filter(n_ConjPere == "Parent") %>%
  pivot_longer(cols = c("n_IdentPere"), 
               values_to = "n_IdentParent",
               values_drop_na = T) %>%
  group_by(n_IdentParent)%>%
  summarise(n_NEnfantsCouple = n(), 
            n_AgeEnfantsCouple = mean(AG))

infos_enfantsMenage <- bind_rows(infos_enfantsMenage, infos_enfantsMenage2)

indiv <- left_join(indiv, infos_enfantsMenage, 
                   by = c("n_IdentIndiv" = "n_IdentParent"))


#infos enfants d'union précédantes 
infos_enfantsMenage <- enfantsMenage %>%
  filter(n_ConjMere != "Parent" | is.na(n_ConjMere)) %>%
  pivot_longer(cols = c("n_IdentMere"), 
               values_to = "n_IdentParent",
               values_drop_na = T) %>%
  group_by(n_IdentParent)%>%
  summarise(n_NEnfantsUnionAnt = n(), 
            n_AgeEnfantsUnionAnt = mean(AG))
infos_enfantsMenage2 <- enfantsMenage %>%
  filter(n_ConjPere != "Parent" | is.na(n_ConjPere))  %>%
  pivot_longer(cols = c("n_IdentPere"), 
               values_to = "n_IdentParent",
               values_drop_na = T) %>%
  group_by(n_IdentParent)%>%
  summarise(n_NEnfantsUnionAnt = n(), 
            n_AgeEnfantsUnionAnt = mean(AG))

infos_enfantsMenage <- bind_rows(infos_enfantsMenage, infos_enfantsMenage2)

indiv <- left_join(indiv, infos_enfantsMenage, 
                   by = c("n_IdentIndiv" = "n_IdentParent"))


# ensuite on ajoute les données sur les enfants hors domicile : 

enfantsHD <- readRDS("Data_output/enfantsHorsDom.Rds") 

list_enfantsHD <- unique(c(enfantsHD$n_IdentMere, 
                           enfantsHD$n_IdentPere))
list_enfantsHD  <- list_enfantsHD[!is.na(list_enfantsHD)]

list_beauxenfantsHD <- c(
  c(enfantsHD[
    enfantsHD$n_ConjPere == "Beau-parent"
    & !is.na(enfantsHD$n_ConjPere), ]$n_IdentConjointPere) %>% unique(),
  c(enfantsHD[
    enfantsHD$n_ConjMere == "Beau-parent"
    & !is.na(enfantsHD$n_ConjMere), ]$n_IdentConjointMere) %>% unique()
) 

list_enfantsHDremisencouple <- c(
  enfantsHD[
    enfantsHD$n_ConjPere == "Beau-parent"
    & !is.na(enfantsHD$n_ConjPere), ]$n_IdentPere,
  enfantsHD[
    enfantsHD$n_ConjMere == "Beau-parent"
    & !is.na(enfantsHD$n_ConjMere), ]$n_IdentMere
) %>%
  unique()


indiv <- indiv %>%
  mutate(n_EnfantsHD = case_when(
    n_IdentIndiv %in% list_enfantsHD ~ TRUE, 
    IDENT_MEN %in% unique(enfantsHD$IDENT_MEN) & !(n_IdentIndiv %in% enfantsMenage$n_IdentIndiv) ~ FALSE, 
    TRUE ~ NA)) %>%
  mutate(n_BeauxEnfantsHD = case_when(
    n_IdentIndiv %in% list_beauxenfantsHD ~ TRUE, 
    IDENT_MEN %in% unique(enfantsHD$IDENT_MEN) & !(n_IdentIndiv %in% enfantsMenage$n_IdentIndiv) ~ FALSE, 
    TRUE ~ NA)) %>%
  mutate(n_RemisEnCoupleEnfantsHD = case_when(
    n_IdentIndiv %in% list_enfantsHDremisencouple ~ TRUE, 
    IDENT_MEN %in% unique(enfantsHD$IDENT_MEN) 
    & !(n_IdentIndiv %in% enfantsMenage$n_IdentIndiv) & n_EnfantsHD ~ FALSE, 
    TRUE ~ NA))

freq(indiv$n_EnfantsHD)
freq(indiv$n_BeauxEnfantsHD)
freq(indiv$n_RemisEnCoupleEnfantsHD)


#infos sur les enfants 
infos_enfantsHD <- enfantsHD %>%
  pivot_longer(cols = c("n_IdentMere", "n_IdentPere"), 
               values_to = "n_IdentParent",
               values_drop_na = T) %>%
  group_by(n_IdentParent)%>%
  summarise(n_NEnfantsHD = n(), 
            n_AgeEnfantsHD = mean(AG))

indiv <- left_join(indiv, infos_enfantsHD, 
                   by = c("n_IdentIndiv" = "n_IdentParent"))

#infos sur les beau-enfants
infos_enfantsHD <- enfantsHD %>%
  filter(n_ConjMere == "Beau-parent") %>%
  pivot_longer(cols = c("n_IdentConjointMere"), 
               values_to = "n_IdentBeauParent",
               values_drop_na = T) %>%
  group_by(n_IdentBeauParent)%>%
  summarise(n_NBeauxEnfantsHD = n(), 
            n_AgeBeauxEnfantsHD = mean(AG))
infos_enfantsHD2 <- enfantsHD %>%
  filter(n_ConjPere == "Beau-parent") %>%
  pivot_longer(cols = c("n_IdentConjointPere"), 
               values_to = "n_IdentBeauParent",
               values_drop_na = T) %>%
  group_by(n_IdentBeauParent)%>%
  summarise(n_NBeauxEnfantsHD = n(), 
            n_AgeBeauxEnfantsHD = mean(AG))
infos_enfantsHD <- bind_rows(infos_enfantsHD, infos_enfantsHD2)

indiv <- left_join(indiv, infos_enfantsHD, 
                   by = c("n_IdentIndiv" = "n_IdentBeauParent"))


#infos enfants du couple 
infos_enfantsHD <- enfantsHD %>%
  filter(n_ConjMere == "Parent") %>%
  pivot_longer(cols = c("n_IdentMere"), 
               values_to = "n_IdentParent",
               values_drop_na = T) %>%
  group_by(n_IdentParent)%>%
  summarise(n_NEnfantsCoupleHD = n(), 
            n_AgeEnfantsCoupleHD = mean(AG))
infos_enfantsHD2 <- enfantsHD %>%
  filter(n_ConjPere == "Parent") %>%
  pivot_longer(cols = c("n_IdentPere"), 
               values_to = "n_IdentParent",
               values_drop_na = T) %>%
  group_by(n_IdentParent)%>%
  summarise(n_NEnfantsCoupleHD = n(), 
            n_AgeEnfantsCoupleHD = mean(AG))

infos_enfantsHD <- bind_rows(infos_enfantsHD, infos_enfantsHD2)

indiv <- left_join(indiv, infos_enfantsHD, 
                   by = c("n_IdentIndiv" = "n_IdentParent"))


#infos enfants d'union précédantes 
infos_enfantsHD <- enfantsHD %>%
  filter(n_ConjMere != "Parent" | is.na(n_ConjMere)) %>%
  pivot_longer(cols = c("n_IdentMere"), 
               values_to = "n_IdentParent",
               values_drop_na = T) %>%
  group_by(n_IdentParent)%>%
  summarise(n_NEnfantsUnionAntHD = n(), 
            n_AgeEnfantsUnionAntHD = mean(AG))
infos_enfantsHD2 <- enfantsHD %>%
  filter(n_ConjPere != "Parent" | is.na(n_ConjPere))  %>%
  pivot_longer(cols = c("n_IdentPere"), 
               values_to = "n_IdentParent",
               values_drop_na = T) %>%
  group_by(n_IdentParent)%>%
  summarise(n_NEnfantsUnionAntHD = n(), 
            n_AgeEnfantsUnionAntHD = mean(AG))

infos_enfantsHD <- bind_rows(infos_enfantsHD, infos_enfantsHD2)

indiv <- left_join(indiv, infos_enfantsHD, 
                   by = c("n_IdentIndiv" = "n_IdentParent"))





# Des variables communes pour les enfants hors et dans le ménage 

indiv <- indiv %>%
  mutate(n_EnfantsTous = case_when(
    n_EnfantsMen | n_EnfantsHD ~ TRUE, 
    !n_EnfantsMen | !n_EnfantsHD ~ FALSE, 
    TRUE ~ NA)) %>%
  mutate(n_BeauxEnfantsTous = case_when(
    n_BeauxEnfantsMen | n_BeauxEnfantsHD ~ TRUE, 
    !n_BeauxEnfantsMen | !n_BeauxEnfantsHD ~ FALSE,
    TRUE ~ NA)) %>%
  mutate(n_RemisEnCoupleEnfantsTous = case_when(
    n_RemisEnCoupleEnfantsMen | n_RemisEnCoupleEnfantsHD ~ TRUE, 
    !n_RemisEnCoupleEnfantsMen | !n_RemisEnCoupleEnfantsHD ~ FALSE, 
    TRUE ~ NA))

indiv %>% 
  group_by(n_RemisEnCoupleEnfantsMen, 
          n_RemisEnCoupleEnfantsHD, 
          n_RemisEnCoupleEnfantsTous) %>% 
  summarise(n = n())


#infos sur les enfants 
infos_enfantsMen <- enfantsMenage %>%
  pivot_longer(cols = c("n_IdentMere", "n_IdentPere"), 
               values_to = "n_IdentParent",
               values_drop_na = T)  %>%
  select(n_IdentIndiv, n_IdentParent, AG)
infos_enfantsHD <- enfantsHD %>%
  pivot_longer(cols = c("n_IdentMere", "n_IdentPere"), 
               values_to = "n_IdentParent",
               values_drop_na = T) %>%
  select(n_IdentIndiv, n_IdentParent, AG)
infos_enfants <- bind_rows(infos_enfantsMen, infos_enfantsHD) %>%
  group_by(n_IdentParent)%>%
  summarise(n_NEnfantsTous = n(), 
            n_AgeEnfantsTous = mean(AG))

indiv <- left_join(indiv, infos_enfants, 
                   by = c("n_IdentIndiv" = "n_IdentParent"))

#infos sur les beau-enfants
infos_enfantsMen <- enfantsMenage %>%
  filter(n_ConjMere == "Beau-parent") %>%
  pivot_longer(cols = c("n_IdentConjointMere"), 
               values_to = "n_IdentBeauParent",
               values_drop_na = T) %>%
  select(n_IdentIndiv, n_IdentBeauParent, AG)

infos_enfantsHD <- enfantsHD %>%
  filter(n_ConjMere == "Beau-parent") %>%
  pivot_longer(cols = c("n_IdentConjointMere"), 
               values_to = "n_IdentBeauParent",
               values_drop_na = T) %>%
  select(n_IdentIndiv, n_IdentBeauParent, AG)

infos_enfantsMen2 <- enfantsMenage %>%
  filter(n_ConjPere == "Beau-parent") %>%
  pivot_longer(cols = c("n_IdentConjointPere"), 
               values_to = "n_IdentBeauParent",
               values_drop_na = T) %>%
  select(n_IdentIndiv, n_IdentBeauParent, AG)

infos_enfantsHD2 <- enfantsHD %>%
  filter(n_ConjPere == "Beau-parent") %>%
  pivot_longer(cols = c("n_IdentConjointPere"), 
               values_to = "n_IdentBeauParent",
               values_drop_na = T) %>%
  select(n_IdentIndiv, n_IdentBeauParent, AG)


infos_enfants <- bind_rows(infos_enfantsMen, infos_enfantsHD, infos_enfantsMen2, infos_enfantsHD2) %>%
  group_by(n_IdentBeauParent)%>%
  summarise(n_NBeauxEnfantsTous = n(), 
            n_AgeBeauxEnfantsTous = mean(AG))

indiv <- left_join(indiv, infos_enfants, 
                   by = c("n_IdentIndiv" = "n_IdentBeauParent"))


# statut parental agrégé (tous les individus)
freq(indiv$COUPLE)
freq(indiv$n_RemisEnCoupleEnfantsMen)
indiv$n_statutConjugalSexe

indiv <- indiv %>%
  mutate(n_StatutParentalMenage = case_when(
    SEXE == "1" & n_EnfantsMen & n_BeauxEnfantsMen ~ "Beau-père avec enfant(s)", 
    SEXE == "1" & !n_EnfantsMen & n_BeauxEnfantsMen ~ "Beau-père sans enfant",
    SEXE == "1" & n_EnfantsMen & COUPLE %in% c("3", "2") ~ "Homme célibataire sans enfant", 
    SEXE == "1" & n_EnfantsMen & COUPLE %in% c("3", "2") ~ "Père célibataire", 
    SEXE == "1" & !n_EnfantsMen & COUPLE == "1" ~ "Homme en couple sans enfant",
    SEXE == "1" & n_EnfantsMen & COUPLE == "1" & n_RemisEnCoupleEnfantsMen ~ "Père en couple avec une personne sans enfant", 
    SEXE == "1" & n_EnfantsMen & COUPLE == "1" & !n_RemisEnCoupleEnfantsMen ~ "Père en couple parental",
    SEXE == "2" & n_EnfantsMen & n_BeauxEnfantsMen ~ "Belle-mère avec enfant(s)", 
    SEXE == "2" & !n_EnfantsMen & n_BeauxEnfantsMen ~ "Belle-mère sans enfant",
    SEXE == "2" & !n_EnfantsMen & COUPLE %in% c("3", "2") ~ "Femme célibataire sans enfant", 
    SEXE == "2" & n_EnfantsMen & COUPLE %in% c("3", "2") ~ "Mère célibataire", 
    SEXE == "2" & !n_EnfantsMen & COUPLE == "1" ~ "Femme en couple sans enfant",
    SEXE == "2" & n_EnfantsMen & COUPLE == "1" & n_RemisEnCoupleEnfantsMen ~ "Mère en couple avec une personne sans enfant", 
    SEXE == "2" & n_EnfantsMen & COUPLE == "1" & !n_RemisEnCoupleEnfantsMen ~ "Mère en couple parental"
  ))
freq(indiv$n_StatutParentalMenage)



tab <- as.data.frame(table(indiv$n_statutConjugalSexe, indiv$n_EnfantsMen, indiv$n_BeauxEnfantsMen, indiv$n_RemisEnCoupleEnfantsMen, useNA = "ifany")) %>%
  filter(Freq > 0)
names(tab) <- c("n_statutConjugalSexe", "n_EnfantsMen", "n_BeauxEnfantsMen", "n_EnfantsAutUnionMen")

# revenus 
revenus <- indiv %>%
  select(starts_with("REV"), c("CHOMAGE", "RETRAITES", "SALAIRES")) %>%
  rowSums(na.rm = T)
indiv$n_REVENUS <- revenus

indiv$n_REVENUSmens <- indiv$n_REVENUS/12

# Patrimoine
patrimoine <- indiv %>%
  select(starts_with("PATF")) %>%
  rowSums(na.rm = T)
indiv$n_PATRIMOINE <- patrimoine
summary(indiv$n_PATRIMOINE)


# On définit les adultes #######################################################
freq(indiv$ENFANT)
indiv <- indiv %>%
  mutate(NONENFANT = if_else(
    !(n_IdentIndiv %in% enfantsMenage$n_IdentIndiv), TRUE, FALSE)) %>%
  mutate(ADULTE = if_else(
    ENFANT != "1" & AG > max(enfantsMenage$AG) & AG < 65, TRUE, FALSE))

freq(indiv$ADULTE)
lprop(table(indiv$ENFANT, indiv$NONENFANT, useNA = "ifany"))
lprop(table(indiv$ENFANT, indiv$ADULTE, useNA = "ifany"))
  
saveRDS(indiv, file = "Data_output/parents.Rds")
rm(list_beauparents, list_parents, list_parentremisencouple, indiv, enfantsMenage, data, enfantsHD, infos_enfants, infos_enfantsHD, infos_enfantsHD2, infos_enfantsMen, infos_enfantsMen2, infos_enfantsMenage, infos_enfantsMenage2, revenus, patrimoine)

