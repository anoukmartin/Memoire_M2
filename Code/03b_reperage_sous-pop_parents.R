
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
names(indiv)


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
  filter(n_ConjMere == "Beau-parent") %>%
  pivot_longer(cols = c("n_IdentConjointMere"), 
               values_to = "n_IdentBeauParent",
               values_drop_na = T) %>%
  group_by(n_IdentBeauParent)%>%
  summarise(n_NBeauxEnfantsMen = n(), 
            n_AgeBeauxEnfantsMen = mean(AG))
infos_enfantsMenage2 <- enfantsMenage %>%
  filter(n_ConjPere == "Beau-parent") %>%
  pivot_longer(cols = c("n_IdentConjointPere"), 
               values_to = "n_IdentBeauParent",
               values_drop_na = T) %>%
  group_by(n_IdentBeauParent)%>%
  summarise(n_NBeauxEnfantsMen = n(), 
            n_AgeBeauxEnfantsMen = mean(AG))
infos_enfantsMenage <- bind_rows(infos_enfantsMenage, infos_enfantsMenage2)

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

lprop(table(indiv$ENFANT, indiv$NONENFANT))
lprop(table(indiv$ENFANT, indiv$ADULTE))
  
saveRDS(indiv, file = "Data_output/parents.Rds")
rm(list_beauparents, list_parents, list_parentremisencouple, indiv, enfantsMenage, data, enfantsHD, infos_enfants, infos_enfantsHD, infos_enfantsHD2, infos_enfantsMen, infos_enfantsMen2, infos_enfantsMenage, infos_enfantsMenage2, revenus, patrimoine)

