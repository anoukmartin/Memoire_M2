
infosBDF <- readRDS("Data_output/infosBDF.Rds")

indiv <- readRDS("Data_output/parents.Rds") %>%
  #filter(NONENFANT) %>%
  rec_DIP(Var = "DIP14", NewVar = "DIPL") %>%
  rec_CSP6(Var = "CS24", NewVar = "CS6") %>%
  rec_CSP12(Var = "CS42", NewVar = "CS12") %>%
  rec_DIP7(Var = "DIP14", NewVar = "DIP7") %>%
  rec_AG6() %>%
  rec_TYPEMPLOI() %>%
  rec_NENFANTS(Var = "n_NEnfantsMen") %>%
  rec_NENFANTS(Var = "n_NEnfantsHD") %>%
  rec_NENFANTS(Var = "n_NEnfantsTous") %>%
  rec_REVENUS(Var = "n_REVENUS", "n_REVENUScut") %>%
  rec_PATRIMOINE(Var = "n_PATRIMOINE", "n_PATRIMOINEcut") %>%
  rec_NAIS7() %>%
  rec_ETAMATRI() %>%
  select(IDENT_MEN, n_IdentIndiv, n_IdentConjoint, SEXE, DIP7, n_PATRIMOINEcut, CS12, AG6, AG, n_REVENUScut, NAIS7, ADULTE, SITUA, TYPEMPLOI, ETAMATRI, starts_with("n_"))

freq(indiv$CS12)
freq(indiv$DIP7)
freq(indiv$n_REVENUScut)
freq(indiv$AG6)
freq(indiv$n_NEnfantsHD)
freq(indiv$NAIS7)
freq(indiv$ADULTE)
freq(indiv$TYPEMPLOI)
freq(indiv$ETAMATRI)

femmes <- indiv %>% 
  filter(SEXE == "2") 
  # rename(n_IdentFemme = n_IdentIndiv, 
  #        n_IdentConjointFemme = n_IdentConjoint)

hommes <- indiv %>% 
  filter(SEXE == "1") 
  # rename(n_IdentHomme = n_IdentIndiv, 
  #        n_IdentConjointHomme = n_IdentConjoint)

coupleshet <- inner_join(hommes %>% mutate(n_IdentIndiv1 = n_IdentIndiv, 
                                           n_IdentConjoint1 = n_IdentConjoint), 
                         femmes %>% mutate(n_IdentIndiv2 = n_IdentIndiv, 
                                           n_IdentConjoint2 = n_IdentConjoint), 
                        by = c("n_IdentConjoint1" = "n_IdentIndiv2"), 
                        suffix = c("_H", "_F")) %>%
  mutate(anomalieIDENTMEN = if_else(IDENT_MEN_H != IDENT_MEN_F, T, F)) %>%
  mutate(hetero = "Hetero")

couplesgay <- inner_join(hommes %>% mutate(n_IdentIndiv1 = n_IdentIndiv, 
                                           n_IdentConjoint1 = n_IdentConjoint), 
                         hommes %>% mutate(n_IdentIndiv2 = n_IdentIndiv, 
                                           n_IdentConjoint2 = n_IdentConjoint), 
                         by = c("n_IdentConjoint1" = "n_IdentIndiv2"), 
                        suffix = c("_H", "_F")) %>%
  mutate(anomalieIDENTMEN = if_else(IDENT_MEN_H != IDENT_MEN_F, T, F)) %>%
  mutate(hetero = "Gay")

coupleslesb <- inner_join(femmes %>% mutate(n_IdentIndiv1 = n_IdentIndiv, 
                                            n_IdentConjoint1 = n_IdentConjoint), 
                          femmes %>% mutate(n_IdentIndiv2 = n_IdentIndiv, 
                                            n_IdentConjoint2 = n_IdentConjoint), 
                          by = c("n_IdentConjoint1" = "n_IdentIndiv2"), 
                          suffix = c("_H", "_F")) %>%
  mutate(anomalieIDENTMEN = if_else(IDENT_MEN_H != IDENT_MEN_F, T, F)) %>%
  mutate(hetero = "Lesb")

celibh <- hommes %>%
  filter(is.na(n_IdentConjoint))
celibh <- left_join(celibh %>% mutate(n_IdentIndiv1 = n_IdentIndiv, 
                                      n_IdentConjoint1 = n_IdentConjoint), 
                    femmes %>% mutate(n_IdentIndiv2 = n_IdentIndiv, 
                                      n_IdentConjoint2 = n_IdentConjoint), 
                    by = c("n_IdentConjoint1" = "n_IdentIndiv2"), 
                    suffix = c("_H", "_F")) %>%
  mutate(anomalieIDENTMEN = if_else(IDENT_MEN_H != IDENT_MEN_F, T, F)) %>%
  mutate(hetero = NA_character_)

celibf <- femmes %>%
  filter(is.na(n_IdentConjoint))

celibf <- left_join(celibf %>% mutate(n_IdentIndiv1 = n_IdentIndiv, 
                                      n_IdentConjoint1 = n_IdentConjoint), 
                    hommes %>% mutate(n_IdentIndiv2 = n_IdentIndiv, 
                                      n_IdentConjoint2 = n_IdentConjoint), 
                    by = c("n_IdentConjoint1" = "n_IdentIndiv2"), 
                    suffix = c("_F", "_H")) %>%
  mutate(anomalieIDENTMEN = if_else(IDENT_MEN_H != IDENT_MEN_F, T, F)) %>%
  mutate(hetero = NA_character_)

menagesHF <- bind_rows(coupleshet, couplesgay, coupleslesb, celibh, celibf) 

menagesHF <- menagesHF %>%
  mutate(couple = case_when(
    n_IdentIndiv1 > n_IdentConjoint1 ~ paste(n_IdentIndiv1, n_IdentConjoint1), 
    n_IdentIndiv1 < n_IdentConjoint1 ~ paste(n_IdentConjoint1, n_IdentIndiv1)
  ))

dupli <- menagesHF %>%
  filter(couple %in% menagesHF$couple[duplicated(menagesHF$couple)]) %>%
  filter(!is.na(couple))
freq(dupli$hetero)
menagesHF2 <- menagesHF[(!duplicated(menagesHF$couple) | is.na(menagesHF$couple)), ]
menagesHF <- menagesHF2

menagesHF <- menagesHF %>%
  mutate(IDENT_MEN = if_else(is.na(IDENT_MEN_H), IDENT_MEN_F, IDENT_MEN_H)) %>%
  select(-c("IDENT_MEN_F", "IDENT_MEN_H"))
menagesHF


# On ajoute les données des ménages correspondants
dep_men <- readRDS("Data_output/DepMenages.Rds")

familles <- readRDS("Data_output/familles.Rds") %>%
  left_join(dep_men %>%
              select(IDENT_MEN, STALOG)) %>%
  var_IDENTIFIANT("PREF", "IDENT_MEN", "n_IdentPRef") %>%
  rec_TYPMEN5() %>%
  rec_TYPMEN(Var = "TYPMEN15", "TYPMEN") %>%
  rec_REVENUS(Var = "NIVIE", NewVar = "NIVIEcut") %>%
  rec_REVENUS(Var = "REVSOC", "REVSOCcut") %>%
  rec_NENFANTS(Var = "NENFANTS")  %>%
  rec_TAU() %>%
  rec_STALOG() %>%
  rec_TYPLOG() %>%
  rec_PATRIB()

freq(familles$STALOG)
freq(familles$TYPLOG)
freq(familles$PATRIB)
    

familles <- familles %>%
  select(IDENT_MEN, n_config, TYPMEN5, TYPMEN,  PONDMEN, NIVIEcut, n_IdentPRef, NENFANTS, REVSOCcut, TAU, STALOG, TYPLOG, PATRIB, DNIVIE2)

freq(familles$TAU)
freq(familles$REVSOCcut)
freq(familles$DNIVIE2)
menagesHF <- left_join(menagesHF, familles, by = "IDENT_MEN") 

# On vérifie qu'on a pas de ménages en double
dup <- menagesHF[duplicated(menagesHF$IDENT_MEN), ]
freq(dup$TYPMEN5)
# ca de concerne que des ménages complexes

#On vire les menages aucun membre n'est la personne de référence du ménage (ie principal apporteur de ressource, actif, agé)
menagesHF <- menagesHF %>%
  filter(n_IdentPRef == n_IdentIndiv1 | n_IdentPRef == n_IdentConjoint1) 

menagesHF <- menagesHF %>%
  filter(ADULTE_H | ADULTE_F) 


dup <- menagesHF[menagesHF$IDENT_MEN %in% menagesHF$IDENT_MEN[duplicated(menagesHF$IDENT_MEN)], ]

dupli <- menagesHF[duplicated(menagesHF), ]
freq(dupli$TYPMEN5)
freq(menagesHF$TYPMEN5)


freq(familles$TYPMEN)
freq(familles$NIVIEcut)

# Les ménages de vieux et de très jeunes sont exclus
miss <- familles %>%
  filter(!(IDENT_MEN %in% menagesHF$IDENT_MEN))
freq(miss$TYPMEN5)
freq(miss$n_config) 

# On ajoute des données sur les enfants du ménage et hors ménage

enfantsMenage <- readRDS("Data_output/enfantsDuMenage.Rds") %>%
  group_by(IDENT_MEN) %>%
  summarise(n_NEnfantsMenage = n(), 
            n_AgeEnfantsMenage = mean(AG, na.rm = T))

menagesHF <- left_join(menagesHF, enfantsMenage, by = "IDENT_MEN")
summary(menagesHF$n_AgeEnfantsMenage)
summary(menagesHF$n_NEnfantsMenage)

enfantsMenage_moins13ans <- readRDS("Data_output/enfantsDuMenage.Rds") %>%
  filter(AG < 13)%>%
  group_by(IDENT_MEN) %>%
  summarise(n_NEnfantsMenage13 = n(), 
            n_AgeEnfantsMenage13 = mean(AG, na.rm = T))

menagesHF <- left_join(menagesHF, enfantsMenage_moins13ans, by = "IDENT_MEN")
summary(menagesHF$n_NEnfantsMenage13)
summary(menagesHF$n_AgeEnfantsMenage13)


enfantsHD <- readRDS("Data_output/enfantsHorsDom.Rds") %>%
  group_by(IDENT_MEN) %>%
  summarise(n_NEnfantsHD = n(), 
            n_AgeEnfantsHD = mean(AG, na.rm = T))

menagesHF <- left_join(menagesHF, enfantsHD, by = "IDENT_MEN")
summary(menagesHF$n_AgeEnfantsHD)
summary(menagesHF$n_NEnfantsHD)

enfantsHD_moins13ans <- readRDS("Data_output/enfantsHorsDom.Rds") %>%
  filter(AG < 13)%>%
  group_by(IDENT_MEN) %>%
  summarise(n_NEnfantsHD13 = n(), 
            n_AgeEnfantsHD13 = mean(AG, na.rm = T))

menagesHF <- left_join(menagesHF, enfantsHD_moins13ans, by = "IDENT_MEN")
summary(menagesHF$n_NEnfantsHD13)
summary(menagesHF$n_AgeEnfantsHD13)

# Recodages famills recomposées 
menagesHF <- menagesHF %>%
  mutate(RECOMP = case_when(
    n_BeauxEnfantsMen_H | n_BeauxEnfantsMen_F ~ T,
    TRUE ~ F)) %>%
  mutate(n_TYPMEN_new = case_when(
    TYPMEN5 == "Couple avec au moins un enfant" & RECOMP  ~ "Recomposée", 
    TYPMEN5 == "Couple avec au moins un enfant"  ~ "Traditionelle", 
    TYPMEN5 == "Famille monoparentale" ~ "Monoparentale", 
    TYPMEN5 == "Autre type de ménage (ménage complexe)" ~ "Complexe", 
    TRUE ~ TYPMEN5)) %>%
  mutate(n_TYPMEN_new = n_TYPMEN_new %>%
           fct_relevel(
             "Couple sans enfant", "Traditionelle", "Recomposée", "Monoparentale",
             "Personne seule", "Complexe"
           ))

# On ajoute des recodages qui synthétise les configurations recomposées
menagesHF <- menagesHF %>%
  mutate(
    n_SexeParent = case_when(
      n_EnfantsMen_H & n_EnfantsMen_F ~ "Mère et père", 
      n_EnfantsMen_F ~ "Mère", 
      n_EnfantsMen_H ~ "Père"), 
    n_EnfantsNouvelleUnion = case_when(
      n_NEnfantsCouple_F > 0 | n_NEnfantsCouple_H > 0 ~ "Oui", 
      TRUE ~ "Non"), 
    n_EnfantsUnionAnt = case_when(
      n_BeauxEnfantsMen_F & n_BeauxEnfantsMen_H ~ "Homme et femme", 
      n_BeauxEnfantsMen_F ~ "Homme", 
      n_BeauxEnfantsMen_H ~ "Femme", 
      TRUE ~ NA_character_), 
    n_RecompGenre = n_EnfantsUnionAnt)

# Recodages configuration familiale + sexe des parents
menagesHF <- menagesHF %>%
  mutate(
    n_TYPMEN_sexe = case_when(
      n_EnfantsMen_H & n_EnfantsMen_F ~ "Mère et père en couple", 
      n_EnfantsMen_H & n_BeauxEnfantsMen_F ~ "Père en couple", 
      n_EnfantsMen_F & n_BeauxEnfantsMen_H ~ "Mère en couple", 
      n_EnfantsMen_H & is.na(n_IdentConjoint_F) ~ "Père célibataire", 
      n_EnfantsMen_F & is.na(n_IdentConjoint_H) ~ "Mère célibataire")) %>%
  mutate(n_TYPMEN_sexe = n_TYPMEN_sexe %>%
           fct_relevel(
             "Mère et père en couple", 
             "Mère célibataire", "Père célibataire",
             "Mère en couple", "Père en couple"))


freq(menagesHF$RECOMP)
freq(menagesHF$n_TYPMEN_new)
freq(menagesHF$n_SexeParent)
freq(menagesHF$n_EnfantsUnionAnt)
freq(menagesHF$n_RecompGenre)
freq(menagesHF$n_TYPMEN_sexe)

# On peut comparer les deux recodages
table(menagesHF$n_TYPMEN_new, menagesHF$n_config, useNA = "ifany")

table(menagesHF$n_TYPMEN_new, menagesHF$n_TYPMEN_sexe, useNA = "ifany")

menagesHF$PONDMEN <- menagesHF$PONDMEN/mean(menagesHF$PONDMEN)

if(any(str_detect(list.files("Data_output"), "familles_FractionClasse.Rds"))){
  fraction <- readRDS("Data_output/familles_FractionClasse.Rds")
  familles <- left_join(familles, fraction, by = "IDENT_MEN")
}
  
  
saveRDS(menagesHF, file = "Data_output/familles_parents.Rds")

rm(celibf, celibh, couplesgay, coupleshet, coupleslesb, dup, dupli, familles, femmes, 
   hommes, indiv, menagesHF, menagesHF2, miss, dep_men, enfantsMenage, enfantsMenage_moins13ans, infosBDF, list_beauxenfantsHD, list_enfantsHD, list_enfantsHDremisencouple, fraction)



