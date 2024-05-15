
infosBDF <- readRDS("Data_output/infosBDF.Rds")


indiv <- readRDS("Data_output/parents.Rds") %>%
  filter(NONENFANT) %>%
  rec_DIP(Var = "DIP14", NewVar = "DIPL") %>%
  rec_CSP6(Var = "CS24", NewVar = "CS6") %>%
  rec_CSP12(Var = "CS42", NewVar = "CS12") %>%
  rec_DIP7(Var = "DIP14", NewVar = "DIP7") %>%
  rec_AG6() %>%
  rec_NENFANTS(Var = "n_NEnfantsMen") %>%
  rec_NENFANTS(Var = "n_NEnfantsHD") %>%
  rec_NENFANTS(Var = "n_NEnfantsTous") %>%
  rec_REVENUS(Var = "n_REVENUS", "n_REVENUScut") %>%
  rec_PATRIMOINE(Var = "n_PATRIMOINE", "n_PATRIMOINEcut") %>%
  rec_NAIS7() %>%
  select(IDENT_MEN, n_IdentIndiv, n_IdentConjoint, SEXE, DIP7, n_PATRIMOINEcut, CS12, AG6, AG, n_REVENUScut, NAIS7, starts_with("n_"))

freq(indiv$CS12)
freq(indiv$DIP7)
freq(indiv$n_REVENUScut)
freq(indiv$AG6)
freq(indiv$n_NEnfantsHD)
freq(indiv$NAIS7)


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
  rec_TYPLOG()

freq(familles$STALOG)
freq(familles$TYPLOG)
    


familles <- familles %>%
  select(IDENT_MEN, n_config, TYPMEN5, TYPMEN,  PONDMEN, NIVIEcut, n_IdentPRef, NENFANTS, REVSOCcut, TAU, STALOG, TYPLOG)

freq(familles$TAU)
freq(familles$REVSOCcut)
menagesHF <- left_join(menagesHF, familles, by = "IDENT_MEN") 

# On vérifie qu'on a pas de ménages en double
dup <- menagesHF[duplicated(menagesHF$IDENT_MEN), ]
freq(dup$TYPMEN5)
# ca de concerne que des ménages complexes

#On vire les menages aucun membre n'est la personne de référence du ménage (ie principal apporteur de ressource, actif, agé)
menagesHF <- menagesHF %>%
  filter(n_IdentPRef == n_IdentIndiv1 | n_IdentPRef == n_IdentConjoint1) 

menagesHF <- menagesHF %>%
  filter(AG_F > 25 & AG_F <= 65 | AG_H > 25 & AG_H <= 65) 


dup <- menagesHF[menagesHF$IDENT_MEN %in% menagesHF$IDENT_MEN[duplicated(menagesHF$IDENT_MEN)], ]

dupli <- menagesHF[duplicated(menagesHF), ]
freq(dup$TYPMEN5)
freq(menagesHF$TYPMEN5)

freq(familles$TYPMEN)
freq(familles$NIVIEcut)

# Les ménages de vieux et de très jeunes sont exclus
miss <- familles %>%
  filter(!(IDENT_MEN %in% menagesHF$IDENT_MEN))
freq(miss$TYPMEN5)
freq(miss$n_config) 

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
    TRUE ~ TYPMEN5))

freq(menagesHF$RECOMP)
freq(menagesHF$n_TYPMEN_new)

# On peut comparer les deux recodages
table(menagesHF$n_TYPMEN_new, menagesHF$n_config, useNA = "ifany")

menagesHF$PONDMEN <- menagesHF$PONDMEN/mean(menagesHF$PONDMEN)

saveRDS(menagesHF, file = "Data_output/familles_parents.Rds")

rm(celibf, celibh, couplesgay, coupleshet, coupleslesb, dup, dupli, familles, femmes, 
   hommes, indiv, menagesHF, menagesHF2, miss)



