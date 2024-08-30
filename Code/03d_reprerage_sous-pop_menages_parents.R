
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

# on ajoute les variables de travail domestqiue 
# NB de fois
travail_domestique <- c("AIDEENFANT", "BRICOLAGE", "CHANGEENFANT", 
                        "COURSES", "CUISINEA", "CUISINEB", "MENAGE", 
                        "JARDINAGE", "REPASSAGE", "VAISSELLE")



travail_domestique <- paste0("NB", travail_domestique)

dep_ind <- readRDS("Data_output/DepIndiv.Rds") %>%
  var_IDENTIFIANT(IdentIndiv = "NOI", IdentMenage = "IDENT_MEN", 
                  NewVarName = "n_IdentIndiv") %>%
  select(n_IdentIndiv, all_of(travail_domestique))

travail_domestique <- travail_domestique %>%
  str_remove("NB") %>%
  str_to_sentence()
travail_domestique
travail_domestique[c(1,3, 5,6)] <- c("Aide scolaire aux enfants",
                                     "Autre aide des enfants", 
                                     "Cuisine du quotidien", 
                                     "Cuisine de récéption")

names(dep_ind)[-1] <- travail_domestique
dep_ind[dep_ind == 9] <- NA_integer_
dep_ind[dep_ind == 8] <- NA_integer_

# A fait ou non 
travail_domestique <- c("AIDEENFANT", "BRICOLAGE", "CHANGEENFANT", 
                       "COURSES", "CUISINEA", "CUISINEB", "MENAGE", 
                       "JARDINAGE", "REPASSAGE", "VAISSELLE")

dep_ind2 <- readRDS("Data_output/DepIndiv.Rds") %>%
  var_IDENTIFIANT(IdentIndiv = "NOI", IdentMenage = "IDENT_MEN", 
                  NewVarName = "n_IdentIndiv") %>%
  select(n_IdentIndiv, all_of(travail_domestique)) %>%
  mutate_at(.vars = vars(travail_domestique), 
            .funs = function(x){
              x %>%
                fct_recode(
                  NULL = "", 
                  "Oui" = "1", 
                  "Non" = "2", 
                  NULL = "8", 
                  NULL = "9") %>%
                as.character()
            })

travail_domestique <- travail_domestique %>%
  str_to_sentence() 
travail_domestique
travail_domestique[c(1,3, 5,6)] <- c("Aide scolaire aux enfants",
                                     "Autre aide des enfants", 
                                     "Cuisine du quotidien", 
                                     "Cuisine de récéption")

names(dep_ind2)[-1] <- paste0("I_", travail_domestique)
dep_ind <- left_join(dep_ind2, dep_ind)
k <- travail_domestique[3]
for(k in travail_domestique){
  dep_ind[, k][
    (dep_ind[[paste0("I_", k)]] == "Non" 
     & !is.na(dep_ind[[paste0("I_", k)]]) 
     & (dep_ind[[k]] == 9 | is.na(dep_ind[k])))
  ] <- 0
}


indiv <- left_join(indiv, dep_ind)

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
    

familles_typemen <- familles %>%
  select(IDENT_MEN, TYPMEN5, n_IdentPRef)

menagesHF <- left_join(menagesHF, familles_typemen, by = "IDENT_MEN") 

# On vérifie qu'on a pas de ménages en double
dup <- menagesHF[duplicated(menagesHF$IDENT_MEN), ]
freq(dup$TYPMEN5)

#On vire les couples aucun membre n'est la personne de référence du ménage (ie principal apporteur de ressource, actif, agé)
menagesHF <- menagesHF %>%
  filter(n_IdentPRef == n_IdentIndiv1 | n_IdentPRef == n_IdentConjoint1) 
dup <- menagesHF[menagesHF$IDENT_MEN %in% menagesHF$IDENT_MEN[duplicated(menagesHF$IDENT_MEN)], ]
freq(dup$TYPMEN5)


familles <- familles %>%
  select(IDENT_MEN, TYPMEN, TYPMEN5, PONDMEN, NIVIEcut, NENFANTS, REVSOCcut, TAU, STALOG, TYPLOG, PATRIB, DNIVIE2, n_IdentPRef)

menagesHF <- left_join(familles, 
                       menagesHF %>%
                         select(-TYPMEN5, -n_IdentPRef),
                        by = "IDENT_MEN")


dup <- menagesHF[duplicated(menagesHF$IDENT_MEN), ]
freq(dup$TYPMEN5)
  
table(menagesHF$ADULTE_H, menagesHF$ADULTE_F, useNA  = "ifany")

menagesHF <- menagesHF %>%
   mutate(ADULTE_C = case_when(
     ADULTE_H | ADULTE_F ~ TRUE, 
     ADULTE_H & is.na(ADULTE_F) ~ T, 
     ADULTE_F & is.na(ADULTE_H) ~ T, 
     is.na(ADULTE_H) & is.na(ADULTE_F) ~ NA,
     TRUE ~ FALSE))

freq(menagesHF$ADULTE_C)
table(menagesHF$ADULTE_C, menagesHF$ADULTE_F, useNA  = "ifany")
table(menagesHF$ADULTE_C, menagesHF$ADULTE_H, useNA  = "ifany")


dup <- menagesHF[menagesHF$IDENT_MEN %in% menagesHF$IDENT_MEN[duplicated(menagesHF$IDENT_MEN)], ]

dupli <- menagesHF[duplicated(menagesHF), ]
freq(dupli$TYPMEN5)
freq(menagesHF$TYPMEN5)

freq(menagesHF$ADULTE_C)

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

enfantsHD_autreparent <- readRDS("Data_output/enfantsHorsDom.Rds") %>%
  filter(HODCO == "3") %>%
  group_by(IDENT_MEN) %>%
  summarise(n_NEnfantsHD_autreparent = n(), 
            n_AgeEnfantsHD_autreparent = mean(AG, na.rm = T))

menagesHF <- left_join(menagesHF, enfantsHD_autreparent, by = "IDENT_MEN")
summary(menagesHF$n_AgeEnfantsHD_autreparent)
summary(menagesHF$n_NEnfantsHD_autreparent)

# Recodages famills recomposées 
menagesHF <- menagesHF %>%
  mutate(
    RECOMP = case_when(
      (n_BeauxEnfantsMen_H | n_BeauxEnfantsMen_F) & (n_EnfantsMen_H | n_EnfantsMen_F) ~ T,
      TRUE ~ F), 
    TRAD = case_when(
      !n_BeauxEnfantsMen_H & !n_BeauxEnfantsMen_F & n_EnfantsMen_H & n_EnfantsMen_F ~ T,
      TRUE ~ F), 
    MONOP = case_when(
      (n_EnfantsMen_H & is.na(n_EnfantsMen_F)) | (n_EnfantsMen_F & is.na(n_EnfantsMen_H)) ~ T,
      TRUE ~ F
    ))
table(menagesHF$RECOMP, menagesHF$TRAD, useNA = "always") 
table(menagesHF$RECOMP, menagesHF$MONOP, useNA = "always") 
table(menagesHF$TRAD, menagesHF$MONOP, useNA = "always")

table(menagesHF$TYPMEN5, menagesHF$TRAD, useNA = "always")
table(menagesHF$TYPMEN5, menagesHF$RECOMP, useNA = "always")
table(menagesHF$TYPMEN5, menagesHF$MONOP, useNA = "always")


names(menagesHF)

# Variable "couple"
menagesHF <- menagesHF %>%
  mutate(n_CPL = case_when(
    !is.na(n_IdentIndiv_H) & !is.na(n_IdentIndiv_F) ~ T, 
    is.na(n_IdentIndiv_H) | is.na(n_IdentIndiv_F) ~ F))
freq(menagesHF$n_CPL)

freq(menagesHF$n_NEnfantsMenage)
# Variale "enfant dans le ménage"
menagesHF <- menagesHF %>%
  mutate(n_EnfantsMenage = case_when(
    n_NEnfantsMenage > 0 ~ T,
    n_NEnfantsMenage == 0 | is.na(n_NEnfantsMenage) ~ F))
freq(menagesHF$n_EnfantsMenage) 

# Variable "enfant du couple" 
table(menagesHF$n_NEnfantsCouple_F, menagesHF$n_NEnfantsCouple_H, useNA = "ifany")
menagesHF$n_NEnfantsCouple <- menagesHF$n_NEnfantsCouple_F
menagesHF <- menagesHF %>%
  mutate(n_EnfantsCouple = case_when(
    n_NEnfantsCouple > 0 ~ T,
    n_NEnfantsCouple == 0 | is.na(n_NEnfantsCouple) ~ F))
freq(menagesHF$n_EnfantsCouple) 
table(menagesHF$n_EnfantsCouple, menagesHF$n_EnfantsMenage, useNA = "ifany")

menagesHF <- menagesHF %>%
  mutate(n_TYPMEN_new = case_when(
    #!ADULTE_C ~ "Autre",
    TYPMEN5 == "Couple avec au moins un enfant" & RECOMP  ~ "Recomposée", 
    TYPMEN5 == "Couple avec au moins un enfant" & TRAD  ~ "Traditionelle", 
    TYPMEN5 == "Famille monoparentale" & MONOP ~ "Monoparentale", 
    TYPMEN5 == "Couple sans enfant" ~ "Couple sans enfant",
    TYPMEN5 == "Personne seule" ~ "Personne seule",
    TRUE ~ "Autre")) %>%
  mutate(n_TYPMEN_new = n_TYPMEN_new %>%
           fct_relevel(
             "Couple sans enfant", "Traditionelle", "Recomposée", "Monoparentale",
             "Personne seule", "Autre"
           ))
summary(menagesHF$AG_F[menagesHF$n_TYPMEN_new != "Autre"])
summary(menagesHF$AG_H[menagesHF$n_TYPMEN_new != "Autre"])
table(menagesHF$TYPMEN5, menagesHF$n_TYPMEN_new)

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

table(menagesHF$n_TYPMEN_new, menagesHF$n_EnfantsMen_H, useNA = "always")
# Recodages configuration familiale + sexe des parents
menagesHF <- menagesHF %>%
  mutate(
    n_TYPMEN_sexe = case_when(
      n_EnfantsMen_H & n_EnfantsMen_F ~ "Mère et père en couple", 
      n_EnfantsMen_H & n_BeauxEnfantsMen_F ~ "Père en couple", 
      n_EnfantsMen_F & n_BeauxEnfantsMen_H ~ "Mère en couple", 
      n_EnfantsMen_H & is.na(n_IdentConjoint_F) ~ "Père célibataire", 
      n_EnfantsMen_F & is.na(n_IdentConjoint_H) ~ "Mère célibataire",
      !is.na(n_IdentIndiv_F) & !is.na(n_IdentIndiv_H) & (!n_EnfantsMen_F | is.na(n_EnfantsMen_F)) & (!n_EnfantsMen_H | is.na(n_EnfantsMen_H)) ~ "Homme et femme en couple",
    (!n_EnfantsMen_F | is.na(n_EnfantsMen_F)) & !is.na(n_IdentIndiv_F) ~ "Femme célibataire",
    (!n_EnfantsMen_H | is.na(n_EnfantsMen_H)) & !is.na(n_IdentIndiv_H) ~ "Homme célibataire")) %>%
  mutate(n_TYPMEN_sexe = n_TYPMEN_sexe %>%
           fct_relevel(
             "Mère et père en couple", 
             "Mère célibataire", "Père célibataire",
             "Mère en couple", "Père en couple", 
             "Homme et femme en couple", 
             "Homme célibataire", 
             "Femme célibataire"))


freq(menagesHF$RECOMP)
freq(menagesHF$n_TYPMEN_new)
freq(menagesHF$n_SexeParent)
freq(menagesHF$n_EnfantsUnionAnt)
freq(menagesHF$n_RecompGenre)
freq(menagesHF$n_TYPMEN_sexe)
table(menagesHF$n_TYPMEN_new, menagesHF$n_TYPMEN_sexe, useNA = "always")

# On peut comparer les deux recodages
table(menagesHF$n_TYPMEN_new, menagesHF$n_TYPMEN_sexe, useNA = "ifany")

# On enregistre les ménages tels quels 
saveRDS(menagesHF, file = "Data_output/menages_parents.Rds")


# On ne conserve que les ménages ou la personne de référence ou son conjoint
menagesHF2 <- menagesHF %>%
  filter(ADULTE_C)

menagesHF2$PONDMEN <- menagesHF2$PONDMEN/mean(menagesHF2$PONDMEN)

infosBDF$champ2_men <- "ménages ordinaires résidant en France formés par des adultes (25-64 ans)"
infosBDF$champ2_fam <- "ménages ordinaires résidant en France formés par des adultes (25-64 ans) vivant avec au moins un enfant (moins de 25 ans)"


if(any(str_detect(list.files("Data_output"), "familles_FractionClasse.Rds"))){
  fraction <- readRDS("Data_output/familles_FractionClasse.Rds")
  menagesHF2 <- left_join(menagesHF2, fraction, by = "IDENT_MEN")
}
  

saveRDS(menagesHF2, file = "Data_output/familles_parents.Rds")
saveRDS(infosBDF, file = "Data_output/infosBDF.Rds")

rm(celibf, celibh, couplesgay, coupleshet, coupleslesb, dup, dupli, familles, femmes, 
   hommes, indiv, menagesHF, menagesHF2, miss, dep_men, enfantsMenage, enfantsMenage_moins13ans, infosBDF, list_beauxenfantsHD, list_enfantsHD, list_enfantsHDremisencouple, fraction)



