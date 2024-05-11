
infosBDF <- readRDS("Data_output/infosBDF.Rds")

indiv <- readRDS("Data_output/parents.Rds") %>%
  rec_DIP(Var = "DIP14", NewVar = "DIPL") %>%
  rec_CSP6(Var = "CS24", NewVar = "CS6") %>%
  rec_CSP12(Var = "CS42", NewVar = "CS12") %>%
  rec_DIP7(Var = "DIP14", NewVar = "DIP7") %>%
  rec_AG6() %>%
  select(IDENT_MEN, n_IdentIndiv, n_IdentConjoint, SEXE, DIPL, DIP7, n_REVENUS, n_PATRIMOINE, CS6, CS12, AG6)

freq(indiv$CS12)
freq(indiv$DIP7)


femmes <- indiv %>% 
  filter(SEXE == "2") %>%
  select(-n_IdentConjoint) %>%
  rename(n_IdentFemme = n_IdentIndiv)
  

hommes <- indiv %>% 
  filter(SEXE == "1") %>%
  rename(n_IdentHomme = n_IdentIndiv)

couples <- full_join(hommes, femmes, 
                        by = c("n_IdentConjoint" = "n_IdentFemme"), 
                        suffix = c("_H", "_F")) %>% 
  select(-IDENT_MEN_F) %>%
  rename(IDENT_MEN = "IDENT_MEN_H")


familles <- readRDS("Data_output/familles.Rds") %>%
  rec_TYPMEN5() %>%
  rec_TYPMEN(Var = "TYPMEN15", "TYPMEN") %>%
  select(IDENT_MEN, n_config, TYPMEN5, TYPMEN,  PONDMEN)
freq(familles$TYPMEN)

