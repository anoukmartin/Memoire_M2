

## Données #####################################################################
indiv  <- readRDS("Data_output/parents.Rds")
infosBDF <- readRDS("Data_output/infosBDF.Rds")
familles <- readRDS("Data_output/familles_parents.Rds") 
familles$n_config <- familles$n_TYPMEN_new
dep_men <- readRDS("Data_output/DepMenages.Rds")

parents <- indiv %>%
  filter(ADULTE) %>%
  filter(n_EnfantsMen) %>%
  left_join(familles %>% 
              select(IDENT_MEN, n_config, TYPMEN5)) %>%
  left_join(dep_men %>%
              select(IDENT_MEN, STALOG)) %>%
  filter(TYPMEN5 != "Autre type de ménage (ménage complexe)") %>%
  rec_SEXE() %>%
  rec_CSP6(Var = "CS24", NewVar = "CS6") %>% 
  rec_CSP12(Var = "CS42", "CS12") %>%
  rec_DIP7(NewVar = "DIP7") %>%
  rec_REVENUS("n_REVENUS") %>%
  #mutate(CS6 = CS6 %>% fct_relevel("Professions intermédiaires")) %>%
  rec_DIP(Var = "DIP14", NewVar = "DIPL") %>% 
  rec_AG6(Var = "AG6") %>%
  rec_STALOG() %>%
  rec_NENFANTS(Var = "n_NEnfantsMen") %>%
  rec_NENFANTS(Var = "n_NEnfantsHD") %>%
  rec_PATRIMOINE()

freq(parents$n_config)
freq(parents$TYPMEN5)
freq(parents$STALOG)
freq(parents$n_PATRIMOINE)

dat <- parents %>%
  as_survey_design(weights = PONDIND) %>%
  select("n_NEnfantsMen", "n_AgeEnfantsMen", "n_NEnfantsCouple", "n_AgeEnfantsCouple", "n_NEnfantsHD", "n_AgeEnfantsHD", "SEXE", "n_config") %>%
  mutate(Ensemble = T, 
         Effectifs = T) 


### Tableau caractéristiques sociales des parents des familles recomposées ####

dat <- parents %>%
  mutate(IPROPLOC = fct_recode(IPROPLOC,
                               "en titre" = "1",
                               "herbergé" = "2"
  )) %>%
  mutate(LOGEMENT = paste0(STALOG, " ", IPROPLOC)) %>%
  mutate(LOGEMENT = case_when(
    str_starts(LOGEMENT, "Propriétaire") ~ LOGEMENT, 
    str_starts(LOGEMENT, "Accédant") ~ LOGEMENT, 
    TRUE ~ STALOG)) %>%
  mutate(SEXE = fct_recode(SEXE, 
                           "Mère" = "Femme", 
                           "Père" = "Homme")) %>%
  as_survey_design(weights = PONDIND) %>%
  select("AG", "DIP7", "CS12", "n_REVENUS", "n_PATRIMOINE", "SEXE", "LOGEMENT", "n_config") %>%
  mutate(Ensemble = T, 
         Effectifs = T)
freq(dat$variables$LOGEMENT)


var_label(dat$variables$AG) <- NULL

tbl <- dat %>%
  tbl_strata(
    strata = n_config,
    .tbl_fun =
      ~ .x %>%
      select(-n_config) %>%
      tbl_svysummary(by = SEXE, , 
                     digits = everything() ~ 0,
                     statistic = list(all_categorical() ~ "{p}", 
                                      all_continuous2() ~ c("{mean}", "{sd}"), 
                                      Effectifs ~ "{n_unweighted}"), 
                     missing = "no"
      ) %>%
      add_stat_label() %>%
      modify_header(all_stat_cols() ~ "{level}") %>%
      add_overall(last = F, col_label = "**Ens**"),
    .header = "**{strata}**"
  )

tot <- dat %>%
  select(-n_config) %>%
  tbl_svysummary(by = SEXE,
                 digits = everything() ~ 0,
                 statistic = list(all_categorical() ~ "{p}", 
                                  all_continuous2() ~ c("{mean}", "{sd}"), 
                                  Effectifs ~ "{n_unweighted}"),
                 missing = "no"
  ) %>%
  modify_header(all_stat_cols() ~ "{level}") %>%
  add_overall(last = F, col_label = "**Ens**")
# modify_header(all_stat_cols() ~ "**Ensemble**") 


# eff <- dat %>%
#   select(-SEXE, -n_config) %>%
#   tbl_svysummary( 
#     digits = everything() ~ 0,
#     statistic = list(all_categorical() ~ "{n_unweighted}",
#                      all_continuous2() ~ "{N_nonmiss_unweighted}")
#   ) %>%
#   modify_header(all_stat_cols() ~ "**Eff**") 

tab <- tbl_merge(tbls = list(tbl, tot), tab_spanner = FALSE) 
tab

saveTableau(tab, 
            type = "des", 
            label = "ParentsCaracteristiquesSociales",
            description = "Caractéristiques sociales des parents des enfants du ménage en fonction de la configuration familiale",
            champ = paste0("parents d'enfants vivants en ", infosBDF$champ), 
            ponderation = T,
            n = nrow(dat$variables))

