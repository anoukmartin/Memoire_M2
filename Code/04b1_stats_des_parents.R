

## Données #####################################################################
indiv  <- readRDS("Data_output/parents.Rds")
infosBDF <- readRDS("Data_output/infosBDF.Rds")
familles <- readRDS("Data_output/familles_parents.Rds") 
familles$n_config <- familles$n_TYPMEN_sexe
familles$n_config <- familles$n_config %>%
  fct_recode(
    "En couple avec l'autre parent" = "Mère et père en couple",
    "Célibataire" = "Mère célibataire",
    "Célibataire" = "Père célibataire",
    "En couple avec une autre personne" = "Mère en couple",
    "En couple avec une autre personne" = "Père en couple"
  ) 
dep_men <- readRDS("Data_output/DepMenages.Rds")
familles$n_TYPMEN_sexe
table(parents$n_NEnfantsUnionAnt, parents$COUPLE, useNA = "always")

parents <- indiv %>%
  filter(ADULTE) %>%
  #filter(n_EnfantsMen) %>%
  left_join(familles %>% 
              select(IDENT_MEN, n_config, TYPMEN5, n_TYPMEN_sexe, n_TYPMEN_new)) %>%
  left_join(dep_men %>%
              select(IDENT_MEN, STALOG)) %>%
  #filter(TYPMEN5 != "Autre type de ménage (ménage complexe)") %>%
  mutate(n_TYPMEN_new = droplevels(n_TYPMEN_new), 
         n_TYPMEN_sexe = droplevels(n_TYPMEN_sexe), 
         n_config = droplevels(n_config))

# Recodages 
parents <- parents %>%
  mutate(COUPLE = case_when(
    COUPLE %in% c("3", "2") ~ "Célibataire", 
    COUPLE == "1" ~ "En couple"
  )) %>%
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
freq(parents$COUPLE)



dat <- parents %>%
  as_survey_design(weights = PONDIND) %>%
  select("AG", "n_NEnfantsMen", "n_AgeEnfantsMen", "n_NEnfantsCouple", "n_AgeEnfantsCouple", "n_NEnfantsHD", "n_AgeEnfantsHD", "SEXE", "n_config") %>%
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
  # mutate(SEXE = fct_recode(SEXE, 
  #                          "Mère" = "Femme", 
  #                          "Père" = "Homme")) %>%
  mutate(n_EnfantsUnionAnt = case_when(
    n_NEnfantsUnionAnt > 0 ~ T, 
    TRUE ~ F
  )) %>%
  mutate(n_config = case_when(
    #COUPLE == "En couple" & n_NEnfantsUnionAnt > 0 & n_NEnfantsCouple >0 ~ "PRECP", 
    COUPLE == "En couple" & n_NEnfantsUnionAnt > 0 ~ "PREC", 
    COUPLE == "Célibataire" & n_NEnfantsUnionAnt > 0 ~ "PC", 
    COUPLE == "En couple" & n_NEnfantsCouple >0 ~ "PEC",
    COUPLE == "En couple" ~ "EC",
    COUPLE == "Célibataire" ~ "C")) %>%
  mutate(n_config = n_config %>% 
           as.factor() %>% 
           relevel("C", "EC", "PEC", "PREC", "PC")) %>%
    as_survey_design(weights = PONDIND) %>%
  select("DIP7", "CS12", "n_REVENUS", "n_config", "SEXE") %>%
  mutate(Ensemble = T, 
         Effectifs = T) 
  
tbl_peres <- dat %>%
  filter(SEXE == "Homme") %>%
  select(-SEXE) %>%
  tbl_svysummary(by = n_config, 
                 digits = everything() ~ 0,
                 statistic = list(all_categorical() ~ "{p}", 
                                  all_continuous2() ~ c("{mean}", "{sd}"), 
                                  Effectifs ~ "{n_unweighted}"), 
                 missing = "no", 
                 label = list(DIP7 ~ "Plus haut niveau de diplôme", 
                              CS12 ~ "Catégorie socioprofessionnelle", 
                              n_REVENUS ~ "Tranche de revenus")
  ) %>%
  add_stat_label() %>%
  modify_header(all_stat_cols() ~ "{level}") %>%
  add_overall(last = T, col_label = "**Ensemble**")


saveTableau(tbl_peres, 
            type = "des", 
            label = "PeresCaracteristiquesSociales",
            description = "Caractéristiques sociales des hommes en fonction de leur statut parental",
            champ = paste0("Hommes vivants en ", infosBDF$champ), 
            ponderation = T,
            n = tbl_peres$N)


tbl_meres <- dat %>%
  filter(SEXE == "Femme") %>%
  select(-SEXE) %>%
  tbl_svysummary(by = n_config, 
                 digits = everything() ~ 0,
                 statistic = list(all_categorical() ~ "{p}", 
                                  all_continuous2() ~ c("{mean}", "{sd}"), 
                                  Effectifs ~ "{n_unweighted}"), 
                 missing = "no", 
                 label = list(DIP7 ~ "Plus haut niveau de diplôme", 
                              CS12 ~ "Catégorie socioprofessionnelle", 
                              n_REVENUS ~ "Tranche de revenus")
  ) %>%
  add_stat_label() %>%
  modify_header(all_stat_cols() ~ "{level}") %>%
  add_overall(last = T, col_label = "**Ensemble**")

tbl_meres

saveTableau(tbl_meres, 
            type = "des", 
            label = "MeresCaracteristiquesSociales",
            description = "Caractéristiques sociales des femme en fonction de leur statut parental",
            champ = paste0("Femmes vivants en ", infosBDF$champ), 
            ponderation = T,
            n = tbl_peres$N)


dat %>%
  filter(SEXE == "Père") %>%
  tbl_strata(
    strata = n_config,
    .tbl_fun =
      ~ .x %>%
      select(-SEXE) %>%
      tbl_svysummary(by = COUPLE, 
                     digits = everything() ~ 0,
                     statistic = list(all_categorical() ~ "{p}", 
                                      all_continuous2() ~ c("{mean}", "{sd}"), 
                                      Effectifs ~ "{n_unweighted}"), 
                     missing = "no", 
                     label = list(DIP7 ~ "Plus haut niveau de diplôme", 
                                  CS12 ~ "Catégorie socioprofessionnelle", 
                                  n_REVENUS ~ "Tranche de revenus")
      ) %>%
      add_stat_label() %>%
      modify_header(all_stat_cols() ~ "{level}") %>%
      add_overall(last = T, col_label = "**Ensemble**"),
    .header = "**{strata}**"
  )
tbl_peres

tot <- dat %>%
  select(-SEXE) %>%
  tbl_svysummary(by = n_config,
                 digits = everything() ~ 0,
                 statistic = list(all_categorical() ~ "{p}", 
                                  all_continuous2() ~ c("{mean}", "{sd}"), 
                                  Effectifs ~ "{n_unweighted}"),
                 missing = "no", 
                 label = list(DIP7 ~ "Plus haut niveau de diplôme", 
                              CS12 ~ "Catégorie socioprofessionnelle", 
                              n_REVENUS ~ "Tranche de revenus")
  ) %>%
  modify_header(all_stat_cols() ~ "**{level}**") %>%
  add_overall(last = T, col_label = "**Ensemble**") %>%
  add_p()
# modify_header(all_stat_cols() ~ "**Ensemble**") 
tot

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

