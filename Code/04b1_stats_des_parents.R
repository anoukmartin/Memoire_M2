

## Données #####################################################################
indiv  <- readRDS("Data_output/parents.Rds")

familles <- readRDS("Data_output/famillesToutes.Rds") 
dep_men <- readRDS("Data_output/DepMenages.Rds")
freq(parents$n_config)

parents <- indiv %>%
  filter(n_EnfantsMen) %>%
  left_join(familles %>% 
              select(IDENT_MEN, n_config, TYPMEN5)) %>%
  left_join(dep_men %>%
              select(IDENT_MEN, STALOG)) %>%
  rec_TYPMEN5() %>%
  filter(TYPMEN5 != "Autre type de ménage (ménage complexe)") %>%
  # mutate(n_config = case_when(
  #   n_EnfantsMen & !n_RemisEnCoupleEnfantsMen & n_config == "Recomposée" ~ "Traditionelle", 
  #   TRUE ~ n_config
  # )) %>%
  rec_SEXE() %>%
  # mutate(typo = paste0(n_config, " ", SEXE)) %>%
  # mutate(typo <- typo %>%  fct_relevel(
  #   "Traditionelle Homme", "Traditionelle Femme", "Monoparentale Homme",
  #   "Monoparentale Femme", "Recomposée Homme", "Recomposée Femme"
  # )) %>%
  rec_CSP6(Var = "CS24", NewVar = "CS6") %>% 
  #mutate(CS6 = CS6 %>% fct_relevel("Professions intermédiaires")) %>%
  rec_DIP(Var = "DIP14", NewVar = "DIPL") %>% 
  rec_AG6(Var = "AG6") %>%
  rec_STALOG() %>%
  mutate(n_NEnfantsMen = case_when(
    n_NEnfantsMen == 1 ~ "un", 
    n_NEnfantsMen == 2 ~ "deux", 
    n_NEnfantsMen == 3 ~ "trois",
    n_NEnfantsMen > 3 ~ "plus de trois")) %>%
  mutate(n_NEnfantsMen = n_NEnfantsMen %>%  fct_relevel(
    "un", "deux", "trois", "plus de trois")) %>% 
  mutate(n_NEnfantsHD = case_when(
    is.na(n_NEnfantsHD) ~ "aucun",
    n_NEnfantsHD == 1 ~ "un", 
    n_NEnfantsHD == 2 ~ "deux", 
    n_NEnfantsHD == 3 ~ "trois",
    n_NEnfantsHD > 3 ~ "plus de trois")) %>%
  mutate(n_NEnfantsHD = n_NEnfantsHD %>%  fct_relevel(
    "aucun", "un", "deux", "trois", "plus de trois")) %>%
  mutate(n_NEnfantsCouple = case_when(
    is.na(n_NEnfantsCouple) ~ "aucun",
    n_NEnfantsCouple == 1 ~ "un", 
    n_NEnfantsCouple == 2 ~ "deux", 
    n_NEnfantsCouple == 3 ~ "trois",
    n_NEnfantsCouple > 3 ~ "plus de trois")) %>%
  mutate(n_NEnfantsCouple = n_NEnfantsCouple %>%  fct_relevel(
    "aucun", "un", "deux", "trois", "plus de trois")) 


freq(parents$TYPMEN5)
freq(parents$STALOG)


dat <- parents %>%
  as_survey_design(weights = PONDIND) %>%
  select("n_NEnfantsMen", "n_AgeEnfantsMen", "n_NEnfantsCouple", "n_AgeEnfantsCouple", "n_NEnfantsHD", "n_AgeEnfantsHD", "SEXE", "n_config") %>%
  mutate(Ensemble = T, 
         Effectifs = T) 





tbl <- dat %>%
  tbl_strata(
    strata = n_config,
    .tbl_fun =
      ~ .x %>%
      tbl_svysummary(by = SEXE, , 
                     digits = everything() ~ 0,
                     statistic = list(all_categorical() ~ "{p}", 
                                      all_continuous2() ~ c("{mean}", "{sd}"), 
                                      Effectifs ~ "{n_unweighted}"), 
                     type = list(n_AgeEnfantsCouple ~ 'continuous2')
      ) %>%
      add_stat_label() %>%
      modify_header(all_stat_cols() ~ "**{level}**") %>%
      add_overall(last = T, col_label = "**Ensemble**"),
    .header = "**{strata}**"
  )

tot <- dat %>%
  select(-SEXE) %>%
  tbl_svysummary( 
    digits = everything() ~ 0,
    statistic = list(all_categorical() ~ "{p}", 
                     all_continuous2() ~ c("{mean}", "{sd}"), 
                     Effectifs ~ "{n_unweighted}"), 
  ) %>%
  modify_header(all_stat_cols() ~ "**Ensemble**") 
eff <- dat %>%
  select(-SEXE) %>%
  tbl_svysummary( 
    digits = everything() ~ 0,
    statistic = list(all_categorical() ~ "{n_unweighted}", 
                     all_continuous2() ~ "{N_obs_unweighted}")
  ) %>%
  modify_header(all_stat_cols() ~ "**Effectifs**") 

tbl_merge(tbls = list(tbl, tot, eff), tab_spanner = FALSE) %>%
  as_kable_extra(digits = 1, booktabs = T, longtable = TRUE,
                 caption = "Enfants des parents", format = "latex") %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down", "repeat_header"),
    font_size = 7) %>%
  add_kablesource_note(N = 2)



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
  select("AG", "DIPL", "CS6", "n_REVENUSmens", "n_PATRIMOINE", "SEXE", "LOGEMENT", "n_config") %>%
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
                                      Effectifs ~ "{n_unweighted}")
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
  ) %>%
  modify_header(all_stat_cols() ~ "{level}") %>%
  add_overall(last = F, col_label = "**Ens**")
# modify_header(all_stat_cols() ~ "**Ensemble**") 


eff <- dat %>%
  select(-SEXE, -n_config) %>%
  tbl_svysummary( 
    digits = everything() ~ 0,
    statistic = list(all_categorical() ~ "{n_unweighted}",
                     all_continuous2() ~ "{N_nonmiss_unweighted}")
  ) %>%
  modify_header(all_stat_cols() ~ "**Eff**") 

tab <- tbl_merge(tbls = list(tbl, tot), tab_spanner = FALSE) 
tab

saveTableau(tab, 
            type = "des", 
            label = "ParentsCaracteristiquesSociales",
            description = "Tableau statitsiques descriptives des parents des enfants du ménage",
            champ = paste0("parents d'enfants vivants en ", infosBDF$champ), 
            ponderation = T,
            n = nrow(dat$variables))

