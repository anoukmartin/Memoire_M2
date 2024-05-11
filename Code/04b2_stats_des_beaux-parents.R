
indiv  <- readRDS("Data_output/parents.Rds")

familles <- readRDS("Data_output/famillesToutes.Rds") 
dep_men <- readRDS("Data_output/DepMenages.Rds")

adultes <- indiv %>%
  #filter(ENFANT != "1" & AG >= 18& AG <70) %>%
  #filter(n_BeauxEnfantsMen) %>%
  left_join(familles %>% 
              select(IDENT_MEN, n_config, TYPMEN5)) %>%
  left_join(dep_men %>%
              select(IDENT_MEN, STALOG)) %>%
  filter(n_config == "Recomposée") %>%
  filter(ENFANT != "1") %>%
  rec_TYPMEN5() %>%
  filter(TYPMEN5 != "Autre type de ménage (ménage complexe)") %>%
  # mutate(n_config = case_when(
  #   n_EnfantsMen & !n_RemisEnCoupleEnfantsMen & n_config == "Recomposée" ~ "Traditionelle", 
  #   TRUE ~ n_config
  # )) %>%
  rec_SEXE() %>%
  mutate(n_StatutEnfants = case_when(
    n_RemisEnCoupleEnfantsMen & n_NEnfantsCouple>0 ~ "précédante et actuelle", 
    n_RemisEnCoupleEnfantsMen | n_config == "Monoparentale" ~ "précédante", 
    n_NEnfantsCouple>0 | n_config == "Traditionelle" ~ "actuelle", 
    TRUE ~ "Sans enfants")) %>%
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
  


var_label(adultes$AG) <- NULL
### Tableaux variables sociaux démo des beaux-parents ####

dat <- adultes %>%
  mutate(SEXE = case_when(
    SEXE == "Homme" ~ "H", 
    SEXE == "Femme" ~ "F"
  )) %>%
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
  mutate(n_StatutParent = case_when(
    n_EnfantsMen & n_BeauxEnfantsMen ~ "Beau-parent avec enfant", 
    n_EnfantsMen & (!n_BeauxEnfantsMen | is.na(n_BeauxEnfantsMen)) ~ "Parent", 
    (!n_EnfantsMen | is.na(n_EnfantsMen)) & n_BeauxEnfantsMen ~ "Beau-parent sans enfant",
    TRUE ~ "Sans enfants")) %>%
  mutate(n_StatutParent = as.factor(n_StatutParent) %>%
           fct_relevel("Parent")) %>%
  # mutate(n_EnfantsMen = as.character(n_EnfantsMen)) %>%
  # mutate(n_BeauxEnfantsMen = as.character(n_BeauxEnfantsMen)) %>%
  as_survey_design(weights = PONDIND) %>%
  select("AG", "DIPL", "CS6", "n_REVENUSmens", "n_PATRIMOINE", "SEXE", "LOGEMENT", "n_StatutParent", "n_StatutEnfants") %>%
  mutate(Ensemble = T, 
         Effectifs = T)

freq(dat$variables$n_StatutParent)
freq(dat$variables$n_StatutEnfants)

var_label(dat$variables$AG) <- NULL

tbl <- dat %>%
  tbl_strata(
    strata = n_StatutParent,
    .tbl_fun =
      ~ .x %>%
      select(-n_StatutParent) %>%
      tbl_svysummary(by = SEXE, 
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
tbl


tot <- dat %>%
  select(-n_StatutParent) %>%
  tbl_svysummary(by = SEXE,
                 digits = everything() ~ 0,
                 statistic = list(all_categorical() ~ "{p}", 
                                  all_continuous2() ~ c("{mean}", "{sd}"), 
                                  Effectifs ~ "{n_unweighted}"), 
  ) %>%
  modify_header(all_stat_cols() ~ "{level}") %>%
  add_overall(last = F, col_label = "**Ens**")
# modify_header(all_stat_cols() ~ "**Ensemble**") 


tab <- tbl_merge(tbls = list(tbl, tot), tab_spanner = FALSE) 
tab

saveTableau(tab, 
            type = "des", 
            label = "BeauxParentsCaracteristiquesSociales",
            description = "Tableau statitsiques descriptives des beaux parents des enfants du ménage",
            champ = paste0("adultes appartenant à des familles recomposée (", infosBDF$champ, ")"), 
            ponderation = T,
            n = nrow(dat$variables))


# les test qui vont avec 
library(sjstats)
dat <- dat %>%
  mutate(typo = paste0(n_StatutParent, " ", SEXE))
dat$variables$PONDIND <- dat$allprob$PONDIND

# Sur le revenu
kwtest <- weighted_mannwhitney(n_REVENUSmens ~ n_StatutParent + PONDIND, 
                               dat$variables)
kwtest
kwtest <- weighted_mannwhitney(n_REVENUSmens ~ SEXE + PONDIND, 
                               dat$variables)
kwtest
kwtest <- weighted_mannwhitney(n_REVENUSmens ~ typo + PONDIND, 
                               dat$variables)
kwtest

# Sur le patrimoine 
kwtest <- weighted_mannwhitney(n_PATRIMOINE ~ n_StatutParent + PONDIND, 
                               dat$variables)
kwtest
kwtest <- weighted_mannwhitney(n_PATRIMOINE ~ SEXE + PONDIND, 
                               dat$variables)
kwtest
kwtest <- weighted_mannwhitney(n_PATRIMOINE ~ typo + PONDIND, 
                               dat$variables)
kwtest


## Tableau sexe statut parent ####

tab <- dat %>%
  tbl_svysummary(by = n_StatutEnfants, 
                 include = "SEXE") %>%
  add_p
tab

#install.packages("GGally")
library(GGally)


gg <- dat$variables %>%
  mutate(n_StatutEnfants %>% as.factor()) %>%
  ggplot() +
  aes(x = n_StatutEnfants, fill = SEXE, weight = PONDIND) +
  geom_bar(position = "fill") +
  geom_text(aes(by = n_StatutEnfants), 
            stat = "prop", position = position_fill(.5)) +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "") +
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_brewer(palette = "Pastel2") + 
  
  #                   labels = paste0(levels(d_clust$CSP), 
  #                                   c(" (1,43%)", " (6,7%)", 
  #                                     " (18,88%)", " (25,96%)", 
  #                                     " (31,99%)", " (15,06%)"))) +
  theme_memoire()
  #theme(legend.position="bottom")+
  #labs(caption = "Source : enquête PACS, 2020.\nChamp : population majeure résidant en France métropolitaine (hors Corse) en logement ordinaire.")

gg

saveTableau(gg, 
            type = "barplot", 
            label = "BPSexe", 
            description = "Sexe des parents et beaux-parents",
            champ = "Adultes apparentant aux familles recomposées vivants dans des ménages ordinaires", 
            ponderation = T,
            n = nrow(dat$variables))
