
indiv  <- readRDS("Data_output/parents.Rds")

familles <- readRDS("Data_output/familles_parents.Rds") 
familles$n_config <- familles$n_TYPMEN_new

dep_men <- readRDS("Data_output/DepMenages.Rds")

adultes <- indiv %>%
  filter(ADULTE) %>%
  left_join(familles %>% 
              select(IDENT_MEN, n_config, TYPMEN5)) %>%
  left_join(dep_men %>%
              select(IDENT_MEN, STALOG)) %>%
  filter(n_config == "Recomposée") %>%
  rec_SEXE() %>%
  rec_CSP12(Var = "CS42", "CS12") %>%
  rec_DIP7(NewVar = "DIP7") %>%
  mutate(n_StatutEnfants = case_when(
    n_RemisEnCoupleEnfantsMen & n_NEnfantsCouple>0 ~ "Précédante et actuelle", 
    n_RemisEnCoupleEnfantsMen | n_config == "Monoparentale" ~ "Précédante", 
    n_NEnfantsCouple>0 | n_config == "Traditionnelle" ~ "Actuelle", 
    TRUE ~ "Sans enfants")) %>%
  rec_NENFANTS("n_NEnfantsMen") %>%
  rec_NENFANTS("n_NBeauxEnfantsMen") %>%
  rec_REVENUS(Var = "n_REVENUS") %>%
  rec_PATRIMOINE() %>%
  rec_STALOG() %>%
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
    n_EnfantsMen & (!n_BeauxEnfantsMen | is.na(n_BeauxEnfantsMen)) ~ "Parent sans beaux-enfants", 
    (!n_EnfantsMen | is.na(n_EnfantsMen)) & n_BeauxEnfantsMen ~ "Beau-parent sans enfant",
    TRUE ~ "Sans enfants")) %>%
  mutate(n_StatutParent = as.factor(n_StatutParent) %>%
           fct_relevel("Parent sans beaux-enfants")) %>%
  # mutate(n_EnfantsMen = as.character(n_EnfantsMen)) %>%
  # mutate(n_BeauxEnfantsMen = as.character(n_BeauxEnfantsMen)) %>%
  mutate(n_EnfantsHD = case_when(n_EnfantsHD~ "Avec enfant(s)",
                                 TRUE ~ "Sans enfant"))
  

var_label(adultes$AG) <- NULL
### Tableaux variables sociaux démo des beaux-parents ####

dat <- adultes %>%
  as_survey_design(weights = PONDIND) %>%
  select("AG", "DIP7", "CS12", "n_REVENUS", "n_PATRIMOINE", "SEXE", "LOGEMENT", "n_StatutParent", "n_StatutEnfants") %>%
  mutate(Ensemble = T, 
         Effectifs = T)

freq(dat$variables$n_StatutParent)
freq(dat$variables$n_StatutEnfants)
freq(dat$variables$n_EnfantsHD)
freq(dat$variables$LOGEMENT)

var_label(dat$variables$AG) <- NULL

# tableau 
dat2 <- dat %>%
  #filter(n_StatutParent != "Parent sans beaux-enfants")  %>%
  # mutate(SEXE = fct_recode(SEXE, 
  #                          "Belle-mère (33%)" = "F", 
  #                          "Beau-père (67%)" = "H")) %>%
  mutate(n_StatutEnfants = fct_recode(n_StatutEnfants,
    "Sans enfants" = "Actuelle", 
    "Sans enfants" = "Sans enfants", 
    "Avec enfants" = "Précédante", 
    "Avec enfants" = "Précédante et actuelle")) %>%
  mutate(var = n_StatutParent %>%
           str_remove("avec enfant") %>%
           str_remove("sans enfant")
  ) %>%
  mutate(var = case_when(
    var == "Parent sans beaux-enfants" ~ "Parent sans beaux-enfants", 
    n_StatutEnfants == "Sans enfants" ~ "Beau-parent sans enfants", 
    n_StatutEnfants == "Avec enfants" ~ "Beau-parent avec enfants"
  )) 


freq(dat2$variables$var)
dat2$variables$SEXE


# Beaux pères vs pères 
tbl <- dat2 %>%
  filter(SEXE == "H") %>% 
  mutate(var = str_replace(var, "parent", "père")) %>%
  mutate(var = str_replace(var, "Parent", "Père")) %>%
  select(-c(n_PATRIMOINE, LOGEMENT, AG, SEXE, n_StatutParent, n_StatutEnfants, n_REVENUS)) %>%
  tbl_svysummary(
    by = var, 
     digits = everything() ~ 0,
     statistic = list(all_categorical() ~ "{p}", 
                      all_continuous2() ~ c("{mean}", "{sd}"), 
                      Effectifs ~ "{n_unweighted}"), 
    label = list(DIP7 ~ "Plus haut niveau de diplôme", 
                 CS12 ~ "Catégorie socioprofessionnelle"),
    missing = "no") %>%
  modify_header(all_stat_cols() ~ "**{level}**") %>%
  add_overall(last = T, col_label = "**Ensemble**") %>%
  add_stat_label() 

tbl
saveTableau(tbl, 
            type = "des", 
            label = "BeauxPeres_CaracteristiquesSociales",
            description = "Tableau statitsiques descriptives des beaux pères des enfants du ménage",
            champ = paste0("hommes appartenant à des familles recomposée (", infosBDF$champ, ")"), 
            ponderation = T,
            n = nrow(dat2$variables %>% filter(SEXE == "H")))


# Belles-mères vs mères
tbl <- dat2 %>%
  filter(SEXE == "F") %>% 
  mutate(var = str_replace(var, "parent", "mère")) %>%
  mutate(var = str_replace(var, "Beau", "Belle")) %>%
  mutate(var = str_replace(var, "Parent", "Mère")) %>%
  select(-c(n_PATRIMOINE, LOGEMENT, AG, SEXE, n_StatutParent, n_StatutEnfants)) %>%
  tbl_svysummary(
    by = var, 
    digits = everything() ~ 0,
    statistic = list(all_categorical() ~ "{p}", 
                     all_continuous2() ~ c("{mean}", "{sd}"), 
                     Effectifs ~ "{n_unweighted}"), 
    label = list(DIP7 ~ "Plus haut niveau de diplôme", 
                 CS12 ~ "Catégorie socioprofessionnelle", 
                 n_REVENUS ~ "Tranche de revenus"),
    missing = "no") %>%
  modify_header(all_stat_cols() ~ "**{level}**") %>%
  add_overall(last = T, col_label = "**Ensemble**")  %>%
  add_stat_label() 
tbl
saveTableau(tbl, 
            type = "des", 
            label = "BeauxMeres_CaracteristiquesSociales",
            description = "Tableau statitsiques descriptives des beaux mères des enfants du ménage",
            champ = paste0("femmes appartenant à des familles recomposée (", infosBDF$champ, ")"), 
            ponderation = T,
            n = nrow(dat2$variables %>% filter(SEXE == "H")))


tbl <- dat2 %>% 
  select(-c(n_PATRIMOINE, LOGEMENT, AG)) %>%
  tbl_strata(
    strata = var,
    .tbl_fun =
      ~ .x %>%
      select(-n_StatutParent) %>%
      tbl_svysummary(by = n_StatutEnfants, 
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
tbl


tot <- dat %>%
  select(-n_StatutParent) %>%
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
# library(sjstats)
# dat <- dat %>%
#   mutate(typo = paste0(n_StatutParent, " ", SEXE))
 dat$variables$PONDIND <- dat$allprob$PONDIND
# 
# # Sur le revenu
# kwtest <- weighted_mannwhitney(n_REVENUSmens ~ n_StatutParent + PONDIND, 
#                                dat$variables)
# kwtest
# kwtest <- weighted_mannwhitney(n_REVENUSmens ~ SEXE + PONDIND, 
#                                dat$variables)
# kwtest
# kwtest <- weighted_mannwhitney(n_REVENUSmens ~ typo + PONDIND, 
#                                dat$variables)
# kwtest
# 
# # Sur le patrimoine 
# kwtest <- weighted_mannwhitney(n_PATRIMOINE ~ n_StatutParent + PONDIND, 
#                                dat$variables)
# kwtest
# kwtest <- weighted_mannwhitney(n_PATRIMOINE ~ SEXE + PONDIND, 
#                                dat$variables)
# kwtest
# kwtest <- weighted_mannwhitney(n_PATRIMOINE ~ typo + PONDIND, 
#                                dat$variables)
# kwtest


## Tableau sexe statut parent ####

tab <- dat %>%
  tbl_svysummary(by = n_StatutEnfants, 
                 include = "SEXE") %>%
  add_p
tab

#install.packages("GGally")
library(GGally)

levels(dat$variables$n_StatutEnfants)
dat$variables <- dat$variables %>%
  mutate(n_StatutEnfants = n_StatutEnfants %>%
           factor(levels = c("Précédante", "Précédante et actuelle", "Actuelle", "Sans enfants")),
         SEXE = SEXE %>% as.factor()) 

gg <- dat$variables %>%
  ggplot() +
  aes(x = n_StatutEnfants, fill = SEXE, weight = PONDIND) +
  geom_bar(position = "fill") +
  geom_text(aes(by = n_StatutEnfants), 
            stat = "prop", position = position_fill(.5)) +
  xlab("Enfants issus de l'union") +
  ylab("Proportion") +
  labs(fill = "") +
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_manual(values = c("#998ec3", "#f1a340"), 
                    name = "Sexe")  +
  
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


## Graph situation famiales hommes femmes en couple 

dat3 <- adultes %>%
  mutate(n_StatutEnfants = fct_recode(n_StatutEnfants,
                                      "Sans enfants" = "Actuelle", 
                                      "Sans enfants" = "Sans enfants", 
                                      "Avec enfant(s)" = "Précédante", 
                                      "Avec enfant(s)" = "Précédante et actuelle")) %>%
  mutate(var = case_when(
    n_StatutEnfants == "Sans enfants" & n_EnfantsHD == "Sans enfant" ~ "Sans enfants", 
    n_StatutEnfants == "Avec enfant(s)" & n_EnfantsHD == "Sans enfant" ~ "Avec enfant(s) cohabitants",
    n_StatutEnfants == "Sans enfants" & n_EnfantsHD == "Avec enfant(s)" ~ "Avec enfant(s) non-cohabitants", 
    n_StatutEnfants == "Avec enfant(s)" & n_EnfantsHD == "Avec enfant(s)" ~ "Avec enfant(s) cohabitants et non-cohabitants")) 

freq(dat3$var)

dat3 <- dat3 %>% 
  pivot_wider(id_cols = IDENT_MEN, 
              names_from = SEXE, 
              values_from = var)





# Pett graph beaux-parents 
## Tableau sexe statut parent ####

tab <- dat %>%
  filter(n_StatutParent != "Parent sans beaux-enfants")  %>%
  tbl_svysummary(by = n_StatutEnfants, 
                 include = "SEXE") %>%
  add_overall() %>%
  add_p
tab

gg <- dat$variables %>%
  filter(n_StatutParent != "Parent sans beaux-enfants")  %>%
  mutate(SEXE = fct_recode(SEXE, 
    "Belle-mère (33%)" = "F", 
    "Beau-père (67%)" = "H"), 
    n_StatutEnfants = fct_recode(n_StatutEnfants,
      "Précédante\net actuelle" = "Précédante et actuelle", 
      "Sans\nenfant" = "Sans enfants")) %>%
  #mutate(n_StatutEnfants = fct_rev(n_StatutEnfants))
  ggplot() +
  aes(x = SEXE, fill = n_StatutEnfants, weight = PONDIND) +
  geom_bar(position = "fill") +
  geom_text(aes(by = SEXE), 
            size = 3,
            stat = "prop", position = position_fill(.5)) +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "Enfants issus\nde l'union") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_brewer(palette = "BrBG") + 
  
  #                   labels = paste0(levels(d_clust$CSP), 
  #                                   c(" (1,43%)", " (6,7%)", 
  #                                     " (18,88%)", " (25,96%)", 
  #                                     " (31,99%)", " (15,06%)"))) +
  theme_memoire() + 
  theme(legend.position="bottom")
#labs(caption = "Source : enquête PACS, 2020.\nChamp : population majeure résidant en France métropolitaine (hors Corse) en logement ordinaire.")

gg

saveTableau(gg, 
            type = "barplot", 
            label = "BP_Sexe_enfants", 
            description = "",
            champ = "beaux-parents apparentant aux familles recomposées vivants dans des ménages ordinaires", 
            ponderation = T,
            n = nrow(dat$variables %>% filter(n_StatutParent != "Parent sans beaux-enfants")))

### Graphique heatmap homme femme
