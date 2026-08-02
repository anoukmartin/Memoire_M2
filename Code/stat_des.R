
familles <- readRDS("Data_output/data_recode/menages_ageminmax.Rds")

indiv <- readRDS("Data_output/data_recode/indiv_in_menagesAge.Rds")

conso <- readRDS("Data_output/conso.Rds")

enfants <- indiv %>%
  filter(ENFANT == "1") %>%
  mutate(
    TAF = as.character(TAF)
  ) %>%
  mutate(
    TAF = as.character(TAF)
  ) %>%
  mutate(TAF2 = case_when(
    str_detect(TAF, "recomposée") ~ "Famille recomposée", 
    TRUE ~ TAF
  )) %>%
  mutate(TAF = TAF %>% fct_relevel(
    "Autre ménage (complexe)", "Famille monoparentale", "Famille recomposée (sans enfant du couple)",
    "Famille recomposée (avec enfant(s) du couple)", "Famille traditionnelle"
  ))

freq(enfants$TAF)
freq(enfants$POSITION_FRATERIE)

enfants %>%
  #filter(str_detect(TAF, "recomposée")) %>%
  filter(TAF != "Autre ménage (complexe)") %>%
  mutate(TAF = TAF %>% droplevels()) %>%
  as_survey_design(ids = IDENT_MEN, weights = PONDIND) %>%
  tbl_svysummary(include = c("TAF", "MOCO_DET", "SEXE", "AGE", "POSITION_FRATERIE", "RANG_ENFANT_SEXE"), 
                 by = MOCO_DET) %>%
  add_overall(last = T)




enfants  %>%
  as_survey_design(weights = PONDIND, 
                   ids = IDENT_MEN) %>%
  tbl_svysummary(
    include = c("SEXE_PARENT", "MOCO_DET", "MOCO_DET_SEXE", "TAF", "TAF2", "ANNEE_BDF", "TDM8_SEXE", "TDM8", "AGE"), 
    by = "TAF2", 
    percent = "column") %>%
  add_overall(last = T)



data_plot <- enfants %>%
  filter(str_detect(MOCO_DET, "recomposée")) %>%
  filter(str_detect(TAF, "recomposée")) %>%

  mutate(MOCO_DET_SEXE = MOCO_DET_SEXE %>%
           as.factor() %>% droplevels(), 
         MOCO_DET= MOCO_DET %>%
           as.factor() %>% droplevels(), 
         TDM8_SEXE = TDM8_SEXE %>% as.factor() %>%
           droplevels())
  
#iorder(data_plot$TDM8_SEXE)

data_plot$TDM8_SEXE <- data_plot$TDM8_SEXE |>
  fct_relevel(
    "Couple avec enfant(s) du couple, et avec au moins un enfant du père",
    "Couple sans enfant du couple, et avec au moins un enfant du père",
    "Couple avec enfant(s) du couple, et avec au moins un enfant de chacun des membres du couple",
    "Couple sans enfant du couple, et avec au moins un enfant de chacun des membres du couple",
    "Couple avec enfant(s) du couple, et avec au moins un enfant de la mère",
    "Couple sans enfant du couple, et avec au moins un enfant de la mère"
  )





levels(data_plot$TAF) <- sapply(levels(data_plot$TAF), function(x){insert_line_breaks(x, 15)})
levels(data_plot$MOCO_DET_SEXE) <- sapply(levels(data_plot$MOCO_DET_SEXE), function(x){insert_line_breaks(x, 20)})
levels(data_plot$TDM8_SEXE) <- sapply(levels(data_plot$TDM8_SEXE), function(x){insert_line_breaks(x, 35)})
levels(data_plot$MOCO_DET) <- sapply(levels(data_plot$MOCO_DET), function(x){insert_line_breaks(x, 25)})

## Reordering data_plot$TAF



enfants$SEXE_PARENT




ggplot(
  data_plot) +
  aes(x = MOCO_DET, fill = TDM8_SEXE, by = MOCO_DET, weight = PONDIND, 
      label = scales::percent(after_stat(prop), accuracy = 1)) +
  geom_bar(position = "fill", color = "black") +
    geom_text(
      stat = "prop", 
      position = position_fill(.5)
    ) +
    scale_y_continuous(labels = scales::percent) +
  labs(
    #title = titre,
    x = NULL,
    y = "Pourcentage",
  ) +
  coord_flip() +
  scale_fill_brewer(palette = "Paired")+
  theme_tufte()+
  theme(legend.position = "bottom", legend.box = "horizontal", legend.title = element_blank())
  


data_plot  %>%
  as_survey_design(weights = PONDIND, 
                   ids = IDENT_MEN) %>%
  tbl_svysummary(
    include = c("SEXE_PARENT", "MOCO_DET", "MOCO_DET_SEXE", "TAF", "TAF2", "ANNEE_BDF", "TDM8_SEXE", "TDM8", "AGE"), 
    by = "MOCO_DET", 
    percent = "column") %>%
  add_overall(last = T)


### Composition des ménages : 

data_plot <- familles %>%
  #filter(str_detect(MOCO_DET, "recomposée")) %>%
  filter(str_detect(TAF, "recomposée")) %>%
  mutate(TDM8_SEXE = TDM8_SEXE %>%
           as.character() %>%
           str_remove_all("Couple avec enfant\\(s\\) du couple, et ") %>%
           str_remove_all("Couple sans enfant du couple, et ") %>%
           as.factor()) 
freq(data_plot$TDM8_SEXE)
levels(data_plot$TAF) <- sapply(levels(data_plot$TAF), function(x){insert_line_breaks(x, 15)})
levels(data_plot$TDM8_SEXE) <- sapply(levels(data_plot$TDM8_SEXE), function(x){insert_line_breaks(x, 15)})

## Reordering data_plot$TAF

ggplot(
  data_plot) +
  aes(x = TAF, by = TAF, fill = TDM8_SEXE, weight = PONDMEN, 
      label = scales::percent(after_stat(prop), accuracy = 1)) +
    geom_bar(position = "fill", color = "black") +
    geom_text(
      stat = "prop", 
      position = position_fill(.5)
    ) +
    scale_y_continuous(labels = scales::percent) +
    labs(
      #title = titre,
      x = NULL,
      y = "Pourcentage",
    ) +
    coord_flip() +
    scale_fill_brewer(palette = "Set2")+
  theme_tufte() +
    theme(legend.position = "bottom", legend.box = "horizontal")
  
  
data_plot %>%
  mutate(TAF = TAF %>% droplevels())%>%
  as_survey_design(weights = PONDMEN, ids = IDENT_MEN)%>%
  tbl_svysummary(include = c("TAF", "TDM8", "TDM8_SEXE"),
                 by = "TAF") %>%
  add_overall(last = T)



## Stats des des adultes ####
indiv$NENF

adultes <- indiv %>%
  filter(ENFANT == "2") %>%
  filter(str_detect(TAF, "recomposée")) %>%
  rec_SEXE() %>%
  mutate(NENFANTS_COMMUNS_HORSDOM = case_when(
    COUPLE == "1" & is.na(NENFANTS_COMMUNS_HORSDOM) ~ 0, 
    COUPLE != "1" & is.na(NENFANTS_COMMUNS_HORSDOM) ~ NA, 
    TRUE ~ NENFANTS_COMMUNS_HORSDOM
  )) %>%
  mutate(NENFANTS_HORSDOM = case_when(
    is.na(NENFANTS_HORSDOM) ~ 0,
    TRUE ~ NENFANTS_HORSDOM
  )) %>%
  mutate(NENFANTS_PROPRES = NENFANTS - NENFANTS_COMMUNS, 
         NENFANTS_PROPRES_HORSDOM = case_when(
           is.na(NENFANTS_HORSDOM) & is.na(NENFANTS_COMMUNS_HORSDOM) ~ 0,
           !is.na(NENFANTS_HORSDOM) & is.na(NENFANTS_COMMUNS_HORSDOM) ~ NENFANTS_HORSDOM,
           is.na(NENFANTS_HORSDOM) & !is.na(NENFANTS_COMMUNS_HORSDOM) ~ NA_real_, # erreur
           TRUE ~ NENFANTS_HORSDOM - NENFANTS_COMMUNS_HORSDOM
         )) %>%
  
  mutate(NENFANTS_PROPRES = if_else(NENFANTS_PROPRES > 0, "Oui", "Non"),
         NENFANTS_COMMUNS = if_else(NENFANTS_COMMUNS > 0, "Oui", "Non"),
         NENFANTS_PROPRES_HORSDOM = if_else(NENFANTS_PROPRES_HORSDOM > 0, "Oui", "Non"),
         NENFANTS_COMMUNS_HORSDOM = if_else(NENFANTS_COMMUNS_HORSDOM > 0, "Oui", "Non"), 
        NBEAUX_ENFANTS  = if_else(NBEAUX_ENFANTS > 0, "Oui", "Non"), 
        NBEAUX_ENFANTS_HORSDOM  = case_when(
          NBEAUX_ENFANTS_HORSDOM > 0 ~"Oui", 
          TRUE ~"Non"), 
        NENFANTS_PROPRES_TOUS = case_when(
          NENFANTS_PROPRES == "Oui" | NENFANTS_PROPRES_HORSDOM == "Oui" ~ "Oui", 
          TRUE ~ "Non"
        ), 
        NENFANTS_COMMUNS_TOUS = case_when(
          NENFANTS_COMMUNS == "Oui" | NENFANTS_COMMUNS_HORSDOM == "Oui" ~ "Oui", 
          TRUE ~ "Non")) %>%
  mutate(MOCO_DET = MOCO_DET %>%
           str_remove("Adulte d’une famille recomposée") %>%
           str_remove_all("\\(|\\)") %>% str_trim() %>% str_to_sentence(), 
         
         TAF = TAF %>%
           str_remove("Famille recomposée") %>%
           str_remove_all("\\(|\\)") %>% str_trim() %>% str_to_sentence())
         

freq(adultes$MOCO_DET)
freq(adultes$NENFANTS_PROPRES)
freq(adultes$NENFANTS_HORSDOM)
freq(adultes$NENFANTS_PROPRES_HORSDOM)
freq(adultes$NENFANTS_COMMUNS_HORSDOM)

familles %>%
  mutate(TAF2 = if_else(str_detect(TAF, "recomposée"), 
                        "Famille recomposée", TAF)) %>%
  as_survey_design(weights = PONDMEN) %>%
  tbl_svysummary(include = c(TDM8_SEXE, TAF2), 
                 by = TAF2) %>%
  add_overall(last = T)


adultes %>%
  as_survey_design(weights = PONDIND, ids = IDENT_MEN) %>%
  tbl_svysummary(include = c(SEXE, NENFANTS_PROPRES, NENFANTS_COMMUNS, NENFANTS_PROPRES_HORSDOM, NENFANTS_COMMUNS_HORSDOM, NBEAUX_ENFANTS, NBEAUX_ENFANTS_HORSDOM, NENFANTS_PROPRES_TOUS, NENFANTS_COMMUNS_TOUS), by = SEXE) %>%
  add_overall(last = T)

adultes$NBEAUX_ENFANTS

adultes <- adultes %>%
  mutate(STAT_PARENTAL = case_when(
    NENFANTS_PROPRES == "Oui" & NBEAUX_ENFANTS == "Oui" ~ "Beau-parent et parent",
    NENFANTS_PROPRES == "Non" & NBEAUX_ENFANTS == "Oui" ~ "Beau-parent sans enfant(s)", 
    NENFANTS_PROPRES == "Oui" & NBEAUX_ENFANTS == "Non"  ~ "Parent sans beaux-enfants"
  ))
adultes$STAT_PARENTAL <- sapply(adultes$STAT_PARENTAL, function(x){insert_line_breaks(x, 30)})

adultes %>%
  ggplot() +
  aes(x = SEXE, fill = STAT_PARENTAL, by = SEXE, weight = PONDIND, 
      label = scales::percent(after_stat(prop), accuracy = 1)) +
  geom_bar(position = "stack", color = "black") +
  geom_text(
    stat = "prop", 
    position = position_stack(.5)
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 0.1)) +
  labs(
    #title = titre,
    x = "Sexe",
    y = "Pourcentage",
    fill = "Lien(s) avec les enfants issus d'union(s)\nprécédante(s) vivants dans le ménage"
  ) +
  coord_flip() +
  #facet_wrap(facet = ~SEXE, nrow = 2, ncol = 1) +
  scale_fill_brewer(palette = "Accent")  +
  theme_tufte()+
  theme(legend.position = "bottom", legend.box = "horizontal")




adultes %>%
  ggplot() +
  aes(x = STAT_PARENTAL, fill = NENFANTS_COMMUNS, by = STAT_PARENTAL, weight = PONDIND, 
      label = scales::percent(after_stat(prop), accuracy = 1)) +
  geom_bar(position = "fill", color = "black") +
  geom_text(
    stat = "prop", 
    position = position_fill(.5)
  ) +
 scale_y_continuous(labels = scales::percent) +
  labs(
    #title = titre,
    x = "(Beaux)-enfants issus d'unions précédantes",
    y = "Pourcentage",
    fill = "Enfant(s) issu(s) de l'union actuelle"
  ) +
  coord_flip() +
  facet_wrap(facet = ~SEXE, nrow = 2, ncol = 1) +
  scale_fill_manual(values = c("#dfc27d", "#80cdc1"))  +
  theme_tufte()+
  theme(legend.position = "bottom", legend.box = "horizontal")


ggplot(adultes) +
  aes(x = MOCO_DET, fill = TAF, weight = PONDIND, 
      label = scales::percent(after_stat(prop), accuracy = 1)) +
  geom_bar(position = "stack", color = "black") +
  geom_text(
    stat = "prop", 
    position = position_stack(.5)
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    #title = titre,
    x = NULL,
    y = "Pourcentage",
  ) +
  coord_flip() +
  facet_wrap(facet = ~SEXE, nrow = 2, ncol = 1) +
  scale_fill_manual(values = c("#dfc27d", "#80cdc1"))  +
  theme_tufte()+
  theme(legend.position = "bottom", legend.box = "horizontal", legend.title = element_blank())




adultes$NENFANTS_PROPRES_HORSDOM





enfantsp_hd <- adultes %>%
  ungroup() %>%
  mutate(
    ENFANTS_PROPRES_HORSDOM_CONJOINT = NBEAUX_ENFANTS_HORSDOM == "Oui",
    ENFANTS_PROPRES_HORSDOM = NENFANTS_PROPRES_HORSDOM == "Oui"
  ) %>%
  mutate(
    ENFANTS_PROPRES_HORSDOM_COUPLE = case_when(
      ENFANTS_PROPRES_HORSDOM & 
        ENFANTS_PROPRES_HORSDOM_CONJOINT ~ "les deux conjoints",
      ENFANTS_PROPRES_HORSDOM & SEXE == "1" ~ "seulement l'homme",
      ENFANTS_PROPRES_HORSDOM_CONJOINT & SEXE_CONJOINT == "1" ~ "seulement l'homme",
      ENFANTS_PROPRES_HORSDOM & SEXE == "2" ~ "seulement la femme",
      ENFANTS_PROPRES_HORSDOM_CONJOINT & SEXE_CONJOINT == "2" ~ "seulement la femme"
    )
  ) %>%
  group_by(IDENT_MEN) %>%
  summarise(
    ENFANTS_PROPRES_HORSDOM_COUPLE = paste0(unique(
      na.omit(ENFANTS_PROPRES_HORSDOM_COUPLE)
    ), collapse = "/")
  ) %>%
  mutate(ENFANTS_PROPRES_HORSDOM_COUPLE = if_else(
    ENFANTS_PROPRES_HORSDOM_COUPLE == "", "Aucun", ENFANTS_PROPRES_HORSDOM_COUPLE
  ))

freq(enfantsp_hd$ENFANTS_PROPRES_HORSDOM_COUPLE)  



data_plot <- familles %>%
  filter(str_detect(TAF, "recomposée")) %>%
  left_join(enfantsp_hd) %>%
  mutate(TDM8_SEXE = TDM8_SEXE %>%
           droplevels() %>%
           fct_infreq(w = PONDMEN))
levels(data_plot$TDM8_SEXE) <- sapply(levels(data_plot$TDM8_SEXE), 
                                      function(x) {insert_line_breaks(x, 20)})

                                      
ggplot(data_plot) + 
  aes(x = TDM8_SEXE, fill = ENFANTS_PROPRES_HORSDOM_COUPLE, by = TDM8_SEXE,
      weight = PONDMEN, 
      label = scales::percent(after_stat(prop), accuracy = 1)) +
  geom_bar(position = "fill", color = "black") +
  geom_text(
    stat = "prop", 
    position = position_fill(.5),
    size = 3
  ) +
  #scale_y_continuous(labels = scales::percent_format(scale = 0.1)) +
  labs(
    #title = titre,
    x = NULL,
    y = "Pourcentage",
    fill = "Enfant(s) vivant(s) hors-domicile"
  ) +
  scale_fill_brewer(palette ="Accent") +
 # coord_flip() +
  theme_tufte()+
  theme(legend.position = "bottom", legend.box = "horizontal")
