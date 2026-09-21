

infosBDF <- readRDS("Data_output/infosBDF.Rds")


familles <- readRDS("Data_output/data_recode/menages_ageminmax.Rds") %>%
  #select(IDENT_MEN, AISE, NIVIE, SEXEREP) %>%
  rec_SEXE(Var = "SEXEREP")



data <- familles %>%
  mutate(
    NIVIE = NIVIE/1200) %>%
  filter(TDM8_SEXE != "Autre ménage (complexe)")%>%
  filter(COUPLE_SEXE == "Couple de sexes différents")%>%
  mutate(TDM8 = droplevels(TDM8), 
         TDM8_SEXE = droplevels(TDM8_SEXE), 
         ANNEE_BDF = as.character(ANNEE_BDF)) %>%
  mutate(TDM8_enfantcouple = case_when(
    str_detect(TDM8_SEXE, "Couple sans enfant") ~ "Couple sans enfant commun", 
    TRUE ~ "Couple avec enfant(s) commun(s)"
  ), 
  TDM8_enfantpropre = case_when(
    str_detect(TDM8_SEXE, "chacun des membres du couple") ~ "enfant(s) des deux membres du couple", 
    str_detect(TDM8_SEXE, "mère") ~ "un ou des enfant(s) de la mère",
    str_detect(TDM8_SEXE, "père") ~ "un ou des enfant(s) du père",
    TRUE ~ "sans enfant d'autre union"))
freq(data$TDM8_enfantcouple)
freq(data$TDM8_enfantpropre)

unique(data$TDM8_SEXE)
var_label(data$NIVIE) <- "Niveau de vie mensuel (en centaine d'euros)"
var_label(data$n_FractionClasse) <- "Position sociale du ménage"
var_label(data$TDM8_SEXE) <- "Configuration parentale"
var_label(data$TDM8) <- "Configuration familiale"
var_label(data$TDM8_enfantcouple) <- "Présence d'enfant(s) issu(s) de l'union actuelle"
var_label(data$TDM8_enfantpropre) <- "Présence d'enfant(s) issu(s) d'autre(s) union(s)"
var_label(data$ANNEE_BDF) <- "Vague d'enquête"
var_label(data$n_RevenusContribF_cut) <- "Contribution des revenus féminins au revenus du ménage"
freq(data$n_RevenusContribF_cut)
#var_label(data$DNIVIE2) <- "Décile de niveau de vie



data$PONDMEN <- data$PONDMEN/mean(data$PONDMEN)
#iorder(data$TDM8_SEXE)
dataplot <- data 
levels(dataplot$TDM8_SEXE) <- sapply(levels(dataplot$TDM8_SEXE), 
                                     function(x) {insert_line_breaks(x, 35)})

ggp <- dataplot %>%   
ggplot() +
  aes(x = TDM8_enfantpropre, fill = SEXEREP, by = TDM8_enfantpropre, weight = PONDMEN,
      label = scales::percent(after_stat(prop), accuracy = 1)) +
  geom_bar(position = "fill", color = "black") +
  geom_text(
    stat = "prop", 
    position = position_fill(.5)
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    #title = titre,
    x = "Présence d'enfants d'unions anterieures",
    y = "Pourcentage",
  ) +
  coord_flip() +
  scale_fill_manual(values = c("#fdb863", "#b2abd2"))  +
  facet_wrap(facets = ~TDM8_enfantcouple) +
  theme_tufte()+
  theme(legend.position = "bottom", legend.box = "horizontal", legend.title = element_blank())
ggp

doc <- read_docx() %>%
  body_add_gg(ggp, width = 8, height = 3)

data$SEXEREP
tab <- data %>%
  mutate(Ensemble = T) %>%
  mutate(Effectifs = T) %>%
  # mutate(SEXEREP = case_when(
  #   SEXEREP == "0" ~ "Homme", 
  #   SEXEREP == "1" ~ "Femme"
  # )) %>%
  filter(TDM8_SEXE != "Autre ménage (complexe)")%>%
  filter(COUPLE_SEXE == "Couple de sexes différents")%>%
  as_survey_design(weights = PONDMEN) %>%
  tbl_svysummary(include = c( "NIVIE", "n_FractionClasse", "TDM8_SEXE", "ANNEE_BDF", "n_RevenusContribF_cut", "SEXEREP", "Ensemble", "Effectifs"), 
                 type = list(NIVIE = "continuous2"), 
                 statistic = list(all_categorical() ~ "{p}", 
                                  Effectifs = "{n_unweighted}"),
                 by = SEXEREP, 
                 percent = "row", 
                 missing = "no")  %>%
  add_p() |> 
  #add_overall(last = T) |>
  as_flex_table() |>
  font(fontname = "Garamond", part = "all") |>
  fontsize(size = 10, part = "all") |>
  autofit()


ano <- data %>%
  filter(is.na(n_RevenusContribF_cut))
freq(ano$TDM8_SEXE)

doc <- doc %>%
  body_add_break() |> 
  body_add_flextable(
    tab |> 
      border_remove() |>
      hline_top(border = fp_border(width = 1)) |>
      hline_bottom(border = fp_border(width = 1)) |>
      autofit()
  )
  

            
# data <- data %>%
#   subset(!(n_TYPMEN_new %in% c("Complexe", "Monoparentale", "Personne seule")))

library(forcats)

data <- data %>%
  mutate(
    n_FractionClasse = fct_relevel(
      n_FractionClasse,
      "'Petits-moyens' [C3]"
    ),
    TDM8_SEXE = fct_relevel(
      TDM8_SEXE,
      "Couple avec uniquement enfant(s) du couple"
    ),
    TDM8_enfantpropre = fct_relevel(
      TDM8_enfantpropre,
      "sans enfant d'autre union"
    ),
    n_RevenusContribF_cut = fct_relevel(
      n_RevenusContribF_cut,
      "Entre 40 et 50%"
    )
  )
freq(data$n_RevenusContribF_cut)

ageenfants <- indiv %>%
  filter(ENFANT == "1") %>%
  group_by(IDENT_MEN) %>%
  summarise(AGE_ENFANTS_MENAGE_MOYEN = mean(AGE, na.rm = TRUE))

data <- left_join(data, ageenfants)
freq(data$NENFANTS)
freq(data$AGE_ENFANTS_MENAGE_MOYEN)
data <- data %>%
  mutate(ENFANTS_MENAGE = case_when(
    NENFANTS == "Aucun" ~ F, 
    TRUE ~ T
  ))

reg <- glm(
  formula = SEXEREP ~ NIVIE + n_FractionClasse + n_RevenusContribF_cut +  ANNEE_BDF + TDM8_enfantcouple*TDM8_enfantpropre   , 
  data = data,
  weights = PONDMEN,
  family = "quasibinomial"
)

data$n_RevenusContribF_cut
summary(reg)


tblreg <- tbl_regression(reg, exponentiate = T, conf.int = F,
                         label = list(NIVIE ~ "Niveau de vie mensuel (en centaine d'euros)", 
                                      n_FractionClasse ~ "Position sociale du ménage")) %>%
  bold_p(t = 0.1)%>%
  bold_labels() %>%
  add_glance_source_note() |> 
  as_flex_table() |>
  font(fontname = "Garamond", part = "all") |>
  fontsize(size = 10, part = "all") |>
  autofit()

tblreg


doc <- doc |>
  body_add_break() |> 
  body_add_flextable(
    tblreg |> 
      border_remove() |>
      hline_top(border = fp_border(width = 1)) |>
      hline_bottom(border = fp_border(width = 1)) |>
      autofit()
  )




print(doc, target = "Resultats/resultats_regression_sexe-repondant-e.docx")

