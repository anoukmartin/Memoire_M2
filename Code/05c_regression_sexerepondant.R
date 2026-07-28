

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
         TDM8_SEXE = droplevels(TDM8_SEXE)) 

var_label(data$NIVIE) <- "Niveau de vie mensuel (en centaine d'euros)"
var_label(data$n_FractionClasse) <- "Position sociale du ménage"
var_label(data$TDM8_SEXE) <- "Configuration parentale"
var_label(data$TDM8) <- "Configuration familiale"
#var_label(data$DNIVIE2) <- "Décile de niveau de vie"


data$PONDMEN <- data$PONDMEN/mean(data$PONDMEN)
iorder(data$TDM8_SEXE)
dataplot <- data 
levels(dataplot$TDM8_SEXE) <- sapply(levels(dataplot$TDM8_SEXE), 
                                     function(x) {insert_line_breaks(x, 35)})

dataplot %>%   
ggplot() +
  aes(x = TDM8_SEXE, fill = SEXEREP, by = TDM8_SEXE, weight = PONDMEN,
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
  scale_fill_manual(values = c("#fdb863", "#b2abd2"))  +
  theme_tufte()+
  theme(legend.position = "bottom", legend.box = "horizontal", legend.title = element_blank())


tab <- data %>%
  mutate(Ensemble = T) %>%
  mutate(SEXEREP = case_when(
    SEXEREP == "0" ~ "Homme", 
    SEXEREP == "1" ~ "Femme"
  )) %>%
  filter(TDM8_SEXE != "Autre ménage (complexe)")%>%
  filter(COUPLE_SEXE == "Couple de sexes différents")%>%
  as_survey_design(weights = PONDMEN) %>%
  tbl_svysummary(include = c("NIVIE", "n_FractionClasse", "n_RevenusContribF", "NENFANTS", "TDM8_SEXE", "SEXEREP", "Ensemble"), 
                 by = SEXEREP, 
                 percent = "row")  %>%
  add_p() |> 
  as_flex_table() |>
  font(fontname = "Garamond", part = "all") |>
  fontsize(size = 10, part = "all") |>
  autofit()

tab


saveTableau(tab, type = "tab", 
            label = "desSexeRep",
            description = "Sexe du répondant en fonction de la classe sociale et de la configuration familiale du ménage", 
            ponderation = T, 
            champ = paste0(infosBDF$champ, "formé par deux individus de sexe différent et dont au moins l'un d'entre eux est agé de 21 à 60 ans"), 
            n = nrow(data))


tab <- data %>%
  mutate(Ensemble = "1") %>%
  as_survey_design(weights = PONDMEN) %>%
  tbl_svysummary(include = c("NIVIE", "n_FractionClasse",  "n_RevenusContribF", "Ensemble"), 
                 by = n_RevenusContribF, 
                 percent = "row")  %>%
  add_p()
tab
            
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
    n_RevenusContribF = fct_relevel(
      n_RevenusContribF,
      "Entre 40 et 50%"
    )
  )
freq(data$n_RevenusContribF)

ageenfants <- indiv %>%
  filter(ENFANT == "1") %>%
  group_by(IDENT_MEN) %>%
  summarise(AGE_ENFANTS_MENAGE_MOYEN = mean(AGE, na.rm = TRUE))

data <- left_join(data, ageenfants)
freq(data$NENFANTS)
data <- data %>%
  mutate(ENFANTS_MENAGE = case_when(
    NENFANTS == "Aucun" ~ F, 
    TRUE ~ T
  ))
reg <- glm(
  formula = SEXEREP ~ NIVIE + n_FractionClasse + TDM8_SEXE, 
  data = data,
  weights = PONDMEN,
  family = "quasibinomial"
)


summary(reg)


tblreg <- tbl_regression(reg, exponentiate = T, 
                         label = list(NIVIE ~ "Niveau de vie mensuel (en centaine d'euros)", 
                                      n_FractionClasse ~ "Position sociale du ménage", 
                                      TDM8_SEXE~ "Configuration parentale du couple")) %>%
  bold_p(t = 0.1)%>%
  bold_labels() %>%
  add_glance_source_note() |> 
  as_flex_table() |>
  font(fontname = "Garamond", part = "all") |>
  fontsize(size = 10, part = "all") |>
  autofit()

tblreg

saveTableau(tblreg, type = "reg", 
            label = "SexeRep",
            description = "Regression sur le sexe du répondant dans les couples enquétés", 
            ponderation = T, 
            champ = paste0(infosBDF$champ, " dont la personne de référence ou le conjoint est un adulte agé de 25 à 65 ans et vivant en couple (hors configuration familiale complexe)"), 
            n = nrow(data))

tbl
