################################################################################-
########## Tests régression situation financière ressentie #####################
################################################################################-

infosBDF <- readRDS("Data_output/infosBDF.Rds")

# 1.1. Régression sur l'aisance budgétaire ressentie ###################

## Construction base de donnée sur laquelle on va travailler ###################

familles <- readRDS("Data_output/data_recode/menages_ageminmax.Rds")
names(familles)
freq(familles$NIVEAU)
freq(familles$AISE)
familles <- familles |>
  mutate(AISE = if_else(AISE %in% c("", "8", "9"), NA, AISE) %>%
           as.factor() %>%
           fct_relevel("5", "4", "3", "2", "1")) %>%
  mutate(AISE2 = case_when(
    AISE %in% c("5", "4") ~ "3", 
    AISE == "3" ~ "2", 
    AISE %in% c("2", "1") ~ "1") %>%
           as.factor() %>%
           fct_relevel("3", "2", "1")) %>%
  mutate(NIVEAU = if_else(NIVEAU %in% c("", "8", "9"), NA, NIVEAU) %>%
           as.factor() %>%
           fct_relevel("6", "5", "4", "3", "2", "1")) %>%
  mutate(NIVIE = NIVIE/1200)
  
freq(familles$AISE)
freq(familles$AISE2)
freq(familles$NIVEAU)





# Qui es le répondant #####
repondants <- readRDS("Data_output/data_recode/indiv_in_menagesAge.Rds") %>% 
  filter(ENFANT == "2") %>%
  mutate(REPONDANT = if_else(NOI == "01", T, F)) %>%
  filter(REPONDANT == T) %>%
  mutate(ANNEE_BDF = as.factor(ANNEE_BDF)) %>%
  rec_SEXE() %>%
  mutate(NENFANTS_HORSDOM = if_else(
    is.na(NENFANTS_HORSDOM), 0, 
    NENFANTS_HORSDOM)) %>%
  mutate(NIVIE = NIVIE/1200)%>%
  mutate(MOCO_DET_SEXE = MOCO_DET_SEXE %>% as.factor() %>% droplevels(),
         MOCO_DET = MOCO_DET %>% as.factor() %>% droplevels()) %>%
  left_join(familles %>%
              select(IDENT_MEN, NIVEAU, AISE, AISE2, NPERS), by = "IDENT_MEN") 
data <- repondants


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
    MOCO_DET = fct_relevel(
      MOCO_DET,
      "Adulte d’une famille traditionnelle"
    ),
    MOCO_DET_SEXE = fct_relevel(
      MOCO_DET_SEXE,
      "Femme d’une famille traditionnelle"
    )
  )

var_label(data$ANNEE_BDF) <- "Vague d'enquête"
var_label(data$NIVIE) <- "Niveau de vie mensuel (en centaines d'euros)"
var_label(data$n_FractionClasse) <- "Position sociale du ménage"
var_label(data$AGE) <- "Age"
var_label(data$NENFANTS) <- "Nombre d'enfants vivants dans le ménage"
var_label(data$NENFANTS_HORSDOM) <- "Nombre d'enfants vivants hors domicile"
var_label(data$SEXE) <- "Sexe"
var_label(data$MOCO_DET) <- "Position dans la famille"
var_label(data$MOCO_DET_SEXE) <- "Position dans la famille"
library(ordinal)

# Explication de l'aisance budgétaire par le seul niveau de vie : 



reg <- clm(AISE ~ ANNEE_BDF + NIVIE + n_FractionClasse + AGE + NENFANTS + NENFANTS_HORSDOM + MOCO_DET*SEXE,
           data = data, 
           weights = PONDIND)
summary(reg)

nominal_test(reg)

tblreg <- tbl_regression(reg, exponentiate = T,
               conf.int = F,
               add_pairwise_contrasts = F,
               pairwise_variables = c("MOCO_DET_SEXE")) %>%
  bold_p(t = 0.1) %>% 
  add_glance_source_note() %>%
  as_flex_table() |>
  font(fontname = "Garamond", part = "all") |>
  fontsize(size = 10, part = "all") |>
  autofit()

tblreg
doc <-
  read_docx() %>%
  body_add_flextable(
    tblreg |> 
      border_remove() |>
      hline_top(border = fp_border(width = 1)) |>
      hline_bottom(border = fp_border(width = 1)) |>
      autofit()
  )


### NIVEAU
reg <- clm(NIVEAU ~ ANNEE_BDF + NIVIE + n_FractionClasse + AGE + NENFANTS + NENFANTS_HORSDOM + MOCO_DET*SEXE,
           data = data, 
           weights = PONDIND)
summary(reg)

nominal_test(reg)

tblreg <- tbl_regression(reg, exponentiate = T,
                         conf.int = F,
                         add_pairwise_contrasts = F,
                         pairwise_variables = c("MOCO_DET_SEXE")) %>%
  bold_p(t = 0.1) %>% 
  add_glance_source_note() %>%
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

print(doc, target = "Resultats/resultats_regression_situ_fin_subj.docx")







reg |> 
  ggstats::ggcoef_model(
    exponentiate = TRUE,
    add_pairwise_contrasts = TRUE,
    pairwise_variables = c("MOCO_DET_SEXE")
  )

#mutate(n_NEnfantsMenage = n_NEnfantsMenage - n_NEnfantsMenage13) %>%
 # mutate(n_NEnfantsHD = if_else(is.na(n_NEnfantsHD), 0, n_NEnfantsHD)) %>%
  #rec_NENFANTS(Var = "n_NEnfantsMenage") %>%
  # rec_NENFANTS(Var = "n_NEnfantsHD") %>%

freq(data$n_NEnfantsMenage13)
freq(data$n_NEnfantsMenage)
freq(data$n_NEnfantsHD)

# mutate(n_configMenage = n_configMenage %>%
  #          fct_relevel("Parents en couple", "Mère en couple", "Père en couple",
  #                      "Mère célibataire", "Père célibataire", "Couple sans enfant",
  #                      "Femme seule", "Homme seul", "Autre type de ménage (ménage complexe)"))

data <- familles %>%
  # subset(!(n_TYPMEN_sexe %in% c("Homme et femme en couple", "Homme célibataire", "Femme célibataire"))) %>%
  mutate(TDM8_SEXE = droplevels(TDM8_SEXE)) %>%
  mutate(PONDFAM = PONDMEN/mean(data$PONDMEN)) # On centre la variable de pondération
class(data$NIVIE)
freq(data$n_TYPMEN_sexe)


library(ordinal)

var_label(data$NIVIE) <- "Niveau de vie mensuel (en centaine d'euros)"
data$n_FractionClasse <- relevel(data$n_FractionClasse, "Classes moyennes superieures [C4]")
var_label(data$n_FractionClasse) <- "Fraction de classe"
var_label(data$n_NEnfantsMenage) <- "Nombre d'enfants"
var_label(data$n_AgeEnfantsMenage) <- "Age moyen des enfants"
var_label(data$TDM8_SEXE) <- "Configuration parentale"
var_label(data$SEXEREP) <- "Sexe du répondant à l'enquête"


## Régression pondérée #########################################################
summary(data$NIVIE)
hist(as.numeric(data$AISE))
plot(data$AISE, data$NIVIE)
freq(data$n_TYPMEN_sexe)
chisq.test(data$AISE, data$TDM8_SEXE)
plot(data$AISE, data$n_AgeEnfantsMenage)
freq(data$n_FractionClasse)
plot(data$n_FractionClasse, data$AISE)
data$NIVIEsq <- data$NIVIE*data$NIVIE
data$AISE
names(data)
data$SEXEREP

data <- data %>%
  mutate(
    n_FractionClasse = fct_relevel(
      n_FractionClasse,
      "'Petits-moyens' [C3]"
    ),
    TDM8_SEXE = fct_relevel(
      TDM8_SEXE,
      "Couple avec uniquement enfant(s) du couple"
    )
  )

reg <- clm(AISE ~ NIVIE + n_FractionClasse + NENFANTS + AGE_ENFANTS + TDM8_SEXE + SEXEREP,
                  data = data, 
                  weights = PONDFAM)


#step(reg)
summary(reg)


freq(data$n_NEnfantsMenage)
freq(data$n_TYPMEN_sexe)
tblreg3 <- tbl_regression(reg, intercept = F, exponentiate = T)
tblreg3 

## Enregistrement des résultats ################################################
saveTableau(tblreg3, 
            type = "Reg",
            label = "aisance", 
            description = "Regression sur l'aisance budgétaire ressentie", 
            champ = paste0(infosBDF$champ, " déclarant au moins un enfant à charge"), 
            ponderation = TRUE, 
            n = reg$n)
            

## Régression pondérée #########################################################
summary(data$NIVEAU)
plot(data$NIVEAU, data$NIVIE)
freq(data$n_TYPMEN_sexe)
chisq.test(data$NIVEAU, data$n_TYPMEN_sexe)
plot(data$NIVEAU, data$n_AgeEnfantsMenage)
freq(data$n_FractionClasse)
plot(data$n_FractionClasse, data$NIVEAU)
data$NIVIEsq <- data$NIVIE*data$NIVIE
data$NIVEAU
names(data)
data$SEXEREP
reg <- clm(NIVEAU ~ NIVIE + n_FractionClasse + n_NEnfantsMenage + n_AgeEnfantsMenage + n_TYPMEN_sexe + SEXEREP,
           data = data, 
           weights = PONDFAM)


#step(reg)
summary(reg)


freq(data$n_NEnfantsMenage)
freq(data$n_TYPMEN_sexe)
tblreg3 <- tbl_regression(reg, intercept = F, exponentiate = T)
tblreg3 

## Enregistrement des résultats ################################################
saveTableau(tblreg3, 
            type = "Reg",
            label = "niveau", 
            description = "Regression sur le niveau de vie estimé ressentie", 
            champ = paste0(infosBDF$champ, " déclarant au moins un enfant à charge"), 
            ponderation = TRUE, 
            n = reg$n)





