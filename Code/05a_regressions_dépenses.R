################################################################################-
##################  Tests régression dépenses ##################################
################################################################################-

infosBDF <- readRDS("Data_output/infosBDF.Rds")


familles <- readRDS("Data_output/familles_parents.Rds") 
menages <- readRDS("Data_output/menages.Rds")
summary(menages$NIVIE)
conso <- readRDS("Data_output/conso.Rds") 
names(conso)  
types_conso <- list(   
  Scolaire = names(conso)[str_starts(names(conso), "C111|C112")],    
  Vetements_enfants = names(conso)[str_starts(names(conso), "C03123|C03213")],
  Vetements_femme = names(conso)[str_starts(names(conso), "C03122|C03212")], 
  Vetements_homme = names(conso)[str_starts(names(conso), "C03121|C03211")],
  Jouets = names(conso)[str_starts(names(conso), "C09311")])  

types_conso
conso2 <- lapply(types_conso, function(x){   
  tab <- conso %>%     
    select(starts_with(x))   
  tab <- rowSums(tab, na.rm = TRUE)   
  consotot<- conso$CTOT %>% as.vector() 
  nenfants <- menages$NENFANTS %>% as.vector()
  tab <- (tab)  
  return(tab)   } ) 

conso2 <- conso2 %>%   
  bind_cols() %>%   
  as.data.frame()  
conso2$`Consommation finale` <- conso$CTOT  
conso2$IDENT_MEN <- menages$IDENT_MEN
conso2$NIVIE <- menages$NIVIE
conso2$NENFANTS2 <- menages$NENFANTS
conso2$REVDISP <- menages$REVDISP
conso2$COEFUC <- menages$COEFFUC
names(data)
data$n_NEnfantsCouple_F

data <- familles %>%   
  left_join(conso2, by = "IDENT_MEN") %>%
  mutate(n_TYPMEN_new = droplevels(n_TYPMEN_new)) %>%
  mutate(NIVIE = NIVIE/1200) %>%
  mutate(n_REVENUS_F = n_REVENUS_F/12000) %>%
  mutate(n_REVENUS_H = n_REVENUS_H/12000) %>%
  mutate(REVDISP = REVDISP/12000) %>%
  mutate(Vetements_enfants = Vetements_enfants/n_NEnfantsMenage13) %>%
  mutate(n_TYPFAM = n_TYPMEN_new) %>%
  mutate(n_TYPFAM = case_when(
    n_TYPFAM == "Recomposée" & n_NEnfantsCouple_F >=1 ~ "Recomposée avec enfants communs", 
    n_TYPFAM == "Recomposée" & is.na(n_NEnfantsCouple_F) ~ "Recomposée sans enfants communs", 
    TRUE ~ n_TYPFAM
  )) %>%
  mutate(n_TYPFAM = n_TYPFAM %>%
           as.factor() %>%
           fct_relevel(c("Traditionelle", "Monoparentale", "Recomposée sans enfants communs", "Recomposée avec enfants communs", "Complexe"))) 

data$n_NFraterie <- rowSums(data[ c("n_NEnfantsMenage", "n_NEnfantsHD")], na.rm = T)

var_label(data$n_NFraterie) <- "Taille de la fraterie"
var_label(data$n_TYPFAM) <- "Configuration familiale du ménage"
var_label(data$n_FractionClasse) <- "Position sociale du ménage"
var_label(data$n_AgeEnfantsMenage) <- "Age moyen des enfants du ménage"
var_label(data$NIVIE) <- "Niveau de vie mensuel (en centaine d'euros)"

data$n_FractionClasse <- relevel(data$n_FractionClasse,  "Classes moyennes superieures [C3]")
var_label(data$n_FractionClasse) <- "Fraction de classe"

# mutate(n_FractionClasse = n_FractionClasse %>% relevel("Classes moyennes salariées [C8]"))

summary(data$Vetements_enfants)
summary(data$Vetements_femme)
summary(data$Vetements_homme)
summary(data$n_NFraterie)
names(data)
summary(data$PONDMEN)



# data <- data %>%
#   as_survey_design(weights = "PONDMEN")

# data$n_configMenage <- data$n_configMenage %>%
#   fct_relevel(
#     "Couple sans enfant",
#     "Femme seule",
#     "Homme seul",
#     "Mère célibataire",
#     "Père célibataire",
#     "Mère en couple",
#     "Père en couple",
#     "Parents en couple",
#     "Autre type de ménage (ménage complexe)"
#   )


## Régression pondérée dépenses enfants ########################################


data$Vetements_enfants
data <- data %>%
  filter(n_NEnfantsMenage13 > 0 & !is.na(n_NEnfantsMenage13))
data$PONDMEN <- data$PONDMEN/mean(data$PONDMEN)
tab <- data %>%
  group_by(n_TYPMEN_new, DNIVIE2) %>%
  #mutate(Vetements_enfants = if_else(is.na(Vetements_enfants), 0, Vetements_enfants)) %>%
  summarise(mean_wtd = wtd.mean(Vetements_enfants, weights = PONDMEN))
  
tab %>%
  pivot_wider(id_cols = DNIVIE2, 
              names_from = n_TYPMEN_new, 
              values_from = mean_wtd)
              

reg <- survreg(Surv(Vetements_enfants+1, Vetements_enfants+1>1, type='left') ~ NIVIE +  n_NFraterie +  n_AgeEnfantsMenage + n_FractionClasse + n_TYPFAM,
               data=data, 
               weights = data$PONDMEN, 
               #subset = !is.na(Vetements_enfants),
               dist='gaussian')

# reg <- lm(
#   Vetements_enfants ~  NIVIE+ n_NEnfantsMenage13 + n_AgeEnfantsMenage13 + n_NEnfantsMenage + n_AgeEnfantsMenage + n_FractionClasse + n_TYPFAM,
#   data = data, 
#   weights = data$PONDMEN, 
#   subset = !is.na(Vetements_enfants))

#reg <- step(reg)
tblreg <- tbl_regression(reg, intercept = T, exponentiate = F) |>
  add_glance_source_note() 

tblreg

#ggcoef_model(reg)

# On enregistre 
saveTableau(tableau = tblreg, 
            typ = "reg", 
            label = "DepVetementEnfants", 
            description = "Regressions sur les dépenses de vetement par enfants", 
            champ = paste0(infosBDF$champ, "formé par au moins un adulte agé et 25 à 65 ans et ayantà charge au moins un enfant de moins de 14 ans"), 
            n = nrow(data), 
            ponderation = T)


# régression sur les services domestiques ######################################

depmen <- readRDS("Data_output/DepMenages.Rds") %>%
  select(IDENT_MEN, starts_with("DEPPER")) %>%
  select(IDENT_MEN, ends_with("_D"))
depmen[is.na(depmen)] <- 0

data <- data %>%
  filter(n_NEnfantsMenage13 > 0 & !is.na(n_NEnfantsMenage13))
data$PONDMEN <- data$PONDMEN/mean(data$PONDMEN)

look_for(depmen)
var_label(depmen) <- var_label(depmen) %>%
  str_remove("Montant définitif total à la charge du ménage pour ") %>%
  str_to_sentence()
data <- data %>%
  left_join(depmen, by = "IDENT_MEN")

data$DEPPER2_D
data$n_NEnfantsMenage13
data$n_TYPMEN_sexe
data$NIVIESquare <- data$NIVIE*data$NIVIE
var_label(data$NIVIESquare) <- "Niveau de vie mensuel au carré (en centaine d'euros)"
data$n_AgeEnfantsMenageSquare <- data$n_AgeEnfantsMenage*data$n_AgeEnfantsMenage
data$DEPPER2_D <- data$DEPPER2_D/data$n_NEnfantsMenage13

tab <- data %>%
  group_by(n_TYPMEN_sexe, DNIVIE2) %>%
  #mutate(Vetements_enfants = if_else(is.na(Vetements_enfants), 0, Vetements_enfants)) %>%
  summarise(mean_wtd = wtd.mean(DEPPER2_D, weights = PONDMEN))

tab %>%
  pivot_wider(id_cols = DNIVIE2, 
            names_from = n_TYPMEN_sexe, 
            values_from = mean_wtd)

data$DEPPER2_D
data$n_REVENUS_F
# Regresion Garde d'enfants
reg <- survreg(Surv(DEPPER2_D+1, DEPPER2_D+1>1, type='left') ~ NIVIE + n_NFraterie + n_AgeEnfantsMenage + n_FractionClasse + n_TYPMEN_sexe,
               data=data, 
               weights = data$PONDMEN, 
               #subset = !is.na(Vetements_enfants),
               dist='gaussian')


tblreg <- tbl_regression(reg, intercept = T) |>
  add_glance_source_note() 

tblreg





#ggcoef_model(reg)

# On enregistre 
saveTableau(tableau = tblreg, 
            typ = "reg", 
            label = "DepGardeEnfants", 
            description = "Regressions sur les dépenses de garde enfants", 
            champ = paste0(infosBDF$champ, "formé par au moins un adulte agé et 25 à 65 ans et ayantà charge au moins un enfant de moins de 14 ans"), 
            n = nrow(data), 
            ponderation = T)


############TRASH ####################


enfants <- readRDS("Data_output/enfantsDuMenage.Rds")
depind <- readRDS("Data_output/DepIndiv.Rds")

tab <- look_for(depind) %>%
  filter(str_detect(label, "Montant")) %>% 
  filter(!str_detect(label, "tranche")) %>% 
  filter(!str_detect(label, "hors domicile")) %>%
  mutate(definitif = if_else(str_detect(variable, "_D"), T, F), 
         var = str_remove(variable, "_D"))  %>%
  arrange(-definitif)
tab$dup <- duplicated(tab$var)
tab <- tab %>% filter(!dup) %>%
  mutate(period = case_when(
    str_detect(label, "mensuel") ~ "mensuel", 
    str_detect(label, "annuel") ~ "annuel", 
    str_detect(label, "habituel") ~ "habituel",
    str_detect(label, "12 mois") ~ "annuel", 
    str_detect(label, "12 derniers mois") ~ "annuel", 
    str_detect(label, "6 mois") ~ "biannuel", 
    str_detect(label, "6 derniers mois") ~ "biannuel", 
    str_detect(label, "2 mois") ~ "2 mois", 
    str_detect(label, "2 derniers mois") ~ "2 mois")) %>%
  arrange(pos)
freq(tab$period)
depind <- depind %>% select(all_of(tab$variable))
freq(tab$pos)

indiv <- left_join(indiv %>% select(IDENT_MEN, NOI, ENFANT), 
                   depind, 
                   by = c("IDENT_MEN", "NOI"))




tab <- data %>%
  group_by(n_TYPMEN_new) %>%
  #mutate(Vetements_enfants = if_else(is.na(Vetements_enfants), 0, Vetements_enfants)) %>%
  summarise(mean_wtd = wtd.mean(Vetements_enfants, weights = PONDMEN))

tab







# Regression dépenses soclaires ################################################

data$n_AgeEnfantsMenage13
data$n_REVENUS_F

reg <- survreg(Surv(Scolaire, Scolaire>1, type='left') ~ NIVIE + n_NEnfantsMenage + n_AgeEnfantsMenage + n_FractionClasse + n_TYPFAM,
               data=data, 
               weights = data$PONDMEN, 
               subset = !is.na(Scolaire),
               dist='gaussian')
# reg <- lm(
#   Vetements_enfants ~  NIVIE+ n_NEnfantsMenage13 + n_AgeEnfantsMenage13 + n_NEnfantsMenage + n_AgeEnfantsMenage + n_FractionClasse + n_TYPFAM,
#   data = data, 
#   weights = data$PONDMEN, 
#   subset = !is.na(Vetements_enfants))

reg <- step(reg)
tblreg1 <- tbl_regression(reg, intercept = T) |>
  add_glance_source_note() 

tblreg1 

# On enregistre 
saveTableau(tableau = tblreg1, 
            typ = "reg", 
            label = "DepScolaireEnfants", 
            description = "Regressions sur les dépenses de scolaires par enfants", 
            champ = paste0(infosBDF$champ, "formé par au moins un adulte agé et 25 à 65 ans et ayantà charge au moins un enfant de moins de 14 ans"), 
            n = tblreg1$table_styling$source_note, 
            ponderation = T)






# Regression habillement femme #################################################
unique(data$DIP7_F)
data$DIP7_F <- data$DIP7_F %>% fct_relevel("Baccalauréat")
unique(data$CS12_F)
data$CS12_F <- data$CS12_F %>% fct_relevel("Médiateur-ice")

# Une premièe regresion simple sans interaction Revenus F avec structure familiale
reg1 <- lm(
  Vetements_femme ~  NIVIE + DIP7_F + CS12_F + n_TYPFAM,
  data = data, 
  weights = data$PONDMEN, 
  subset = !is.na(n_IdentIndiv_F))

#reg1 <- step(reg1)

tblreg1 <- tbl_regression(reg1, intercept = T) |>
  add_glance_source_note() 


tblreg1 



# Idem mais avec interaction revenu F structure familiale 
reg2 <- update(reg1, ~ . + n_REVENUS_F*n_TYPFAM)                       

#reg2 <- step(reg2)

tblreg2 <- tbl_regression(reg2, intercept = T) |>
  add_glance_source_note() 
tblreg2

# library(ggstats)

# library(ggstats)

# ggcoef_compare(models = list("Simple" = reg1,  "Avec interaction" = reg2), 
#                conf.level = 0.90, 
#                intercept = F,
#                variable_labels = list(
#                  NIVIE = "Niveau de vie du ménage", 
#                  DIP7_F = "Diplôme", 
#                  CS12_F = "CSP", 
#                  n_TYPFAM = "Type de ménage", 
#                  n_REVENUS_F = "Revenus"),
#                type = "faceted") +
#   theme_tufte()



