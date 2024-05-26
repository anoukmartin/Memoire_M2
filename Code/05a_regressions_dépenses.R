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
  mutate(NIVIE = NIVIE/12000) %>%
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

summary(data$Vetements_enfants)
summary(data$Vetements_femme)
summary(data$Vetements_homme)
names(data)
summary(data$PONDMEN)
data$PONDMEN <- data$PONDMEN/mean(data$PONDMEN)


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
  filter(n_NEnfantsMenage13 > 0 & !is.na(n_NEnfantsMenage13 ))
  
tab <- data %>%
  group_by(n_TYPMEN_new) %>%
  #mutate(Vetements_enfants = if_else(is.na(Vetements_enfants), 0, Vetements_enfants)) %>%
  summarise(mean_wtd = wtd.mean(Vetements_enfants, weights = PONDMEN))
  
tab

reg <- survreg(Surv(Vetements_enfants+1, Vetements_enfants+1>1, type='left') ~ NIVIE + n_NEnfantsMenage13 + n_AgeEnfantsMenage13 + n_FractionClasse + n_TYPFAM,
               data=data, 
               weights = data$PONDMEN, 
               subset = !is.na(Vetements_enfants),
               dist='gaussian')

# reg <- lm(
#   Vetements_enfants ~  NIVIE+ n_NEnfantsMenage13 + n_AgeEnfantsMenage13 + n_NEnfantsMenage + n_AgeEnfantsMenage + n_FractionClasse + n_TYPFAM,
#   data = data, 
#   weights = data$PONDMEN, 
#   subset = !is.na(Vetements_enfants))

#reg <- step(reg)
tblreg1 <- tbl_regression(reg, intercept = T) |>
  add_glance_source_note() 

tblreg1 
ggcoef_model(reg)

# On enregistre 
saveTableau(tableau = tblreg1, 
            typ = "reg", 
            label = "DepVetementEnfants", 
            description = "Regressions sur les dépenses de vetement par enfants", 
            champ = paste0(infosBDF$champ, "formé par au moins un adulte agé et 25 à 65 ans et ayantà charge au moins un enfant de moins de 14 ans"), 
            n = nrow(data), 
            ponderation = T)



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

reg1 <- step(reg1)

tblreg1 <- tbl_regression(reg1, intercept = T) |>
  add_glance_source_note() 


tblreg1 



# Idem mais avec interaction revenu F structure familiale 
reg2 <- update(reg1, ~ . + n_REVENUS_F*n_TYPFAM)                       

reg2 <- step(reg2)

tblreg2 <- tbl_regression(reg2, intercept = T) |>
  add_glance_source_note() 
tblreg2

# library(ggstats)

# library(ggstats)

ggcoef_compare(models = list("Simple" = reg1,  "Avec interaction" = reg2), 
               conf.level = 0.90, 
               intercept = F,
               variable_labels = list(
                 NIVIE = "Niveau de vie du ménage", 
                 DIP7_F = "Diplôme", 
                 CS12_F = "CSP", 
                 n_TYPFAM = "Type de ménage", 
                 n_REVENUS_F = "Revenus"),
               type = "faceted") +
  theme_tufte()



