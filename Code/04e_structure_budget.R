
# DATA #########################################################################
## Données sur les couples/méanges de notre popilatio ##########################
familles <- readRDS("Data_output/familles_parents.Rds") %>%
  mutate(n_TYPFAM = n_TYPMEN_new) %>%
  mutate(n_TYPFAM = case_when(
    n_TYPFAM == "Recomposée" & n_NEnfantsCouple_F >=1 ~ "Recomposée avec enfants communs", 
    n_TYPFAM == "Recomposée" & is.na(n_NEnfantsCouple_F) ~ "Recomposée sans enfants communs", 
    TRUE ~ n_TYPFAM
  )) %>%
  mutate(n_TYPFAM = n_TYPFAM %>%
           as.factor() %>%
           fct_relevel(c("Traditionelle", "Monoparentale", "Recomposée sans enfants communs", "Recomposée avec enfants communs", "Complexe")))

## DOnnées sur des ménages à ajouter ###########################################
menages <- readRDS("Data_output/menages.Rds")
var <- c("IDENT_MEN", "REVDISP", "NIVIE", "COEFFUC", "NENFANTS", "REVSOC", "REV701", "REV700", "CHOMAGE", "RETRAITES", "REVTOT") 

familles <- left_join(familles %>%
                        select(-NENFANTS), 
                      menages %>%
                        select(all_of(var)), 
                      by = "IDENT_MEN")
names(familles)

## Données de consommation #####################################################
conso <- readRDS("Data_output/conso.Rds")
names(conso)  

# Conso agrégée
types_conso <- names(conso)[-c(1, 248:249)] %>%
  str_sub(1,3) %>%
  unique()
types_conso

conso2 <- lapply(types_conso,
                 function(x){   
                   tab <- conso %>%
                     select(starts_with(paste0(x)))  
                   tab <- rowSums(tab, na.rm = TRUE) 
                   consotot<- conso$CTOT %>% as.vector()  
                   #tab <- (tab/consotot)*100   
                   return(tab) } )  

conso2 <- conso2 %>%   
  bind_cols() %>%   
  as.data.frame()  

names(conso2) <- c("ALIMENTATION",                     
                   "ALCOOLS,TABACS ETSTUPEFIANTS ", 
                   "HABILLEMENT ET CHAUSSURES",
                   "LOGEMENT ET CHARGES", 
                   "MEUBLES ET ENTRETIEN DE LA MAISON", 
                   "SANTE",        
                   "TRANSPORTS",                    
                   "COMMUNICATIONS", 
                   "LOISIRS ET CULTURE",  
                   "ENSEIGNEMENT",  
                   "RESTAURATION ET HÔTELS",  
                   "BIENS ET SERVICES DIVERS", 
                   "HORS-CHAMP", 
                   "APL") %>%    str_to_sentence()     

conso2$`Consommation finale` <- conso$CTOT  
conso2$IDENT_MEN <- conso$IDENT_MEN

# Conso moins agrégée

types_conso <- names(conso)[-c(1,248,249)] %>%
  str_sub(1,4) %>%
  unique()

types_conso

conso3 <- lapply(types_conso,
                 function(x){   
                   tab <- conso %>%
                     select(starts_with(paste0(x)))  
                   tab <- rowSums(tab, na.rm = TRUE) 
                   # consotot<- conso$CTOT %>% as.vector()  
                   # tab <- (tab/consotot)*100   
                   return(tab) } )  

conso3 <- conso3 %>%   
  bind_cols() %>%   
  as.data.frame()  

names(conso3)   <- types_conso
conso3$`Consommation finale` <- conso$CTOT  
conso3$IDENT_MEN <- conso$IDENT_MEN




# TABLEAUX #####################################################################

# Tbaleau structure budgétaire agrégée ####
## Données finales sur lesquelles on fait les tableaux  ########################
data <- familles %>%   
  left_join(conso2, by = "IDENT_MEN") %>%   
  mutate(#`Consommation finale par UC` = `Consommation finale`/COEFFUC,
         `Revenus non individualisables` = REVDISP - (n_REVENUS_F + n_REVENUS_H)) 

data[, names(conso2)[-c(16)]] <- (data[, names(conso2)[-c(16)]]/data$`Consommation finale`)*100
data
## Tableau : structure des budgets #############################################
names(data)
class(data$Alimentation)
tab <- data %>%  
  mutate(Effectifs = 1) %>%
  as_survey_design(weights = "PONDMEN") %>%  
  select(-PONDMEN, -IDENT_MEN) %>%  
  tbl_svysummary(by = "n_TYPMEN_new",  
                 include = c(names(conso2)[-c(13:16)], "Effectifs"),
                 type = list(everything() ~ "continuous", 
                             Effectifs ~ "dichotomous"),
                 statistic = list(all_continuous() ~"{mean}",
                                  Effectifs ~ "{n}"),
                 value = list(Effectifs ~ "1"),
                 missing = "no")   
  
tab <- tab %>% 
  modify_spanning_header(all_stat_cols() ~ "**Configuration familiale**") %>%
  add_overall(last = T)  %>% 
  modify_header(all_stat_cols() ~ "**{level}**") %>%
  add_p() 
  
tab

saveTableau(tableau = tab,
            type = "tab",
            label = "structure_budget", 
            description = "Structure budgétaire en fonction des structures familiales",
            ponderation = T, n = tab$N, 
            champ = paste0("Ménages formés par au moins un individu adulte agé de 25 à 65 ans"))


# Tableau moins agrégée #####
## Données finales sur lesquelles on fait les tableaux  (moins agrgées ) #######
data <- familles %>%   
  left_join(conso3, by = "IDENT_MEN") %>%   
  mutate(`Consommation finale par UC` = `Consommation finale`/COEFFUC,
         `Revenus non individualisables` = REVDISP - (n_REVENUS_F + n_REVENUS_H)) 

data[, names(conso3)[-c(57:58)]] <- (data[, names(conso3)[-c(57:58)]]/data$`Consommation finale`)*100
data

## Tableau : structure des budgets (moins agrégées ) ###############################

tab <- data %>%  
  mutate(Effectifs = 1) %>%
  as_survey_design(weights = "PONDMEN") %>%  
  select(-PONDMEN, -IDENT_MEN) %>%  
  tbl_svysummary(by = "n_TYPMEN_new",  
                 include = c(names(conso3)[-c(57:58)], "Effectifs"),
                 statistic = list(all_continuous() ~"{mean}", 
                                  Effectifs ~ "{n}"),  
                 value = list(Effectifs ~ "1"), 
                 type = list(everything() ~ "continuous", 
                             Effectifs ~ "dichotomous"),        
                 missing = "no")   

tab <- tab %>% 
  modify_spanning_header(all_stat_cols() ~ "**Configuration familiale**") %>%
  add_overall(last = T)  %>% 
  modify_header(all_stat_cols() ~ "**{level}**") %>%
  add_p() 

tab

saveTableau(tableau = tab,
            type = "tab",
            label = "structure_budget_moins_agrégé", 
            description = "Structure budgétaire en fonction des structures familiales",
            ponderation = T, n = tab$N, 
            champ = paste0("Ménages formés par au moins un individu adulte agé de 25 à 65 ans"))


## Tableau : revenus et consommation en fonction de la structure ##############################
names(data)

tab <- data %>%  
  mutate(Effectifs = "1") %>%
  as_survey_design(weights = "PONDMEN") %>%  
  select(-PONDMEN, -IDENT_MEN, ) %>%  
  tbl_svysummary(by = "n_TYPMEN_new",
                 include = c("REVDISP", 
                             "Consommation finale", 
                             "COEFFUC", 
                             "NIVIE", 
                             "Consommation finale par UC", 
                             "n_REVENUS_H", 
                             "n_REVENUS_F",
                             "Effectifs"),
                 statistic = list(all_continuous() ~"{mean}", 
                                  Effectifs ~ "{n_unweighted}"),  
                 value = list(Effectifs ~ "1"), 
                 type = list(all_continuous2() ~ "continuous",
                             Effectifs ~ "dichotomous"),        
                 missing = "no")   

tab <- tab %>% 
  modify_spanning_header(all_stat_cols() ~ "**Structure familiale**") %>%
  add_overall(last = T)  %>% 
  modify_header(all_stat_cols() ~ "**{level}**") %>%
  add_p() 

tab

saveTableau(tableau = tab,
            type = "tab",
            label = "revenus_conso_typmenage", 
            description = "Revenus et consommation totales en fonction des structures familiales",
            ponderation = T, n = tab$N, 
            champ = paste0("Ménages formés par au moins un individu adulte agé de 25 à 65 ans"))



# Tableau taux d'effort agrégée ####
## Données finales sur lesquelles on fait les tableaux  ########################
data <- familles %>%   
  left_join(conso2, by = "IDENT_MEN") %>%   
  mutate(#`Consommation finale par UC` = `Consommation finale`/COEFFUC,
    `Revenus non individualisables` = REVDISP - (n_REVENUS_F + n_REVENUS_H)) %>%
  filter(REVDISP > 0) %>%
  filter(TYPMEN5 %in% c("Couple avec au moins un enfant", "Famille monoparentale")) %>%
  mutate(n_TYPMEN_new = n_TYPMEN_new %>% droplevels())

freq(data$TYPMEN5)
data[, names(conso2)[-c(16)]] <- (data[, names(conso2)[-c(16)]]/data$REVDISP)*100
anomalies <- data[data$REVDISP <= 100, ]
anomalies$REVSOC
anomalies$NIVIE
anomalies$`Logement et charges`
data

## Tableau : structure des budgets #############################################

tab <- data %>%  
  mutate(Effectifs = "1") %>%
  as_survey_design(weights = "PONDMEN") %>%  
  select(-PONDMEN, -IDENT_MEN) %>%  
  tbl_svysummary(by = "n_TYPMEN_new",  
                 include = c(names(conso2)[-c(13:16)], "Effectifs"),
                 statistic = list(all_continuous() ~"{mean}", 
                                  Effectifs ~ "{n_unweighted}"),  
                 type = list(everything() ~ "continuous", 
                             Effectifs ~ "dichotomous"), 
                 value =list(Effectifs = "1"),
                 missing = "no")   

tab <- tab %>% 
  modify_spanning_header(all_stat_cols() ~ "**Configuration familiale**") %>%
  add_overall(last = T)  %>% 
  modify_header(all_stat_cols() ~ "**{level}**") %>%
  add_p() 

tab

saveTableau(tableau = tab,
            type = "tab",
            label = "taux_deffort_conso", 
            description = "Taux d'effort en fonction des structures familiales",
            ponderation = T, n = tab$N, 
            champ = paste0("Ménages formés par au moins un individu adulte agé de 25 à 65 ans et dont le revenu disponible est positif"))



# REGRESSION ###################################################################
names(conso)
names(conso2)
# make a list of independent variables
varlist <- names(conso2)[-c(13:14, 16)]
# varlist <- names(conso)[-c(1, 248:249)]
# varlist <- varlist[str_starts(varlist, "C13")]
varlist


## Données finales sur lesquelles on fait les regressions#######################

data0 <- familles %>% 
  left_join(conso2 %>% 
              select(any_of(varlist), "IDENT_MEN"), 
            by = "IDENT_MEN") %>%   
  mutate(EPARGNE = REVDISP - `Consommation finale`) %>%
  mutate(`Revenus non individualisables` = REVDISP - (n_REVENUS_F + n_REVENUS_H), 
         REVSOC2 = REVSOC -(CHOMAGE+RETRAITES)) %>%
  mutate_at(.vars = vars("n_REVENUS_F", "n_REVENUS_H", "Revenus non individualisables", "REVSOC2", "NIVIE", "REVDISP"),
            .funs = function(x){x/100}) %>%
  mutate(STALOG = fct_collapse(STALOG,
    Propriétaire = c("Accédant à la propriété", "Propriétaire ou copropriétaire")))

    
freq(data0$TYPMEN5)
summary(data0$EPARGNE)
summary(data0$Alimentation)

data0 <- data0 %>%
  filter(n_TYPMEN_new %in% c("Traditionelle", "Recomposée")) %>%
  mutate(n_TYPMEN_new = n_TYPMEN_new %>% droplevels(), 
         n_TYPFAM = n_TYPFAM %>% droplevels(), 
         n_FractionClasse = n_FractionClasse %>% fct_relevel(
           "Classes moyennes superieures [C4]", "Classes populaires précaires [C3]",
           "Classes populaires célibataires et urbaines [C6]", "Petits indépendants [C5]",
           "Petits-moyens [C1]", "Bourgeoisie culturelle [C2]", "Bourgeoisie économique [C7]"
         ))
var_label(data0$n_REVENUS_F) <- "Revenu féminin"
var_label(data0$n_REVENUS_H) <- "Revenu masculin"
var_label(data0$n_FractionClasse) <- "Fraction de classe"
var_label(data0$n_TYPMEN_new) <- "Configuration familiale"
var_label(data0$STALOG) <- "Statut d'occupation du logement"
data0$PONDMEN <- data0$PONDMEN/mean(data0$PONDMEN)

## Régressions revenus HF sur tous les postes budgétaires #######################

names(data0)
#Nombre d’enfants, âge de l’homme et son carré, diplôme de l’homme et diplôme de la femme (en quatre postes), position professionnelle de l’emploi de l’homme et de celui de la femme, région de résidence, degré d’urbanisation de la commune de résidence.
varlist <- c(varlist, "EPARGNE")

lapply(varlist, function(x){plot(density(data0[[x]]), 
                                 main=x)})


### Regression simple sans interactions ####
# create regression function
fitreg <- function(x) { 
  survreg(Surv(get(x)+1, get(x)+1>=1, type='left') ~ n_REVENUS_F + n_REVENUS_H + n_FractionClasse + NENFANTS + STALOG,
          data=data0,
          weights = data0$PONDMEN, 
          dist='gaussian')}


# on applique 
results <- lapply(varlist, fitreg)
names(results) <- varlist



# On regarder les résidus 
lapply(varlist, function(x) {
  plot(density(residuals(results[[x]])), 
       main = x)})
Sys.sleep((1/60)*2)

saveTableau(tableau = results, 
            type = "reg_brute", 
            label = "RevenusHF_sur_conso_simple", 
            description = "Regresison revenus h/f simple sur poste budgétaire", 
            champ = "ménages formées par des couples dont au moins l'un des membres est un adulte agé de 25 à 56 ans et vivant avec au moins un enfant de moins de 25 ans", 
            ponderation = T, 
            n = nrow(data0))


### Regression simple avec TYpe de ménage ####
# create regression function
fitreg <- function(x) { 
  survreg(Surv(get(x)+1, get(x)+1>=1, type='left') ~ n_REVENUS_F + n_REVENUS_H + n_TYPMEN_new + n_FractionClasse + NENFANTS + STALOG,
          data=data0,
          weights = data0$PONDMEN, 
          dist='gaussian')}


# on applique 
results <- lapply(varlist, fitreg)
names(results) <- varlist

# On regarder les résidus 
lapply(varlist, function(x) {
  plot(density(residuals(results[[x]])), 
       main = x)})

Sys.sleep((1/60)*2)

saveTableau(tableau = results, 
            type = "reg_brute", 
            label = "RevenusHF_etTYPMEN_sur_conso_simple", 
            description = "Regresison revenus h/f avec type de famille sur poste budgétaire", 
            champ = "ménages formées par des couples dont au moins l'un des membres est un adulte agé de 25 à 56 ans et vivant avec au moins un enfant de moins de 25 ans", 
            ponderation = T, 
            n = nrow(data0))



### Regression simple avec TYpe de ménage et interaction HF et type de ménage ####
# create regression function

fitreg <- function(x) { 
  survreg(Surv(get(x)+1, get(x)+1>=1, type='left') ~ n_FractionClasse + STALOG + NENFANTS + n_REVENUS_F:n_TYPMEN_new + n_REVENUS_H:n_TYPMEN_new + n_TYPMEN_new, 
          data=data0,
          weights = data0$PONDMEN, 
          dist='gaussian')}


# on applique 
results <- lapply(varlist, fitreg)
Sys.sleep((1/60)*2)
names(results) <- varlist

# On regarder les résidus 
lapply(varlist, function(x) {
  plot(density(residuals(results[[x]])), 
       main = x)})


summary(results$Alimentation)
saveTableau(tableau = results, 
            type = "reg_brute", 
            label = "RevenusHF_etTYPMEN_sur_conso_interaction", 
            description = "Regresison revenus h/f avec type de famille sur poste budgétaire et interaction", 
            champ = "ménages formées par des couples dont au moins l'un des membres est un adulte agé de 25 à 56 ans et vivant avec au moins un enfant de moins de 25 ans", 
            ponderation = T, 
            n = nrow(data0))



#### Mise en forme ####
##### Tableau conso / epargne ####

tab1 <- results$`Consommation finale` %>%
  tbl_regression() %>%
  add_significance_stars(hide_ci = F, hide_se = T)
tab2 <- results$EPARGNE %>%
  tbl_regression() %>%
  add_significance_stars(hide_ci = F, hide_se = T)

tab3 <- tbl_merge(list(tab1, tab2)) %>%
  as_kable_extra(caption = "Regression sur le montant de consommation et d'épargne annuel", 
                 digits = 1, booktabs = T, format = "latex") %>%
  kable_styling(
    latex_options = c("hold_position", "repeat_header"),
    font_size = 7)
tab3
saveTableau(tableau = tab3, 
            type = "reg", 
            label = "RevenusHF_etTYPMEN_sur_consototepargne_interaction", 
            description = "Regresison revenus h/f avec type de famille sur poste budgétaire et interaction", 
            champ = "ménages formées par des couples dont au moins l'un des membres est un adulte agé de 25 à 56 ans et vivant avec au moins un enfant de moins de 25 ans", 
            ponderation = T, 
            n = nrow(data0))

#### Graph coeef ####
reg_dat <- ggcoef_compare(results, 
                          return_data = T, 
                          add_reference_rows = TRUE,
                          conf.level = 0.90)
reg_dat$variable
#summary(results$Alimentation)
reg_dat1 <- reg_dat %>%
  filter(var_type == "interaction")

# On ne garde que ce qui est significatif 
reg_dat1 <- reg_dat1 %>%
  #filter(p.value <= 0.1) %>%
  filter(!(model %in% c("Consommation finale", "EPARGNE")))

gg <- ggcoef_plot(reg_dat1, 
                  x = "estimate", 
                  y = "label", 
                  colour = "label", 
                  errorbar_coloured = T, 
                  stripped_rows = F) +
  facet_wrap(.~model, ncol = 1,  switch = "y") +
  scale_color_brewer(palette = "PuOr") +
  scale_x_continuous(name = "Beta (euros dépensés pour une augmentation du revenu de 100 euros)") +
  theme_tufte(base_size = 14)+
  theme(strip.text.y.left = element_text(angle = 0, hjust = 1, vjust = 0.5), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        panel.background = element_rect(fill = "white", ),
        panel.border = element_rect(fill = NA, colour = "white"),
        legend.position = "right", 
        panel.grid.major.x = element_line(colour = "grey80"), 
        panel.grid.minor.x = element_line(colour = "grey80"), 
        legend.title = element_text(size = rel(0.85), face = "bold"),
        legend.text = element_text(size = rel(0.70), face = "bold"),
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.key.size = unit(1.5, "lines"),
        legend.background = element_rect(fill = "transparent", colour = NA),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, size = rel(0.60)))


gg
saveTableau(tableau = gg, 
            type = "reg_graph", 
            label = "RevenusHF_etTYPMEN_sur_conso_interaction", 
            description = "Regresison revenus h/f avec type de famille sur poste budgétaire et interaction", 
            champ = "ménages formées par des couples dont au moins l'un des membres est un adulte agé de 25 à 56 ans et vivant avec au moins un enfant de moins de 25 ans", 
            ponderation = T, 
            n = nrow(data0))


### Regression sur uniquement les familles recomposées #########################
data1 <- data0 %>%
  filter(n_TYPMEN_new == "Recomposée") %>%
  mutate(n_EnfantsMen_H = n_EnfantsMen_H %>%
           as.character() %>%
           fct_recode(
             "Beau-père\nsans enfant" = "FALSE",
             "Père" = "TRUE") %>%
           fct_relevel("Père"), 
         n_EnfantsMen_F = n_EnfantsMen_F %>%
           as.character() %>%
           fct_recode(
             "Belle-mère\nsans enfant" = "FALSE",
             "Mère" = "TRUE") %>%
           fct_relevel("Mère"))
data1$PONDMEN <- data1$PONDMEN/mean(data1$PONDMEN)

# create regression function

fitreg <- function(x) { 
  survreg(Surv(get(x)+1, get(x)+1>=1, type='left') ~ n_REVENUS_F:n_EnfantsMen_F + n_REVENUS_H:n_EnfantsMen_H + n_FractionClasse + NENFANTS + STALOG,
          data=data1,
          weights = data1$PONDMEN, 
          dist='gaussian')}


# on applique 
results <- lapply(varlist, fitreg)
Sys.sleep((1/60)*2)
names(results) <- varlist

# On regarder les résidus 
lapply(varlist, function(x) {
  plot(density(residuals(results[[x]])), 
       main = x)})

summary(results$Alimentation)


saveTableau(tableau = results, 
            type = "reg_brute", 
            label = "Fam_recomp_RevenusHF_etEnfantsMenage_sur_conso_interaction", 
            description = "Regression revenus h/f avec ", 
            champ = "ménages formées par des couples dont au moins l'un des membres est un adulte agé de 25 à 56 ans et vivant avec au moins un enfants de moins de 25 ans issu d'une union précédante", 
            ponderation = T, 
            n = nrow(data1))

##### mise en page #####

# On récupère des données 
reg_dat <- ggcoef_compare(results, 
                          return_data = T, 
                          add_reference_rows = TRUE,
                          conf.level = 0.90)

#summary(results$Alimentation)
reg_dat1 <- reg_dat %>%
  filter(var_type == "interaction") %>%
  filter(!(model %in% c("Consommation finale", "EPARGNE"))) 

gg <- ggcoef_plot(reg_dat1, 
                  x = "estimate", 
                  y = "label", 
                  colour = "label", 
                  errorbar_coloured = T, 
                  stripped_rows = F) +
  facet_wrap(.~model, ncol = 1,  switch = "y") +
  scale_color_brewer(palette = "PuOr") +
  xlab(label = "Beta (euros dépensés pour une augmentation du revenu de 100 euros)") +
  theme_tufte(base_size = 14)+
  theme(strip.text.y.left = element_text(angle = 0, hjust = 1, vjust = 0.5), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        panel.background = element_rect(fill = "white", ),
        panel.border = element_rect(fill = NA, colour = "white"),
        #legend.position = "bottom", 
        panel.grid.major.x = element_line(colour = "grey80"), 
        panel.grid.minor.x = element_line(colour = "grey80"), 
        legend.title = element_text(size = rel(0.85), face = "bold"),
        legend.text = element_text(size = rel(0.70), face = "bold"),
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.key.size = unit(1.5, "lines"),
        legend.background = element_rect(fill = "transparent", colour = NA),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, size = rel(0.60)))


gg
saveTableau(tableau = gg, 
            type = "reg_graph", 
            label = "Fam_recomp_RevenusHF_etEnfantsMenage_sur_conso_interaction", 
            description = "Regression revenus h/f avec ", 
            champ = "ménages formées par des couples dont au moins l'un des membres est un adulte agé de 25 à 56 ans et vivant avec au moins un enfants de moins de 25 ans issu d'une union précédante", 
            ponderation = T, 
            n = nrow(data1))



#### Regression sur l'épagne et l'investissement ###############################

varlist <- names(conso)[str_starts(names(conso), "C13")]
## Données finales sur lesquelles on fait les regressions#######################
data0 <- familles %>%   
  left_join(conso %>% 
              select(any_of(varlist), "IDENT_MEN"), 
            by = "IDENT_MEN") %>%   
  mutate(`Revenus non individualisables` = REVDISP - (n_REVENUS_F + n_REVENUS_H), 
         REVSOC2 = REVSOC -(CHOMAGE+RETRAITES)) %>%
  mutate_at(.vars = vars("n_REVENUS_F", "n_REVENUS_H", "Revenus non individualisables", "REVSOC2", "NIVIE"),
            .funs = function(x){x/100}) %>%
  mutate(STALOG = fct_collapse(STALOG,
                               Propriétaire = c("Accédant à la propriété", "Propriétaire ou copropriétaire")))

freq(data0$STALOG)
data0 <- data0 %>%
  filter(n_TYPMEN_new %in% c("Traditionelle", "Recomposée")) %>%
  mutate(n_TYPMEN_new = n_TYPMEN_new %>% droplevels(), 
         n_TYPFAM = n_TYPFAM %>% droplevels())
var_label(data0$n_REVENUS_F) <- "Revenu féminin"
var_label(data0$n_REVENUS_H) <- "Revenu masculin"



### Regression simple avec TYpe de ménage et interaction HF et type de ménage ####
# create regression function
fitreg <- function(x) { 
  survreg(Surv(get(x)+1, get(x)+1>=1, type='left') ~ n_REVENUS_F:n_TYPMEN_new + n_REVENUS_H:n_TYPMEN_new + n_TYPMEN_new + n_FractionClasse + NENFANTS + STALOG,
          data=data0,
          weights = data0$PONDMEN, 
          dist='gaussian')}


# on applique 
results <- lapply(varlist, fitreg)
names(results) <- varlist

# On regarder les résidus 
lapply(varlist, function(x) {
  plot(density(residuals(results[[x]])), 
       main = x)})

names(results) <- var_label(conso[, varlist]) %>% as.vector() %>%
  str_sub(start = 0, end = 30)

Sys.sleep((1/60)*2)
saveTableau(tableau = results, 
            type = "reg_brute", 
            label = "RevenusHF_etTYPMEN_sur_conso13_interaction", 
            description = "Regresison revenus h/f avec type de famille sur poste budgétaire et interaction", 
            champ = "ménages formées par des couples dont au moins l'un des membres est un adulte agé de 25 à 56 ans et vivant avec au moins un enfant de moins de 25 ans", 
            ponderation = T, 
            n = nrow(data0))




























## Mise en forme ############################################""

# # On enregistre les résultats 
# ## Un grand tableau synthétique 
# tbls <- lapply(varlist, function(x) {
#   tbl <- results[[x]] %>%
#     tbl_regression() %>%
#     add_significance_stars(
#       hide_se = TRUE,
#       pattern = "{estimate}{stars}  \n[{conf.low} ; {conf.high}]"
#     ) %>%
#     add_glance_source_note() %>%
#     modify_header(estimate ~ "**Beta (95% CI)**") %>%
#     modify_footnote(estimate ~ "CI = Confidence Interval", abbreviation = TRUE) %>%
#     modify_spanning_header(c("estimate") ~ paste0("**", x, "**"))
#   return(tbl)
# })


