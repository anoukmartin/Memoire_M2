
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
var <- c("IDENT_MEN", "REVDISP", "NIVIE", "COEFFUC", "NENFANTS", "REVSOC", "REV701", "REV700", "CHOMAGE", "RETRAITES") 

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
types_conso <- c(paste0("0", 1:9), 10:12)    

conso2 <- lapply(types_conso,
                 function(x){   
                   tab <- conso %>%
                     select(starts_with(paste0("C", x)))  
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
                   "BIENS ET SERVICES DIVERS") %>%    str_to_sentence()     

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

## Données finales sur lesquelles on fait les tableaux  ########################
data <- familles %>%   
  left_join(conso2, by = "IDENT_MEN") %>%   
  mutate(`Consommation finale par UC` = `Consommation finale`/COEFFUC,
         `Revenus non individualisables` = REVDISP - (n_REVENUS_F + n_REVENUS_H))


## Tableau : structure des budgets #############################################

tab <- data %>%  
  mutate(Effectifs = 1) %>%
  as_survey_design(weights = "PONDMEN") %>%  
  select(-PONDMEN, -IDENT_MEN) %>%  
  tbl_svysummary(by = "n_TYPMEN_new",  
                 include = c(names(conso2)[-14], "Effectifs"),
                 statistic = list(all_continuous() ~"{mean}", 
                                  Effectifs ~ "{n}"),  
                 type = list(everything() ~ "continuous", 
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
            label = "structure_budget", 
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


# REGRESSION ###################################################################
names(conso)
# make a list of independent variables
varlist <- names(conso2)[-c(13:14)]
# varlist <- names(conso)[-c(1, 248:249)]
# varlist <- varlist[str_starts(varlist, "C13")]
varlist


## Données finales sur lesquelles on fait les regressions#######################
data0 <- familles %>%   
  left_join(conso2 %>% 
              select(any_of(varlist), "IDENT_MEN"), 
            by = "IDENT_MEN") %>%   
  mutate(`Revenus non individualisables` = REVDISP - (n_REVENUS_F + n_REVENUS_H), 
         REVSOC2 = REVSOC -(CHOMAGE+RETRAITES)) %>%
  mutate_at(.vars = vars("n_REVENUS_F", "n_REVENUS_H", "Revenus non individualisables", "REVSOC2", "NIVIE"),
            .funs = function(x){x/12000}) 

data0 <- data0 %>%
  filter(n_TYPMEN_new %in% c("Traditionelle", "Recomposée")) %>%
  mutate(n_TYPMEN_new = n_TYPMEN_new %>% droplevels(), 
         n_TYPFAM = n_TYPFAM %>% droplevels())
var_label(data0$n_REVENUS_F) <- "Revenu féminin"
var_label(data0$n_REVENUS_H) <- "Revenu masculin"

## Régressions revenus HF sur tous les postes budgétaires #######################

names(data0)
#Nombre d’enfants, âge de l’homme et son carré, diplôme de l’homme et diplôme de la femme (en quatre postes), position professionnelle de l’emploi de l’homme et de celui de la femme, région de résidence, degré d’urbanisation de la commune de résidence.

# create regression function
fitreg <- function(x) { 
  survreg(Surv(get(x)+1, get(x)+1>=1, type='left') ~ n_REVENUS_F*n_TYPMEN_new + n_REVENUS_H*n_TYPMEN_new + n_FractionClasse + NENFANTS,
          data=data0,
          weights = data0$PONDMEN, 
          dist='gaussian')}

# on applique 
results <- lapply(varlist, fitreg)

 
names(results) <- varlist

ggcoef_compare(results, 
               include = c("n_REVENUS_F:n_TYPMEN_new", 
                           "n_TYPMEN_new:n_REVENUS_H"), 
               shape_values = c(16, 4), 
               shape = "significance", 
               conf.level = 0.90)



reg_dat <- ggcoef_compare(results, 
                          return_data = T, 
                          add_reference_rows = TRUE,
                          conf.level = 0.90)
reg_dat <- reg_dat %>%
  filter(!(variable %in% c("n_FractionClasse", "NENFANTS", "n_TYPFAM"))) 

plot_dat <- reg_dat %>%
  mutate(Rev = if_else(str_detect(label, "féminin"), "Féminin", "Masculin"), 
         Fam = if_else(str_detect(label, "Traditionelle"), "Traditionelle", "Recomposée"))
  
  
plot_dat %>%
  ggplot(aes(x = label, y = estimate, fill = Rev, alpha = Fam)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(x=label, ymin=conf.low, ymax=conf.high)) +
  facet_wrap(vars(model),
             scales="free")
             


reg_dat2 <- reg_dat %>%
  filter(p.value <= 0.1)

ggcoef_plot(reg_dat2, dodged = TRUE, facet_row = "model", scales = "free")

