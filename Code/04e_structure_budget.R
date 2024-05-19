
# DATA #########################################################################
## Données sur les couples/méanges de notre popilatio ##########################
familles <- readRDS("Data_output/familles_parents.Rds") 

## DOnnées sur des ménages à ajouter ###########################################
menages <- readRDS("Data_output/menages.Rds")
familles <- left_join(familles, 
                      menages %>%
                        select(IDENT_MEN, REVDISP, NIVIE, COEFFUC), 
                      by = "IDENT_MEN")

## Données de consommation #####################################################
conso <- readRDS("Data_output/conso.Rds")
names(conso)   
types_conso <- c(paste0("0", 1:9), 10:12)    

conso2 <- lapply(types_conso,
                 function(x){   
                   tab <- conso %>%
                     select(starts_with(paste0("C", x)))  
                   tab <- rowSums(tab, na.rm = TRUE) 
                   consotot<- conso$CTOT %>% as.vector()  
                   tab <- (tab/consotot)*100   
                   return(tab) } )  

conso2 <- conso2 %>%   
  bind_cols() %>%   
  as.data.frame()  

names(conso2) <- c("ALIMENTATION",                     
                   "ALCOOLS, TABACS ET STUPEFIANTS ", 
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

## Données finales sur lesquelles on fait les tableaux  ########################
data <- familles %>%   
  left_join(conso2, by = "IDENT_MEN") %>%   
  select("PONDMEN", "n_TYPMEN_new", all_of(names(conso2)), 
         "REVDISP", "COEFFUC", "NIVIE") %>%
  mutate(`Consommation finale par UC` = `Consommation finale`/COEFFUC)


# TABLEAUX #####################################################################

## Tableau : structure des budgets #############################################


tab <- data %>%  
  mutate(Effectifs = 1) %>%
  as_survey_design(weights = "PONDMEN") %>%  
  select(-PONDMEN, -IDENT_MEN) %>%  
  tbl_svysummary(by = "n_TYPMEN_new",                   
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
