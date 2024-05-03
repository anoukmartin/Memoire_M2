
################################################################################- 
#########################  REPERAGE SOUS-POP  ##################################
################################################################################- 

# On adopte une approche par les enfants, parce que c'est ce qui est usuel dans
# dans la quantification du nombre de familles recomposées, dans cette partie on
# identifie les situations des parents

indiv <- readRDS("Data_output/indiv.Rds") %>%
  var_IDENTIFIANT(NewVarName = "n_IdentIndiv", 
                  IdentIndiv = "NOI", 
                  IdentMenage = "IDENT_MEN") %>%
  var_IDENTIFIANT(NewVarName = "n_IdentConjoint", 
                  IdentMenage = "IDENT_MEN", 
                  IdentIndiv = "CONJOINT")
names(indiv)


enfantsMenage <- readRDS("Data_output/enfantsDuMenage.Rds") 

list_parents <- unique(enfantsMenage$n_IdentMere, 
                       enfantsMenage$n_IdentMere) 
list_parents <- list_parents[!is.na(list_parents)]

list_beauparents <- c(
  enfantsMenage[
    enfantsMenage$n_ConjPere == "Beau-parent"
    & !is.na(enfantsMenage$n_ConjPere), ]$n_IdentConjointPere,
  enfantsMenage[
    enfantsMenage$n_ConjMere == "Beau-parent"
    & !is.na(enfantsMenage$n_ConjMere), ]$n_IdentConjointMere
  ) %>%
  unique()

indiv <- indiv %>%
  mutate(n_Parent = case_when(
    n_IdentIndiv %in% list_parents ~ TRUE, 
    IDENT_MEN %in% unique(enfantsMenage$IDENT_MEN) & ENFANT != "1" ~ FALSE, 
    TRUE ~ NA)) %>%
  mutate(n_BeauParent = case_when(
    n_IdentIndiv %in% list_beauparents ~ TRUE, 
    IDENT_MEN %in% unique(enfantsMenage$IDENT_MEN) & ENFANT != "1" ~ FALSE, 
    TRUE ~ NA))

indiv$pon
indiv %>% 
  as_survey_design(weigths = PONDIND) %>%
  rec_CS
  tbl_svysummary(by = n_BeauParent, 
                 include = c("SEXE", "AGE", 
                 )



           
# # On charge les données
# 
# enfantsTous <- readRDS("Data_output/enfantsTous.Rds")
# 
# 
# # Données sur les enfants
# enfantsTous <- allData$enfantsTous
# enfantsTous$n_IdentIndiv
# 
# # Données sur les parents
# indiv <- allData$indiv
# parents <- indiv %>%
#   var_IDENTIFIANT(IdentIndiv = "NOI",
#                   IdentMenage = "IDENT_MEN",
#                   NewVarName = "n_IdentIndiv")
# identParents <- unique(enfantsTous$n_IdentMere, enfantsTous$n_IdentPere)
# parents$n_IdentIndiv
# parents <- parents %>%
#   select(n_IdentIndiv %in% identParents)
# 
# indiv$
# var_IDENTIFIANT()