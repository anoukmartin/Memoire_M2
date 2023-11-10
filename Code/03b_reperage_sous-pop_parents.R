
################################################################################- 
#########################  REPERAGE SOUS-POP  ##################################
################################################################################- 

# On adopte une approche par les enfants, parce que c'est ce qui est usuel dans
# dans la quantification du nombre de familles recomposées, dans cette partie on
# identifie les situations des parents

# # On charge les données
# indiv <- readRDS("Data_output/indiv.Rds")
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