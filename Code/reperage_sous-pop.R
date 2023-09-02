

## Approche par les enfants ####

names(indiv)

indiv$IDENT_IND

# On définit qui sont considéré comme des enfants 
indiv$ENFRP # Variable enfant au sens du recensement
indiv$ENFANT # Variable au sens du TCM (budget commun en plus)
tbl_cross(indiv, ENFANT, ENFRP) # On regarde si elles se recoupent 
temp <- indiv %>%
  filter(ENFANT == "2" & ENFRP == "1")
# Donc on a trois individus assez agés qui sont considérés comme enfant au sens 
# du RP mais pas du TCM
rm(temp)

# On va définir une limite d'âge 
enfants <- indiv %>%
  filter(AG <= 25)  %>% # on peut changer en le seuil, 25 ans me paraît bien seuil d'ouverture du RSA 
  filter(ENFANT == "1") # puis on ne prend que ceux qui sont considérés comme enfant au sens du TCM (idem recensement car on virer les individus agés)

# Ensuite on va regarder avec qui vivent ces enfants
enfants <- enfants %>%
  mutate(n_NPARENTS = case_when(
    PER1E == "1" & MER1E == "1" ~ "les deux",
    PER1E == "1" & MER1E != "1" ~ "le père uniquement",
    PER1E != "1" & MER1E == "1" ~ "la mère uniquement",
    PER1E != "1" & MER1E != "1" ~ "aucun des deux"))
tbl_summary(enfants, 
            include = n_NPARENTS)
# Aucun enfants ne vit avec aucun de ses parents,
# enfnant vivant avec uniquement leur mère surreprésenté, parce que famille 
# mono sont plus souvent des mères isolées que des pères, + sur-échantillon 
# dans l'enquète BDF

# On récupère l'id individuel du père, de la mère et de l'enfant
tbl_summary(enfants, 
            include = c("NOI", "MER2E", "PER2E"), 
            type = list(everything() ~ "categorical"))
# Comme les identifiants individuels peuvent avoir 1 ou plusieurs chiffres, on ajoute des "0"
# On utilise cette fonction pour faire ces identifiants individuels
var_IDENTIFIANT <- function(data, IdentIndiv, IdentMenage, NewVarName){
  data$tempIndiv <- data[, IdentIndiv] %>% as.vector()
  data$tempMenage <- data[, IdentMenage] %>% as.vector()
  data <- data %>%
    mutate(tempVar = case_when(
      str_length(tempIndiv) == 1 ~ paste0(tempMenage, "0", tempIndiv), 
      str_length(tempIndiv) == 2 ~ paste0(tempMenage, tempIndiv))) 
  names(data)[names(data) == "tempVar"] <- NewVarName
  return(data)
}
class(data$tempIndiv)
data$BS
str_length(as.character(data$tempIndiv))
str_length(data$tempIndiv)

data <- enfants
test <- enfants %>%
  var_IDENTIFIANT(IdentIndiv = "NOI", IdentMenage = "IDENT_MEN", NewVarName = "n_IdentIndiv")

test$n_IdentIndiv
enfants$NOI
filter()
  
enfants <- enfants %>%
  mutate(n_NOI = if_else)
  mutate(n_IDENTENFANT = paste0(IDENT_MEN, NOI))



# Pour ceux qui ne vivent qu'avec un parent, on récupère le NOI du parent
monop <- enfants %>%
  filter(n_NPARENTS %in% c("père", "mère")) %>%
  select(IDENT_IND, IDENT_MEN, n_NPARENTS, PER1E, PER2E, MER1E, MER2E) 
# On crée l'ID du père et de la mère 
