################################################################################- 
#########################  REPERAGE SOUS-POP  ##################################
################################################################################- 

# On adopte une approche par les enfants, parce que c'est ce qui est usuel dans 
# dans la quantification du nombre de familles recomposées. Ici grace aux 
# situations des enfants on identifie quels sont les ménages avec une configuation 
# familiale recomposée

# On charge les données 
enfantsTous <- readRDS("Data_output/enfantsTous.Rds") 
menages <- readRDS("Data_output/menages.Rds")


# Les enfants du ménage ######################################################## 
enfantsD <- enfantsTous %>%
  filter(n_statutResid == "Enfant du ménage (au sens du TCM)") %>%
  mutate(n_IdentMenage = str_sub(n_IdentIndiv, 1, 5)) %>%
  mutate(n_NOI = str_sub(n_IdentIndiv, 6, 7)) 

# tableau des configuration
config <- enfantsD %>%
  pivot_wider(id_cols = n_IdentMenage, 
              names_from = n_NOI, 
              names_prefix = "EnfantNOI_", 
              values_from = n_configFamEnfantsPSexe) %>%
  mutate(across(.cols = starts_with("EnfantNOI_"), 
                .fns = function(x){
                  x = labelled(x, label = "Configuration familiale de l'enfant de NOI")
                })) 

# On essaye de synthétiser par ménage (prendre en compte la situation de tous les 
# enfants du ménages)
configSynthese <- enfantsD %>%
  group_by(n_IdentMenage) %>%
  reframe(mods = unique(n_configFamEnfantsPSexe)) %>%
  ungroup()
compte <- configSynthese %>%
  group_by(n_IdentMenage) %>%
  summarise(n_config = n(), 
            names_config = paste(mods, collapse = ", ")) %>%
  mutate()
configSynthese <- configSynthese %>%
  mutate(value = T) %>%
  pivot_wider(id_cols = n_IdentMenage, 
              names_from = mods, values_fill = F) %>% 
  left_join(compte)

rm(compte)
# On crée deux variable : 
## - une de configuration familiale :
configSynthese$n_configFam <- NA
## - indicatrice d'enfants d'une nouvelle union :
configSynthese$n_enfantNewUnion <- NA

# Pour tous les ménages pour lesquels on a une unique configuration renseignée, 
# on garde cette valeur
configSynthese[
  configSynthese$n_config == 1, "n_configFam"
] <- configSynthese[
  configSynthese$n_config == 1, "names_config"
]

# Pour ceux qui sont recomposés, on ajoute qu'il n'y a pas d'enfant de la 
# nouvelle union (sinon, on aurait eu également "configuration traditionnelle")
# de renseignée 
configSynthese[
  configSynthese$n_config == 1 
  & (configSynthese$`Configuration recomposée maternelle` | configSynthese$`Configuration recomposée paternelle`),
  "n_enfantNewUnion"
] <- 0

# Pour ceux qui sont tard, on ajoute qu'il y des enfants de la 
# nouvelle union
configSynthese[
  configSynthese$n_config == 1 & configSynthese$`Configuration traditionnelle`,
  "n_enfantNewUnion"
] <- 1

# Pour les cas ou on a plusieurs configurations 
## Cas de famille recomposées par les deux membres du couple
configSynthese[
  configSynthese$n_config == 2 
  & configSynthese$`Configuration recomposée maternelle` 
  & configSynthese$`Configuration recomposée paternelle`, 
  c("n_configFam", "n_enfantNewUnion")
] <- list("Configuration recomposée maternelle et paternelle", 0)

## Cas de famille recomposée par une femme avec enfant d'une nouvelle union 
configSynthese[
  configSynthese$n_config == 2 
  & configSynthese$`Configuration recomposée maternelle` 
  & configSynthese$`Configuration traditionnelle`,
  c("n_configFam", "n_enfantNewUnion")
] <- list("Configuration recomposée maternelle", 1)

## Cas de famille recomposée par un homme avec enfant d'une nouvelle union 
configSynthese[
  configSynthese$n_config == 2 
  & configSynthese$`Configuration recomposée paternelle` 
  & configSynthese$`Configuration traditionnelle`,
  c("n_configFam", "n_enfantNewUnion")
] <- list("Configuration recomposée paternelle", 1)


## Cas de famille recomposée par les deux conjoints avec enfant de la nouvelle
# union
configSynthese[
  configSynthese$n_config == 3 
  & configSynthese$`Configuration recomposée paternelle` 
  & configSynthese$`Configuration traditionnelle`
  & configSynthese$`Configuration recomposée maternelle`,
  c("n_configFam", "n_enfantNewUnion")
] <- list("Configuration recomposée maternelle et paternelle", 1)

# On a plusieurs ménages avec des configurations a priori incohérentes :
# ex : monoparentale paternelle et recomposée maternelle 
tab <- configSynthese[
  is.na(configSynthese$n_configFam), ]
# Mais il peut s'agir de ménages ou il y a plusieurs couples et/ou des enfants 
# de plusieurs générations. On va regarder :
tab <- tab |>
  left_join(menages |>
              select("IDENT_MEN", "NCOUPLES", "NENFANTS", "TYPMEN5"), 
            by = c("n_IdentMenage" = "IDENT_MEN")) |> 
  rec_TYPMEN5()
freq(tab$TYPMEN5)
# Ce sont tous des ménages complexes, contrairement à la plus part des autres
configSynthese |>
  left_join(menages |>
              select("IDENT_MEN", "NCOUPLES", "NENFANTS", "TYPMEN5"), 
            by = c("n_IdentMenage" = "IDENT_MEN")) |> 
  rec_TYPMEN5() |> 
  select(n_configFam, n_config, TYPMEN5) |>
  tbl_summary(by = "TYPMEN5")

# Donc on va recoder en considérant que la structure de ces familles ne s'assimile
# pas aux catégories proposées 
configSynthese[
  is.na(configSynthese$n_configFam), "n_configFam" ] <- "Configuration complexe"

rm(tab) #cleanning 

# on a également un cas avec monoparentale ou recomposée non identifiée parce que 
# le statut conjugale de la mère était manquant;
tab <- configSynthese[
  configSynthese$n_configFam == "Configuration monoparentale ou recomposée maternelle", ]
# on regarde les infos qu'on a sur ce ménage
tab <- tab |>
  left_join(menages |>
              select("IDENT_MEN", "NCOUPLES", "NENFANTS", "TYPMEN5"), 
            by = c("n_IdentMenage" = "IDENT_MEN")) |> 
  rec_TYPMEN5()
# Il s'agit d'un ménage complexe avec aucun couple et un enfant, on peut supposer 
# qu'il s'assimile d'avantage une famille monoparentale
# On recode en config mono maternelle
configSynthese[
  configSynthese$n_configFam == "Configuration monoparentale ou recomposée maternelle", "n_configFam" ] <- "Configuration monoparentale maternelle"
rm(tab)

# On va ajouter un recodage qui distingue configuration et genre de la configuration 
freq(configSynthese$n_configFam)
configSynthese <- configSynthese %>%
  mutate(
    n_genreFam = case_when(
      n_configFam %in% c("Configuration complexe") ~ NA, 
      n_configFam %in% c("Configuration traditionnelle") ~ "Les deux",
      n_configFam %in% c("Configuration monoparentale maternelle", "Configuration recomposée maternelle") ~ "Femme", 
      n_configFam %in% c("Configuration monoparentale paternelle", "Configuration recomposée paternelle") ~ "Homme", 
      n_configFam == "Configuration recomposée maternelle et paternelle" ~ "Les deux"),
    n_config = case_when(
      n_configFam == "Configuration complexe" ~ "Complexe",
      n_configFam == "Configuration traditionnelle" ~ "Traditionnelle",
      n_configFam %in% c("Configuration monoparentale maternelle", "Configuration monoparentale paternelle") ~ "Monoparentale", 
      n_configFam %in% c("Configuration recomposée maternelle", "Configuration recomposée paternelle", "Configuration recomposée maternelle et paternelle") ~ "Recomposée"))

# on supprime cette variable peu utile
configSynthese$names_config <- NULL
# On labelise les variables synthétiques
configSynthese$n_config <- labelled(configSynthese$n_config, 
                                    label = "Configuration familiale")
configSynthese$n_genreFam <- labelled(configSynthese$n_genreFam, 
                                    label = "Sexe des parents")
configSynthese$n_enfantNewUnion <- labelled(configSynthese$n_enfantNewUnion, 
                                    label = "Existance d'enfants issus de l'union actuelle")

# configSynthese %>%
#   select(n_config, n_genreFam, n_enfantNewUnion) %>%
#   tbl_summary(by = "n_config")

familles <- menages %>%
  left_join(config, by = c("IDENT_MEN" = "n_IdentMenage")) %>%
  left_join(configSynthese,  by = c("IDENT_MEN" = "n_IdentMenage"))

familles %>%
  select(TYPMEN5, n_config) %>%
  rec_TYPMEN5() %>%
  tbl_summary(by = "TYPMEN5")

saveData(familles, label = "familles")

rm(config, configSynthese)



# Les enfants hors domicile ####################################################

enfantsHD <- enfantsTous %>%
  filter(n_statutResid == "Enfant résidant hors domicile" & 
           n_configFamEnfantsS %in% c("Configuration monoparentale", 
                                      "Configuration recomposée", 
                                      "Configuration traditionnelle")) %>%
  mutate(n_IdentMenage = str_sub(n_IdentIndiv, 1, 5)) %>%
  mutate(n_NOI = str_sub(n_IdentIndiv, 6, 7))

# tableau des configuration
config <- enfantsHD %>%
  pivot_wider(id_cols = n_IdentMenage, 
              names_from = n_NOI, 
              names_prefix = "EnfantNOI_", 
              values_from = n_configFamEnfantsSSexe) %>%
  mutate(across(.cols = starts_with("EnfantNOI_"), 
                .fns = function(x){
                  x = labelled(x, label = "Configuration familiale secondaire de l'enfant de NOI")
                })) 


# On essaye de synthétiser par ménage (prendre en compte la situation de tous les 
# enfants du ménages)
configSynthese <- enfantsHD %>%
  group_by(n_IdentMenage) %>%
  reframe(mods = unique(n_configFamEnfantsSSexe)) %>%
  ungroup()
freq(configSynthese$mods)

NBenfantsHD <- enfantsHD %>% 
  select(n_IdentMenage) %>%
  group_by(n_IdentMenage) %>%
  summarise(n_NEnfantsHD = n())
 
compte <- configSynthese %>%
  group_by(n_IdentMenage) %>%
  summarise(n_config = n(), 
            names_config = paste(mods, collapse = ", ")) %>%
  mutate()

configSynthese <- configSynthese %>%
  mutate(value = T) %>%
  pivot_wider(id_cols = n_IdentMenage, 
              names_from = mods, values_fill = F) %>% 
  left_join(compte)



rm(compte)
freq(configSynthese$n_config)
freq(configSynthese$names_config)




# On crée deux variable : 
## - une de configuration familiale :
configSynthese$n_configFamTemp <- NA
## - indicatrice d'enfants d'une nouvelle union :
configSynthese$n_enfantNewUnionHD <- NA

# Pour tous les ménages pour lesquels on a une unique configuration renseignée, 
# on garde cette valeur
configSynthese[
  configSynthese$n_config == 1, "n_configFamTemp"
] <- configSynthese[
  configSynthese$n_config == 1, "names_config"
]

# Pour ceux qui sont recomposés, on ajoute qu'il n'y a pas d'enfant de la 
# nouvelle union (sinon, on aurait eu également "configuration traditionnelle")
# de renseignée 
configSynthese[
  configSynthese$n_config == 1 
  & (configSynthese$`Configuration recomposée maternelle` | configSynthese$`Configuration recomposée paternelle`),
  "n_enfantNewUnionHD"
] <- 0

# Pour ceux qui sont tard, on ajoute qu'il y des enfants de la 
# nouvelle union
configSynthese[
  configSynthese$n_config == 1 & configSynthese$`Configuration traditionnelle`,
  "n_enfantNewUnionHD"
] <- 1

# Pour les cas ou on a plusieurs configurations 
## Cas de famille recomposées par les deux membres du couple
configSynthese[
  configSynthese$n_config == 2 
  & configSynthese$`Configuration recomposée maternelle` 
  & configSynthese$`Configuration recomposée paternelle`, 
  c("n_configFamTemp", "n_enfantNewUnionHD")
] <- list("Configuration recomposée maternelle et paternelle", 0)

## Cas de famille recomposée par une femme avec enfant d'une nouvelle union 
configSynthese[
  configSynthese$n_config == 2 
  & configSynthese$`Configuration recomposée maternelle` 
  & configSynthese$`Configuration traditionnelle`,
  c("n_configFamTemp", "n_enfantNewUnionHD")
] <- list("Configuration recomposée maternelle", 1)

## Cas de famille recomposée par un homme avec enfant d'une nouvelle union 
configSynthese[
  configSynthese$n_config == 2 
  & configSynthese$`Configuration recomposée paternelle` 
  & configSynthese$`Configuration traditionnelle`,
  c("n_configFamTemp", "n_enfantNewUnionHD")
] <- list("Configuration recomposée paternelle", 1)


## Cas de famille recomposée par les deux conjoints avec enfant de la nouvelle
# union
configSynthese[
  configSynthese$n_config == 3 
  & configSynthese$`Configuration recomposée paternelle` 
  & configSynthese$`Configuration traditionnelle`
  & configSynthese$`Configuration recomposée maternelle`,
  c("n_configFamTemp", "n_enfantNewUnionHD")
] <- list("Configuration recomposée maternelle et paternelle", 1)

# On un ménage avec une configurations a priori incohérente :
# ex : monoparentale paternelle et monoparentale paternelle
tab <- configSynthese[
  is.na(configSynthese$n_configFamTemp), ]
# Mais il peut s'agir de ménages ou il y a plusieurs couples et/ou des enfants 
# de plusieurs générations. On va regarder :
tab <- tab |>
  left_join(menages |>
              select("IDENT_MEN", "NCOUPLES", "NENFANTS", "TYPMEN5"), 
            by = c("n_IdentMenage" = "IDENT_MEN")) |> 
  rec_TYPMEN5()
freq(tab$TYPMEN5)
# Ce sont tous des ménages complexes, contrairement à la plus part des autres
configSynthese |>
  left_join(menages |>
              select("IDENT_MEN", "NCOUPLES", "NENFANTS", "TYPMEN5"), 
            by = c("n_IdentMenage" = "IDENT_MEN")) |> 
  rec_TYPMEN5() |> 
  select(n_configFamTemp, n_config, TYPMEN5) |>
  tbl_summary(by = "TYPMEN5")

# Donc on va recoder en considérant que la structure de ces familles ne s'assimile
# pas aux catégories proposées 
configSynthese[
  is.na(configSynthese$n_configFamTemp), "n_configFam" ] <- "Configuration complexe"

rm(tab) #cleanning 

# On va ajouter un recodage qui distingue configuration et genre de la configuration 
freq(configSynthese$n_configFamTemp)

configSynthese <- configSynthese %>%
  mutate(
    n_genreFamTemp = case_when(
      n_configFamTemp %in% c("Configuration complexe") ~ NA, 
      n_configFamTemp %in% c("Configuration traditionnelle") ~ "Les deux",
      n_configFamTemp %in% c("Configuration monoparentale maternelle", "Configuration recomposée maternelle") ~ "Femme", 
      n_configFamTemp %in% c("Configuration monoparentale paternelle", "Configuration recomposée paternelle") ~ "Homme", 
      n_configFamTemp == "Configuration recomposée maternelle et paternelle" ~ "Les deux"),
    n_configTemp = case_when(
      n_configFamTemp == "Configuration complexe" ~ "Complexe",
      n_configFamTemp == "Configuration traditionnelle" ~ "Traditionnelle",
      n_configFamTemp %in% c("Configuration monoparentale maternelle", "Configuration monoparentale paternelle") ~ "Monoparentale", 
      n_configFamTemp %in% c("Configuration recomposée maternelle", "Configuration recomposée paternelle", "Configuration recomposée maternelle et paternelle") ~ "Recomposée"))

# on supprime cette variable peu utile
configSynthese$names_config <- NULL
# On labelise les variables synthétiques
configSynthese$n_configTemp <- labelled(configSynthese$n_configTemp, 
                                    label = "Configuration familiale temporaire")
configSynthese$n_genreFamTemp <- labelled(configSynthese$n_genreFamTemp, 
                                      label = "Sexe des parents (configuration temporaire)")
configSynthese$n_enfantNewUnionHD <- labelled(configSynthese$n_enfantNewUnionHD, 
                                            label = "Existance d'enfants issus de l'union actuelle hors domicile")

# configSynthese %>%
#   select(n_configTemp, n_genreFamTemp, n_enfantNewUnionHD) %>%
#   tbl_summary(by = "n_configTemp")


# Infos sur les enfants HD 

sitEnfHD <- readRDS("Data_output/enfantsHorsDom.Rds") %>%
  reframe(.by = "IDENT_MEN", 
          n_EnfantHDCharge = min(HODCHARG), 
          n_EnfantHDAide = min(HODAID))

tab <- readRDS("Data_output/enfantsHorsDom.Rds") %>%
  reframe(.by = "IDENT_MEN", 
          n_NPARENTS = unique(n_NPARENTS)) %>%
  mutate(value = "1") %>%
  pivot_wider(id_cols = "IDENT_MEN", names_from = "n_NPARENTS", names_prefix = "n_", values_from = "value", values_fill = "0")

sitEnfHD <- left_join(sitEnfHD, tab)

famillesTemp <- menages %>%
  left_join(config, by = c("IDENT_MEN" = "n_IdentMenage")) %>%
  left_join(configSynthese,  by = c("IDENT_MEN" = "n_IdentMenage")) %>%
  left_join(NBenfantsHD, by = c("IDENT_MEN" = "n_IdentMenage")) %>%
  left_join(sitEnfHD, by = c("IDENT_MEN" = "IDENT_MEN"))

saveData(famillesTemp, label = "famillesTemporaires")


menages$IDENT_MEN
names(familles)
names(famillesTemp)
famillesToutes <- menages %>%
  left_join(familles[, c("IDENT_MEN", 
                         "Configuration traditionnelle", 
                         "Configuration recomposée maternelle", 
                         "Configuration monoparentale maternelle", 
                         "Configuration monoparentale paternelle", 
                         "Configuration recomposée paternelle", 
                         "Configuration monoparentale ou recomposée maternelle", 
                         "n_config",   
                         "n_configFam", 
                         "n_enfantNewUnion", 
                         "n_genreFam")]) %>%
  left_join(famillesTemp[, c("IDENT_MEN", 
                             "n_configFamTemp", 
                             "n_enfantNewUnionHD", 
                             "n_configTemp", 
                             "n_genreFamTemp", 
                             "n_NEnfantsHD")])


famillesToutes %>%
  select(all_of(c("n_configFamTemp",
                  "n_enfantNewUnionHD", 
                  "n_configTemp", 
                  "n_genreFamTemp", 
                  "n_config", 
                  "n_configFam", 
                  "n_enfantNewUnion",
                  "n_genreFam"))) %>%
  tbl_cross(n_genreFam, n_genreFamTemp)


famillesToutes <- famillesToutes %>%
  mutate(
    n_configSynth = case_when(
      n_configTemp == "Monoparentale" & is.na(n_config) ~ "Temporairement monoparentale", 
      n_configTemp == "Recomposée" & (is.na(n_config) | n_config == "Traditionnelle") ~ "Temporairement recomposée", 
      n_configTemp == "Traditionnelle" & is.na(n_config) ~ "Temporairement traditionnelle", 
      TRUE ~ n_config), 
    n_genreFamSynth = case_when(
      !is.na(n_config) ~ n_genreFam, 
      is.na(n_config) ~ n_genreFamTemp)) %>%
  mutate(n_genreFamSynth = labelled(n_genreFamSynth, 
                                    label = "Sexe des parents"))

famillesToutes %>%
  select(all_of(c("n_configSynth"))) %>%
  tbl_summary()

famillesToutes <- famillesToutes %>% 
  mutate(
    n_configFamSynth = case_when(
      is.na(n_configTemp) ~ n_configFam, 
      !is.na(n_configTemp) ~ str_replace_all(n_configFamTemp, "Configuration", "Configuration temporairement")))

famillesToutes %>%
  select(all_of(c("n_configFamSynth"))) %>%
  tbl_summary()

famillesToutes %>%
  select(all_of(c("n_genreFamSynth"))) %>%
  tbl_summary()

nbenfants <- famillesToutes[, c("NENFANTS", "n_NEnfantsHD")] %>%
  as.matrix()
nbenfants <- rowSums(nbenfants, na.rm = T)



famillesToutes$n_NEnfantsTous <- nbenfants
famillesToutes <- famillesToutes %>%
  mutate(n_NEnfantsTous = labelled(n_NEnfantsTous, 
                                   label = "Nombre total d'enfants du ménage ou vivant hors domicile"))
rm(nbenfants)

names(famillesToutes)
freq(famillesToutes$n_genreFam)
famillesToutes <- famillesToutes %>%
  mutate(n_ParentsMenage = case_when(
    n_genreFam == "Les deux" ~ "Parents en couple", 
    n_genreFam == "Femme" & n_config == "Monoparentale" ~ "Mère célibataire",
    n_genreFam == "Homme" & n_config == "Monoparentale" ~ "Père célibataire",
    n_genreFam == "Femme" & n_config == "Recomposée" ~ "Mère en couple",
    n_genreFam == "Homme" & n_config == "Recomposée" ~ "Père en couple",
    TRUE ~ "Sans enfants")) %>%
  mutate(n_ParentsHorsDom = case_when(
    n_genreFamTemp == "Les deux" ~ "Parents en couple", 
    n_genreFamTemp == "Femme" & n_configTemp == "Monoparentale" ~ "Mère célibataire",
    n_genreFamTemp == "Homme" & n_configTemp == "Monoparentale" ~ "Père célibataire",
    n_genreFamTemp == "Femme" & n_configTemp == "Recomposée" ~ "Mère en couple",
    n_genreFamTemp == "Homme" & n_configTemp == "Recomposée" ~ "Père en couple",
    TRUE ~ "Sans enfants"))



### Ages moyens des enfants ########################################
# ages des enfants du ménage 
enfantsD <- readRDS("Data_output/enfantsDuMenage.Rds") %>%
  select(IDENT_MEN, n_IdentIndiv, AG) 

agesEnfMen <- enfantsD %>% 
  group_by(IDENT_MEN) %>%
  reframe(n_ageMoyEnfMen = mean(AG))

# age moyen des enfant HD
enfantsHD <- readRDS("Data_output/enfantsHorsDom.Rds") %>%
  select(IDENT_MEN, n_IdentIndiv, AG) 
agesEnfHD <- enfantsHD %>%
  group_by(IDENT_MEN) %>%
  reframe(n_ageMoyEnfHD = mean(AG))

# age moyens de tous les enfants 
agesEnfants <- bind_rows(enfantsD, enfantsHD) %>%
  group_by(IDENT_MEN) %>%
  reframe(n_ageMoyEnfTous = mean(AG))

# on ajoute ces variables aux données des familles 
famillesToutes <- famillesToutes %>%
  left_join(agesEnfMen) %>%
  left_join(agesEnfHD) %>%
  left_join(agesEnfants)

freq(famillesToutes$TYPMEN5)
famillesToutes$SEXEPR
tab <- famillesToutes %>%
  rec_TYPMEN5() %>%
  mutate(n_configMenage = case_when(
    n_ParentsMenage != "Sans enfants" ~ n_ParentsMenage, 
    TYPMEN5 == "Personne seule" & SEXEPR == "1" ~ "Homme seul", 
    TYPMEN5 == "Personne seule" & SEXEPR == "2" ~ "Femme seule",
    TRUE ~ TYPMEN5))  %>%
  mutate(n_configMenage = fct_relevel(
    n_configMenage, 
    "Femme seule", "Homme seul","Couple sans enfant", 
    "Mère célibataire", "Père célibataire", 
    "Mère en couple", "Père en couple", "Parents en couple",
    "Autre type de ménage (ménage complexe)"
  )) %>%
  # mutate(fct_relevel(n_configMenage,
  #   levels = c("Homme seul", "Femme seule", "Couple sans enfant", 
  #              "Mère célibataire", "Père célibataire",
  #              "Mère en couple", "Père en couple", "Parents en couple", 
  #              "Autre type de ménage (ménage complexe)")
  # )) %>%
  select(n_configMenage) 
famillesToutes$n_configMenage <- tab$n_configMenage
famillesToutes <- as.data.frame(famillesToutes)
freq(famillesToutes$n_configMenage)
tab <- table(famillesToutes$TYPMEN5, famillesToutes$n_configFamSynth) %>%
  as.matrix() %>% as.data.frame()

famillesToutes <- famillesToutes %>%
  mutate(CSMEN6 = str_sub(CSMEN, 1, 1)) %>%
  mutate(CSMEN6 = fct_recode(CSMEN6,
    NULL = "",
    NULL = "0",
    "Agriculteurs" = "1",
    "ACCE" = "2",
    "CPIS" = "3",
    "Professions intermédiaires" = "4",
    "Employés" = "5",
    "Ouvriers" = "6",
    NULL = "H"
  ))

freq(famillesToutes$n_configMenage)

saveData(famillesToutes, label = "famillesToutes")



rm(config, configSynthese, menages, enfantsD, enfantsHD, 
   agesEnfMen, agesEnfants, agesEnfHD, tab, sitEnfHD,
   NBenfantsHD, enfantsTous, famillesTemp, familles, famillesToutes)



