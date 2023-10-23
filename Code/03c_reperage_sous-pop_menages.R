################################################################################- 
#########################  REPERAGE SOUS-POP  ##################################
################################################################################- 

# On adopte une approche par les enfants, parce que c'est ce qui est usuel dans 
# dans la quantification du nombre de familles recomposées. Ici grace aux 
# situations des enfants on identifie quels sont les ménages avec une configuation 
# familiale recomposée

enfantsTous <- readRDS("Data_output/enfantsTous.Rds") 
menages <- readRDS("Data_output/menages.Rds")

enfantsD <- enfantsTous %>%
  filter(n_statutResid == "Enfant du ménage (au sens du TCM)") %>%
  mutate(n_IdentMenage = str_sub(n_IdentIndiv, 1, 5)) %>%
  mutate(n_NOI = str_sub(n_IdentIndiv, 6, 7)) 

config <- enfantsD %>%
  pivot_wider(id_cols = n_IdentMenage, 
              names_from = n_NOI, 
              names_prefix = "EnfantNOI_", 
              values_from = n_configFamEnfantsPSexe) %>%
  mutate(across(.cols = starts_with("EnfantNOI_"), 
                .fns = function(x){
                  x = labelled(x, label = "Configuration familial de l'enfant de NOI")
                })) 

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

configSynthese$n_configFam <- NA
configSynthese$n_enfantNewUnion <- NA

# Pour tous les ménages pour lesquels on a une unique configuration renseignée, 
# on garde cette valeur
configSynthese[
  configSynthese$n_config == 1, "n_configFam"
] <- configSynthese[
  configSynthese$n_config == 1, "names_config"
]

# Pour ceux qui sont recomposés, on ajoute qu'il n'y a pas d'enfant de la nouvelle union 
configSynthese[
  configSynthese$`Configuration recomposée maternelle` | configSynthese$`Configuration recomposée paternelle` ,
  "n_enfantNewUnion"
] <- F


# Pour les cas ou on a plusieurs configurations 
configSynthese[
  configSynthese$n_config == 2 
  & configSynthese$`Configuration recomposée maternelle` 
  & configSynthese$`Configuration recomposée paternelle`, 
  c("n_configFam", "n_enfantNewUnion")
] <- list("Configuration recomposée maternelle et paternelle", F)

configSynthese[
  configSynthese$n_config == 2 
  & configSynthese$`Configuration recomposée maternelle` 
  & configSynthese$`Configuration traditionelle`,
  c("n_configFam", "n_enfantNewUnion")
] <- list("Configuration recomposée maternelle", T)

configSynthese[
  configSynthese$n_config == 2 
  & configSynthese$`Configuration recomposée paternelle` 
  & configSynthese$`Configuration traditionelle`,
  c("n_configFam", "n_enfantNewUnion")
] <- list("Configuration recomposée paternelle", T)

configSynthese[
  configSynthese$n_config == 3 
  & configSynthese$`Configuration recomposée paternelle` 
  & configSynthese$`Configuration traditionelle`
  & configSynthese$`Configuration recomposée maternelle`,
  c("n_configFam", "n_enfantNewUnion")
] <- list("Configuration recomposée maternelle et paternelle", T)

tab <- configSynthese[
  is.na(configSynthese$n_configFam), ]




configMatrice <- config
configMatrice <- as.matrix(configMatrice)

  filter(is.na(EnfantNOI_03))
  filter(EnfantNOI_03 != EnfantNOI_04
         |EnfantNOI_03 != EnfantNOI_04
           )


familles <- menages %>%
  right_join(config, by = c("IDENT_MEN" = "n_IdentMenage"))






