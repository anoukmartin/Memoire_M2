# Bibliothèques nécessaires
library(dplyr)
library(ggplot2)
library(forcats)
library(GGally)

# Chargement des données # =====================
# Lecture des fichiers RDS
infosBDF <- readRDS("Data_output/infosBDF.Rds")

familles <- readRDS("Data_output/familles_parents.Rds") 
menages <- readRDS("Data_output/menages.Rds")
menages$COEFFUC

# Transformation des données # ==========================
# Sélection des colonnes pertinentes et transformation de certaines variables
menages <- menages %>%
  select(IDENT_MEN, 
         COEFFUC,
         REVACT, # revenu d'activité
         REVINDEP, SALAIRES, REV_ETRANGER, # décomposition
         REVSOC, # revenus sociaux
         CHOMAGE, RETRAITES, PREST_PRECARITE_VIEIL, PREST_PRECARITE_HAND, 
         PREST_FAM_AF, 
         PREST_FAM_ARS, 
         PREST_FAM_AUTRES, PREST_FAM_CF,  
         PREST_PRECARITE_RSA, PPA, PREST_LOGEMENT, PREST_FAM_TOT, # décomposition
         REVPAT, # revenu du patrimoine
         REV700, REV701, # transferts entre ménages
         REV999, # Autres ressources 
         IMPOTREV_M, TAXHAB_M, # Impôts et taxes 
         REVDISP, REVTOT) %>%
  mutate(IMPOTREV_M = -IMPOTREV_M, # Changement du signe des impôts sur le revenu
         TAXHAB_M  = -TAXHAB_M,    # Changement du signe des taxes d'habitation
         IMPOTS_DIRECTS = IMPOTREV_M + TAXHAB_M, # Calcul des impôts directs
         TRANSFERTS_INTERMENAGES = REV700 + REV701, # Calcul des transferts entre ménages
         MINIMA_SOC = PREST_PRECARITE_VIEIL + PREST_PRECARITE_HAND + PREST_PRECARITE_RSA + PPA) # Calcul des minimas sociaux

# Affichage des noms des colonnes du dataframe menages
names(menages)

menages <- menages %>%
  left_join(familles %>% 
              select(IDENT_MEN, n_REVENUS_F, n_REVENUS_H))

# Calcul des parts de revenus # ===========================
# Calcul des parts de chaque type de revenu par rapport au revenu disponible
revenus_comp <- lapply(menages[, -1], function(x){
  revdisp <- menages$REVDISP %>% as.vector()
  coeffuc <- menages$COEFFUC
  part <- (x / revdisp)*100
  return(part)
})



# Ajout de la colonne IDENT_MEN aux parts de revenus
revenus_comp$IDENT_MEN <- menages$IDENT_MEN
revenus_comp <- revenus_comp %>%
  as.data.frame()

# Filtrage et jointure # ====================
# Filtrage des ménages présents dans le dataframe familles et jointure des données
revenus_comp <- revenus_comp %>%
  filter(IDENT_MEN %in% familles$IDENT_MEN) %>%
  left_join(familles %>%
              select(n_TYPMEN_new, n_TYPMEN_sexe, PONDMEN, IDENT_MEN), 
            by = "IDENT_MEN")



# Un graphique avec tous les types de revenus ###################################
## Calcul des moyennes pondérées # =============================
# Calcul des moyennes pondérées par type de ménage et type de revenu
revenus_comp2 <- revenus_comp %>%
  pivot_longer(cols = names(menages)[-1]) %>%
  group_by(n_TYPMEN_sexe, name) %>%
  summarise(value_mean = weighted.mean(x = value, w = PONDMEN, na.rm = TRUE))


# Création de la liste des types de revenus d'intérêt
liste <- c("IMPOTS_DIRECTS",
           "SALAIRES", "REVINDEP", "CHOMAGE", "RETRAITES", 
           "TRANSFERTS_INTERMENAGES",
           "PREST_FAM_TOT", "PREST_LOGEMENT", "MINIMA_SOC")
# Filtrage des types de revenus et réordonnancement des facteurs
revenus_comp3 <- revenus_comp2 %>%
  filter(name %in% liste) %>%
  mutate(name = name %>% 
           fct_relevel(liste) %>% 
           fct_rev())

# Création du graphique # =====================
gg <- ggplot(aes(x = n_TYPMEN_sexe, y = value_mean, fill = name), data = revenus_comp3) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(value_mean, 1)), 
            position = position_stack(.5), 
            size = 3, 
            family = "serif") +
  geom_hline(yintercept = 0) +
  labs(x = "Configuration familiale du ménage", 
       y = "Part dans le revenu disponible du ménage") + 
  scale_fill_manual(
    values = alpha(
      rev(c("#49006a", "#084081", "#2b8cbe", "#7bccc4", "#ccebc5", 
            "#feb24c", "#fcbba1", "#fb6a4a", "#de2d26")), 
      0.5), 
    labels = rev(c("Impôts\ndirects", "Salaires", "Revenus d'activité\nindépendante", 
                   "Chômage", "Retraites", "Transferts \nentre ménages", 
                   "Prestations \nfamiliales", "Prestations \nlogement", 
                   "Minimas \nsociaux")),
    name = "Type de revenu :") +
  theme_memoire() + 
  theme(axis.line.x = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1))

gg

## Enregistrement du graphique 
saveTableau(tableau = gg,
            type = "plot", 
            label = "compRevDisp", 
            description = "Composition du revenu disponible des ménages en fonction de la configuration familiale", 
            ponderation = TRUE, 
            champ = infosBDF$champ, 
            n = nrow(familles))


## Version montant brut 

revenus_comp <- lapply(menages[, -1], function(x){
  revdisp <- menages$REVDISP %>% as.vector()
  coeffuc <- menages$COEFFUC
  part <- (x / coeffuc)/12
  return(part)
})


# Ajout de la colonne IDENT_MEN aux parts de revenus
revenus_comp$IDENT_MEN <- menages$IDENT_MEN
revenus_comp <- revenus_comp %>%
  as.data.frame()

revenus_comp2 <- revenus_comp %>%
  left_join(familles %>%
              select(n_TYPMEN_new, n_TYPMEN_sexe, PONDMEN, IDENT_MEN), 
            by = "IDENT_MEN") %>%
  pivot_longer(cols = names(menages)[-1]) %>%
  group_by(n_TYPMEN_sexe, name) %>%
  summarise(value_mean = weighted.mean(x = value, w = PONDMEN, na.rm = TRUE))



#Filtrage des types de revenus et réordonnancement des facteurs
revenus_comp3 <- revenus_comp2 %>%
  filter(name %in% liste) %>%
  mutate(name = name %>% 
           fct_relevel(liste) %>% 
           fct_rev())
revenus_comp4 <- revenus_comp3 %>%
  group_by(n_TYPMEN_sexe) %>%
  summarise(effet_sociofiscal = sum(value_mean))

# Création du graphique # =====================
gg <- ggplot(aes(x = n_TYPMEN_sexe, y = value_mean, fill = name), data = revenus_comp3) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(value_mean, 1)), 
            position = position_stack(.5), 
            size = 3, 
            family = "serif") +
  geom_hline(yintercept = 0) +
  labs(x = "Configuration familiale du ménage", 
       y = "Part dans le revenu disponible du ménage") + 
  scale_fill_manual(
    values = alpha(
      rev(c("#49006a", "#084081", "#2b8cbe", "#7bccc4", "#ccebc5", 
            "#feb24c", "#fcbba1", "#fb6a4a", "#de2d26")), 
      0.5), 
    labels = rev(c("Impôts\ndirects", "Salaires", "Revenus d'activité\nindépendante", 
                   "Chômage", "Retraites", "Transferts \nentre ménages", 
                   "Prestations \nfamiliales", "Prestations \nlogement", 
                   "Minimas \nsociaux")),
    name = "Type de revenu :") +
  theme_memoire() + 
  theme(axis.line.x = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1))

gg

## Enregistrement du graphique 
saveTableau(tableau = gg,
            type = "plot", 
            label = "compRevDisp", 
            description = "Composition du revenu disponible des ménages en fonction de la configuration familiale", 
            ponderation = TRUE, 
            champ = infosBDF$champ, 
            n = nrow(familles))




# Un graphique avec seulement les préstations sociales ########################
## Calcul des moyennes pondérées # =============================
# Calcul des moyennes pondérées par type de ménage et type de revenu

revenus_comp <- lapply(menages[, -1], function(x){
  revdisp <- menages$REVDISP %>% as.vector()
  coeffuc <- menages$COEFFUC
  part <- (x / 12)
  return(part)
})


# Ajout de la colonne IDENT_MEN aux parts de revenus
revenus_comp$IDENT_MEN <- menages$IDENT_MEN
revenus_comp <- revenus_comp %>%
  as.data.frame()

revenus_comp2 <- revenus_comp %>%
  left_join(familles %>%
              select(n_TYPMEN_new, n_TYPMEN_sexe, PONDMEN, IDENT_MEN), 
            by = "IDENT_MEN") %>%
  pivot_longer(cols = names(menages)[-1]) %>%
  group_by(n_TYPMEN_sexe, name) %>%
  summarise(value_mean = weighted.mean(x = value, w = PONDMEN, na.rm = TRUE))

# Filtrage des types de revenus # =============================
## Création de la liste des types de revenus d'intérêt
liste <- c("IMPOTREV_M", "TAXHAB_M", 
           "PREST_PRECARITE_VIEIL", "PREST_PRECARITE_HAND", "PREST_PRECARITE_RSA", "PPA", 
           "PREST_LOGEMENT", 
           "PREST_FAM_AF", "PREST_FAM_ARS", "PREST_FAM_CF", "PREST_FAM_AUTRES") 

# Filtrage des types de revenus et réordonnancement des facteurs
revenus_comp3 <- revenus_comp2 %>%
  filter(name %in% liste) %>%
  mutate(name = name %>% 
           fct_relevel(liste) %>% 
           fct_rev())
revenus_comp4 <- revenus_comp3 %>%
  group_by(n_TYPMEN_sexe) %>%
  summarise(effet_sociofiscal = sum(value_mean))



# Création du graphique # =====================

gg <- ggplot(aes(x = n_TYPMEN_sexe, y = value_mean, fill = name), data = revenus_comp3) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(value_mean, 1)), 
            position = position_stack(.5), 
            size = 3, 
            family = "serif") +
  geom_hline(yintercept = 0) +
  #geom_point(aes(x = n_TYPMEN_new, y = effet_sociofiscal), data = revenus_comp4) +
  labs(x = "Configuration familiale du ménage", 
       y = "Montant mensuel") + 
  scale_fill_manual(
  values = alpha(
    rev(c("#49006a", "#cab2d6",
          "#084081", "#2b8cbe", "#7bccc4", "#ccebc5",
          "#33a02c", 
          "#feb24c", "#fcbba1", "#fb6a4a", "#de2d26")),
    0.5),
    labels = rev(c("Impôts sur le revenu ", "Taxe d'habitation", 
                   "Minimum viellesse", "AAH", "RSA", "Prime d'activité", 
                   "Aides au logement", 
                   "Allocations familiales \nde base", 
                   "Allocations familiales \npour la rentrée scolaire", 
                   "Complément familial",
                   "Autre prestations \nfamiliales")),
      name = "Type de revenu :") +
  theme_memoire() + 
  theme(axis.line.x = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1)) 

gg

## Enregistrement du graphique 
saveTableau(tableau = gg,
            type = "plot", 
            label = "compRevenusTransfert", 
            description = "Par dans le revenu disponible des revenus de transfert (hors revenus de remplacement)", 
            ponderation = TRUE, 
            champ = infosBDF$champ, 
            n = nrow(familles))


