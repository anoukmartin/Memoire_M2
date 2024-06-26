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

# Calcul des parts de revenus # ===========================
# Calcul des parts de chaque type de revenu par rapport au revenu disponible
revenus_comp <- lapply(menages[, -1], function(x){
  revdisp <- menages$REVDISP %>% as.vector()
  part <- (x / revdisp) * 100
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
              select(n_TYPMEN_new, PONDMEN, IDENT_MEN), 
            by = "IDENT_MEN")

# Transformation en format long # =============================
# Transformation des données en format long pour faciliter le calcul des moyennes et la création du graphique
revenus_comp2 <- revenus_comp %>%
  pivot_longer(cols = names(menages)[-1])

# Un graphique avec tous les types de revenus ###################################
## Calcul des moyennes pondérées # =============================
# Calcul des moyennes pondérées par type de ménage et type de revenu
revenus_comp2 <- revenus_comp2 %>%
  group_by(n_TYPMEN_new, name) %>%
  summarise(value_mean = weighted.mean(x = value, w = PONDMEN, na.rm = TRUE))


# Filtrage des types de revenus # =============================
## Création de la liste des types de revenus d'intérêt
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
gg <- ggplot(aes(x = n_TYPMEN_new, y = value_mean, fill = name), data = revenus_comp3) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(value_mean, 1)), 
            position = position_stack(.5), 
            size = 3, 
            family = "serif") +
  geom_hline(yintercept = 0) +
  labs(x = "Type de ménage", 
       y = "Fréquence") + 
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
  group_by(n_TYPMEN_new) %>%
  summarise(effet_sociofiscal = sum(value_mean))



# Création du graphique # =====================

ggplot(aes(x = n_TYPMEN_new, y = value_mean, fill = name), data = revenus_comp3) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(value_mean, 1)), 
            position = position_stack(.5), 
            size = 3, 
            family = "serif") +
  geom_hline(yintercept = 0) +
  #geom_point(aes(x = n_TYPMEN_new, y = effet_sociofiscal), data = revenus_comp4) +
  labs(x = "Type de ménage", 
       y = "Fréquence") + 
  # scale_fill_manual(
    # values = alpha(
    #   rev(c("#49006a", "#084081", "#2b8cbe", "#7bccc4", "#ccebc5",
    #         "#feb24c", "#fcbba1", "#fb6a4a", "#de2d26")),
    #   0.5)) +
  #   labels = rev(c("Impôts\ndirects", "Salaires", "Revenus d'activité\nindépendante", 
  #                  "Chômage", "Retraites", "Transferts \nentre ménages", 
  #                  "Prestations \nfamiliales", "Prestations \nlogement", 
  #                  "Minimas \nsociaux")),
  #   name = "Type de revenu :") +
  theme_memoire() + 
  theme(axis.line.x = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1))




##############" TEST #########################################################
# Bibliothèques nécessaires
library(dplyr)
library(ggplot2)
library(forcats)

# Chargement des données
# =====================
familles <- readRDS("Data_output/familles_parents.Rds")
menages <- readRDS("Data_output/menages.Rds")

menages$COEFFUC
# Transformation des données
# ==========================
menages <- menages %>%
  select(IDENT_MEN, 
         COEFFUC,
         REVDISP, # revenu disponible
         IMPOTREV_M, TAXHAB_M, # Impôts
         PREST_PRECARITE_VIEIL, PREST_PRECARITE_HAND, PREST_PRECARITE_RSA, PPA, 
         PREST_LOGEMENT, PREST_FAM_TOT) %>%
  mutate(IMPOTS_DIRECTS = IMPOTREV_M + TAXHAB_M, # Calcul des impôts directs
         MINIMA_SOC = PREST_PRECARITE_VIEIL + PREST_PRECARITE_HAND + PREST_PRECARITE_RSA + PPA, # Calcul des minimas sociaux
         PREST_SOC = PREST_LOGEMENT + PREST_FAM_TOT) # Autres prestations sociales

# Calcul du revenu disponible avant prestations et impôts
# =======================================================
menages <- menages %>%
  mutate(REVDISP_AVANT = REVDISP - IMPOTS_DIRECTS - MINIMA_SOC - PREST_SOC)

# Création du dataframe pour le graphique
# =======================================
familles <- left_join(familles, menages, by = "IDENT_MEN")
effet_composantes <- familles  %>%
  select(IDENT_MEN, n_TYPMEN_new, PONDMEN, REVDISP_AVANT, IMPOTS_DIRECTS, MINIMA_SOC, PREST_SOC, REVDISP, COEFFUC) %>%
  pivot_longer(cols = c(REVDISP_AVANT, IMPOTS_DIRECTS, MINIMA_SOC, PREST_SOC, REVDISP),
               names_to = "Type_Composante",
               values_to = "Montant") %>%
  mutate(Montant = Montant/COEFFUC) %>%
  mutate(Type_Composante = recode(Type_Composante,
                                  "REVDISP_AVANT" = "Revenu Dispo. Avant",
                                  "IMPOTS_DIRECTS" = "Impôts Directs",
                                  "MINIMA_SOC" = "Minimas Sociaux",
                                  "PREST_SOC" = "Prestations Sociales",
                                  "REVDISP" = "Revenu Dispo. Après"))

# Calcul des moyennes pondérées
# =============================
effet_composantes_mean <- effet_composantes %>%
  group_by(n_TYPMEN_new, Type_Composante) %>%
  summarise(value_mean = weighted.mean(x = Montant, w = PONDMEN, na.rm = TRUE))

# Création du graphique
# =====================
ggplot(aes(x = n_TYPMEN_new, y = value_mean, fill = Type_Composante), data = effet_composantes_mean) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(value_mean, 1)), 
            position = position_dodge(0.9), 
            size = 3, 
            family = "serif", 
            vjust = -0.5) +
  labs(x = "Type de ménage", 
       y = "Montant moyen", 
       fill = "Type de composante", 
       title = "Effet des Composantes Sociales et Fiscales sur le Revenu Disponible des Ménages",
       subtitle = "Comparaison avant et après les prestations et impôts par configuration familiale") + 
  scale_fill_manual(values = c("Revenu Dispo. Avant" = "skyblue", 
                               "Impôts Directs" = "red", 
                               "Minimas Sociaux" = "lightgreen", 
                               "Prestations Sociales" = "yellow",
                               "Revenu Dispo. Après" = "steelblue")) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#### TEST 2 ####################################################################
# Bibliothèques nécessaires
library(dplyr)
library(ggplot2)
library(forcats)

# Chargement des données
# =====================
familles <- readRDS("Data_output/familles_parents.Rds")
menages <- readRDS("Data_output/menages.Rds")

# Transformation des données
# ==========================
menages <- menages %>%
  select(IDENT_MEN, 
         REVDISP, # revenu disponible
         IMPOTREV_M, TAXHAB_M, # Impôts
         PREST_PRECARITE_VIEIL, PREST_PRECARITE_HAND, PREST_PRECARITE_RSA, PPA, 
         PREST_LOGEMENT, PREST_FAM_TOT, # prestations %>%
  mutate(IMPOTS_DIRECTS = IMPOTREV_M + TAXHAB_M, # Calcul des impôts directs
         MINIMA_SOC = PREST_PRECARITE_VIEIL + PREST_PRECARITE_HAND + PREST_PRECARITE_RSA + PPA, # Calcul des minimas sociaux
         PREST_SOC = PREST_LOGEMENT + PREST_FAM_TOT) # Autres prestations sociales

# Calcul du niveau de vie avant redistribution
# ============================================
menages <- menages %>%
  mutate(NIV_VIE_AVANT_REDISTRIB = REVDISP + IMPOTS_DIRECTS - MINIMA_SOC - PREST_SOC)

# Création du dataframe pour le graphique
# =======================================
effet_redistribution <- menages %>%
  select(IDENT_MEN, n_TYPMEN_new, PONDMEN, NIV_VIE_AVANT_REDISTRIB, REVDISP, IMPOTS_DIRECTS, MINIMA_SOC, PREST_SOC) %>%
  pivot_longer(cols = c(NIV_VIE_AVANT_REDISTRIB, REVDISP),
               names_to = "Type_Revenu",
               values_to = "Montant") %>%
  mutate(Type_Revenu = recode(Type_Revenu,
                              "NIV_VIE_AVANT_REDISTRIB" = "Niveau de Vie Avant Redistribution",
                              "REVDISP" = "Niveau de Vie Après Redistribution"))

# Calcul des moyennes pondérées
# =============================
effet_redistribution_mean <- effet_redistribution %>%
  group_by(n_TYPMEN_new, Type_Revenu) %>%
  summarise(value_mean = weighted.mean(x = Montant, w = PONDMEN, na.rm = TRUE))

# Création du graphique
# =====================
ggplot(aes(x = n_TYPMEN_new, y = value_mean, fill = Type_Revenu), data = effet_redistribution_mean) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(value_mean, 1)), 
            position = position_dodge(0.9), 
            size = 3, 
            family = "serif", 
            vjust = -0.5) +
  labs(x = "Type de ménage", 
       y = "Montant moyen", 
       fill = "Type de revenu", 
       title = "Effet du Système Sociofiscal sur le Niveau de Vie des Ménages",
       subtitle = "Comparaison avant et après redistribution par configuration familiale") + 
  scale_fill_manual(values = c("Niveau de Vie Avant Redistribution" = "skyblue", 
                               "Niveau de Vie Après Redistribution" = "steelblue")) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
