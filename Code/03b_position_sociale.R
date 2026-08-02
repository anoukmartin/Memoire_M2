
library(GDAtools)
library(cluster)

library(kableExtra)

infosBDF <<- readRDS("Data_output/infosBDF.Rds")



#familles <- readRDS("Data_output/data_familles_parents.Rds")
familles <- readRDS("Data_output/data_recode/menages.Rds")

familles$TDM8
familles$AGPR

library(dplyr)
library(ggplot2)

library(dplyr)
library(ggplot2)
library(RColorBrewer)
familles <- familles %>%
  mutate(TDM8 = as.factor(TDM8)) %>%
  mutate(TDM8_SEXE = as.factor(TDM8_SEXE)) %>%
  mutate(MENAGE_RAPPORT_IMMIGR = as.factor(MENAGE_RAPPORT_IMMIGR)) %>%
  mutate(MENAGE_PUBLICPRIVE = as.factor(MENAGE_PUBLICPRIVE)) %>%
  mutate(n_Diffage_HF = AGE_H - AGE_F)



levels(familles$TDM8) <- sapply(levels(familles$TDM8),function(x){  insert_line_breaks(x, 30)}, USE.NAMES = F)


  


age_min <- 21
age_max <- 60



familles %>%
  filter(!is.na(AGEPR), !is.na(TDM8)) %>%
  count(AGEPR , TDM8) %>%
  group_by(AGEPR) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = AGEPR, y = n, fill = TDM8)) +
  geom_area(position = "stack") +
  #scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "Âge de la personne de référence du ménage",
    y = "Répartition des types de ménage",
    fill = "Type de ménage"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  geom_vline(aes(xintercept = age_min)) + 
  geom_vline(aes(xintercept = age_max)) 

familles %>%
  filter(!is.na(AGEPR), !is.na(TDM8)) %>%
  count(AGEPR , TDM8) %>%
  group_by(AGEPR) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = AGEPR, y = prop, fill = TDM8)) +
  geom_area(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "Âge de la personne de référence du ménage",
    y = "Répartition des types de ménage",
    fill = "Type de ménage"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  geom_vline(aes(xintercept = age_min)) + 
  geom_vline(aes(xintercept = age_max)) 


  
familles <- familles %>%
  filter(AGEPR %in% age_min:age_max | AGECJ %in% age_min:age_max)

summary(familles$n_Diffage_HF)
familles <- familles %>%
  mutate(n_Diffage_HF_cut = case_when(
    COUPLE_SEXE == "Couple de sexes différents" & n_Diffage_HF < -2 ~ "Femme plus agée que l'homme (+ de 2 ans d'écart)", 
    COUPLE_SEXE == "Couple de sexes différents" & n_Diffage_HF < 2 ~ "Conjoints du même age (+- 2 ans d'écart)", 
    COUPLE_SEXE == "Couple de sexes différents" & n_Diffage_HF < 7 ~ "Homme plus agé que la femme (- de 7 ans d'écart)", 
    COUPLE_SEXE == "Couple de sexes différents" & n_Diffage_HF >= 7  ~ "Homme plus agé que la femme (+ de 7 ans d'écart)")
  ) %>%
  mutate(n_Diffage_HF_cut = as.factor(n_Diffage_HF_cut))
freq(familles$n_Diffage_HF_cut)         
familles$COUPLE_SEXE       
         


# library(factoextra)
# library(tidyverse)
# library(data.table)
# library("janitor")
# library(ggrepel)



# Sélection des variables de l'analyse ----

# On garde tous les individus mais on ne sélectionne ici que les variables actives

names(familles)


summary(familles$n_REVENUS_indiv_H)
summary(familles$n_REVENUS_indiv_F)

dep_men <- readRDS("Data_output/DepMenages.Rds")
 
summary(familles$n_RevenusContribF)
freq(familles$n_RevenusContribF)
summary(familles$NIVIE)

familles <- familles %>%
  rec_PROP("n_RevenusContribF", NewVar = "n_RevenusContribF_cut") %>%
  left_join(dep_men %>%
              select(IDENT_MEN, STALOG)) %>%
  rec_TYPMEN5() %>%
  rec_TYPMEN(Var = "TYPMEN15", "TYPMEN") %>%
  rec_REVENUS(Var = "NIVIE", NewVar = "NIVIEcut") %>%
  rec_REVENUS(Var = "REVSOC", "REVSOCcut") %>%
  rec_NENFANTS(Var = "NENFANTS")  %>%
  rec_TAU() %>%
  rec_STALOG() %>%
  rec_TYPLOG() %>%
  rec_PATRIB() %>%
  rec_TYPVOIS()

# Variables numériques
summary(familles$n_RevenusContribF)
summary(familles$NIVIE)
summary(familles$REVSOC)

# Variables recodées
freq(familles$TYPVOIS)
freq(familles$n_RevenusContribF_cut)
freq(familles$STALOG)
freq(familles$TYPMEN5)
freq(familles$TYPMEN)
freq(familles$NIVIEcut)
freq(familles$REVSOCcut)
freq(familles$NENFANTS)
freq(familles$TAU)
freq(familles$TYPLOG)
freq(familles$PATRIB)
freq(familles$MENAGE_RAPPORT_IMMIGR)
freq(familles$MENAGE_PUBLICPRIVE)
freq(familles$n_Diffage_HF_cut)

d_acm <- familles %>% 
  #rename(DNIVIE = "DNIVIE2") %>%
  select( 
    #starts_with("n_REVENUS_indiv"),
    #starts_with("n_RevenusContribF"),
    #starts_with("n_PATRIMOINEcut"),
    starts_with("CS12_"), 
    starts_with("DIP7_"), 
    starts_with("TYPEMPLOI_"),
    starts_with("SITUA_"),
    MENAGE_RAPPORT_IMMIGR,
    MENAGE_PUBLICPRIVE, 
    COUPLE_SEXE, 
    n_Diffage_HF_cut,
    TYPVOIS, 
    #starts_with("IMMIGR"),
    #NENFANTS, TYPMEN,
    NIVIEcut, 
    #NIVIEcut,
    TAU, TYPLOG, STALOG, PATRIB)  %>%
  select(-ends_with("2")) %>%
  mutate_all(factor)
names(d_acm)


lapply(d_acm, freq)

# Vaiables supplémentaires 

d_acm_sup <- familles %>%
  select(starts_with("NAIS7_"), 
         starts_with("NATIO7_"), 
         starts_with("AG6_"),
         n_RevenusContribF_cut,
         NENFANTS, TYPMEN, TDM8, TDM8_SEXE, TAF) %>%
  mutate(TDM8 = as.factor(TDM8), 
         TDM8_SEXE = as.factor(TDM8_SEXE))

lapply(d_acm_sup, class)

# On met la variable de poids à l'echelle 
summary(familles$PONDMEN)
d_acm$poids <- familles$PONDMEN/mean(familles$PONDMEN)

poidsACMspe <- d_acm$poids

d_acm <- d_acm %>% select(-poids)

# vars_sup <- pacsACM %>%
#   mutate(nbNA = rowSums(is.na(pacsACM %>% 
#                                 select("pacs_Q45_1":"pacs_Q30")))) %>%
#   filter(nbNA <= 6) %>%
#   select(all_of(var_ill)) %>%
#   mutate_all(factor)
# 
# names(vars_sup) <- c("Sexe", "Age", "Nationalité", "Statut conjugal", "Activité", 
#                      "Niveau de diplome", "Type d'entreprise", "Nature du contrat",
#                      "Temps de travail", "Nombre d'enfants", "Logement", 
#                      "Revenu", "CSP", 
#                      "Type de ménage")


# Sélection des modalités à mettre en supplémentaire ----

# on remplace les NA par une modalité "vide"
d_acm <- d_acm %>% 
  mutate_if(is.factor,
            fct_na_value_to_level,
            level = "NA") %>% 
  droplevels()


liste_moda <- getindexcat(as.data.frame(d_acm))
liste_moda

index_modasup <- which(str_ends(liste_moda, ".NA")
                       | str_ends(liste_moda, ".Retraité-e") 
                       | str_ends(liste_moda, ".Retraité-e ou retiré-e des affaires ou en préretraite")
                       | str_ends(liste_moda, ".Etudiant-e, élève, en formation ou en stage non rémunéré") 
                       | str_ends(liste_moda, ".Autre inactif-ve"))
# numéros d'index des modalités "vide"
index_modasup
liste_moda[index_modasup]
freq(familles$SITUA_F)

# Réalisation de l'ACM spé ----

acm_spe <- speMCA(as.data.frame(d_acm), 
                  excl = index_modasup,
                  ncp = 43,
                  row.w = poidsACMspe)


# plot.speMCA(acm_spe, type="v", axes=c(1,2), cex = 0.1)
# plot.speMCA(acm_spe, type="v", axes=c(3,4))
#install.packages("explor")
#library(explor)
#explor(acm_spe)

# données sur les variables supplémentaires
acm_sup <- supvars(acm_spe, d_acm_sup)
#explor(acm_spe)


barplot(acm_spe$eig$rate[1:20])

plot(acm_spe$eig$rate[1:20] %>% diff() %>% diff(), type = "b")
abline(h = 0)
acm_spe$eig$rate[1:20] %>% diff() %>% diff()

acmstop <- 7
sum(acm_spe$eig$rate[1:acmstop])

# Variables illustratives pour l'ACM 

# Pour avoir les données pour les variables suplémentaires
#acm_spe_varssup <- supvars(acm_spe, vars_sup)
library(explor)
#explor::explor(acm_spe)
coord <- acm_sup$coord

# CAH ----
d_cah <- acm_spe$ind$coord[, 1:acmstop]

md <- daisy(d_cah, metric = "euclidean") # matrice de distances

arbre <- hclust(md, method = "ward.D2") # agrégation critère de ward

dend <- as.dendrogram(arbre)

plot(dend)

# plot(dend, main = "Clusters",
#      horiz = TRUE, leaflab = "none",
#      xlim = c(78, 30))
# text(x = 20, y = 0, "blabla")
# #text(x, y, labels)


inertie <- sort(arbre$height, decreasing = TRUE)
plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie") %>%
  grid

plot(inertie[1:20] %>% 
  diff() %>%
  diff(), type = "b")
abline(h = 0)
inertie[1:20] %>% 
  diff() %>%
  diff()

# sauts d'inertie à 7
typo <- cutree(arbre, 6)

typo %>% freq


# 
# On intègre le résultat dans les données
typo <- typo %>% as_factor() %>%
  fct_recode(
    "Classes superieures à dominante économique/privé [C6]" = "6", #OK
    "'Petits-moyens' [C3]" = "3", #OK
    "Classes populaires précaires [C5]" = "5", #OK
    "Classes populaires immigrées urbaines [C2]" = "2", #OK
    "Classes moyennes-superieures du public [C4]" = "4",
    "Petits indépendants et salariés subalternes du rural  [C1]" = "1") %>%
  fct_relevel(
    "Classes populaires précaires [C5]",  #OK
    "Classes populaires immigrées urbaines [C2]",  #OK
    "Petits indépendants et salariés subalternes du rural  [C1]",
    "'Petits-moyens' [C3]",
    "Classes moyennes-superieures du public [C4]",
    "Classes superieures à dominante économique/privé [C6]")
    
    
   


freq(typo)


# # test 
# 
# # Calculer le nombre d'individus par cluster
# cluster_sizes <- table(typo)
# 
# # Calculer les pourcentages par cluster
# cluster_percentages <- round(100 * cluster_sizes / sum(cluster_sizes), 2)
# 
# cut_height <- max(arbre$height[which(diff(cutree(arbre, k = 8)) != 0)])
# 
# # Dessiner le dendrogramme avec une coupe à 7 clusters
# plot(arbre, labels = FALSE, hang = -1, main = "Dendrogramme coupé à 7 clusters")
#      #ylim = c(45, max(arbre$height)))
# rect.hclust(arbre, k = 7, cluster = typo)
# i <- 6
# arbre$merge
# for(i in 1:7) {
#   cluster_pos <- which(typo == levels(typo)[i])
#   dat <- arbre$merge %>% as.data.frame() 
#   mean_x <- mean(dat[cluster_pos, 1], na.rm = T)
#   text(mean_x, 25, 
#        labels = paste0(cluster_sizes[i], " (", cluster_percentages[i], "%)"), 
#        col = "blue", cex = 0.5)
# }
# 
# for(i in 1:7) {
#   cluster_pos <- which(clusters == i)
#   
#   # Position moyenne des branches dans ce cluster
#   mean_x <- mean(as.numeric(names(clusters)[cluster_pos]))
#   
#   # Ajout du texte avec le nombre et le pourcentage
#   text(mean_x, 45, 
#        labels = paste0(cluster_sizes[i], " (", cluster_percentages[i], "%)"), 
#        col = "blue", pos = 3, cex = 0.8)
# }
# 
# # Dessiner le dendrogramme avec une coupe à 7 clusters
# plot(dend, labels = FALSE, hang = -1, main = "Dendrogramme avec coupe en 7 clusters")
# rect.hclust(arbre, k = 7, border = 2:8)
# 



# Taleau stats des dans les clusters ----
d_acm$typo <- typo

d_acm2 <- d_acm %>%
  #bind_cols(d_acm_sup) %>%
  mutate_if(is.factor,
            fct_na_level_to_value, 
            extra_levels = "NA")

nrow(d_acm2)
tab <- joli_tableau(data = d_acm2, by = "typo", vars_quali = names(d_acm2), weigths = poidsACMspe, 
                    tableau_titre = "Structure des clusters (variables actives)", source = paste0(infosBDF$nom, ", ", infosBDF$vague),
                   champ = "ménages ordinaires formés par des adultes (25-65 ans). (n = 12 355)", 
                   lecture = "blabla")

tab <- tab %>%
  column_spec(1, "2in") %>%
  column_spec(2, "0.7in") %>%
  column_spec(3, "0.7in") %>%
  column_spec(4, "0.7in") %>%
  column_spec(5, "0.7in") %>%
  column_spec(6, "0.7in") %>%
  column_spec(7, "0.7in") %>%
  column_spec(8, "0.7in")  

saveTableau(tab, type = "tab", label = "clusters_composition_act", 
            description = "composition sociale des clusters",
            ponderation = T, 
            n = nrow(familles),
            champ = paste0("Menages formés par individus agés de ", age_min, " à ", age_max, " ans."))

### tableau des variables supplémentaires 

d_acm3 <- d_acm_sup %>%
  mutate_if(is.factor,
            fct_na_level_to_value, 
            extra_levels = "NA")
d_acm3$typo <- typo

nrow(d_acm3)
tab <- joli_tableau(data = d_acm3, by = "typo", vars_quali = names(d_acm3), weigths = poidsACMspe, 
                    tableau_titre = "Structure des clusters (variables supplémentaires)", source = paste0(infosBDF$nom, ", ", infosBDF$vague),
                    champ = "ménages ordinaires formés par des adultes (25-65 ans). (n = 12 355)", 
                    lecture = "blabla")
tab <- tab %>%
  column_spec(1, "2in") %>%
  column_spec(2, "0.7in") %>%
  column_spec(3, "0.7in") %>%
  column_spec(4, "0.7in") %>%
  column_spec(5, "0.7in") %>%
  column_spec(6, "0.7in") %>%
  column_spec(7, "0.7in") %>%
  column_spec(8, "0.7in") 

saveTableau(tab, type = "tab", label = "clusters_composition_sup", 
            description = "composition sociale des clusters",
            ponderation = T, 
            n = nrow(familles),
            champ = paste0("Menages formés par individus agés de ", age_min, " à ", age_max, " ans."))



# coordonées des cluster sur les axes ----

d_cah <- as.data.frame(d_cah)
d_cah$typo <- typo
d_cah$pond <- poidsACMspe
summary(poidsACMspe)

tab <- d_cah %>%
  as_survey_design(weights = pond) %>%
  tbl_svysummary(by = typo, 
                 statistic = all_continuous() ~ "{mean}") %>%
  add_p()
tab
# les pvalues sont significatives 

tab2 <- tab$table_body  %>%
  filter(row_type == "level")%>%
  filter(variable != "pond") %>%
  select(variable, starts_with("stat_")) 
tab2
tab2 <- t(tab2) %>%
  as.data.frame() 
names(tab2) <- tab2[1, ]
tab2 <- tab2[-1, ]
tab2 <- tab2 %>%
  mutate_all(.funs = function(x){str_replace_all(x, ",", ".")}) %>%
  mutate_all(as.numeric) %>%
  rownames_to_column(var = "cluster") %>%
  mutate(cluster = str_replace(cluster, "stat_", "C")) 

tab3 <- wtd.table(d_cah$typo, weights = d_cah$pond) %>%
  as.data.frame() %>%
  bind_cols(as.data.frame(freq(d_cah$typo))[, 1]) %>%
  rename(n = `...3`)

tab3$Prop <- round((tab3$Freq/sum(tab3$Freq))*100,1)
tab3 <- tab3 %>%
  separate(Var1, into = c("name", "cluster"), sep = "\\[") %>%
  mutate(cluster = str_remove_all(cluster, "]"))

tab2 <-tab2 %>%
  pivot_longer(cols = starts_with("dim")) %>%
  mutate(name = str_replace(name, "dim.", "Dim "))

tab2$cluster
tab2_summary <- tab2 %>%
  group_by(name) %>%
  summarise(m = min(value), 
            M = max(value))
tab2_summary$eig <- round(acm_spe$eig$rate[1:acmstop], 2) 
tab2_summary <- tab2_summary %>%
  mutate(dimLabel = paste0(name, " (", eig, "%)"))
tab2 <- left_join(tab2, tab2_summary[, c(1,5)])
tab2_summary

tab2_summary$Mlabel <- c("Capitaux\nélevés",
                         "Urbain",
                         "Hauts\npatrimoines",
                         "Couples\nbiactivité",
                         "Faible\ncapital cult",
                         "Privé",
                         "Indépendant-e-s")

tab2_summary$mlabel <- c("Capitaux\nfaibles",
                         "Rural",
                         "Faibles\npatrimoines",
                         "Femmes\ncélibataires",
                         "Fort\ncapital cult", 
                         "Public",
                         "Salarié-e-s")

gg <- ggplot(tab2) +
  theme_void() +
  aes(x = value, y = 1) +
  geom_hline(yintercept = 1, col = "grey") +
  annotate("point", x = 0, y = 1, shape = 3, col = "grey", size = 3) +
  annotate("text", x = 0, y = 1, label = "0", vjust = 2, size = 3, col = "grey") +
  geom_point() +
  geom_text_repel(aes(label = cluster),
                  direction="x", vjust = -2,
                  max.overlaps=10000) +
  facet_wrap(dimLabel ~., ncol = 1, scales="free") #+
  # geom_label(data = tab2_summary, aes(x = m-(0.15*(M-m)), y = 1, label = mlabel), size = 3) +
  # geom_label(data = tab2_summary, aes(x = M+(0.15*(M-m)), y = 1, label = Mlabel), size = 3)
gg

saveTableau(gg, type = "plot", label = "culsters_position",
            description = "positions des cluster sur les différentes dimentions de l'ACM",
            ponderation = T,
            n = "?",
            champ = "adultes (25-70 ans) vivant en ménage ordinaire")

 
# Description statistique des axes ----

variances <- as.data.frame(acm_spe$eig$rate[1:15]) %>% 
  add_column(Axes = factor(paste("Axe", 1:15))) %>%
  rename(`% de variance` = `acm_spe$eig$rate[1:15]`) %>%
  mutate(Axes = fct_relevel(Axes, paste("Axe", 1:15)))

# Indic stat modalités actives ----

## Seuil de contribution ----

seuil <- mean(acm_spe$var$contrib)
#seuil <- 1.5 # j'ai mis un seuil arbitraire plus élevé sinon c'est le bazar

## Frequences ----

frequences <- as.data.frame(unlist(map(d_acm, table))) %>% 
  mutate(labels_longs = rownames(.)) %>% 
  rename(n = "unlist(map(d_acm, table))") %>% 
  separate(labels_longs, c("variables", "modalites"), sep = "\\.") %>%
  group_by(variables) %>% 
  mutate(pourcentage = round(100 * n / nrow(d_acm), 1)) %>%
  ungroup() %>% 
  select(variables, modalites, n, pourcentage)  # sélectionner les variables dans un ordre plus lisible

#on enlève les lignes qui correspondent aux modalités vides
frequences <- frequences[-index_modasup, ]

## Coordonnées ----

coordonnees <- as.data.frame(round(acm_spe$var$coord, 2)) %>%
  rename_all(tolower) %>% 
  rename_all(~str_replace(., " ", "")) %>% 
  rename_all(~str_c(., "coord", sep = "_")) %>% 
  rownames_to_column(var = "modalites") %>%
  separate(modalites, c("variables", "modalites"), sep = "\\.") 

## Contributions ----

contributions <- as.data.frame(round(acm_spe$var$contrib, 2))  %>% 
  rename_all(tolower) %>% 
  rename_all(~str_replace(., " ", "")) %>% 
  rename_all(~str_c(., "contrib", sep = "_")) %>% # idem sauf qu'ici on obtient "dim1_contrib"
  rownames_to_column(var = "modalites") %>%
  separate(modalites, c("variables", "modalites"), sep = "\\.") 

## Cos2 ----

cos2 <- as.data.frame(round(acm_spe$var$cos2, 2)) %>% 
  rename_all(tolower) %>%
  rename_all(~str_replace(., " ", "")) %>% 
  rename_all(~str_c(., "cos2", sep = "_")) %>% # idem avec "cos2" 
  rownames_to_column(var = "modalites") %>%
  separate(modalites, c("variables", "modalites"), sep = "\\.") 

## vtest ----
vtest <- as.data.frame(round(acm_spe$var$v.test, 2)) %>% 
  rename_all(tolower) %>%
  rename_all(~str_replace(., " ", "")) %>% 
  rename_all(~str_c(., "vtest", sep = "_")) %>% # idem avec vtest
  rownames_to_column(var = "modalites") %>%
  separate(modalites, c("variables", "modalites"), sep = "\\.") 

## Assemblage ----
resultats_actives <- frequences %>% 
  right_join(coordonnees, by = c("variables", "modalites")) %>% 
  right_join(contributions,  by = c("variables", "modalites")) %>% 
  right_join(cos2, by = c("variables", "modalites")) %>%  
  right_join(vtest, by = c("variables", "modalites")) %>% 
  mutate(type = "Variable active") %>% # ajout d'une colonne contenant la chaîne de caractères "Variable active" (pour pouvoir distinguer plus tard avec les variables supplémentaires)
  select(type, variables, modalites, n, pourcentage,
         contains(paste0("dim.", 1:8, "_")))





# Tableau des contrib #########################################################

sum(variances$`% de variance`[1:acmstop])

tabcontrib <- lapply(1:acmstop, function(dim){
  names(resultats_actives)
  # On selectionne les bonnes lignes et colones 
  tab <- resultats_actives %>%
    select(c("variables", "modalites", "n", "pourcentage", 
             paste0("dim.", dim, "_contrib"), 
             paste0("dim.", dim, "_coord"))) %>%
    rename(contib = paste0("dim.", dim, "_contrib"), 
           coord = paste0("dim.", dim, "_coord")) %>%
    mutate(variables = str_remove(variables, "n_")) %>%
    separate(col = "variables", 
             into = c("Variable", "Echelle"), 
             sep = "_") %>%
    mutate(Echelle = case_when(
      is.na(Echelle) ~ "Menage", 
      Echelle == "H" ~ "Homme", 
      Echelle == "F" ~ "Femme"))
  
  # Modalités contribuant le plus 
  m <- mean(tab$contib)
  tab <- tab %>%
    filter(contib > m) %>%
    arrange(desc(coord))
  
  
  # Mise en forme 
  tableau_titre <- paste0(
    "Axe ", dim, " (", round(variances$`% de variance`[dim], 2),
    "\\%) : Modalités contribuant plus que la moyenne ",
    " (n=", nrow(tab), ")")
  tab <- tab %>%
    kbl(digits = 1, booktabs = T, longtable = TRUE,
        caption = tableau_titre, 
        format = "latex",
        col.names = c("Variable", "Echelle", "Modalite", "n", "%", "contrib", "coord")) %>%
    kable_styling(
      #full_width = T,
      font_size = 7,
      latex_options = c("hold_position", "scale_down", "repeat_header")) %>%
    # add_header_above(c(" " = 3, 
    #                    "Coordonées" = 8, 
    #                    "Contribution" = 8), bold = TRUE) %>%
    footnote(general = paste0(c("Source :", infosBDF$nom, ", ", infosBDF$vague), collapse = ""), 
             escape = F)
  
  return(tab)
  }
)

tabcontrib[[2]]

saveTableau(tabcontrib, type = "tabs", 
            label = "contribmoda", 
            description = "contributions aux 7 axes de l'ACM", 
            champ = "variables actives", 
            n = "ca dépend", ponderation = T)



# on ajoute la typo sur les données familles 

familles$n_FractionClasse <- typo %>% as.factor()
freq(familles$n_FractionClasse)



saveRDS(familles, "Data_output/data_recode/menages_ageminmax.Rds")

saveRDS(familles %>% 
          select(IDENT_MEN, n_FractionClasse), 
        "Data_output/data_recode/IDENT_MEN_FractionClasse.Rds")

indiv_in_menagesAge <- readRDS("Data_output/data_recode/indiv.Rds") %>%
  filter(IDENT_MEN %in% familles$IDENT_MEN) %>%
  left_join(familles %>%
              select(IDENT_MEN, n_FractionClasse, NIVIE))

saveRDS(indiv_in_menagesAge, 
        "Data_output/data_recode/indiv_in_menagesAge.Rds")

# du ménage 
rm(acm_spe, acm_sup, arbre, contributions, coordonnees, cos2, d_acm, d_acm_sup, d_acm2, d_cah, dend, familles, frequences, gg, infosBDF, resultats_actives, tab, tab2, tab2_summary, tabcontrib, variances, vtest, index_modasup, inertie, liste_moda, md, poidsACMspe, seuil, typo)

