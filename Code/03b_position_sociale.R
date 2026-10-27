
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
  count(AGEPR , TDM8, wt = PONDMEN) %>%
  group_by(AGEPR) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = AGEPR, y = prop, fill = TDM8)) +
  geom_area(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_grey() +
  labs(
    x = "Âge de la personne de référence du ménage",
    y = "Répartition des types de ménage",
    fill = "Type de ménage"
  ) +
  theme_tufte() +
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
dir.create("Resultats")
library(officer)
doc <- read_docx()

tab <- familles %>%
  mutate(Effectifs = T) %>%
  as_survey_design(weights = PONDMEN) %>%
  tbl_svysummary(
    include = c("TDM8", "TDM8_SEXE",  "ANNEE_BDF", "Effectifs"), 
    statistic = list(all_categorical() ~ "{p}", 
                     Effectifs ~ "{n_unweighted}"),
    by = "ANNEE_BDF", 
    percent = "column") %>%
  add_overall(last = T) %>%
  as_flex_table() |>
  font(fontname = "Garamond", part = "all") |>
  fontsize(size = 10, part = "all") |>
  autofit()
tab

doc <- doc %>%
  body_add_break() |> 
  body_add_flextable(
    tab |> 
      border_remove() |>
      hline_top(border = fp_border(width = 1)) |>
      hline_bottom(border = fp_border(width = 1)) |>
      autofit()
  )
print(doc, target = "Resultats/resultats_menages_age.docx")



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


ggplot(
  data.frame(
    axe = factor(1:15),
    inertie = acm_spe$eig$rate[1:15],
    keep = c(rep(T, 7), rep(F, 15-7))
  ),
  aes(x = axe, y = inertie, fill = keep),
  
) +
  geom_col() +
  #geom_qq_line()+
  geom_text(
    aes(label = scales::label_number(
      decimal.mark = ",",
      accuracy = 0.1
    )(inertie)),
    vjust = -0.3
  ) + 
  labs(
    x = "Dimension de l'ACM",
    y = "Inertie (en %)"
  ) +
  scale_fill_grey(start = 0.8, end = 0.2) +
  theme_tufte() +
  theme(legend.position = "no")

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
arbre
dend <- as.dendrogram(arbre)

plot(dend)


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

library(ggplot2)
library(guideR)
library(scales)

keep <- c(rep(T, 6), rep(F, 15-6))
p<- arbre |>
  guideR::plot_inertia_from_tree() &
  ggthemes::theme_tufte() &
  theme(panel.grid = element_line(linewidth = 0.3, colour = "grey90"))
p
class(p)
summary(p)

p[[1]] <- p[[1]] + 
  aes(color = c(keep[2:15], F), group = 2) +
  labs(y = "Inertie", 
       x = "") +
  scale_color_grey(start = 0.8, end = 0.4) +
  theme(legend.position = "none")
p[[1]]

class(p[[2]])
str(p[[2]])
summary(p[[2]])
p[[2]]$layers[[1]]$aes_params$fill <- NULL
p[[2]]$layers[[1]]$mapping

p[[2]] <- p[[2]] + 
  aes(fill = keep) +
  labs(y = "Perte d'inertie\nabsolue", 
       x = "") +
  scale_fill_grey(start = 0.8, end = 0.4) +
  theme(legend.position = "none")
p[[2]]
summary(p[[2]])  

summary(p[[3]])
p[[3]]$layers[[1]]$aes_params$colour <- NULL
p[[3]]$layers[[2]]$aes_params$colour <- NULL
p[[3]]$layers[[1]]$mapping
p[[3]] <- p[[3]] +
  aes(color = c(keep[2:15], F), group = 1) +
  labs(y = "Perte d'inertie\nrelative", 
       x = "Nombre de clusters") +
  scale_color_grey(start = 0.8, end = 0.4) +
  theme(legend.position = "none")
p[[3]]


p

inertie <- guideR::get_inertia_from_tree(arbre)[1:15, ]
inertie$keep = c(rep(TRUE, 6), rep(FALSE, 9))
inertie$classes <- factor(
  paste0(0:14, " à ", 1:15),
  levels = paste0(0:14, " à ", 1:15),
  ordered = TRUE
)

ggplot(inertie, aes(x = classes, y = inertia, fill = keep)) +
  geom_col() +
  geom_text(
    aes(label = label_number(
      decimal.mark = ",",
      accuracy = 1,
    )(inertie$inertia)),
    size = 3,
    vjust = -0.3
  ) +
  labs(y = "Inertie") +
  scale_fill_grey(start = 0.8, end = 0.2) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.12))
  ) +
  theme_tufte() +
  theme(legend.position = "none")

ggplot(inertie, aes(x = classes, y = absolute_loss, fill = keep)) +
  geom_col() +
  geom_text(
    aes(label = label_number(
      decimal.mark = ",",
      accuracy = 1,
    )(absolute_loss)),
    size = 3,
    vjust = 1
  ) +
  labs(y = "Perte d'inertie absolue") +
  scale_fill_grey(start = 0.8, end = 0.2) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.12))
  ) +
  theme_tufte() +
  theme(legend.position = "none")


ggplot(inertie, aes(x = classes, y = relative_loss, fill = keep)) +
  geom_col() +
  geom_text(
    aes(label = label_number(
      decimal.mark = ",",
      accuracy = 0.01,
    )(relative_loss)),
    size = 3,
    vjust = 1
  ) +
  labs(y = "Perte d'inertie relative") +
  scale_fill_grey(start = 0.8, end = 0.2) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.12))
  ) +
  theme_tufte() +
  theme(legend.position = "none")






inertie <- sort(arbre$height, decreasing = TRUE)

df_inertie <- data.frame(
  n_classes = length(arbre$height):1,
  inertie = inertie
)

ggplot(df_inertie, aes(x = n_classes, y = inertie)) +
  geom_step() +
  geom_vline(xintercept = 6, linetype = "dashed", colour = "grey50") +
  geom_point(data = subset(df_inertie, n_classes == 6)) +
  scale_x_reverse(breaks = 1:20) +
  scale_y_continuous(
    labels = scales::label_number(decimal.mark = ",")
  ) +
  labs(
    x = "Nombre de classes",
    y = "Inertie"
  ) +
  theme_tufte()

# sauts d'inertie à 7
typo <- cutree(arbre, 6)

typo %>% freq



# 
# On intègre le résultat dans les données
typo <- typo %>%
  as_factor() %>%
  fct_recode(
    "Classes supérieures : cadres du privé et professions libérales (13%) [C6]" = "6",
    'Classes moyennes salariés : biactivité et accession à la porpiété (24%) [C3]' = "3",
    "Classes populaires précaires et isolées (11%) [C5]" = "5",
    "Classes populaires immigrées urbaines et mono-actives (7%) [C2]" = "2",
    "Classes moyennes-supérieures urbaines : diplomées et salariées du public  (21%) [C4]" = "4",
    "Classes populaires prériurbaines et rurales : petits indépendants et salariés non-qualifiés propriétaires (23%) [C1]" = "1"
  ) %>%
  fct_relevel(
    "Classes populaires précaires (11%) [C5]",
    "Classes populaires immigrées urbaines (7%) [C2]",
    "Petits indépendants et salariés subalternes du rural (23%) [C1]",
    "'Petits-moyens' (24%) [C3]",
    "Classes moyennes-supérieures du public (21%) [C4]",
    "Classes supérieures à dominante économique/privé (13%) [C6]"
  )


temp <- data.frame(typo, poidsACMspe)


freq(temp$typo)

temp %>%
  group_by(typo) %>%
  summarise(
    n = sum(poidsACMspe, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct = n / sum(n) * 100
  )

# Dendrogram avec nom des clusters 

library(dendextend)
library(ggdendro)


dend

# Données nécessaires au ggplot
dd <- dendro_data(dend, type = "rectangle")

# Ordre des individus sur le dendrogramme
ordre <- arbre$order

# Correspondance position sur le dendrogramme -> cluster
clusters <- data.frame(
  pos = seq_along(ordre),
  cluster = typo[ordre]
)

# Position centrale de chaque cluster
positions_clusters <- clusters %>%
  group_by(cluster) %>%
  summarise(
    y = mean(pos),
    .groups = "drop"
  )

# Hauteur maximale de chaque cluster
# (extrémité supérieure de la branche correspondant au cluster)
hauteurs_clusters <- dd$segments %>%
  filter(xend != x | yend != y) %>%
  mutate(
    cluster = sapply(x, function(x) {
      # position de feuille la plus proche
      clusters$cluster[which.min(abs(clusters$pos - x))]
    })
  ) %>%
  group_by(cluster) %>%
  summarise(
    x = max(y, na.rm = TRUE),
    .groups = "drop"
  )

# Fusion des positions
labels_clusters <- left_join(
  positions_clusters,
  hauteurs_clusters,
  by = "cluster"
)


# Marge pour placer les labels légèrement à droite des branches
marge <- max(dd$segments$y, na.rm = TRUE) * 0.03

datadendplot <- dd$segments %>%
  filter(y >= 40) %>%
  mutate(yend = if_else(yend < 40, 40, yend))

xcluster <-   datadendplot %>%
  filter(yend == 40) %>%
  select(x)
labels_clusters <- labels_clusters %>%
  arrange(y)
labels_clusters$y2 <- xcluster$x
labels_clusters$cluster <- sapply(as.character(labels_clusters$cluster), 
                                  function(x){insert_line_breaks(x, 32)}) %>%
  as.factor()

# Dendrogramme
ggplot() +
  geom_segment(
    data = datadendplot,
    aes(
      x = y,
      y = x,
      xend = yend,
      yend = xend,
      
    ),
    linewidth = 0.5,
  ) +
  scale_x_reverse(limits = c(10, 90)) +
  geom_text(
    data = labels_clusters,
    aes(
      x = 38,
      y = y2,
      label = cluster
    ),
    hjust = 0,
    fontface = "bold",
    size = 4) +
  theme_tufte()+
  #theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
    #plot.margin = margin(10, 100, 10, 10)
  ) +
  labs(
    x = NULL,
    y = NULL
  )





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
                    champ = "ménages ordinaires formés par des adultes en ages de vivre avec des enfants.", 
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
class(tab)
tab
tab %>%
  flextable() |>
  font(fontname = "Garamond", part = "all") |>
  fontsize(size = 10, part = "all") |>
  autofit()

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

design <- d_cah %>%
  as_survey_design(weights = pond)

tab <- design %>%
  group_by(typo) %>%
  summarise(
    across(
      starts_with("Dim."),
      list(
        mean = ~survey_mean(.x, na.rm = TRUE),
        sd   = ~survey_sd(.x, na.rm = TRUE)
      )
    )
  )

tab <- tab %>%
  mutate(
    across(
      ends_with("_mean"),
      ~abs(.x) / get(sub("_mean$", "_sd", cur_column())),
      .names = "{.col}_pertinence"
    )
  )


# 
# tab <- d_cah %>%
#   as_survey_design(weights = pond) %>%
#   tbl_svysummary(by = typo, 
#                  statistic = all_continuous2() ~ "{mean}") %>%
#   add_p()
# tab

# les pvalues sont significatives 
tab
tab2 <- tab  %>%
  select("typo", ends_with("_mean"))
  
levels(tab2$typo) <- sapply(levels(tab2$typo), function(x){insert_line_breaks(x, 21)})


tab3 <- wtd.table(d_cah$typo, weights = d_cah$pond) %>%
  as.data.frame() %>%
  bind_cols(as.data.frame(freq(d_cah$typo))[, 1]) %>%
  rename(n = `...3`)

tab3$Prop <- round((tab3$Freq/sum(tab3$Freq))*100,1)


tab2 <-tab2 %>%
  pivot_longer(cols = starts_with("dim")) %>%
  mutate(name = str_replace(name, "dim.", "Dim ") %>% str_remove("_mean"))

tab2$typo
tab2_summary <- tab2 %>%
  group_by(name) %>%
  summarise(m = min(value), 
            M = max(value))
tab2_summary$eig <- round(acm_spe$eig$rate[1:acmstop], 2) 
tab2_summary <- tab2_summary %>%
  mutate(dimLabel = paste0(name, " (", eig, "%)"))
tab2 <- left_join(tab2, tab2_summary[, c(1,5)])
tab2_summary

tab2$dimLabel <- paste0(tab2$dimLabel, 
                        c(" : Capital économique et propriété immobilière", 
                          " : Capital culturel et scolaire et localisation résidentielle", 
                          " : Indépendance professionnelle et salariat intermédiaire", 
                          " : Classes populaires : Immigration, salariat masculin et organisation domestique", 
                          " : Précarité économique", 
                          " : Fractions supérieures : privé vs public", 
                          " : Qualifications et différenciation sexuée des trajectoires professionnelles"))


tab2_summary$Mlabel <- c("Elevé",
                         "Elevé",
                         "Cadres du privés et\nindépendants fortunés",
                         "Ouvriers issus de l'immigration",
                         "Secteur privé et indépendants",
                         "Privé",
                         "Indépendant-e-s")

tab2_summary$mlabel <- c("Capital économique faible",
                         "Capital culturel faible",
                         "Professions intérmédiaires du public",
                         "Petits indépendants pauvres ruraux",
                         "Cadres du public", 
                         "Public",
                         "Salarié-e-s")
library(ggh4x)

# Limites symétriques autour de 0 pour chaque facette
lims <- lapply(split(tab2$value, tab2$dimLabel), function(x) {
  m <- max(abs(x), na.rm = TRUE)
  c(-m, m)
})

gg <- ggplot(tab2) +
  theme_void() +
  aes(x = value, y = 1) +
  geom_hline(yintercept = 1, col = "grey") +
  annotate("point", x = 0, y = 1, shape = 3, col = "grey", size = 3) +
  annotate("text", x = 0, y = 1, label = "0", vjust = 2, size = 3, col = "grey") +
  geom_point() +
  geom_text_repel(aes(label = typo),
                  size = 3,
                  direction="x", vjust = -2,
                  max.overlaps=10000) +
  facet_wrap(dimLabel ~ ., ncol = 1, scales = "free_x") +
  facetted_pos_scales(
    x = lapply(names(lims), function(nm) {
      scale_x_continuous(limits = lims[[nm]])
    })
  ) +
  theme(
    strip.text = element_text(size = 11)
  )
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

