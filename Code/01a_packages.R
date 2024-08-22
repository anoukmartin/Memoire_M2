
# Git et gestion du coffre aws
library(git2r)
library(aws.s3)

# Packages Rmd 
library(rmarkdown)
library(knitr)
# On met cette fonction en français
combine_words <- function(words){ 
  str <- combine_words(words, and = " et ", oxford_comma = FALSE)
  return(str)
}
library(pagedown)
library(bookdown)

# from sas to R, vecteurs labelisés
library(haven)
library(labelled)

# manipulation de données
library(tidyverse)
library(dplyr)
library(forcats)
library(tidyr)
library(stringr)

# Données pondérées et plans de sondage complexes
library(survey)
library(srvyr)

# ACM 
library("FactoMineR")
library("GDAtools")
library(cluster)

# Mise en forme des tableaux
library(gtsummary)
theme_gtsummary_compact()
theme_gtsummary_continuous2(set_theme = TRUE)
theme_gtsummary_language(language = "fr", decimal.mark = ",", big.mark = " ")
theme_gtsum <- get_gtsummary_theme()
theme_gtsum$`pkgwide-str:theme_name` = "Compact_custom"
theme_gtsum$`tbl_summary-fn:addnl-fn-to-run` = bold_labels
theme_gtsum$`tbl_svysummary-fn:addnl-fn-to-run` = bold_labels
theme_gtsum$`tbl_regression-fn:addnl-fn-to-run` = bold_labels
theme_gtsum$`tbl_summary-arg:statistic` = list(
  all_continuous() ~ c("{mean}",
                       "{p25} - {p75}",
                       "{min} - {max}"),
  all_categorical() ~ "{p}% ({n})", 
  all_dichotomous() ~ "{p}% ({n})")
                                       
# theme_gtsum$`tbl_summary-str:continuous_stat` = c("{mean}",
#                                                   "{p25} - {p75}",
#                                                   "{min} - {max}")
# theme_gtsum$`tbl_summary-str:categorical_stat` = "{p}% ({n})"

# theme_gtsum$`tbl_svysummary-str:continuous_stat` = c("{mean}", 
#                                                     "{p25} - {p75}", 
#                                                     "{min} - {max}")
# theme_gtsum$`tbl_svysummary-arg:statistics` = "{p} ({n_unweighted})"

check_gtsummary_theme(theme_gtsum)
set_gtsummary_theme(theme_gtsum)
rm(theme_gtsum)


library(questionr)
library(gt)
library(gtExtras)
gt_theme_custom <- function(gt_object) {
  stopifnot(`'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?` = "gt_tbl" %in% 
              class(gt_object))
  gt_object %>% 
    opt_table_font(font = list(google_font("Alegreya"), 
                               default_fonts()))}

# Une fonction pour les tableaux "gt" 
add_gtsource_note <- function(gt_object, 
                              source = paste0(infosBDF$nom, 
                                              ", ", 
                                              infosBDF$vague), 
                              champ = infosBDF$champ, 
                              N, 
                              lecture = NULL) {
  
  stopifnot(`'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?` = "gt_tbl" %in% 
              class(gt_object))
  
  if(!is_null(lecture)) {
    lecture <- paste0("**Lecture** : ", lecture)
  } else {
    message("l'argument lecture est manquant")
  }
  if(!is_null(source)) {
    source <- paste0("**Source** : ", source)
  } else {
    message("l'arguement source est manquant")
  }
  if(!is_null(champ)){
    champ <- paste0("**Champ** : ", champ, " (N = ", N, ").")
  } else {
    message("les arguments champ et/ou N sont manquants")
  }
  
  gt_object <- gt_object %>%
    gt::tab_source_note(md(c(source, champ, lecture)))
  return(gt_object)
}


library(flextable)
library(kableExtra)
# Une fonction pour les tableaux "gt" 
add_kablesource_note <- function(kbl_object, 
                              source = paste0(infosBDF$nom, 
                                              ", ", 
                                              infosBDF$vague), 
                              champ = infosBDF$champ, 
                              N, 
                              lecture = NULL) {
  
  if(!is_null(lecture)) {
    lecture <- paste0("Lecture : ", lecture)
  } else {
    message("l'argument lecture est manquant")
  }
  if(!is_null(source)) {
    source <- paste0("Source : ", source)
  } else {
    message("l'arguement source est manquant")
  }
  if(!is_null(champ)){
    champ <- paste0("Champ : ", champ, " (N = ", N, ").")
  } else {
    message("les arguments champ et/ou N sont manquants")
  }
  
  kbl_object <- kbl_object %>%
    kableExtra::footnote(general = c(source, champ, lecture), 
                         threeparttable = T)
  return(kbl_object)
}

# Mise en forme graphiques
library(ggplot2)
library(ggmosaic)
library("RColorBrewer")
library(wesanderson)
library(ggthemes)
theme_get()
theme_set(theme_minimal())

#theme_set(theme(base_family = "Times New Roman"))
theme_memoire <- function(base_size = 14) {
  ggthemes::theme_tufte(base_size = base_size) %+replace%
    theme(
      # L'ensemble de la figure
      plot.title = element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),
      # Zone où se situe le graphique
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      # Les axes
      axis.title = element_text(size = rel(0.85), face = "bold"),
      axis.text = element_text(size = rel(0.70), face = "bold"),
      axis.line = element_line(color = "black", arrow = arrow(length = unit(0.3, "lines"), type = "closed")),
      # La légende
      legend.title = element_text(size = rel(0.85), face = "bold"),
      legend.text = element_text(size = rel(0.70), face = "bold"),
      legend.key = element_rect(fill = "transparent", colour = NA),
      legend.key.size = unit(1.5, "lines"),
      legend.background = element_rect(fill = "transparent", colour = NA),
      plot.caption.position = "plot",
      plot.caption = element_text(hjust = 0, size = rel(0.60))
      # Les étiquettes dans le cas d'un facetting
      # strip.background = element_rect(fill = "#17252D", color = "#17252D"),
      # strip.text = element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5,0,5,0))
    )
}


library(GGally)
library(Cairo)
library(ggrepel)



