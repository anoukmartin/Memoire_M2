
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
theme_gtsum$`tbl_summary-str:continuous_stat` = c("{mean}", 
                                                  "{p25} - {p75}", 
                                                  "{min} - {max}")
theme_gtsum$`tbl_summary-str:categorical_stat` = "{p} ({n})"

# theme_gtsum$`tbl_svysummary-str:continuous_stat` = c("{mean}", 
#                                                     "{p25} - {p75}", 
#                                                     "{min} - {max}")
# theme_gtsum$`tbl_svysummary-arg:statistics` = "{p} ({n_unweighted})"

check_gtsummary_theme(theme_gtsum)
set_gtsummary_theme(theme_gtsum)
rm(theme_gtsum)


library(questionr)
library(gt)
library(flextable)

# Mise en forme graphiques
library(ggplot2)
theme_get()
theme_set(theme_minimal())







