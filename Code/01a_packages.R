library(haven)
library(labelled)

library(git2r)
library(stringr)
library(aws.s3)

library(dplyr)
library(forcats)
library(tidyr)

library(questionr)
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
theme_gtsum$`tbl_summary-str:continuous_stat` = c("{mean}", "{p25} - {p75}", "{min} - {max}")
check_gtsummary_theme(theme_gtsum)
set_gtsummary_theme(theme_gtsum)
rm(theme_gtsum)

library(gt)
library(flextable)

# Mise en forme graphiques
library(ggplot2)
theme_get()
theme_set(theme_minimal())

# Packages Rmd 
library(rmarkdown)
library(knitr)
# On met cette fonction en français
combine_words <- combine_words(words, and = " et ", oxford_comma = FALSE)
library(pagedown)




