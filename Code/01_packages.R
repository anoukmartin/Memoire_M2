library(haven)
library(git2r)
library(stringr)
library(aws.s3)

library(dplyr)
library(forcats)
library(tidyr)

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
check_gtsummary_theme(theme_gtsum)
set_gtsummary_theme(theme_gtsum)

library(gt)

# Mise en forme graphiques
library(ggplot2)
theme_get()
theme_set(theme_minimal())
