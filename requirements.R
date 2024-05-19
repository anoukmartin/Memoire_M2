packagesList <- c("haven",
                 "labelled", 
                 "git2r", 
                 "tidyverse", 
                 "dplyr",
                 "questionr",
                 "FactoMineR",
                 "GDAtools",
                 "gtsummary",
                 "gt",
                 "gtExtras",
                 "rmarkdown", 
                 "knitr", 
                  "survey", 
                  "srvyr", 
                  "flextable", 
                  "pagedown", 
                 "ordinal", 
                 "kableExtra", 
                 "ggmosaic", 
                 "tabulapdf",
                 "bookdown", 
                 "GGally", 
                 "Cairo", 
                 "wesanderson")
install.packages(packagesList)

remotes::install_github("lme4/lme4",dependencies=TRUE)




#remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer", "vqf/nVennR"))
