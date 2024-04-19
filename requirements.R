packagesList <- c("haven",
                 "labelled", 
                 "git2r", 
                 "tidyverse", 
                 "questionr",
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
                 "ggmosaic")
install.packages(packagesList)

remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer", "vqf/nVennR"))
