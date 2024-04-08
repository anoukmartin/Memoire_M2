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
                 "kableExtra")
install.packages(packagesList)

remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"))
