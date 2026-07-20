
library(tabulapdf)
# Individus 
# Variables 
vars <- extract_text("Documentation/Dictionnaire.pdf", pages = 19:21)
vars2 <- lapply(vars, function(x) {
  x %>% 
    as.data.frame() %>%
    separate_longer_delim(cols = ".", delim = "\n") %>%
    filter(between(row_number(), 1, n()-3))
})
vars2 <- bind_rows(vars2)
vars2 <- vars2[-1, ]
for (i in length(vars2):1){
  if(str_starts(vars2[i], "[:lower:]|[:blank:]")){
    vars2[i-1] <- paste(vars2[i-1], vars2[i])
    vars2 <- vars2[-i]
  }
}
vars2[94] <- paste(vars2[94], vars2[95])
vars2 <- vars2[-95]
vars2 <- vars2 %>%
  as.data.frame() %>%
  rename(Vars = ".")
vars2 <- vars2 %>%
  mutate(theme = if_else(str_detect(str_sub(Vars, 2, 2), "[:lower:]"), Vars, NA)) 

for (i in 2:nrow(vars2)){
  if(is.na(vars2$theme[i])){
    vars2$theme[i] <- vars2$theme[i-1]
  }
}
vars2 <- vars2 %>%
  filter(Vars != theme) %>%
  separate(Vars, into = c("variable", "label"), sep = "\\s", extra = "merge")
vars2$table = "Individu"

# Modalités 
dic <- extract_text("Documentation/Dictionnaire.pdf", pages = 159:181) 
dic2 <- lapply(dic, function(x) {
  x %>% 
    as.data.frame() %>%
    separate_longer_delim(cols = ".", delim = "\n") %>%
    filter(between(row_number(), 2, n()-3))
    
})
dic2[1]
dic2 <- bind_rows(dic2)
dic2 <- dic2[-1, ]
for (i in length(dic2):1){
  if(str_starts(dic2[i], "[:lower:]|[:blank:]|[:punct:]")){
    dic2[i-1] <- paste(dic2[i-1], dic2[i])
    dic2 <- dic2[-i]
  }
}
dic2[55] <- paste(dic2[55], dic2[56])
dic2 <- dic2[-c(56)]
dic2[74] <- paste(dic2[74], dic2[75])
dic2 <- dic2[-c(75)]

dic2 <- dic2 %>%
  as.data.frame() %>%
  rename(Dic = ".")

dic2 <- dic2 %>%
  mutate(Variable_nom = if_else(str_detect(str_sub(Dic, 2, 2), "[:upper:]") 
                            | Dic %in% vars2$Variable 
                            | str_starts(Dic, "I_"), Dic, NA)) %>%
  mutate(Variable_description = if_else(!is.na(lag(Variable_nom)), Dic, NA))

for (i in 2:nrow(dic2)){
  if(is.na(dic2$Variable_nom[i])){
    dic2$Variable_nom[i] <- dic2$Variable_nom[i-1]
  }
  if(is.na(dic2$Variable_description[i])){
    dic2$Variable_description[i] <- dic2$Variable_description[i-1]
  }
}
dic2 <- dic2 %>%
  filter(Dic != Variable_nom) %>%
  filter(Dic != Variable_description)

dic2 <- dic2 %>%
  separate(col = "Dic", 
           into = c("Modalite_code", "Modalite_label"), 
           sep = "\\s", 
           extra = "merge")

dic2 <- dic2 %>%
  mutate(
    Valeurs = if_else(
      str_starts(Modalite_label, "à"),
      paste(Modalite_code, Modalite_label), 
      NA), 
    Valeur_min = if_else(
      str_starts(Modalite_label, "à"),
      Modalite_code %>% str_trim(), 
      NA), 
    Valeur_max = if_else(
      str_starts(Modalite_label, "à"),
      Modalite_label %>%
        str_remove_all("à") %>%
        str_trim(), 
      NA))

dic2$Variable_classe <- NA 
dic2[is.na(dic2$Valeurs), "Variable_classe"] <- "Character"
dic2[!is.na(dic2$Valeurs), 
     c("Variable_classe", "Modalite_code", "Modalite_label")] <- c("Character", NA, NA)

dic2 <- left_join(dic2, vars2, by = c("Variable_nom" = "variable"))
names(dic2)
dic2 <- dic2[, c("Table" = "table", 
         "Variable_nom", "Variable_label" = "label", "Variable_description", 
         "Variable_theme" = "theme", "Variable_classe", 
         "Modalite_code", "Modalite_label", "Valeurs", "Valeur_min", "Valeur_max")]

saveRDS(dic2, "Documentation/Dictionnaire_individu.rds")
dic2 <- read_csv2("dic.csv")

dic_LIENXX <- extract_areas("Documentation/Dictionnaire.pdf", pages = 174)
