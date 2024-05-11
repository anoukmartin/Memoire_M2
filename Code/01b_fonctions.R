

################################################################################-
############################ DES FONCTIONS #####################################
################################################################################-

################################################################################-
# I. Des fonctions de gestion ##################################################
################################################################################-


#' Enregistrer un tableau de données avec toutes les infos nécessaires 
#'
#' @param tableau 
#' @param label 
#' @param description 
#' @param champ 
#' @param n 
#' @param ponderation 
#' @param resPath 
#'
#' @return
#' @export
#'
#' @examples
saveTableau <- function(tableau, type, label, description, champ, n, ponderation, resPath = "Resultats"){
  if(!dir.exists(resPath)) {dir.create(resPath)}
  tab <- NULL
  tab$label <- label
  tab$descrption <- description
  tab$tableau <- tableau
  tab$champ <- champ
  tab$n <- n
  tab$ponderation <- ponderation
  save(tab, 
       file = paste0(resPath, "/", type, "_", label, ".Rdata"))
}

saveData <- function(data, label, dataPath = "Data_output"){
  if(!dir.exists(dataPath)) {dir.create(dataPath)}
  saveRDS(data, 
          file = paste0(dataPath, "/", label, ".Rds"))
}

loadData <- function(data = "all", dataPath = "Data_output"){
  listData <- list.files(dataPath) %>%
    str_remove_all(".Rds")
  if(data == "all") {
    data <- listData
  } else {
    if (any(!(data %in% listData))) {
      missing <- data[!(data %in% listData)]
      warningCondition(paste0(combine_words(data), " n'existe(nt) pas dans le dossier ", dataPath, "/"))
    } 
  }
  listData <- listData[listData %in% data]
  temp <- sapply(X = listData, 
                 FUN = function(x){
                   readRDS(file = paste0(dataPath, "/", x, ".Rds"))
                   }
  )
  return(temp)
}

loadResultats <- function(resultats = "all", resPath = "Resultats"){
  listRes <- list.files(resPath) %>%
    str_remove_all(".Rds")
  if(resultats == "all") {
    resultats <- listRes
  } else {
    if (any(!(resultats %in% listRes))) {
      missing <- resultats[!(resultats %in% listRes)]
      warningCondition(paste0(combine_words(missing), " n'existe(nt) pas dans le dossier ", resPath, "/"))
    } 
  }
  listRes <- listRes[listRes %in% resultats]
  temp <- sapply(X = listRes, FUN = function(x){
    readRDS(file = paste0(resPath, "/", x, ".Rds"))
  }
  )
  return(temp)
}


################################################################################-
# II. Des fonctions de recodages ###############################################
################################################################################-


#' Créé un identifiant individuel 
#'
#' @param data un tableau de données
#' @param IdentIndiv la variable d'identification individuelle au sein du ménage
#' @param IdentMenage la variable d'identification du méange
#' @param NewVarName le nom que l'on veut donner à la nouvelle variable
#'
#' @return data (le tableau de données) avec une nouvelle variable d'identification (nommée avec NewVarName construite par combinaison de IdentIndiv et IdentMenage 
#' @export
#'
#' @examples
#' 
#' 
var_IDENTIFIANT <- function(data, IdentIndiv, IdentMenage, NewVarName){
  data$tempIndiv <- NA
  data[, "tempIndiv"] <- data[, IdentIndiv] 
  data$tempMenage <- NA
  data[, "tempMenage"] <- data[, IdentMenage]
  data <- data %>%
    mutate(tempVar = case_when(
      str_length(tempIndiv) == 1 ~ paste0(tempMenage, "0", tempIndiv), 
      str_length(tempIndiv) == 2 ~ paste0(tempMenage, tempIndiv))) 
  names(data)[names(data) == "tempVar"] <- NewVarName
  data$tempIndiv <- NULL
  data$tempMenage <- NULL
  return(data)
}

#' Recodage de la variable couple
#'
#' @param data 
#' @param NewVar 
#'
#' @return
#' @export
#'
#' @examples
rec_COUPLE <- function(data, NewVar = FALSE) {
  data <- data %>%
    mutate(temp = case_when(
      COUPLE == "1" ~ "Oui, avec une personne qui vit dans le logement", 
      COUPLE == "2" ~ "Oui, avec une personne qui ne vit pas dans le logement", 
      COUPLE == "3" ~ "Non"))
  if(isFALSE(NewVar)){
    data$COUPLE <- data$temp 
    data$temp <- NULL
  } else { 
    names(data)[names(data) == "temp"] <- NewVar
    }
  return(data)
}

rec_TYPMEN5 <- function(data, NewVar = FALSE) {
  data <- data %>%
    mutate(temp = case_when(
      TYPMEN5 == "1" ~ "Personne seule", 
      TYPMEN5 == "2" ~ "Famille monoparentale", 
      TYPMEN5 == "3" ~ "Couple sans enfant", 
      TYPMEN5 == "4" ~ "Couple avec au moins un enfant",
      TYPMEN5 == "5" ~ "Autre type de ménage (ménage complexe)"))
  if(isFALSE(NewVar)){
    data$TYPMEN5 <- data$temp 
    data$temp <- NULL
  } else { 
    names(data)[names(data) == "temp"] <- NewVar
  }
  return(data)
}

– Célibataire en emploi (19 %) 
– Célibataire sans emploi (6 %) 
– Couple sans actif en emploi (6 %)
– Couple avec un seul actif en emploi44 (24 %) 
– Couple avec deux actifs en emploi (46 %)

rec_TYPMEN <- function(data, Var, NewVar = FALSE) {
  data$temp <- NULL
  data[, "temp"] <- data[, Var]
  data$temp
  data <- data %>%
    mutate(temp = fct_collapse(
      temp,
     "Célibataire en emploi" = c("10", "21"), 
     "Célibataire sans emploi" = c("11", "22", "23"),
     "Couple sans actif en emploi" = c("33", "43", "44"),
     "Couple avec un seul actif en emploi" = c("41", "31"),
     "Couple avec deux actifs en emploi" = c("42", "32"), 
     "Autre"= c("51", "52", "53")
    ))
  if(isFALSE(NewVar)){
    data[, Var] <- data[, "temp"]
    data$temp <- NULL
  } else { 
    names(data)[names(data) == "temp"] <- NewVar
  }
  return(data)
}


rec_CSP6 <- function(data, Var, NewVar = FALSE) {
  data$temp <- NULL
  data[, "temp"] <- data[, Var]
  data$temp
  data <- data %>%
    mutate(temp = str_sub(temp, 1, 1)) %>%
    mutate(temp = fct_recode(temp,
                             NULL = "",
                             NULL = "0",
                             "Agriculteurs" = "1",
                             "ACCE" = "2",
                             "CPIS" = "3",
                             "Professions intermédiaires" = "4",
                             "Employés" = "5",
                             "Ouvriers" = "6",
                             "Retraités" = "7", 
                             "Autres inactifs" = "8", 
                             NULL = "H", 
                             NULL = "h"
    ) %>%
      fct_relevel(
        "Agriculteurs", "ACCE", "CPIS", "Professions intermédiaires",
        "Employés", "Ouvriers", "Retraités", "Autres inactifs"
      ))
  if(isFALSE(NewVar)){
    data[, Var] <- data[, "temp"]
    data$temp <- NULL
  } else { 
    names(data)[names(data) == "temp"] <- NewVar
  }
  return(data)
}

rec_CSP12 <- function(data, Var, NewVar = FALSE) {
  data$temp <- NULL
  data[, "temp"] <- data[, Var]
  data$temp
  data <- data %>%
    mutate(temp = fct_collapse(
      temp,
      NULL = c("", "00", "ho", "20", "30", "40", "50", "60"),
     "Petit-e indépendant-e" = c("11", "12", "13", "21", "22"), 
     "Cadres d'entreprise, professions libérales ou chef-fe d'entreprise" = c("23", "31", "37", "38"),
     "Cadre du public ou de la culture" = c("33", "34", "35"),
     "Médiateur-ice" = c("42", "43", "44", "45", "46"),
     "Technicien-ne" = c("47", "48"),
     "Employé-e qualifié-e" = c("52", "53", "54"),
     "Employé-e non qualifié-e" = c("55", "56"),
     "Ouvrier-e qualifié-e" = c("62", "63", "64", "65"),
     "Ouvrier-e non qualifié-e" = c("67", "68", "69"),
     "Retraité-e" = c("71", "72", "74", "75", "77", "78"),  
     "Autre inactif-ve" = c("81", "83", "84", "85", "86")
    ))
  if(isFALSE(NewVar)){
    data[, Var] <- data[, "temp"]
    data$temp <- NULL
  } else { 
    names(data)[names(data) == "temp"] <- NewVar
  }
  return(data)
}

rec_SEXE <- function(data, Var = "SEXE", NewVar = FALSE) {
  data$temp <- NULL
  data[, "temp"] <- data[, Var]
  data$temp
  data <- data %>%
    mutate(temp = fct_recode(temp,
                             "Homme" = "1",
                             "Femme" = "2"
    ))
  if(isFALSE(NewVar)){
    data[, Var] <- data[, "temp"]
    data$temp <- NULL
  } else { 
    names(data)[names(data) == "temp"] <- NewVar
  }
  return(data)
}


rec_DIP <- function(data, Var, NewVar = FALSE) {
  data$temp <- NULL
  data[, "temp"] <- data[, Var]
  data$temp
  data <- data %>%
    mutate(temp = fct_recode(temp,
      NULL = "",
      "Bac+5 et plus" = "10",
      "Bac+5 et plus" = "12",
      "Bac+5 et plus" = "20",
      "Bac+1 à Bac+3" = "30",
      "Bac+1 à Bac+3" = "31",
      "Bac+1 à Bac+3" = "33",
      "Bac et équivalents" = "41",
      "Bac et équivalents" = "42",
      "Bac et équivalents" = "43",
      "Bac et équivalents" = "44",
      "CAP, BEP et équivalents" = "50",
      "BEPC, Brevet ou en dessous" = "60",
      "BEPC, Brevet ou en dessous" = "70",
      "BEPC, Brevet ou en dessous" = "71"
    ))
  if(isFALSE(NewVar)){
    data[, Var] <- data[, "temp"]
    data$temp <- NULL
  } else { 
    names(data)[names(data) == "temp"] <- NewVar
  }
  return(data)
}

rec_DIP7 <- function(data, Var = "DIP14", NewVar = FALSE) {
  data$temp <- NULL
  data[, "temp"] <- data[, Var]
  data$temp
  data <- data %>%
    mutate(temp = fct_collapse(temp,
                             NULL = "",
                             "Diplôme universitaire du 3eme cycle, ingénieur, grande école" = c("10", "12"),
                             "Diplôme universitaire de 2eme cycle" = c("20"),
                             "Diplôme universitaire de 1er cycle, BTS, DUT, diplôme santé social (niveau bac + 2)" = c("33", "31", "30"),
                             "Baccalauréat" = c("44","43", "42", "41"),
                             "CAP ou BEP" = c("50"),
                             "Brevet des collèges" = c("60"),
                             "Sans diplôme ou CEP" = c("70", "71")))
  if(isFALSE(NewVar)){
    data[, Var] <- data[, "temp"]
    data$temp <- NULL
  } else { 
    names(data)[names(data) == "temp"] <- NewVar
  }
  return(data)
}

rec_DIP14 <- function(data, Var = "DIP14", NewVar = FALSE) {
  data$temp <- NULL
  data[, "temp"] <- data[, Var]
  data$temp
  data <- data %>%
    mutate(temp = fct_recode(temp,
                             NULL = "",
                             "DIPLOME DE 3E CYCLE UNIVERSITAIRE, DOCTORAT" = "10",
                             "DIPLOME D'INGENIEUR, D'UNE GRANDE ECOLE" = "12",
                             "DIPLOME DE 2E CYCLE UNIVERSITAIRE" = "20",
                             "DIPLOME DE 1ER CYCLE UNIVERSITAIRE" = "30",
                             "BTS, DUT OU EQUIVALENT" = "31",
                             "DIPLOME DES PROFESSIONS SOCIALES ET DE LA SANTE DE NIVEAU BAC+2" = "33",
                             "BACCALAUREAT GENERAL, BREVET SUPERIEUR, CAPACITE EN DROIT, DAEU..." = "41",
                             "BACCALAUREAT TECHNOLOGIQUE" = "42",
                             "BACCALAUREAT PROFESSIONNEL" = "43",
                             "BREVET PROFESSIONNEL OU DE TECHNICIEN" = "44",
                             "CAP, BEP OU DIPLOME DE MEME NIVEAU" = "50",
                             "BREVET DES COLLEGES, BEPC" = "60",
                             "CERTIFICAT D'ETUDES PRIMAIRES" = "70",
                             "AUCUN DIPLOME" = "71"
    ) %>% str_to_sentence())
  if(isFALSE(NewVar)){
    data[, Var] <- data[, "temp"]
    data$temp <- NULL
  } else { 
    names(data)[names(data) == "temp"] <- NewVar
  }
  return(data)
}


rec_AG6 <- function(data, Var = "AG6", NewVar = FALSE) {
  data$temp <- NULL
  data[, "temp"] <- data[, Var]
  data$temp
  data <- data %>%
    mutate(temp = fct_recode(temp,
                             "de 0 à 14 ans" = "00",
                             "de 15 à 29 ans" = "15",
                             "de 30 à 39 ans" = "30",
                             "de 40 à 49 ans" = "40",
                             "de 50 à 59 ans" = "50",
                             "60 ans et plus" = "60"
                             ))
  if(isFALSE(NewVar)){
    data[, Var] <- data[, "temp"]
    data$temp <- NULL
  } else { 
    names(data)[names(data) == "temp"] <- NewVar
  }
  return(data)
}


rec_STALOG <- function(data, Var = "STALOG", NewVar = FALSE) {
  data$temp <- NULL
  data[, "temp"] <- data[, Var]
  data$temp
  data <- data %>%
    mutate(temp = fct_recode(temp,
                             "Accédant à la propriété" = "1",
                             "Propriétaire ou copropriétaire" = "2",
                             "Autre" = "3",
                             "Locataire" = "4",
                             "Locataire" = "5",
                             "Autre" = "6"
    ))
  if(isFALSE(NewVar)){
    data[, Var] <- data[, "temp"]
    data$temp <- NULL
  } else { 
    names(data)[names(data) == "temp"] <- NewVar
  }
  return(data)
}

## Tableau croisé, khi2 et résidus #############################################


# Tableau statistiques descriptives des clusters 

# joli_tableau(data= parents,
#              by = "typo",
#              vars_quali = c("SEXE", "AG6"),
#              vars_quanti = c("n_REVENUS", "n_PATRIMOINE"),
#              weigths = parents$PONDIND,
#              tableau_titre = "blabla",
#              source = "", champ = "", lecture = "")

joli_tableau <- function(data, 
                         by, 
                         vars_quali,
                         vars_quanti, 
                         vars_binom,
                         weigths, 
                         tableau_titre, 
                         source, champ, lecture){

  # Tableau : description socio-démo des clusters  ----
  
  # Calculs ----
  # vars_keep <- names(vars_sup)
  # vars_keep <- vars_keep[!(c(vars_keep %in% c("Nationalité", "Nature du contrat")))]
  
  ### Mise à l"échelle des poids ----
  weigths <- weigths/mean(weigths)
  
  ## Variables quali ----
  ### Tableau croisés des effectifs (pondérés) ----
  tabs <- sapply(1:(length(vars_quali)),
                 FUN = function(x){
                   temp <- data[, vars_quali[x]] %>% unlist()
                   cross <- data[, by] %>% unlist()
                   tab <- wtd.table(x = temp, 
                                    y = cross, 
                                    weights = weigths, 
                                    normwt = T, 
                                    useNA = "no") %>%
                     as.table()
                   return(tab)
                 }
  )
  names(tabs) <- vars_quali
  
  ### Tableau croisé avec les proprotion en colones ----
  proptabs <- sapply(tabs, cprop)
  
  ### Test du chi2 ---
  chi <- sapply(tabs, chisq.test)
  
  ### Résidus du test ----
  residus <- sapply(tabs, chisq.residuals)
  
  ## Assemblage ----
  
  #install.packages("kableExtra")
  
  cleantabs <- lapply(
    1:length(proptabs), 
    FUN = function(x){
      tab <- proptabs[[x]] %>% 
        as.data.frame() %>%
        filter(Var1 != "Total") %>%
        mutate(nameVar1 = names(proptabs)[x])
    })
  
  cleanresidus <- lapply(
    1:length(residus), 
    FUN = function(x){
      tab <- residus[[x]] %>% 
        as.data.frame() %>%
        mutate(nameVar1 = names(proptabs)[x])
    })
  
  
  
  tableau <- bind_rows(cleantabs) %>%
    pivot_wider(names_from = Var2,
                values_from = Freq)
  
  cleanresidus <- bind_rows(cleanresidus) %>%
    pivot_wider(names_from = Var2,
                values_from = Freq)

  
  tableau <- tableau %>%
    mutate(nameVar1 = nameVar1 %>% as.factor() %>% fct_inorder()) 
  
  pvalue <- as.data.frame(chi) %>%
    rownames_to_column() %>%
    filter(rowname == "p.value") %>%
    select(-rowname) %>%
    t() %>% 
    as.data.frame() %>%
    rownames_to_column()
  names(pvalue) <- c("nameVar1", "p.valeur")
  pvalue$pvalue <- pvalue$p.valeur %>% 
    as.numeric()
  pvalue$pvalue <- case_when(
    pvalue$pvalue < 0.001 ~ "<0.001", 
    pvalue$pvalue >= 0.001 ~ paste0("=", round(pvalue$pvalue, digits = 2)))
  
  
  ## Mise en forme ----
  groups <- data.frame(
    nameVar1 = unique(tableau$nameVar1))
  groups <- left_join(groups, 
                      count(tableau, nameVar1))
  group <- groups$n
  names(group) <- groups$nameVar1
  names(group) <- paste0(names(group), " (p ", pvalue$pvalue, ")")
  
  pvalue <- pvalue %>% left_join(group %>% 
                                   as.data.frame() %>% 
                                   rownames_to_column(var = "nameVar1"))
  
  pvaleur <- pvalue$.
  names(pvaleur) <- pvalue$pvalue
  
  
  palette <- c("#fc8d59", "#91bfdb", "#000000")
  
  
  names(tableau)[1] <- " "
  
  tableau <- tableau %>%
    select(-nameVar1)
  cleanresidus <- cleanresidus %>%
    select(-nameVar1)
  
  tableau_beau <- tableau %>%
    kbl(digits = 1, booktabs = T, longtable = TRUE,
        caption = tableau_titre) %>%
    kable_styling(
      latex_options = c("hold_position", "scale_down", "repeat_header")) %>%
    pack_rows(index=group) 
  n <- length(unique(data[, by] %>% unlist()))
  for(k in 1+(1:n)){
    tableau_beau <- tableau_beau %>%
      column_spec(k, 
                  color = case_when(
                    cleanresidus[, k] >= 2 ~ palette[1],
                    cleanresidus[, k] <= -2 ~ palette[2], 
                    TRUE ~ palette[3]), 
                  bold = case_when(
                    cleanresidus[, k] >= 2 ~ T,
                    cleanresidus[, k] <= -2 ~ T, 
                    TRUE ~ F))
  }
  tableau_beau <- tableau_beau %>%
   # add_header_above(c(" " = 1, by = n, " " = 1), bold = TRUE) %>%
    footnote(general = c("Test du khi2 : p = pvalue, \\\\textcolor[HTML]{91bfdb}{bleu} = résidu < 2, \\\\textcolor[HTML]{fc8d59}{rouge} = résidu > 2", 
                         paste0("Source : ", source),
                         paste0("Champ : ", champ), 
                         paste0("Lecture : ", lecture)),
             escape = F)
  return(tableau_beau)
}
  
  
