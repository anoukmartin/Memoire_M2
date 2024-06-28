

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

rec_ETAMATRI <- function(data, NewVar = FALSE) {
  data <- data %>%
    mutate(temp = case_when(
      ETAMATRI == "1" ~ "Célibataire", 
      ETAMATRI == "2" ~ "Marié-e", 
      ETAMATRI == "3" ~ "Veuf-ve", 
      ETAMATRI == "4" ~ "Divorcé-e"))
  if(isFALSE(NewVar)){
    data$ETAMATRI <- data$temp 
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
    )%>%
      fct_recode(NULL = "NULL"))
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
     "Cadre/chef-fe d'entreprise, profession libérale" = c("23", "31", "37", "38"),
     "Cadre du public/culture" = c("33", "34", "35"),
     "Médiateur-ice" = c("42", "43", "44", "45", "46"),
     "Technicien-ne" = c("47", "48"),
     "Employé-e qualifié-e" = c("52", "53", "54"),
     "Employé-e non qualifié-e" = c("55", "56"),
     "Ouvrier-e qualifié-e" = c("62", "63", "64", "65"),
     "Ouvrier-e non qualifié-e" = c("67", "68", "69"),
     "Retraité-e" = c("71", "72", "74", "75", "77", "78"),  
     "Autre inactif-ve" = c("81", "83", "84", "85", "86")
    ) %>%
      fct_recode(NULL = "NULL"))
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
                             "Doctorat, ingénieur, grande école" = c("10", "12"),
                             "Master, bac+6" = c("20"),
                             "Licence, BTS, DUT, santé social " = c("33", "31", "30"),                       "Baccalauréat" = c("44","43", "42", "41"),
                             "CAP ou BEP" = c("50"),
                             "Brevet des collèges" = c("60"),
                             "Sans diplôme ou CEP" = c("70", "71")) %>%
             fct_recode(NULL = "NULL"))
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

rec_REVENUS <- function(data, Var = "DIP14", NewVar = FALSE) {
  data$temp <- NULL
  data[, "temp"] <- data[, Var]
  data$temp
  data <- data %>% 
    mutate(temp = cut(temp/12,
                      breaks = c(-Inf, 0, 550, 850, 1250, 1750, 2250, 2950, Inf), 
                 labels = c("Sans revenus", "Moins de 550", "Entre 550 et 850", 
                            "Entre 850 et 1250", 
                            "Entre 1250 et 1850", "Entre 1750 et 2250", 
                            "Entre 2250 et 2950", "Plus de 2950")))
  if(isFALSE(NewVar)){
    data[, Var] <- data[, "temp"]
    data$temp <- NULL
  } else { 
    names(data)[names(data) == "temp"] <- NewVar
  }
  return(data)
}

rec_PATRIB  <- function(data, Var = "PATRIB", NewVar = FALSE) {
  data$temp <- NULL
  data[, "temp"] <- data[, Var]
  data$temp
  data <- data %>% 
    mutate(temp = fct_collapse(
      temp,
      NULL = c("", "99", "98"), 
      "0 à moins de 5 000 euros" = "1", 
      "5 000 de 30 000 euros" = c("2", "3", "4"), 
      "30 000 à moins de 200 000 euros" = c("5", "6", "7", "8"),
      "200 000 à moins de 350 000 euros" = c("9", "10", "11"), 
      "350 000 euros et plus" = c("12", "13", "14", "15")) %>%
    fct_recode(NULL = "NULL"))
  if(isFALSE(NewVar)){
    data[, Var] <- data[, "temp"]
    data$temp <- NULL
  } else { 
    names(data)[names(data) == "temp"] <- NewVar
  }
  return(data)
}


      

rec_PATRIMOINE <- function(data, Var = "n_PATRIMOINE", NewVar = FALSE) {
  data$temp <- NULL
  data[, "temp"] <- data[, Var]
  data$temp
  data <- data %>% 
    mutate(temp = cut(temp,
                      breaks = c(-Inf, 0, 1000, 4000, 10000, Inf), 
                      labels = c("Sans économies", "Moins de 1000", 
                                 "Entre 1 000 et 4 000", 
                                 "Entre 5 000 et 10 000", 
                                 "Plus de 10 000")))
  if(isFALSE(NewVar)){
    data[, Var] <- data[, "temp"]
    data$temp <- NULL
  } else { 
    names(data)[names(data) == "temp"] <- NewVar
  }
  return(data)
}

rec_NENFANTS <- function(data, Var, NewVar = FALSE) {
  data$temp <- NULL
  data[, "temp"] <- data[, Var]
  data$temp
  data <- data %>% 
    mutate(temp = cut(temp,
                      breaks = c(-0.5, 0.5, 1.5, 2.5, 3.5, Inf), 
                      labels = c("Aucun", "Un", "Deux", 
                                 "Trois", 
                                 "Quatre et plus")) %>%
             fct_na_value_to_level(level = "Aucun"))
  if(isFALSE(NewVar)){
    data[, Var] <- data[, "temp"]
    data$temp <- NULL
  } else { 
    names(data)[names(data) == "temp"] <- NewVar
  }
  return(data)
}

rec_PROP <- function(data, Var, NewVar = FALSE) {
  data$temp <- NULL
  data[, "temp"] <- data[, Var]
  data$temp
  data <- data %>% 
    mutate(temp = cut(temp,
                      breaks = c(-Inf, 20, 40, 50, 60, Inf), 
                      labels = c("Moins de 20%", "Entre 20 et 40%",
                                 "Entre 40 et 50%", "Entre 50% et 60%", "Plus de 60%")))
  if(isFALSE(NewVar)){
    data[, Var] <- data[, "temp"]
    data$temp <- NULL
  } else { 
    names(data)[names(data) == "temp"] <- NewVar
  }
  return(data)
}


rec_TAU <- function(data, Var = "TAU", NewVar = FALSE) {
  data$temp <- NULL
  data[, "temp"] <- data[, Var]
  data$temp
  data <- data %>%
    mutate(temp = fct_collapse(
      temp,
      NULL = "",
      "Commune rurale" = c("00"),
      "Commune urbaine de moins de 20 000 habitants" = c("01", "02"),
      "Commune urbaine de 20 000 à 200 000 habitants" = c("03", "04", "05", "06", "07"),
      "Commune urbaine de plus de 200 000 habitants, hors agglomération parisienne " = c("08", "09"), 
      "Commune de l'agglomération parisienne" = c("10")) %>%
        fct_recode(NULL = "NULL"))
  if(isFALSE(NewVar)){
    data[, Var] <- data[, "temp"]
    data$temp <- NULL
  } else { 
    names(data)[names(data) == "temp"] <- NewVar
  }
  return(data)
}


rec_TYPLOG <- function(data, Var = "TYPLOG", NewVar = FALSE) {
  data$temp <- NULL
  data[, "temp"] <- data[, Var]
  data$temp
  data <- data %>%
    mutate(temp = fct_collapse(
      temp,
      NULL = "",
      "Maison" = c("1", "2"),
      "Appartement" = c("3", "4", "5"),
      "Autre logement" = c("6", "7")) %>%
        fct_recode(NULL = "NULL"))
  if(isFALSE(NewVar)){
    data[, Var] <- data[, "temp"]
    data$temp <- NULL
  } else { 
    names(data)[names(data) == "temp"] <- NewVar
  }
  return(data)
}

rec_NAIS7 <- function(data, Var = "NAIS7", NewVar = FALSE) {
  data$temp <- NULL
  data[, "temp"] <- data[, Var]
  data$temp
  data <- data %>%
    mutate(temp = fct_collapse(
      temp,
      NULL = "",
      "France métropolitaine" = c("1"),
      "DOM-TOM" = c("2"),
      "UE" = c("3", "4"), 
      "Afrique" = c("5", "6"), 
      "Autre" = c("7"))%>%
        fct_recode(NULL = "NULL"))
  if(isFALSE(NewVar)){
    data[, Var] <- data[, "temp"]
    data$temp <- NULL
  } else { 
    names(data)[names(data) == "temp"] <- NewVar
  }
  return(data)
}

rec_TYPEMPLOI <- function(data, Var = "TYPEMPLOI", NewVar = FALSE) {
  data$temp <- NULL
  data[, "temp"] <- data[, Var]
  data$temp
  data <- data %>%
    mutate(temp = fct_collapse(
      temp,
      NULL = c("", "8", "9"),
      "Durée déterminée" = c("1", "2", "3", "4", "5"),
      "Durée indéterminée à temps complet" = c("6"),
      "Durée indéterminée à temps partiel" = c("7")) %>%
        fct_recode(NULL = "NULL"))
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
        #format = "latex",
        caption = tableau_titre) %>%
    kable_styling(
      #font_size = 7,
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
  
  

insert_line_breaks <- function(text, max_length = 160) {
  # Split the text into words
  words <- unlist(strsplit(text, " "))
  
  # Initialize variables
  current_line <- ""
  result <- ""
  
  # Loop through each word
  for (word in words) {
    # Check if adding the next word exceeds the max length
    if (nchar(current_line) + nchar(word) + 1 > max_length) {
      # If it does, add the current line to the result and start a new line
      result <- paste(result, current_line, sep = "\n")
      current_line <- word
    } else {
      # Otherwise, add the word to the current line
      if (nchar(current_line) > 0) {
        current_line <- paste(current_line, word, sep = " ")
      } else {
        current_line <- word
      }
    }
  }
  
  # Add the last line to the result
  result <- paste(result, current_line, sep = "\n")
  
  # Remove the leading newline character if present
  result <- sub("^\n", "", result)
  
  return(result)
}

# Exemple d'utilisation
texte <- "Ceci est un exemple de texte assez long pour démontrer comment la fonction peut insérer des retours à la ligne tous les 120 caractères sans couper les mots. Chaque mot sera maintenu intact, et les retours à la ligne seront ajoutés de manière appropriée."
cat(insert_line_breaks(texte))
