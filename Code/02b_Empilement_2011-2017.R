# Empiler 2011 et 2017 : 


# library(haven)
# library(dplyr)
# library(purrr)

path_2017 <- "Data_input/BDF_2017/sas"
path_2011 <- "Data_input/BDF_2011/sas"
path_out  <- "Data_input/BDF_bind"
dir.create(path_out)

files_2017 <- list.files(path_2017, pattern = "\\.sas7bdat$", full.names = TRUE)

file_2017 <- files_2017[17]

process_file <- function(file_2017) {
  
  name <- basename(file_2017)
  file_2011 <- file.path(path_2011, name)
  
  if (!file.exists(file_2011)) {
    message("\nTABLE ABSENTE en 2011 : ", name)
    return(NULL)
  }
  
  df_2017 <- read_sas(file_2017)
  df_2011 <- read_sas(file_2011)
  
  names(df_2017) <- str_to_upper(names(df_2017))
  names(df_2011) <- str_to_upper(names(df_2011))
  
  
  # Test de variables en commun entre les deux vagues
  vars_2017 <- names(df_2017)
  vars_2011 <- names(df_2011)
  
  common_vars <- intersect(vars_2017, vars_2011)
  
  only_2017 <- setdiff(vars_2017, vars_2011)
  only_2011 <- setdiff(vars_2011, vars_2017)
  
  pct_common <- length(common_vars) / length(union(vars_2017, vars_2011)) * 100
  
  message("\n--- ", name, " ---")
  message("Variables communes entre 2011 et 2017 : ", round(pct_common, 1), "%")
  
  if (length(only_2017) > 0) {
    message("Uniquement en 2017 : ", paste(only_2017, collapse = ", "))
  } else {
    message("Uniquement en 2017 : aucune")
  }
  
  if (length(only_2011) > 0) {
    message("Uniquement en 2011 : ", paste(only_2011, collapse = ", "))
  } else {
    message("Uniquement en 2011 : aucune")
  }
  
  
  # Test de compatbilités des types de variables : 
  type_conflicts <- c()
  
  for (v in common_vars) {
    
    cl17 <- class(df_2017[[v]])[1]
    cl11 <- class(df_2011[[v]])[1]
    
    if (cl17 != cl11) {
      
      type_conflicts <- c(type_conflicts, v)
      
      message(
        "Conflit de type pour ", v,
        " : 2011=", cl11,
        " / 2017=", cl17
      )
      
      # harmonisation prudente
      df_2011[[v]] <- as.character(df_2011[[v]])
      df_2017[[v]] <- as.character(df_2017[[v]])
    }
  }
  # Revalorisation des variables contenant des montants en 2011 ###################
  IPC2017 <- 84.45 # source insee
  IPC2011 <- 80.97 # source insee
  coef_ipc <-  IPC2017/IPC2011
  vars_rev <- names(df_2011)[str_detect(names(df_2011), "^REV")]
  df_2011$REV
  
  
  
  
  
  # Au cas par cas : 
  if(str_detect(file_2017, "MENAGE")) {
    # on doit récupéré le montant de la taxe d'habitation de l'impot sur le revenu dans les dépenses du ménage
    
  depmen2011 <- read_sas(file.path(path_2011, "DEPMEN.sas7bdat")) %>%
    mutate(
      IMPOTREV_M = rowSums(
      across(c(MIRPP1_D, MIRPP2_D, MIRPP3_D, MIRPP4_D)),
      na.rm = TRUE
    )) %>%
    select(IDENT_MEN, TAXHAB_M = MHAB_D, IMPOTREV_M)
  # Quelque changemnts de noms de variable
  df_2011 <- df_2011 %>%
    rename(
      PPA = RSA_ACT,,
      I_PPA = I_RSA_ACT) %>%
    # En 2011 on a pas le niveau de vie calculé a partir du revenu disponible, mais des variables de revenus total par UC, donc on doit le recalculer
    left_join(depmen2011, by = "IDENT_MEN") %>%
    mutate(REVDISP = REVTOT - IMPOTREV_M - TAXHAB_M) %>% # Calcul du revenu disponible du ménage 
    mutate(NIVIE = REVDISP/COEFFUC) # Calcul du niveau de vie du ménage
  # summary(df_2011$REVDISP)
  # summary(df_2011$NIVIE)
  # summary(df_2017$NIVIE)
  # plot(df_2017$REVDISP, df_2017$NIVIE)
  # plot(df_2011$REVDISP, df_2011$NIVIE)
  
  
  
  }
  
  

  df_2017 <- df_2017 %>%
    mutate(annee_BDF = 2017, 
           IDENT_MEN = paste0("17_", IDENT_MEN))
  
  df_2011 <- df_2011 %>%
    mutate(annee_BDF = 2011, 
           IDENT_MEN = paste0("11_", IDENT_MEN))
  
  df_final <- bind_rows(df_2011, df_2017)
  df_final <- copy_labels(df_2017, df_final)
  
  
  write_sas(
    df_final,
    file.path(path_out, name)
  )
  
  return(NULL)
}


process_file(file_2017 = files_2017[16])
walk(files_2017[c(8, 10, 11:14, 16:19)], process_file)


