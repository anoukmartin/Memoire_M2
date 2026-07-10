


path_2017 <- "Data_input/BDF_2017"
path_2011 <- "Data_input/BDF_2011"
path_out  <- "Data_input/BDF_bind"
dir.create(path_out)


path_log <- file.path(path_out, "logs")

dir.create(
  path_log,
  recursive = TRUE,
  showWarnings = FALSE
)

files_2017 <- list.files(path_2017, pattern = "\\.sas7bdat$", full.names = TRUE)

file_2017 <- files_2017[8]

process_file <- function(file_2017) {
  
  name <- basename(file_2017)
  file_2011 <- file.path(path_2011, name)
  
  # ------------------------------------------------------------------
  # Initialisation du rapport
  # ------------------------------------------------------------------
  
  log_txt <- character()
  
  add_log <- function(...) {
    log_txt <<- c(log_txt, paste0(...))
  }
  
  add_log("# ", name)
  add_log("")
  add_log("Date : ", Sys.time())
  add_log("")
  
  # ------------------------------------------------------------------
  # Vérification présence fichier
  # ------------------------------------------------------------------
  
  if (!file.exists(file_2011)) {
    
    add_log("## ERREUR")
    add_log("")
    add_log("Table absente en 2011")
    
    writeLines(
      log_txt,
      file.path(
        path_log,
        paste0(tools::file_path_sans_ext(name), ".txt")
      )
    )
    
    return(NULL)
  }
 
  # ------------------------------------------------------------------
  # Lecture
  # ------------------------------------------------------------------
  
  df_2017 <- read_sas(file_2017)
  df_2011 <- read_sas(file_2011)
  
  names(df_2017) <- str_to_upper(names(df_2017))
  names(df_2011) <- str_to_upper(names(df_2011))
  
  add_log("## Dimensions initiales")
  add_log("")
  add_log(
    "2011 : ",
    nrow(df_2011),
    " lignes x ",
    ncol(df_2011),
    " variables"
  )
  
  add_log(
    "2017 : ",
    nrow(df_2017),
    " lignes x ",
    ncol(df_2017),
    " variables"
  )
  
  add_log("")
  
  # ------------------------------------------------------------------
  # Comparaison variables
  # ------------------------------------------------------------------
  
  vars_2017 <- names(df_2017)
  vars_2011 <- names(df_2011)
  
  common_vars <- intersect(vars_2017, vars_2011)
  
  only_2017 <- setdiff(vars_2017, vars_2011)
  only_2011 <- setdiff(vars_2011, vars_2017)
  
  pct_common <- round(
    length(common_vars) /
      length(union(vars_2017, vars_2011)) * 100,
    1
  )
  
  add_log("## Comparaison des variables")
  add_log("")
  add_log(
    "Variables communes : ",
    pct_common,
    "%"
  )
  add_log(
    "Nombre variables communes : ",
    length(common_vars)
  )
  add_log("")
  
  add_log("### Variables uniquement en 2011")
  add_log(
    if(length(only_2011) == 0)
      "Aucune"
    else
      paste(only_2011, collapse = ", ")
  )
  
  add_log("")
  
  add_log("### Variables uniquement en 2017")
  add_log(
    if(length(only_2017) == 0)
      "Aucune"
    else
      paste(only_2017, collapse = ", ")
  )
  
  add_log("")
  
  # ------------------------------------------------------------------
  # Conflits de type
  # ------------------------------------------------------------------
  
  add_log("## Conflits de type")
  add_log("")
  
  type_conflicts <- character()
  
  for(v in common_vars){
    
    cl17 <- class(df_2017[[v]])[1]
    cl11 <- class(df_2011[[v]])[1]
    
    if(cl17 != cl11){
      
      type_conflicts <- c(type_conflicts, v)
      
      add_log(
        "- ",
        v,
        " : 2011=",
        cl11,
        " | 2017=",
        cl17
      )
      
      df_2011[[v]] <- as.character(df_2011[[v]])
      df_2017[[v]] <- as.character(df_2017[[v]])
    }
  }
  
  if(length(type_conflicts) == 0){
    add_log("Aucun conflit")
  }
  
  add_log("")
  log_txt
  
  # ------------------------------------------------------------------
  # Gestion des modalités 98 ou 99 des variables quantis et quali
  # ------------------------------------------------------------------
# exemple 
  # dat <- df_2011
  # v <- names(dat)[10]
  
  
  add_log("## Gestion des faux NA (98 ou 99) ")

  datasets <- list(df_2011 = df_2011, df_2017 = df_2017)
  
  for (nm in names(datasets)) {
    
    dat <- datasets[[nm]]
    
    add_log("### ", nm)

    for(v in names(dat)){
      
      x <- dat[[v]]
      
      # --------------------------
      # CHARACTER
      # --------------------------
      if(is.character(x)){
        
        n98 <- sum(x == "98", na.rm = TRUE)
        n99 <- sum(x == "99", na.rm = TRUE)
        
        if(n98 > 0){
          x[x == "98"] <- NA
          add_log(v, " : 98 -> NA (", n98, ")")
        }
        
        if(n99 > 0){
          x[x == "99"] <- NA
          add_log(v, " : 99 -> NA (", n99, ")")
        }
        
        dat[[v]] <- x
      }
      
      # --------------------------
      # NUMERIC
      # --------------------------
      if(is.numeric(x)){
        m <- max(x, na.rm = T)
        if(m >=10){
        NAvals <- c(paste0(rep("9", str_length(as.character(m))), collapse = ""), 
                    paste0(c(rep("9", str_length(m)-2), "98"), collapse = ""))
        NAvals <- as.numeric(NAvals)
        if(any(x %in% c(NAvals))){
        # on prend uniquement les codes plausibles 89/99 
        n98 <- sum(x == NAvals[2], na.rm = TRUE)
        n99 <- sum(x == NAvals[1], na.rm = TRUE)
        
        if(n98 > 0){
          x[x == NAvals[2]] <- NA
          add_log(v, " : 98 -> NA (", n98, ")")
        }
        
        if(n99 > 0){
          x[x == NAvals[1]] <- NA
          add_log(v, " : 99 -> NA (", n99, ")")
        }
        }
        dat[[v]] <- x
      }
      }
    }
    
    assign(nm, dat)
  }
      
  
  
  # ------------------------------------------------------------------
  # Revalorisation IPC
  # ------------------------------------------------------------------
  
  IPC2017 <- 84.45
  IPC2011 <- 80.97
  
  coef_ipc <- IPC2017 / IPC2011
  
  labels_2011 <- sapply(
    df_2011,
    function(x) attr(x, "label")
  )
  
  labels_clean <- tolower(
    ifelse(is.na(labels_2011), "", labels_2011)
  )
  
  vars_revalo <- names(df_2011)[
    sapply(df_2011, is.numeric) &
      (
        str_detect(names(df_2011), "^REV|^C0|^C1|^CTOT") |
          str_detect(labels_clean, "montant")
      ) &
      !str_detect(labels_clean, "tranche")
  ]
  
  add_log("## Revalorisation IPC")
  add_log("")
  add_log(
    "Coefficient appliqué : ",
    round(coef_ipc, 6)
  )
  add_log("")
  add_log(
    "Nombre de variables revalorisées : ",
    length(vars_revalo)
  )
  add_log("")
  
  summaries_before <- lapply(
    df_2011[vars_revalo],
    summary
  )
  
  df_2011 <- df_2011 %>%
    mutate(
      across(
        all_of(vars_revalo),
        ~ round(.x * coef_ipc, 0)
      )
    )
  
  summaries_after <- lapply(
    df_2011[vars_revalo],
    summary
  )
  summaries_2017 <- lapply(
    df_2017 %>% select(any_of(vars_revalo)),
    summary
  )
  
  for(v in vars_revalo){
    
    add_log("### ", v)
    
    add_log(
      "Label : ",
      labels_2011[v]
    )
    
    add_log(
      "2011 avant revalo : ",
      paste(
        names(summaries_before[[v]]),
        round(summaries_before[[v]], 2),
        collapse = " | "
      )
    )
    
    add_log(
      "2011 après revalo : ",
      paste(
        names(summaries_after[[v]]),
        round(summaries_after[[v]], 2),
        collapse = " | "
      )
    )
    
    if(v %in% names(df_2017)){
    add_log(
      "2017 (sans revalo) : ",
      paste(
        names(summaries_2017[[v]]),
        round(summaries_2017[[v]], 2),
        collapse = " | "
      )
      
    )} else {
      add_log(
      "2017 (sans revalo) : Variable absente")}
    
    add_log("")
  }
  log_txt
  # ------------------------------------------------------------------
  # Traitements spécifiques à la table
  # ------------------------------------------------------------------
  
  if(str_detect(name, "MENAGE")){
    
    add_log("## Traitements spécifiques MENAGE")
    add_log("")
    add_log("- renommage RSA_ACT -> PPA")
    add_log("- renommage I_RSA_ACT -> I_PPA")
    add_log("- calcul REVDISP")
    add_log("- calcul NIVIE")
    add_log("")
    
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
  
  # ------------------------------------------------------------------
  # Empilement
  # ------------------------------------------------------------------

  df_2017 <- df_2017 %>%
    mutate(
      annee_BDF = 2017,
      IDENT_MEN = paste0("17_", IDENT_MEN)
    )
  if("IDENT_IND" %in% names(df_2017)) {
    df_2017 <- df_2017 %>%
      mutate(
        IDENT_IND = paste0("17_", IDENT_IND))
  }
  
  df_2011 <- df_2011 %>%
    mutate(
      annee_BDF = 2011,
      IDENT_MEN = paste0("11_", IDENT_MEN)
    )
  if("IDENT_IND" %in% names(df_2011)) {
    df_2011 <- df_2011 %>%
      mutate(
        IDENT_IND = paste0("11_", IDENT_IND))
  }
  
  df_final <- bind_rows(df_2011, df_2017)
  df_final <- copy_labels(df_2017, df_final)
  
  write_sas(
    df_final,
    file.path(path_out, name)
  )
  
  # ------------------------------------------------------------------
  # Résumé final
  # ------------------------------------------------------------------
  
  add_log("## Résultat final")
  add_log("")
  
  add_log(
    "Nombre lignes final : ",
    nrow(df_final)
  )
  
  add_log(
    "Nombre variables final : ",
    ncol(df_final)
  )
  
  add_log(
    "Fichier écrit : ",
    file.path(path_out, name)
  )
  
  # ------------------------------------------------------------------
  # Sauvegarde du rapport
  # ------------------------------------------------------------------
  
  writeLines(
    log_txt,
    file.path(
      path_log,
      paste0(
        tools::file_path_sans_ext(name),
        ".txt"
      )
    )
  )
  
  invisible(NULL)
}


process_file(file_2017 = files_2017[14])
walk(files_2017[c(8, 10, 11:14, 16:19)], process_file)
