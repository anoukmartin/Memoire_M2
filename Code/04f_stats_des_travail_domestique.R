
# AIDEENFANT L'individu a aidé les enfants dans leurs devoirs pendant la semaine précédant l'entretien 
# BRICOLAGE L'individu a fait du bricolage pendant la semaine précédant l'entretien 
#CHANGEENFANT L'individu a changé les enfants, les a habillés ou leur a donné à manger pendant la semaine précédant l'entretien 
# COURSES L'individu a fait des courses pendant la semaine précédant l'entretien 
# CUISINEA L'individu a fait de la cuisine de tous les jours pendant la semaine précédant l'entretien 
# CUISINEB L'individu a fait de la cuisine de réception pendant la semaine précédant l'entretien 
# MENAGE L'individu a fait le ménage pendant la semaine précédant l'entretien 
# REPASSAGE L'individu a fait du repassage pendant la semaine précédant l'entretien 
# VAISSELLE L'individu a fait la vaisselle pendant la semaine précédant l'entretien \
# 


# dONN2ES SUR LES COUPLES 
familles <- readRDS("Data_output/familles_parents.Rds") 
  #filter(TYPMEN5 %in% c("Couple avec au moins un enfant", "Famille monoparentale"))
dim(familles)

# Données ur le travail domestique
travail_domestique <- c("AIDEENFANT", "BRICOLAGE", "CHANGEENFANT", 
                        "COURSES", "CUISINEA", "CUISINEB", "MENAGE", 
                        "JARDINAGE", "REPASSAGE", "VAISSELLE")

dep_ind <- readRDS("Data_output/DepIndiv.Rds") %>%
  var_IDENTIFIANT(IdentIndiv = "NOI", IdentMenage = "IDENT_MEN", 
                  NewVarName = "n_IdentIndiv") %>%
  select(n_IdentIndiv, all_of(travail_domestique))
travail_domestique <- str_to_sentence(travail_domestique)
travail_domestique[c(1,3, 5,6)] <- c("Aide scolaire aux enfants",
                                     "Habillage des enfants", 
                                     "Cuisine du quotidien", 
                                     "Cuisine de récéption")

names(dep_ind)[-1] <- travail_domestique
levels(familles$n_TYPMEN_new)
# Données sur les individus  
parents <- readRDS("Data_output/parents.Rds") %>%
  filter(n_IdentIndiv %in% c(familles$n_IdentIndiv_F, familles$n_IdentIndiv_H)) %>%
  left_join(familles %>%
              mutate(var = case_when(
                n_TYPMEN_new == "Traditionnelle" ~ "Mère et père en couple traditionnel", 
                n_TYPMEN_new == "Recomposée" & n_TYPMEN_sexe == "Mère et père en couple" ~ "Mère et père en couple recomposé", 
                n_TYPMEN_new == "Recomposée" & n_TYPMEN_sexe == "Mère en couple" ~ "Mère en couple recomposé", 
                n_TYPMEN_new == "Recomposée" & n_TYPMEN_sexe == "Père en couple" ~ "Père en couple recomposé", 
                n_TYPMEN_new == "Autre" ~ "Autre",
                TRUE ~ n_TYPMEN_sexe),) %>%
              select(IDENT_MEN, n_TYPMEN_new, var), 
            by = "IDENT_MEN") %>%
  left_join(dep_ind, by = "n_IdentIndiv") 
# Modules sur le W domestique qui n'est posé qu'à la moitiée de l'échantillon de ménage, (FA de numéro pairs, mais on a pas cet identifiant, donc on )
freq(parents$n_TYPMEN_new)
freq(parents$var)
parents <- parents %>%
  filter(eval(parse(text = paste0("`", 
                                  travail_domestique, 
                                  "` != ''", 
                                  collapse = " | ")))) %>%
  mutate_at(.vars = vars(travail_domestique), 
            .funs = function(x){
              x %>%
                fct_recode(
                  NULL = "", 
                  "Oui" = "1", 
                  "Non" = "2", 
                  NULL = "8", 
                  NULL = "9") %>%
                as.character()
            })

summary(parents$PONDIND)
parents$PONDIND <- parents$PONDIND/mean(parents$PONDIND)
freq(parents$n_BeauxEnfantsHD)
freq(parents$`Aide scolaire aux enfants`)



#levels(tab$n_BeauxEnfantsMen) <- c("Parent fam rec", "Beau-Parent", "Parent fam trad")
data <- parents %>%
  filter(n_TYPMEN_new %in% c("Monoparentale", "Traditionnelle", "Recomposée")) %>%
  mutate(n_BeauxEnfantsMen = case_when(
    is.na(n_BeauxEnfantsHD) ~ "Parent fam trad", 
    n_BeauxEnfantsMen ~ "Beau parent", 
    TRUE ~ "Parent fam rec"
  )) %>%
  rec_SEXE() %>%
  mutate(n_TYPMEN_new = n_TYPMEN_new %>% 
           droplevels()) %>%
  as_survey_design(weights = PONDIND)

parents$PONDIND <- parents$PONDIND/mean(parents$PONDIND)

tab <- tbl_strata(
  data = data, 
  strata = SEXE, 
  .tbl_fun =
    ~ .x %>%
    mutate(Effectifs = "1") %>%
    tbl_svysummary(by = n_TYPMEN_new, 
                   include = c(travail_domestique, "Effectifs"),
                   missing = "no", 
                   type = list(c(travail_domestique, "Effectifs") ~ "dichotomous"), 
                   statistic = list(all_dichotomous() ~ "{p}", 
                                    Effectifs ~ "{N_unweighted}"),
                   value = list(travail_domestique ~ "Oui", 
                                Effectifs ~ "1")
                   ) %>%
    add_p()%>%
    #add_difference() %>%
    #add_overall(last = T) %>%
    add_stat_label(label = list(all_dichotomous() ~ "", 
                                Effectifs ~ "(non-pondérés)")) %>%
    modify_header(all_stat_cols() ~ "**{level}**\n({style_percent(p)}%)", 
                  label ~ ""),
  .header = "**{strata}**\n({style_percent(p)}%)") 

tab

margev <- data %>%
  mutate(Effectifs = "1") %>%
  tbl_svysummary(include = c(travail_domestique, "Effectifs"),
                 missing = "no",  
                 type = list(c(travail_domestique, Effectifs) ~ "dichotomous"), 
                 statistic = list(all_dichotomous() ~ "{p}", 
                                  Effectifs ~ "{n_unweighted}"),
                 value = list(travail_domestique ~ "Oui", 
                              Effectifs ~ "1")
    ) %>%
  
  add_stat_label(label = list(all_dichotomous() ~ "", 
                              Effectifs ~ "(non-pondérés)")) %>%
  modify_header(all_stat_cols() ~ "**Ensemble**", 
                label = "")
  
margev


tab <- tbl_merge(list(tab, margev), 
          tab_spanner = FALSE)
tab          

saveTableau(tab, 
            type = "tab", 
            label = "Wdom_typefam_sexe", 
            description = "Travail domestique réalisé en fonction du type de famille et du sexe de l'individu",
            n = margev$N, 
            champ = "Individus adultes agés de 25 à 65 ans ou en couple avec un adulte agé de 25 à 65 ans, formant des ménages ordinaires avec au moins un enfant.", 
            ponderation = T)




# Tableaux parents dans différentes familles 
freq(data$variables$n_EnfantsMen)
table(data$variables$n_EnfantsMen, data$variables$n_TYPMEN_new, useNA = "ifany")

data2 <- parents %>%
  mutate(n_ParentTypFam = case_when(
    n_TYPMEN_new == "Traditionnelle" ~ "Parent en famille traditionnelle", 
    n_TYPMEN_new == "Monoparentale" ~ "Parent en famille monoparentale",
    n_TYPMEN_new == "Recomposée" & n_EnfantsMen ~ "Parent en famille recomposée", 
    n_TYPMEN_new == "Recomposée" & !n_EnfantsMen ~ "Beau-parent sans enfants en famille recomposée")) %>%
  mutate(n_EnfantsMen = case_when(
    n_EnfantsMen ~ "Parent", 
    !n_EnfantsMen | is.na(n_EnfantsMen) ~ "Adulte sans enfants"
  )) %>%
  filter(n_EnfantsMen == "Parent") %>%
  mutate(n_TYPMEN_new  = n_TYPMEN_new %>% as.factor() %>%droplevels())

data2$PONDIND <- data2$PONDIND/mean(data2$PONDIND)

tab <- data2 %>%
  as_survey_design(weights = PONDIND) %>%
  mutate(Effectifs = "1") %>%
  tbl_svysummary(by = n_TYPMEN_new, 
                  include = c(travail_domestique, "Effectifs"),
                  missing = "no", 
                  type = list(everything() ~ "dichotomous"), 
                  statistic= list(travail_domestique ~ "{p}", 
                                  Effectifs ~ "{N_unweighted}"),
                   value = list(everything() ~ "Oui", 
                                Effectifs ~ "1")) %>%
  modify_header(all_stat_cols() ~ "**{level}**\n({style_percent(p)}%)", 
                label ~ "") %>%
  add_overall(last = T, col_label = "**Ensemble**") %>%
  add_p() %>%
  add_stat_label(label = list(all_dichotomous() ~ " %", 
                              Effectifs ~ "(non-pondérés)")) 


tab

list.files("Resultats")
saveTableau(tableau = tab,
            type = "tab", 
            label = "Wdom_parents", 
            description = "Travail domestique des parent en fonction de la configuration familiale du ménage", 
            champ = "Parents agés de 25 à 65 ans ou en couple avec un adulte agé de 25 à 65 ans, formant des ménages ordinaires et vivant avec au moins un de leurs enfants", 
            ponderation = T,
            n = tab$N)

## Tableau prent dans les différentes configurations parentales 

freq(data$variables$n_EnfantsMen)
table(data$variables$n_EnfantsMen, data$variables$n_TYPMEN_new, useNA = "ifany")

data2 <- parents %>%
  mutate(n_ParentTypFam = case_when(
    n_TYPMEN_new == "Traditionnelle" ~ "Parent en famille traditionnelle", 
    n_TYPMEN_new == "Monoparentale" ~ "Parent en famille monoparentale",
    n_TYPMEN_new == "Recomposée" & n_EnfantsMen ~ "Parent en famille recomposée", 
    n_TYPMEN_new == "Recomposée" & !n_EnfantsMen ~ "Beau-parent sans enfants en famille recomposée")) %>%
  mutate(n_EnfantsMen = case_when(
    n_EnfantsMen ~ "Parent", 
    !n_EnfantsMen | is.na(n_EnfantsMen) ~ "Adulte sans enfants"
  )) %>%
  filter(n_EnfantsMen == "Parent") %>%
  #filter(n_TYPMEN_new != "Autre") %>%
  mutate(var  = var %>% as.factor() %>%droplevels())

data2$PONDIND <- data2$PONDIND/mean(data2$PONDIND)

tab <- data2 %>%
  filter(SEXE == "2") %>%
  filter(str_detect(var, "mère") | str_detect(var, "Mère") ) %>%
  mutate(var = droplevels(var)) %>%
  as_survey_design(weights = PONDIND) %>%
  mutate(Effectifs = "1") %>%
  tbl_svysummary(by = var, 
                 include = c(travail_domestique, "Effectifs"),
                 missing = "no", 
                 type = list(everything() ~ "dichotomous"), 
                 statistic= list(travail_domestique ~ "{p}", 
                                 Effectifs ~ "{N_unweighted}"),
                 value = list(everything() ~ "Oui", 
                              Effectifs ~ "1")) %>%
  modify_header(all_stat_cols() ~ "**{level}**\n({style_percent(p)}%)", 
                label ~ "") %>%
  add_overall(last = T, col_label = "**Ensemble**") %>%
  add_p() %>%
  add_stat_label(label = list(all_dichotomous() ~ " %", 
                              Effectifs ~ "(non-pondérés)")) 


tab

list.files("Resultats")
saveTableau(tableau = tab,
            type = "tab", 
            label = "Wdom_parentsconfig", 
            description = "Travail domestique des parent en fonction de la configuration familiale du ménage", 
            champ = "Parentx agés de 25 à 65 ans ou en couple avec un adulte agé de 25 à 65 ans, formant des ménages ordinaires et vivant avec au moins un de leurs enfants", 
            ponderation = T,
            n = tab$N)
            
# Tableau beaux-pères, belles-mpères, pères, mères ##############################
freq(parents$n_EnfantsMen)

## Données ####
### Données adultes recomposées ####
data_recomp <- parents %>%
  filter(n_TYPMEN_new == "Recomposée")
data_recomp$PONDIND <- data_recomp$PONDIND/mean(data_recomp$PONDIND)

### Données parents ####
data_parents <- parents %>%
  filter(n_TYPMEN_new == "Recomposée" & n_EnfantsMen) %>%
  rec_SEXE() %>%
  mutate(SEXE = SEXE %>%
           fct_recode(
             "Mère" = "Femme",
             "Père" = "Homme"))
freq(data_parents$SEXE)
summary(data_parents$PONDMEN)
data_parents$PONDMEN <- data_parents$PONDMEN/mean(data_parents$PONDMEN)
  
### Données beaux parents ####
data_beauxparents <- parents %>%
  filter(n_TYPMEN_new == "Recomposée" & n_BeauxEnfantsMen) %>%
  rec_SEXE() %>%
  mutate(SEXE = SEXE %>%
           fct_recode(
             "Belle-mère" = "Femme",
             "Beau-père" = "Homme"))
freq(data_beauxparents$SEXE) 
summary(data_beauxparents$PONDMEN)
data_beauxparents$PONDMEN <- data_beauxparents$PONDMEN/mean(data_beauxparents$PONDMEN)

## Tableaux croisés #####

tab <- data_recomp %>%
  rec_SEXE() %>%
  mutate(n_Enfants = case_when(
    n_EnfantsMen & n_BeauxEnfantsMen ~ "Avec enfants et beaux-enfants",
    n_EnfantsMen ~ "Avec enfants",
    n_BeauxEnfantsMen ~ "Avec beaux-enfants")) %>%
  # mutate(n_Enfants = n_Enfants %>%
  #          )
  as_survey_design(weights = PONDIND) %>%
  tbl_strata(
    strata = "SEXE",
    .tbl_fun =
      ~ .x %>%
      mutate(Effectifs = "1") %>%
      tbl_svysummary(by = n_Enfants,
                     include = c(travail_domestique, "Effectifs"),
                     missing = "no",
                     type = list(c(travail_domestique, "Effectifs") ~ "dichotomous"),
                     statistic = list(all_dichotomous() ~ "{p}",
                                      Effectifs ~ "{N_unweighted}"),
                     value = list(travail_domestique ~ "Oui", 
                                  Effectifs ~ "1")
      ) %>%
      add_p()%>%
      #add_difference() %>%
      #add_overall(last = T) %>%
      add_stat_label(label = list(all_dichotomous() ~ "",
                                  Effectifs ~ "(non-pondérés)")) %>%
      modify_header(all_stat_cols() ~ "**{level}**\n({style_percent(p)}%)",
                    label ~ ""),
    .header = "**{strata}**\n({style_percent(p)}%)")
tab

### tableau parents #####
tab1 <- data_parents %>%
  as_survey_design(weights = PONDMEN) %>%
  mutate(Effectifs = "1") %>%
  tbl_svysummary(by = SEXE, 
                 include = c(travail_domestique, "Effectifs"),
                 missing = "no", 
                 type = list(everything() ~ "dichotomous"), 
                 statistic= list(travail_domestique ~ "{p}", 
                                 Effectifs ~ "{N_unweighted}"),
                 value = list(everything() ~ "Oui", 
                              Effectifs = "1")) %>%
  #add_overall(last = T) %>%
  add_p() %>%
  add_stat_label(label = list(all_dichotomous() ~ "", 
                              Effectifs ~ "(non-pondérés)")) %>%
  modify_header(all_stat_cols() ~ "**{level}** ({style_percent(p)}%)") 
tab1

### tableau beaux parents ####

tab2 <- data_beauxparents %>%
  as_survey_design(weights = PONDMEN) %>%
  mutate(Effectifs = "1") %>%
  tbl_svysummary(by = SEXE, 
                 include = c(travail_domestique, "Effectifs"),
                 missing = "no", 
                 type = list(everything() ~ "dichotomous"), 
                 statistic= list(travail_domestique ~ "{p}", 
                                 Effectifs ~ "{N_unweighted}"),
                 value = list(everything() ~ "Oui", 
                              Effectifs = "1")) %>%
  #add_overall(last = T) %>%
  add_p() %>%
  add_stat_label(label = list(all_dichotomous() ~ "", 
                              Effectifs ~ "(non-pondérés)")) %>%
  modify_header(all_stat_cols() ~ "**{level}** ({style_percent(p)}%)") 
tab2

### Marge verticale ####

margev <- data_recomp %>%
  as_survey_design(weights = PONDMEN) %>%
  mutate(Effectifs = "1") %>%
  tbl_svysummary(include = c(travail_domestique, "Effectifs"),
                 missing = "no", 
                 type = list(everything() ~ "dichotomous"), 
                 statistic= list(travail_domestique ~ "{p}", 
                                 Effectifs ~ "{N_unweighted}"),
                 value = list(everything() ~ "Oui", 
                              Effectifs ~ "1")) %>%
  add_stat_label(label = list(all_dichotomous() ~ "", 
                              Effectifs ~ "(non-pondérés)")) %>%
  modify_header(all_stat_cols() ~ "**{level}** ({style_percent(p)}%)") 
margev

# calcul % des parents et beaux parents 
#P
f_parents <- wtd.table(data_recomp$n_EnfantsMen, 
                       weights = data_recomp$PONDMEN) %>%
  as.data.frame() 
f_parents <- f_parents %>%
  mutate(Prop = round((Freq/sum(f_parents$Freq))*100, 0))
f_parents

# BP
f_bp <- wtd.table(data_recomp$n_BeauxEnfantsMen, 
                       weights = data_recomp$PONDMEN) %>%
  as.data.frame() 
f_bp <- f_bp %>%
  mutate(Prop = round((Freq/sum(f_parents$Freq))*100, 0))
f_bp

tab3 <- tbl_merge(list(tab1, tab2, margev), 
          tab_spanner = c(paste0("**Parents** (", f_parents$Prop[2], "%)"), 
                          paste0("**Beaux-parents** (", f_bp$Prop[2], "%)"), 
                          NA))
tab3
### Enregistrement ####
list.files("Resultats")
saveTableau(tableau = tab3, 
            type = "tab", 
            label = "Wdom_PBP_sexe", 
            description = "Travail domestique réalisé en fonction du statut parental et du sexe", 
            ponderation = T, 
            champ = "Individus adultes agés de 25 à 65 ans ou en couple avec un adulte agé de 25 à 65 ans, formant des ménages ordinaires recomposés", 
            n = nrow(data_recomp))





## Regression avec effet fixe du ménage ########################################

### Données finales sur lesquelles on fait les regressions######################

data0 <- parents 
freq(data0$n_TYPMEN_new)
data0 <- data0 %>%
  rec_SEXE() %>%
  filter(n_TYPMEN_new %in% c("Traditionnelle", "Recomposée")) %>%
  mutate(n_TYPMEN_new = n_TYPMEN_new %>% droplevels()) %>%
  mutate_at(.vars = travail_domestique, 
            .funs = function(x){
              if_else(x == "Oui", 1, 0)
            }) %>%
  mutate_at(.vars = c("n_EnfantsMen", "n_BeauxEnfantsMen"), 
            .funs = function(x){
              if_else(x, "Oui", "Non") %>%
                as_factor() %>%
                fct_relevel("Oui")
            })
            

data0$n_BeauxEnfantsMen
data0$SEXE
### regression #################################################################

## Régressions revenus HF sur tous les postes budgétaires #######################
row.names(freq(data0$Vaisselle))
names(data0)
lapply(travail_domestique, function(x){
  barplot(freq(data0[[x]])$n,
          names.arg=row.names(freq(data0[[x]])),
  main = x)
  })

# Installer et charger le package fixest si nécessaire
#install.packages("fixest")
#library(fixest)
# 
# # Modèle avec fixest
# model <- glm(Menage ~ n_EnfantsMen:SEXE + n_BeauxEnfantsMen:SEXE,
#                data=data0,
#                weights = data0$PONDIND, 
#                family = "quasibinomial")
# 
# # Résumé du modèle
# summary(model)


### Regression simple sans interactions ####
# create regression function
# fitreg <- function(x){ 
#   feglm.fit(get(x) ~ n_EnfantsMen*SEXE + n_BeauxEnfantsMen*SEXE | IDENT_MEN,
#           data=data0,
#           weights = data0$PONDIND, 
#           family = "quasibinomial")}
# 
# 
# # on applique 
# results <- lapply(travail_domestique, fitreg)
# names(results) <- travail_domestique
# 
# ggcoef_compare(results)


# On regarder les résidus 
lapply(varlist, function(x) {
  plot(density(residuals(results[[x]])), 
       main = x)})
Sys.sleep((1/60)*2)





# MEME CHOSE VERSION NOMBRE DE JOURS ###########################################



# Données ur le travail domestique
travail_domestique <- c("AIDEENFANT", "BRICOLAGE", "CHANGEENFANT", 
                        "COURSES", "CUISINEA", "CUISINEB", "MENAGE", 
                        "JARDINAGE", "REPASSAGE", "VAISSELLE")
travail_domestique <- paste0("NB", travail_domestique)

dep_ind <- readRDS("Data_output/DepIndiv.Rds") %>%
  var_IDENTIFIANT(IdentIndiv = "NOI", IdentMenage = "IDENT_MEN", 
                  NewVarName = "n_IdentIndiv") %>%
  select(n_IdentIndiv, all_of(travail_domestique))

travail_domestique <- travail_domestique %>%
  str_remove("NB") %>%
  str_to_sentence()
travail_domestique
travail_domestique[c(1,3, 5,6)] <- c("Aide scolaire aux enfants",
                                     "Habillage des enfants", 
                                     "Cuisine du quotidien", 
                                     "Cuisine de récéption")

names(dep_ind)[-1] <- travail_domestique



# Données sur les individus  
parents <- parents  %>%
  rename_at(.vars = vars(travail_domestique), 
            .funs = function(x){paste0("I_", x)
            })
names(parents)
freq(parents$I_Menage)
parents <- parents %>%
  left_join(dep_ind, by = "n_IdentIndiv") 
freq(parents$Menage)
for(k in travail_domestique){
  parents[, k][
    (parents[[paste0("I_", k)]] == "Non" 
      & !is.na(parents[[paste0("I_", k)]]) 
      & (parents[[k]] == 9 | is.na(parents[[k]]))), 
    ] <- 0
}

freq(parents$`Habillage des enfants`)
parents <- parents %>%
  mutate_at(.vars = vars(travail_domestique), 
            .funs = function(x){
              case_when(x == 9  ~ NA, 
                        TRUE ~ x)})

summary(parents$PONDIND)
parents$PONDIND <- parents$PONDIND/mean(parents$PONDIND)
freq(parents$n_BeauxEnfantsHD)
freq(parents$`Aide scolaire aux enfants`)
freq(parents$Menage)
summary(parents$Menage)

#levels(tab$n_BeauxEnfantsMen) <- c("Parent fam rec", "Beau-Parent", "Parent fam trad")
data <- parents %>%
  mutate(n_BeauxEnfantsMen = case_when(
    is.na(n_BeauxEnfantsHD) ~ "Parent fam trad", 
    n_BeauxEnfantsMen ~ "Beau parent", 
    TRUE ~ "Parent fam rec"
  )) %>%
  rec_SEXE() %>%
  mutate(n_TYPMEN_new = n_TYPMEN_new %>% 
           droplevels()) %>%
  as_survey_design(weights = PONDIND)

tab <- tbl_strata(
  data = data, 
  strata = SEXE, 
  .tbl_fun =
    ~ .x %>%
    mutate(Effectifs = "1") %>%
    tbl_svysummary(by = n_TYPMEN_new, 
                   include = c(travail_domestique, "Effectifs"),
                   missing = "no", 
                   value = list(Effectifs ~ "1"), 
                   type = list("Effectifs" ~ "dichotomous", 
                               travail_domestique ~ "continuous"), 
                   statistic = list(all_continuous() ~ "{mean}", 
                                    Effectifs ~ "{N_unweighted}")
    ) %>%
    add_p()%>%
    #add_difference() %>%
    #add_overall(last = T) %>%
    add_stat_label(label = list(all_continuous() ~ "", 
                                Effectifs ~ "(non-pondérés)")) %>%
    modify_header(all_stat_cols() ~ "**{level}**\n({style_percent(p)}%)", 
                  label ~ ""),
  .header = "**{strata}**\n({style_percent(p)}%)") 

tab
margev <- data %>%
  mutate(Effectifs = "1") %>%
  tbl_svysummary(include = c(travail_domestique, "Effectifs"),
                 missing = "no", 
                 type = list("Effectifs" ~ "dichotomous", 
                             travail_domestique ~ "continuous"), 
                 value = list(Effectifs ~ "1"), 
                 statistic = list(all_continuous() ~ "{mean}", 
                                  Effectifs ~ "{N_unweighted}")
  ) %>%
  #add_difference() %>%
  #add_overall(last = T) %>%
  add_stat_label(label = list(all_continuous() ~ "", 
                              Effectifs ~ "(non-pondérés)")) %>%
  modify_header(all_stat_cols() ~ "**{level}**\n({style_percent(p)}%)", 
                label ~ "")
margev
margev$table_body$label <- c(travail_domestique, "Effectifs")


tab <- tbl_merge(list(tab, margev), 
                 tab_spanner = FALSE)
tab          

saveTableau(tab, 
            type = "tab", 
            label = "Wdom_typefam_sexe_nbjours", 
            description = "Travail domestique réalisé en fonction du type de famille et du sexe de l'individu",
            n = margev$N, 
            champ = "Individus adultes agés de 25 à 65 ans ou en couple avec un adulte agé de 25 à 65 ans, formant des ménages ordinaires avec au moins un enfant.", 
            ponderation = T)




# Tableaux beaux-parents vs parents 
freq(data$variables$n_EnfantsMen)
table(data$variables$n_EnfantsMen, data$variables$n_TYPMEN_new, useNA = "ifany")

data2 <- parents %>%
  mutate(n_ParentTypFam = case_when(
    n_TYPMEN_new == "Traditionnelle" ~ "Parent en famille traditionnelle", 
    n_TYPMEN_new == "Monoparentale" ~ "Parent en famille monoparentale",
    n_TYPMEN_new == "Recomposée" & n_EnfantsMen ~ "Parent en famille recomposée", 
    n_TYPMEN_new == "Recomposée" & !n_EnfantsMen ~ "Beau-parent sans enfants en famille recomposée")) %>%
  as_survey_design(weights = PONDMEN)



tab <- data2 %>%
  mutate(Effectifs = "1") %>%
  tbl_svysummary(by = n_ParentTypFam, 
                 include = c(travail_domestique, "Effectifs"),
                 missing = "no", 
                 type = list(everything() ~ "continuous", 
                             Effectifs ~ "dichotomous"), 
                 statistic= list(travail_domestique ~ "{mean}", 
                                 Effectifs ~ "{N_unweighted}"),
                 value = list(everything() ~ "Oui", 
                              Effectifs ~ "1")) %>%
  add_overall(last = T) %>%
  add_p() %>%
  add_stat_label(label = list(all_continuous() ~ "", 
                              Effectifs ~ "(non-pondérés)")) %>%
  modify_header(all_stat_cols() ~ "**{level}**\n({style_percent(p)}%)", 
                label ~ "")


tab$table_body$label <- c(travail_domestique, "Effectifs")
tab
list.files("Resultats")
saveTableau(tableau = tab,
            type = "tab", 
            label = "Wdom_parents_beauxparents_nbjours", 
            description = "Travail domestique en fonction du rôle familial", 
            champ = "Individus adultes agés de 25 à 65 ans ou en couple avec un adulte agé de 25 à 65 ans, formant des ménages ordinaires avec au moins un enfant", 
            ponderation = T,
            n = tab$N)

# Tableau beaux-pères, belles-mpères, pères, mères ##############################
freq(parents$n_EnfantsMen)

## Données ####
### Données adultes recomposées ####
data_recomp <- parents %>%
  filter(n_TYPMEN_new == "Recomposée")
data_recomp$PONDMEN <- data_recomp$PONDMEN/mean(data_recomp$PONDMEN)

### Données parents ####
data_parents <- parents %>%
  filter(n_TYPMEN_new == "Recomposée" & n_EnfantsMen) %>%
  rec_SEXE() %>%
  mutate(SEXE = SEXE %>%
           fct_recode(
             "Mère" = "Femme",
             "Père" = "Homme"))
freq(data_parents$SEXE)
summary(data_parents$PONDMEN)
data_parents$PONDMEN <- data_parents$PONDMEN/mean(data_parents$PONDMEN)

### Données beaux parents ####
data_beauxparents <- parents %>%
  filter(n_TYPMEN_new == "Recomposée" & n_BeauxEnfantsMen) %>%
  rec_SEXE() %>%
  mutate(SEXE = SEXE %>%
           fct_recode(
             "Belle-mère" = "Femme",
             "Beau-père" = "Homme"))
freq(data_beauxparents$SEXE) 
summary(data_beauxparents$PONDMEN)
data_beauxparents$PONDMEN <- data_beauxparents$PONDMEN/mean(data_beauxparents$PONDMEN)

## Tableaux croisés #####

### tableau parents #####
tab1 <- data_parents %>%
  as_survey_design(weights = PONDMEN) %>%
  mutate(Effectifs = "1") %>%
  tbl_svysummary(by = SEXE, 
                 include = c(travail_domestique, "Effectifs"),
                 missing = "no", 
                 type = list(everything() ~ "continuous", 
                             Effectifs ~ "dichotomous"), 
                 value = list(Effectifs ~ "1"), 
                 statistic= list(travail_domestique ~ "{mean}", 
                                 Effectifs ~ "{N_unweighted}")) %>%
  #add_overall(last = T) %>%
  add_p() %>%
  add_stat_label(label = list(all_continuous() ~ "", 
                              Effectifs ~ "(non-pondérés)")) %>%
  modify_header(all_stat_cols() ~ "**{level}** ({style_percent(p)}%)", 
                label ~ "") 
tab1

### tableau beaux parents ####

tab2 <- data_beauxparents %>%
  as_survey_design(weights = PONDMEN) %>%
  mutate(Effectifs = "1") %>%
  tbl_svysummary(by = SEXE, 
                 include = c(travail_domestique, "Effectifs"),
                 missing = "no", 
                 type = list(everything() ~ "continuous", 
                             Effectifs ~ "dichotomous"), 
                 statistic= list(travail_domestique ~ "{mean}", 
                                 Effectifs ~ "{N_unweighted}")) %>%
  #add_overall(last = T) %>%
  add_p() %>%
  add_stat_label(label = list(all_continuous() ~ "", 
                              Effectifs ~ "(non-pondérés)")) %>%
  modify_header(all_stat_cols() ~ "**{level}** ({style_percent(p)}%)", 
                label ~ "") 
tab2

### Marge verticale ####

margev <- data_recomp %>%
  as_survey_design(weights = PONDMEN) %>%
  mutate(Effectifs = "1") %>%
  tbl_svysummary(include = c(travail_domestique, "Effectifs"),
                 missing = "no", 
                 type = list(everything() ~ "continuous", 
                             Effectifs ~ "dichotomous"), 
                 statistic= list(travail_domestique ~ "{mean}", 
                                 Effectifs ~ "{N_unweighted}")) %>%
  #add_overall(last = T) %>%
  add_stat_label(label = list(all_continuous() ~ "", 
                              Effectifs ~ "(non-pondérés)")) %>%
  modify_header(all_stat_cols() ~ "**{level}** ({style_percent(p)}%)", 
                label ~ "") 
margev

# calcul % des parents et beaux parents 
#P
f_parents <- wtd.table(data_recomp$n_EnfantsMen, 
                       weights = data_recomp$PONDMEN) %>%
  as.data.frame() 
f_parents <- f_parents %>%
  mutate(Prop = round((Freq/sum(f_parents$Freq))*100, 0))
f_parents

# BP
f_bp <- wtd.table(data_recomp$n_BeauxEnfantsMen, 
                  weights = data_recomp$PONDMEN) %>%
  as.data.frame() 
f_bp <- f_bp %>%
  mutate(Prop = round((Freq/sum(f_parents$Freq))*100, 0))
f_bp

tab <- tbl_merge(list(tab1, tab2, margev), 
                 tab_spanner = c(paste0("**Parents** (", f_parents$Prop[2], "%)"), 
                                 paste0("**Beaux-parents** (", f_bp$Prop[2], "%)"), 
                                 NA))
tab$table_body$label <- c(travail_domestique, "Effectifs")
tab
### Enregistrement ####
list.files("Resultats")
saveTableau(tableau = tab, 
            type = "tab", 
            label = "Wdom_PBP_sexe_nbjours", 
            description = "Travail domestique réalisé en fonction du statut parental et du sexe", 
            ponderation = T, 
            champ = "Individus adultes agés de 25 à 65 ans ou en couple avec un adulte agé de 25 à 65 ans, formant des ménages ordinaires recomposés", 
            n = nrow(data_recomp))


# INEGALITE LW######################

familles <- readRDS("Data_output/familles_parents.Rds")
familles$`Cuisine du quotidien_F`
familles <- familles %>%
  mutate(Cuisine_ineg = `Cuisine du quotidien_F` - `Cuisine du quotidien_H`, 
         Vaisselle_ineg = `Vaisselle_F` - `Vaisselle_H`, 
         Courses_ineg = `Courses_F` - `Courses_H`, 
         AideEnfant_ineg = `Autre aide des enfants_F` - `Autre aide des enfants_H`)
summary(familles$Cuisine_ineg)
summary(familles$Courses_ineg)
summary(familles$Vaisselle_ineg)
summary(familles$AideEnfant_ineg)
summary(familles$`Autre aide des enfants_F`)


library(gridExtra)

data <- familles %>%
  filter(!is.na(couple)) %>%
  mutate(n_TYPMEN_new = droplevels(n_TYPMEN_new), 
         n_TYPMEN_sexe= droplevels(n_TYPMEN_sexe))

# Créer les graphiques
plot_cuisine <- ggplot(data, aes(x = n_TYPMEN_sexe, y = Cuisine_ineg, weight = PONDMEN)) +
  geom_bar(stat = "summary") +
  labs(title = "Cuisine", x = "Configuration familiale", y = "Inégalité") +
  theme_memoire()
plot_cuisine

plot_vaisselle <- ggplot(data, aes(x = n_TYPMEN_sexe, y = Vaisselle_ineg, weight = PONDMEN)) +
  geom_bar(stat = "summary", fun = "mean", aes(fill = n_TYPMEN_sexe)) +
  labs(title = "Vaisselle", x = "Configuration familiale", y = "Inégalité") +
  theme_minimal()
plot_vaisselle

plot_courses <- ggplot(data, aes(x = n_TYPMEN_sexe, y = Courses_ineg, weight = PONDMEN)) +
  geom_bar(stat = "summary", aes(fill = n_TYPMEN_sexe)) +
  labs(title = "Vaisselle", x = "Configuration familiale", y = "Inégalité") +
  theme_minimal()
plot_courses

plot_aideenfant <- ggplot(data, aes(x = n_TYPMEN_sexe, y = AideEnfant_ineg, weight = PONDMEN)) +
  geom_bar(stat = "summary", aes(fill = n_TYPMEN_sexe)) +
  labs(title = "Vaisselle", x = "Configuration familiale", y = "Inégalité") +
  theme_minimal()
plot_aideenfant

# Afficher côte à côte
grid.arrange(plot_cuisine, plot_vaisselle, ncol = 2)
