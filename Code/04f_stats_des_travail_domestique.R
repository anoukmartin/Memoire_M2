
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
familles <- readRDS("Data_output/familles_parents.Rds") %>%
  filter(TYPMEN5 %in% c("Couple avec au moins un enfant", "Famille monoparentale"))
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

# Données sur les individus  
parents <- readRDS("Data_output/parents.Rds") %>%
  filter(n_IdentIndiv %in% c(familles$n_IdentIndiv_F, familles$n_IdentIndiv_H)) %>%
  left_join(familles %>%
              select(IDENT_MEN, n_TYPMEN_new, n_FractionClasse), 
            by = "IDENT_MEN") %>%
  left_join(dep_ind, by = "n_IdentIndiv") 
# Modules sur le W domestique qui n'est posé qu'à la moitiée de l'échantillon de ménage, (FA de numéro pairs, mais on a pas cet identifiant, donc on )
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
  strata = n_TYPMEN_new, 
  .tbl_fun =
    ~ .x %>%
    mutate(Effectifs = "1") %>%
    tbl_svysummary(by = SEXE, 
                   include = c(travail_domestique, "Effectifs"),
                   missing = "no", 
                   type = list(c(travail_domestique, "Effectifs") ~ "dichotomous"), 
                   statistic = list(all_dichotomous() ~ "{p}", 
                                    Effectifs ~ "{N_unweighted}"),
                   value = list(travail_domestique ~ "Oui")
                   ) %>%
    #add_difference() %>%
    #add_overall(last = T) %>%
    add_stat_label(label = list(all_dichotomous() ~ "(%)", 
                                Effectifs ~ "(non-pondérés)")) %>%
    modify_header(all_stat_cols() ~ "**{level}**\n({style_percent(p)}%)"),
  .header = "**{strata}**\n({style_percent(p)}%)") 


margev <- data %>%
  mutate(Effectifs = "1") %>%
  tbl_svysummary(include = c(travail_domestique, "Effectifs"),
                 missing = "no", 
                 type = list(c(travail_domestique, Effectifs) ~ "dichotomous"), 
                 statistic = list(all_dichotomous() ~ "{p}", 
                                  Effectifs ~ "{n_unweighted}"),
                 value = list(travail_domestique ~ "Oui")
    ) %>%
  
  add_stat_label(label = list(all_dichotomous() ~ "(en %)", 
                              Effectifs ~ "(non-pondérés)")) %>%
  modify_header(all_stat_cols() ~ "**Ensemble**")
  
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




# Tableaux beaux-parents vs parents 
freq(data$variables$n_EnfantsMen)
table(data$variables$n_EnfantsMen, data$variables$n_TYPMEN_new, useNA = "ifany")

data2 <- parents %>%
  mutate(n_ParentTypFam = case_when(
    n_TYPMEN_new == "Traditionelle" ~ "Parent en famille traditionelle", 
    n_TYPMEN_new == "Monoparentale" ~ "Parent en famille monoparentale",
    n_TYPMEN_new == "Recomposée" & n_EnfantsMen ~ "Parent en famille recomposée", 
    n_TYPMEN_new == "Recomposée" & !n_EnfantsMen ~ "Beau-parent sans enfants en famille recomposée")) %>%
  as_survey_design(weights = PONDMEN)

      
                          
tab <- data2 %>%
  mutate(Effectifs = "1") %>%
  tbl_svysummary(by = n_ParentTypFam, 
                  include = c(travail_domestique, "Effectifs"),
                  missing = "no", 
                  type = list(everything() ~ "dichotomous"), 
                  statistic= list(travail_domestique ~ "{p}", 
                                  Effectifs ~ "{N_unweighted}"),
                   value = list(everything() ~ "Oui")) %>%
  add_overall(last = T) %>%
  add_p() %>%
  add_stat_label(label = list(all_dichotomous() ~ "(en %)", 
                              Effectifs ~ "(non-pondérés)")) %>%
  modify_header(all_stat_cols() ~ "**{level}** ({style_percent(p)}%)") 


tab
list.files("Resultats")
saveTableau(tableau = tab,
            type = "tab", 
            label = "Wdom_parents_beauxparents", 
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
                 type = list(everything() ~ "dichotomous"), 
                 statistic= list(travail_domestique ~ "{p}", 
                                 Effectifs ~ "{N_unweighted}"),
                 value = list(everything() ~ "Oui")) %>%
  #add_overall(last = T) %>%
  add_p() %>%
  add_stat_label(label = list(all_dichotomous() ~ "(en %)", 
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
                 value = list(everything() ~ "Oui")) %>%
  #add_overall(last = T) %>%
  add_p() %>%
  add_stat_label(label = list(all_dichotomous() ~ "(en %)", 
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
                 value = list(everything() ~ "Oui")) %>%
  add_stat_label(label = list(all_dichotomous() ~ "(en %)", 
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

tab <- tbl_merge(list(tab1, tab2, margev), 
          tab_spanner = c(paste0("**Parents** (", f_parents$Prop[2], "%)"), 
                          paste0("**Beaux-parents** (", f_bp$Prop[2], "%)"), 
                          NA))

### Enregistrement ####
list.files("Resultats")
saveTableau(tableau = tab, 
            type = "tab", 
            label = "Wdom_PBP_sexe", 
            description = "Travail domestique réalisé en fonction du statut parental et du sexe", 
            ponderation = T, 
            champ = "Individus adultes agés de 25 à 65 ans ou en couple avec un adulte agé de 25 à 65 ans, formant des ménages ordinaires recomposés", 
            n = nrow(data_recomp))

