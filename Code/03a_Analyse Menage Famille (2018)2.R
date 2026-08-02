##############################.
# 0. Packages
##############################.

library(dplyr)
library(tidyr)


#############################.
# 1. Données ####
#############################.
indiv <- readRDS("Data_output/indiv.Rds") 

indiv <- pad_2digits(
  indiv,
  c("NOI", "CONJOINT", "PER2E", "MER2E")
)



indiv[indiv == ""] <- NA
freq(indiv$NOI)
freq(indiv$CONJOINT)
unique(indiv$CONJOINT)
# Traitement des anomalies 
# Dans le ménage 17_08797 on a des liens familiaux chelous (deux conjoint pour une femme, dont le plus jeune est l'enfant de l'autre conjoint, et qui a le meme ages que ses frèrs et soeurs qui sont aussi les enfants de leur pères. Cela peu arriver, mais il nou semble plus probable qu'il y ai une erreur de codage 
anomalie <- filter(indiv, IDENT_MEN == "17_08797")
anomalie[1, "LIEN_03"] # indique un lien de conjugalité 
anomalie[1, "LIEN_03"] <- "32" # On replace par le code "Beau parent", comme pour les frères et soeurs de l'individu 03
anomalie[3, "LIEN_01"] # indique un lien de conjugalité 
anomalie[3, "LIEN_01"] <- "31" # On replace par le code "bel-enfant", comme pour les frères et soeurs de l'individu 03

indiv <- filter(indiv, IDENT_MEN != "17_08797")
indiv <- bind_rows(indiv, anomalie)
rm(anomalie)



###############################.
## Variables socio démo individuelles ####
###########################################.
freq(indiv$SITUA)
freq(indiv$STATUT)

indiv_socdem <- indiv %>%
  #filter(ENFANT == "2") %>%
  #filter(NONENFANT) %>%
  #rec_EXPART(Var = "EXPART") %>%
  rec_SITUA(Var = "SITUA") %>%
  rec_DIP(Var = "DIP14", NewVar = "DIPL") %>%
  rec_CSP6(Var = "CS24", NewVar = "CS6") %>%
  rec_CSP12(Var = "CS42", NewVar = "CS12") %>%
  rec_DIP7(Var = "DIP14", NewVar = "DIP7") %>%
  rec_AG6() %>%
  rec_TYPEMPLOI() %>%
  rec_STATUT() %>%
  # rec_NENFANTS(Var = "n_NEnfantsMen") %>%
  # rec_NENFANTS(Var = "n_NEnfantsHD") %>%
  # rec_NENFANTS(Var = "n_NEnfantsTous") %>%
  mutate(n_REVENUS_indiv = rowSums(
    pick(RETRAITES, starts_with("REV"), CHOMAGE, SALAIRES),
    na.rm = TRUE
  ))%>%
  mutate(n_PATRIMOINE_indiv = rowSums(
    pick(starts_with("PATF")),
    na.rm = TRUE
  )) %>%
  # 81% de ces deux variables ne sont pas remplies (autre cas parent inactif pe)
  rec_CSP12(Var = "CSACTMERE") %>% 
  rec_CSP12(Var = "CSACTPERE") %>%
  # Idm entre 65 et 75 % pour le diplome des parents =
  rec_DIP7(Var = "DIP14MERE", NewVar = "DIP7MERE") %>%
  rec_DIP7(Var = "DIP14PERE", NewVar = "DIP7PERE") %>%
  rec_NATIO7() %>%
  rec_REVENUS(Var = "n_REVENUS_indiv", "n_REVENUS_indiv_cut") %>%
  rec_PATRIMOINE(Var = "n_PATRIMOINE_indiv", "n_PATRIMOINE_indiv_cut") %>%
  rec_NAIS7() %>%
  rec_ETAMATRI() %>%
  mutate(IMMIGR = case_when(
    NATIO7 != "Française de naissance" & !(NAIS7 %in% c("France métropolitaine", "DOM-TOM")) ~ "Immigré(e)", 
    NATIO7 != "Française de naissance" & (NAIS7 %in% c("France métropolitaine", "DOM-TOM")) ~ "Né(e) en France d'une autre nationalité (naturalisé ou non)",
    NATIO7 == "Française de naissance" & (NAIS7 %in% c("DOM-TOM")) ~ "Né(e) français(e) né(e) dans les DOM-TOM", NATIO7 == "Française de naissance" & (NAIS7 %in% c("France métropolitaine")) ~ "Né(e) français(e) né(e) en France hexagonale", 
    NATIO7 == "Française de naissance" & !(NAIS7 %in% c("France métropolitaine", "DOM-TOM")) ~ "Né(e) français(e) né(e) à l'étranger"))
    
indiv_socdem  
    
freq(indiv_socdem$IMMIGR)
freq(indiv_socdem$SITUA)
table(indiv_socdem$SITUA, indiv_socdem$TYPEMPLOI)

# Tris a plat 
hist(indiv$AGE)
hist(indiv_socdem$n_REVENUS_indiv)
hist(indiv_socdem$n_PATRIMOINE_indiv)
summary(indiv_socdem$n_REVENUS_indiv)
summary(indiv_socdem$n_PATRIMOINE_indiv)
freq(indiv_socdem$DIP14)
freq(indiv_socdem$DIPL)
freq(indiv_socdem$CS12)
freq(indiv_socdem$DIP7)
freq(indiv_socdem$n_REVENUS_indiv_cut)
freq(indiv_socdem$AG6)
freq(indiv_socdem$NAIS7)
freq(indiv_socdem$NATIO7)
#freq(indiv_socdem$ADULTE)
freq(indiv_socdem$TYPEMPLOI)
freq(indiv_socdem$ETAMATRI)
tab <- table(indiv_socdem$NAIS7, indiv_socdem$NATIO7) %>%
  as.data.frame()
tbl_summary(indiv_socdem, 
            include = c("NAIS7", "NATIO7"), 
            by = "NAIS7", 
            percent = "cell")
freq(indiv_socdem$CSACTMERE)
freq(indiv$CSACTPERE)
freq(indiv$DIP14MERE)
freq(indiv$DIP14PERE)
freq(indiv_socdem$DIP7MERE)


variables_socdem <- c(
  "SEXE",
  "AGE", 
  "AG6",
  "ETAMATRI",
  "COUPLE",
  "DIPL",
  "DIP14",
  "DIP7",
  "CS6",
  "CS12",
  "TYPEMPLOI",
  "SITUA",
  "STATUT",
  "n_REVENUS_indiv",
  "n_PATRIMOINE_indiv",
  "n_REVENUS_indiv_cut",
  "n_PATRIMOINE_indiv_cut",
  "NAIS7",
  "NATIO7",
  "IMMIGR"
)


##############################
# 1. Passage du TCM en format long
##############################

# Chaque individu devient une ligne par relation avec un autre membre du ménage
liens_long <- indiv_socdem %>%
  select(
    IDENT_MEN,
    IDENT_IND,
    BS, 
    APART,
    NOI,
    ENFANT, 
    ENFRP,
    CONJOINT,
    COUPLE, 
    SEXE,
    IMMIGR,
    starts_with("LIEN_")
  ) %>%
  left_join(indiv_socdem %>% 
              select(IDENT_MEN, NOI, SEXE, IMMIGR) %>%
              rename(SEXE_CONJOINT = SEXE, IMMIGR_CONJOINT = IMMIGR),
            by = c("IDENT_MEN", "CONJOINT" = "NOI")) %>%
  
  pivot_longer(
    cols = starts_with("LIEN_"),
    names_to = "NOI_XX",
    values_to = "LIEN"
  ) %>%
  mutate(
    NOI_XX = sub("LIEN_", "", NOI_XX)
  ) %>%
  filter(!is.na(LIEN) & LIEN != "" & LIEN != "00") %>%
  mutate(
    LIEN_LIB = case_when(
      LIEN == "00"  ~ "Sans objet",
      LIEN == "01"  ~ "Conjoint",
      LIEN == "02"  ~ "Enfant",
      LIEN == "03"  ~ "Parent",
      LIEN == "10" ~ "Frere_soeur",
      LIEN == "21" ~ "Petit_enfant",
      LIEN == "22" ~ "Grand_parent",
      LIEN == "31" ~ "Beau_fils_belle_fille",
      LIEN == "32" ~ "Beau_parent",
      LIEN == "40" ~ "Autre_lien_familial",
      LIEN == "50" ~ "Lien_familial_indetermine",
      LIEN == "60" ~ "Ami",
      LIEN == "90" ~ "Autre_lien_non_familial",
      TRUE ~ NA_character_
    )
  )

head(liens_long, 15)



### Enfants hors domicile ####################################

enfantsHD <- readRDS("Data_output/enfHD.Rds")
enfantsHD <- pad_2digits(enfantsHD, "NUMORDRE")

names(enfantsHD)
freq(enfantsHD$HODLN01)
# Chaque individu devient une ligne par relation avec un autre membre du ménage
liens_long_hd <- enfantsHD %>%
  select(
    IDENT_MEN,
    NOI_ENFANTHORSDOM = NUMORDRE,
    starts_with("HODLN")
  ) %>%
  pivot_longer(
    cols = starts_with("HODLN"),
    names_to = "NOIPARENT_MENAGE",
    values_to = "ENFANT_DE"
  ) %>%
  mutate(NOIPARENT_MENAGE = str_remove(NOIPARENT_MENAGE, "HODLN")) %>%
  filter(ENFANT_DE == 1)
 

enfantsHD$ANNEE_BDF
head(liens_long_hd, 15)



###############################################################
# 1. TABLES DE RELATIONS DE BASE
###############################################################


#--------------------------------------------------------------
# Couples
#
# NOI est en couple avec NOI_XX
#--------------------------------------------------------------

couples <- liens_long %>%
  filter(LIEN == "01") %>%
  select(
    IDENT_MEN,
    NOI,
    CONJOINT, 
    SEXE, 
    SEXE_CONJOINT, 
    IMMIGR, 
    IMMIGR_CONJOINT
  ) %>%
  # left_join(indiv_socdem %>%
  #             select(IDENT_MEN, NOI, any_of(variables_socdem)), 
  #           by = c("IDENT_MEN", "NOI")) %>%
  # left_join(indiv_socdem %>%
  #             select(IDENT_MEN, CONJOINT = NOI, any_of(variables_socdem)), 
  #           by = c("IDENT_MEN", "CONJOINT"), 
  #           suffix = c("", "_CONJOINT")) %>%
  mutate(COUPLE_SEXE = case_when(
    SEXE != SEXE_CONJOINT ~ "Couple de sexes différents", 
    SEXE == SEXE_CONJOINT & SEXE == "1" ~ "Couple d'hommes", 
    SEXE == SEXE_CONJOINT & SEXE == "2" ~ "Couple de femmes"
  ),
  COUPLE_SEXE_DET = case_when(
    SEXE == "1" & SEXE_CONJOINT == "2" ~ "Homme en couple avec une femme", 
    SEXE == "2" & SEXE_CONJOINT == "1" ~ "Femme en couple avec un homme", 
    SEXE == SEXE_CONJOINT & SEXE == "1" ~ "Homme en couple avec un homme", 
    SEXE == SEXE_CONJOINT & SEXE == "2" ~ "Femme en couple avec un femme")) 



str(couples)
freq(couples$COUPLE_SEXE)
freq(couples$COUPLE_SEXE_DET)
table(couples$IMMIGR, couples$IMMIGR_CONJOINT)



anomalies <- couples %>%
  filter(is.na(COUPLE_SEXE))
anomalies <- indiv %>%
  filter(IDENT_MEN %in% anomalies$IDENT_MEN)
# 17 individus dont la ligne du conjoint est manquante dans la table individu. 

#--------------------------------------------------------------
# Parents
#
# NOI est enfant de NOI_XX
#--------------------------------------------------------------

parents <- liens_long %>%
  filter(LIEN == "02") %>%
  select(
    IDENT_MEN,
    ENFANT = NOI,
    SEXE_ENFANT = SEXE,
    PARENT = NOI_XX
  ) %>%
  left_join(indiv %>%
              select(IDENT_MEN, PARENT = NOI, SEXE_PARENT = SEXE))

str(parents)

#--------------------------------------------------------------
# Enfants
#
# NOI est parent de NOI_XX
#--------------------------------------------------------------

enfants <- liens_long %>%
  filter(LIEN == "03") %>%
  select(
    IDENT_MEN,
    PARENT = NOI,
    SEXE_PARENT = SEXE,
    ENFANT = NOI_XX
  ) %>%
  left_join(indiv %>% 
              select(IDENT_MEN, ENFANT = NOI, AGE_ENFANT = AGE))

str(enfants)

#--------------------------------------------------------------
# Frère-soeur
#
# NOI est frere/soeur de NOI_XX
#--------------------------------------------------------------


freres_soeurs <- liens_long %>%
  filter(LIEN == "10") %>%
  select(
    IDENT_MEN,
    NOI = NOI,
    FRERE_SOEUR = NOI_XX
  )

str(freres_soeurs)

###############################################################
# 2. VARIABLES INDIVIDUELLES DE BASE
###############################################################


#--------------------------------------------------------------
# Nombre de parents
#--------------------------------------------------------------

nb_parents <- parents %>%
  
  group_by(
    IDENT_MEN,
    ENFANT
  ) %>%
  
  summarise(
    NPARENTS = n_distinct(PARENT),
    SEXE_PARENT = list(SEXE_PARENT),
    .groups="drop"
  ) %>%
  
  rename(
    NOI = ENFANT
  )

nb_parents <- nb_parents %>%
  mutate(
    SEXE_PARENT = map_chr(SEXE_PARENT, \(x) {
      
      x <- sort(na.omit(x))
      
      case_when(
        identical(x, c("1", "2")) ~ "Vit avec son père et sa mère",
        identical(x, "2") ~ "Vit avec sa mère",
        identical(x, "1") ~ "Vit avec son père",
        length(x) == 0 ~ "Sexe du/des parents indéterminé",
        TRUE ~ NA_character_
      )
      
    })
  )

str(nb_parents)
freq(nb_parents$NPARENTS)
freq(nb_parents$SEXE_PARENT)


# -------------------------------------------------------------.
# Nombre de parent dans le ménage des enfant hors domicile ----
# -------------------------------------------------------------.

nb_parents_menageHD <- liens_long_hd %>%
  group_by(
    IDENT_MEN,
    NOI_ENFANTHORSDOM
  ) %>%
  summarise(
    NPARENTS_MENAGEHD = n_distinct(NOIPARENT_MENAGE),
    PARENTS_MENAGE = paste0(NOIPARENT_MENAGE, collapse = ";"),
    .groups="drop"
  )
  

#--------------------------------------------------------------.
# Nombre d'enfants ----
#--------------------------------------------------------------.

nb_enfants <- enfants %>%

  
  group_by(
    IDENT_MEN,
    PARENT
  ) %>%
  
  summarise(
    NENFANTS = n_distinct(ENFANT),
    AGE_ENFANTS_MOYEN = mean(AGE_ENFANT),
    .groups="drop"
  ) %>%
  
  rename(
    NOI = PARENT
  )


str(nb_enfants)
freq(nb_enfants$NENFANTS)

# -------------------------------------------------------------.
# Nombre d'enfant hors domicile ----
# -------------------------------------------------------------.
enfantsHD$ANNEE_BDF
nb_enfants_horsdom <- liens_long_hd %>%
  left_join(enfantsHD %>%
              mutate(AGE_ENFANT = ANNEE_BDF - HODAN) %>%
              select(IDENT_MEN, NOI_ENFANTHORSDOM = NUMORDRE, SEXE_ENFANT = HODSEX, AGE_ENFANT)) %>%
  group_by(
    IDENT_MEN,
    NOIPARENT_MENAGE
  ) %>%
  
  summarise(
    NENFANTS_HORSDOM = n_distinct(NOI_ENFANTHORSDOM),
    AGE_ENFANTS_HORSDOM_MOYEN = mean(AGE_ENFANT),
    .groups="drop"
  ) %>%
  
  rename(
    NOI = NOIPARENT_MENAGE
  )


str(nb_enfants_horsdom)
freq(nb_enfants_horsdom$NENFANTS_HORSDOM)



#--------------------------------------------------------------
# Nombre de conjoints
#--------------------------------------------------------------

nb_conjoint <- couples %>%
  
  group_by(
    IDENT_MEN,
    NOI
  ) %>%
  summarise(
    NCONJOINT = n_distinct(CONJOINT),
    NOI_CONJOINT = unique(CONJOINT), 
    SEXE_CONJOINT = unique(SEXE_CONJOINT),
    .groups="drop"
  )


str(nb_conjoint)
freq(nb_conjoint$NCONJOINT)
freq(nb_conjoint$NOI_CONJOINT)
freq(nb_conjoint$SEXE_CONJOINT)


#--------------------------------------------------------------
# Nombre frères-soeurs déclarées
#--------------------------------------------------------------

nb_freres_soeurs <- freres_soeurs %>%
  
  group_by(
    IDENT_MEN,
    NOI
  ) %>%
  
  summarise(
    N_FRERESSOEURS_delca = n_distinct(FRERE_SOEUR),
    .groups="drop"
  )


str(nb_freres_soeurs)



###############################################################
# 3. BEAUX-ENFANTS
###############################################################
#
# Enfants du conjoint qui ne sont pas mes enfants
#
###############################################################


enfants_conjoint <- couples %>%
  
  left_join(
    enfants %>%
      select(-SEXE_PARENT),
    by=c(
      "IDENT_MEN",
      "CONJOINT"="PARENT"
    )
  ) %>%
  
  # select(
  #   IDENT_MEN,
  #   NOI,
  #   ENFANT
  # ) %>%
  filter(!is.na(ENFANT))

enfants_propres <- enfants %>%
  select(-SEXE_PARENT) %>%
  rename(
    NOI=PARENT
  ) %>%
  filter(!is.na(ENFANT))

str(enfants_propres)
freq(enfants_propres$ENFANT)

beaux_enfants <- enfants_conjoint %>%
  anti_join(
    enfants_propres,
    by=c(
      "IDENT_MEN",
      "NOI",
      "ENFANT"
    ) 
  ) 
str(beaux_enfants)
freq(beaux_enfants$ENFANT)
freq(beaux_enfants$COUPLE_SEXE)

nb_beaux_enfants <- beaux_enfants %>%
  group_by(
    IDENT_MEN,
    NOI
  ) %>%
  
  summarise(
    NBEAUX_ENFANTS =
      n_distinct(ENFANT),
    .groups="drop"
  )

str(nb_beaux_enfants)
freq(nb_beaux_enfants$NBEAUX_ENFANTS)


## Enfants communs au couple

enfants_communs <- enfants_conjoint %>%
  inner_join(
    enfants_propres,
    by=c(
      "IDENT_MEN",
      "NOI",
      "ENFANT"
    ) 
  ) 
str(enfants_communs)
freq(enfants_communs$ENFANT)
freq(enfants_communs$COUPLE_SEXE)

nb_enfants_communs <- enfants_communs %>%
  group_by(
    IDENT_MEN,
    NOI
  ) %>%
  
  summarise(
    NENFANTS_COMMUNS =
      n_distinct(ENFANT),
    .groups="drop"
  )

str(nb_enfants_communs)
freq(nb_enfants_communs$NENFANTS_COMMUNS)



###############################################################
# 3b. BEAUX-ENFANTS HORS DOMICILE ####
###############################################################
#
# Enfants du conjoint vivant hors domicile qui ne sont pas mes enfants
#
###############################################################


enfantsHD_conjoint <- couples %>%
  
  left_join(
    liens_long_hd %>%
      select(-ENFANT_DE),
    by=c(
      "IDENT_MEN",
      "CONJOINT"="NOIPARENT_MENAGE"
    )
  ) %>%
  filter(!is.na(NOI_ENFANTHORSDOM))

enfantsHD_propres <- liens_long_hd %>%
  select(-ENFANT_DE) %>%
  rename(
    NOI=NOIPARENT_MENAGE
  ) %>%
  filter(!is.na(NOI_ENFANTHORSDOM))

str(enfantsHD_propres)
freq(enfantsHD_propres$NOI_ENFANTHORSDOM)

beaux_enfantsHD <- enfantsHD_conjoint %>%
  anti_join(
    enfantsHD_propres,
    by=c(
      "IDENT_MEN",
      "NOI",
      "NOI_ENFANTHORSDOM"
    ) 
  ) 
str(beaux_enfantsHD)
freq(beaux_enfantsHD$NOI_ENFANTHORSDOM)
freq(beaux_enfantsHD$COUPLE_SEXE)

nb_beaux_enfantsHD <- beaux_enfantsHD %>%
  group_by(
    IDENT_MEN,
    NOI
  ) %>%
  
  summarise(
    NBEAUX_ENFANTS_HORSDOM =
      n_distinct(NOI_ENFANTHORSDOM),
    .groups="drop"
  )

str(nb_beaux_enfantsHD)
freq(nb_beaux_enfantsHD$NBEAUX_ENFANTS_HORSDOM)


## Enfants communs au couple

enfantsHD_communs <- enfantsHD_conjoint %>%
  inner_join(
    enfantsHD_propres,
    by=c(
      "IDENT_MEN",
      "NOI",
      "NOI_ENFANTHORSDOM"
    ) 
  ) 
str(enfantsHD_communs)
freq(enfantsHD_communs$NOI_ENFANTHORSDOM)
freq(enfantsHD_communs$COUPLE_SEXE)

nb_enfantsHD_communs <- enfantsHD_communs %>%
  group_by(
    IDENT_MEN,
    NOI
  ) %>%
  
  summarise(
    NENFANTS_COMMUNS_HORSDOM =
      n_distinct(NOI_ENFANTHORSDOM),
    .groups="drop"
  )

str(nb_enfantsHD_communs)
freq(nb_enfantsHD_communs$NENFANTS_COMMUNS_HORSDOM)

###############################################################
# 4. BEAUX-PARENTS
###############################################################
#
# Conjoint d'un parent
# qui n'est pas un parent légal
#
###############################################################


beaux_parents <- parents %>%
  
  left_join(
    couples,
    by=c(
      "IDENT_MEN",
      "PARENT"="NOI"
    )
  ) %>%
  
  filter(
    !is.na(CONJOINT)
  ) %>%
  
  rename(
    CONJOINT_PARENT = CONJOINT,
    SEXE_CONJOINT_PARENT = SEXE_CONJOINT
  ) 




beaux_parents <- beaux_parents %>%
  
  anti_join(
    parents %>%
      rename(
        CONJOINT_PARENT=PARENT
      ),
    by=c(
      "IDENT_MEN",
      "ENFANT",
      "CONJOINT_PARENT"
    )
  ) %>%
  rename(BEAUPARENT = CONJOINT_PARENT, 
         SEXE_BEAUPARENT = SEXE_CONJOINT_PARENT)

# pas d'anomlies (aucun n'enfant n'a 2 beaux-parents)
anomalies <- beaux_parents %>%
  mutate(ID = str_glue("{IDENT_MEN}0{ENFANT}"))
anomalies <- anomalies %>%
  filter(ID %in% anomalies$ID[duplicated(anomalies$ID)])

nb_beaux_parents <- beaux_parents %>%
  
  group_by(
    IDENT_MEN,
    ENFANT
  ) %>%
  
  summarise(
    NBEAUX_PARENTS =
      n_distinct(BEAUPARENT),
    SEXE_BEAUPARENT = list(na.omit(SEXE_BEAUPARENT)),
    BEAUPAR2E = list(na.omit(BEAUPARENT)),
    .groups="drop"
  ) %>%
  
  rename(
    NOI=ENFANT
  )

str(nb_beaux_parents)
freq(nb_beaux_parents$NBEAUX_PARENTS)
unique(nb_beaux_parents$SEXE_BEAUPARENT)

nb_beaux_parents <- nb_beaux_parents %>%
  mutate(
    SEXE_BEAUPARENT = map_chr(SEXE_BEAUPARENT, \(x) {
      
      x <- sort(na.omit(x))
      
      case_when(
        identical(x, c("1", "2")) ~ "Vit avec un beau-père et une belle-mère",
        identical(x, "2") ~ "Vit avec une belle-mère",
        identical(x, "1") ~ "Vit avec un beau-père",
        length(x) == 0 ~ "Sexe beau-parent inconnu",
        TRUE ~ NA_character_
      )
      
    }), 
    BEAUPAR2E = as.character(BEAUPAR2E)
  )

freq(nb_beaux_parents$SEXE_BEAUPARENT)

# ####################NBEAUX_PARENTS# ###############################################################
# # 5. FRATRIES
# ###############################################################
# #
# # On compare tous les enfants du ménage entre eux
# #
# ###############################################################
# parents_beauxparents <- parents %>% left_join(beaux_parents)
paires_fratrie <- parents %>%

  select(
    IDENT_MEN,
    ENFANT1=ENFANT,
    PARENT1=PARENT
  ) %>%

  inner_join(
    parents %>%
      select(
        IDENT_MEN,
        ENFANT2=ENFANT,
        PARENT2=PARENT
      ),
    by="IDENT_MEN"
  ) %>%

  filter(
    ENFANT1 != ENFANT2
  )


parents_communs <- paires_fratrie %>%

  group_by(
    IDENT_MEN,
    ENFANT1,
    ENFANT2
  ) %>%

  summarise(

    NPARENTS_COMMUNS_MENAGE =
      n_distinct(
        intersect(
          PARENT1,
          PARENT2
        )
      ),

    .groups="drop"

  ) %>%
  left_join(nb_beaux_parents %>%
              rename(NBEAUX_PARENTS_ENFANT1 = NBEAUX_PARENTS, 
                     SEXE_BEAUPARENT_ENFANT1 = SEXE_BEAUPARENT), by = c("IDENT_MEN", "ENFANT1" = "NOI")) %>%
  left_join(nb_parents %>%
              rename(NPARENTS_ENFANT1 = NPARENTS, 
                     SEXE_PARENT_ENFANT1 = SEXE_PARENT), by = c("IDENT_MEN", "ENFANT1" = "NOI")) %>%
  left_join(nb_beaux_parents %>%
              rename(NBEAUX_PARENTS_ENFANT2 = NBEAUX_PARENTS,
                     SEXE_BEAUPARENT_ENFANT2 = SEXE_BEAUPARENT), by = c("IDENT_MEN", "ENFANT2" = "NOI")) %>%
  left_join(nb_parents %>%
              rename(NPARENTS_ENFANT2 = NPARENTS, 
                     SEXE_PARENT_ENFANT2 = SEXE_PARENT), by = c("IDENT_MEN", "ENFANT2" = "NOI")) %>%
  mutate()

 
# freq(parents_communs$NPARENTS_COMMUNS_MENAGE)
# # On a bcp d'enfant qui ont un seul parent en commun mais prbablement lié aux familles monoparentales
# 
###############################################################
# 6. FRERES / SOEURS TOUS CONFONDUS
###############################################################



head(freres_soeurs)
parents_communs
nb_freres_soeurs <- bind_rows(

  parents_communs %>%
    select(
      IDENT_MEN,
      NOI=ENFANT1,
      AUTRE=ENFANT2
    ),

  parents_communs %>%
    select(
      IDENT_MEN,
      NOI=ENFANT2,
      AUTRE=ENFANT1
    )

) %>%

  group_by(
    IDENT_MEN,
    NOI
  ) %>%

  summarise(
    N_FRERES_SOEURS_TOUS =
      n_distinct(AUTRE),
    .groups="drop"
  )

freq(nb_freres_soeurs$N_FRERES_SOEURS_TOUS)


nb_freres_soeurs <- nb_freres_soeurs 



# ###############################################################
# # 7. FRERES / SOEURS GERMAINS
# ###############################################################
# 
# 
# freres_soeurs <- parents_communs %>%
#   
#   filter(
#     NPARENTS_COMMUNS==2
#   )
# 
# 
# 
# nb_freres_soeurs <- bind_rows(
#   
#   freres_soeurs %>%
#     select(
#       IDENT_MEN,
#       NOI=ENFANT1,
#       AUTRE=ENFANT2
#     ),
#   
#   freres_soeurs %>%
#     select(
#       IDENT_MEN,
#       NOI=ENFANT2,
#       AUTRE=ENFANT1
#     )
#   
# ) %>%
#   
#   group_by(
#     IDENT_MEN,
#     NOI
#   ) %>%
#   
#   summarise(
#     N_FRERES_SOEURS_GERMAINS =
#       n_distinct(AUTRE),
#     .groups="drop"
#   )
# 
# freq(nb_freres_soeurs$N_FRERES_SOEURS_GERMAINS)
# 
###############################################################
# 7. DEMI-FRERES / DEMI-SOEURS
###############################################################


demi_freres_soeurs <- parents_communs %>%
  filter(
    NPARENTS_COMMUNS_MENAGE == 1 & (NBEAUX_PARENTS_ENFANT1 > 0 | NBEAUX_PARENTS_ENFANT2 > 0)
  )


nb_demi_freres_soeurs <- bind_rows(

  demi_freres_soeurs %>%
    select(
      IDENT_MEN,
      NOI=ENFANT1,
      AUTRE=ENFANT2
    ),

  demi_freres_soeurs %>%
    select(
      IDENT_MEN,
      NOI=ENFANT2,
      AUTRE=ENFANT1
    )

) %>%

  group_by(
    IDENT_MEN,
    NOI
  ) %>%

  summarise(
    N_DEMI_FRERES_SOEURS =
      n_distinct(AUTRE),
    .groups="drop"
  )
str(nb_demi_freres_soeurs)
freq(nb_demi_freres_soeurs$N_DEMI_FRERES_SOEURS)


###############################################################
# 8. QUASI-FRERES / SOEURS PAR ALLIANCE
###############################################################
#
# Aucun parent commun
# mais un parent de l'un est conjoint du parent de l'autre
#
###############################################################


quasi_freres_soeurs <- parents_communs %>%
  filter(
    NPARENTS_COMMUNS_MENAGE == 0 & (NBEAUX_PARENTS_ENFANT1 > 0 | NBEAUX_PARENTS_ENFANT2 > 0)
  )



nb_quasi_freres_soeurs <- bind_rows(

  quasi_freres_soeurs %>%
    select(
      IDENT_MEN,
      NOI=ENFANT1,
      AUTRE=ENFANT2
    ),

  quasi_freres_soeurs %>%
    select(
      IDENT_MEN,
      NOI=ENFANT2,
      AUTRE=ENFANT1
    )

) %>%

  group_by(
    IDENT_MEN,
    NOI
  ) %>%

  summarise(
    N_QUASI_FRERES_SOEURS =
      n_distinct(AUTRE),
    .groups="drop"
  )


str(nb_quasi_freres_soeurs)
freq(nb_quasi_freres_soeurs$N_QUASI_FRERES_SOEURS)

###############################################################.
# Position dans l'adelphie ####
###############################################################.

enfants2 <- indiv %>%
  filter(ENFANT == "1") %>%
  group_by(IDENT_MEN) %>%
  arrange(desc(AGE), .by_group = TRUE) %>%
  mutate(RANG_ENFANT = row_number()) %>%
  mutate(
    RANG_ENFANT = row_number(),
    POSITION_FRATERIE = case_when(
      n() == 1 ~ "Enfant unique",
      n() > 1 & row_number() == 1 ~ "Aîné-e",
      n() > 1 & row_number() == n() ~ "Benjamin-e",
      n() > 1  ~ "Cadet-te"
    )
  ) %>%
  ungroup() %>%
  group_by(IDENT_MEN, SEXE) %>%
  arrange(desc(AGE), .by_group = TRUE) %>%
  mutate(RANG_ENFANT_SEXE = row_number()) %>%
  ungroup()


freq(enfants2$RANG_ENFANT)
freq(enfants2$POSITION_FRATERIE)
freq(enfants2$RANG_ENFANT_SEXE)


###############################################################
# 9. ASSEMBLAGE FINAL DANS INDIV
###############################################################


indiv_fam <- indiv %>%
  
  left_join(
    nb_parents,
    by=c(
      "IDENT_MEN",
      "NOI"
    )
  ) %>%

  
  left_join(
    nb_enfants,
    by=c(
      "IDENT_MEN",
      "NOI"
    ))%>%
  left_join(
    nb_enfants_horsdom,
    by=c(
      "IDENT_MEN",
      "NOI"
    ) 
    
  ) %>%
  
  left_join(
    nb_conjoint,
    by=c(
      "IDENT_MEN",
      "NOI"
    )
  ) %>%
  left_join(
    couples %>%
      select(IDENT_MEN, NOI, COUPLE_SEXE, COUPLE_SEXE_DET), 
    by = c("IDENT_MEN", "NOI")
      
  ) %>%
  
  left_join(
    nb_beaux_enfants,
    by=c(
      "IDENT_MEN",
      "NOI"
    )
  ) %>%
  
  left_join(
    nb_enfants_communs,
    by=c(
      "IDENT_MEN",
      "NOI"
    )
  ) %>%
  left_join(
    nb_beaux_enfants %>%
      rename("NBEAUX_ENFANTS_CONJOINT" = "NBEAUX_ENFANTS"),
    by=c(
      "IDENT_MEN",
      "CONJOINT" = "NOI"
    )
  ) %>%
  
  left_join(
    nb_beaux_enfantsHD,
    by=c(
      "IDENT_MEN",
      "NOI"
    )
  ) %>%
  
  left_join(
    nb_enfantsHD_communs,
    by=c(
      "IDENT_MEN",
      "NOI"
    )
  ) %>%
  
  left_join(
    nb_beaux_enfantsHD %>%
      rename("NBEAUX_ENFANTS_HORSDOM_CONJOINT" = "NBEAUX_ENFANTS_HORSDOM"),
    by=c(
      "IDENT_MEN",
      "CONJOINT" = "NOI"
    )
  ) %>%

  left_join(
    nb_beaux_parents,
    by=c(
      "IDENT_MEN",
      "NOI"
    )
  ) %>%
  
  left_join(
    nb_freres_soeurs,
    by=c(
      "IDENT_MEN",
      "NOI"
    )
  ) %>%
  
  left_join(
    nb_demi_freres_soeurs,
    by=c(
      "IDENT_MEN",
      "NOI"
    )
  ) %>%
  
  left_join(
    nb_quasi_freres_soeurs,
    by=c(
      "IDENT_MEN",
      "NOI"
    )
  ) %>%
  left_join(
    enfants2 %>%
      select(IDENT_MEN, NOI, RANG_ENFANT, RANG_ENFANT_SEXE, POSITION_FRATERIE),
    by=c(
                "IDENT_MEN",
                "NOI"
              )) %>%
  
  mutate(
    
    across(
      c(
        NPARENTS,
        NENFANTS,
        NCONJOINT,
        NBEAUX_ENFANTS,
        NBEAUX_ENFANTS_CONJOINT,
        NENFANTS_COMMUNS,
        NBEAUX_PARENTS,
        N_FRERES_SOEURS_TOUS,
        N_DEMI_FRERES_SOEURS,
        N_QUASI_FRERES_SOEURS
      ),
      ~replace_na(.x,0)
    )
    
  )


#nouvelles variables
names(indiv_fam)[!(names(indiv_fam) %in% names(indiv))]

###############################################################
# FIN
#
# indiv_fam est la table indiv enrichie
#
###############################################################


for (var in c("NPARENTS",
              "SEXE_PARENT",
              "NENFANTS",
              "NCONJOINT",
              "NBEAUX_ENFANTS",
              "NBEAUX_ENFANTS_CONJOINT",
              "NENFANTS_COMMUNS",
              "NBEAUX_PARENTS",
              "SEXE_BEAUPARENT", 
              "N_FRERES_SOEURS_TOUS",
              "N_DEMI_FRERES_SOEURS",
              "N_QUASI_FRERES_SOEURS")) {
  print(paste0("# Tris a plat de ", var))
  print(freq(indiv_fam[[var]]))
}


  

###############################################################
# 6. MOCO_DET
###############################################################


indiv_fam <- indiv_fam %>%
  
  mutate(
    #--------------------------------------------------------------
    # ENFANTS
    #--------------------------------------------------------------
    
    MOCO_DET_ENF = case_when(
      
      ENFANT=="1" &
        NPARENTS==2 &
        #NBEAUX_PARENTS==0 &
        N_QUASI_FRERES_SOEURS+N_DEMI_FRERES_SOEURS==0
      ~"Enfant d’une famille traditionnelle",
      
      
      ENFANT=="1" &
        NPARENTS==1 &
        NBEAUX_PARENTS==0
      ~"Enfant d’une famille monoparentale",
      
      
      ENFANT=="1" &
        NPARENTS==2 & N_QUASI_FRERES_SOEURS+N_DEMI_FRERES_SOEURS > 0
      ~"Enfant du couple dans une famille recomposée",
      
      
      ENFANT=="1" &
        NPARENTS==1 &
        NBEAUX_PARENTS>0
      ~"Enfant d’un seul des adultes de la famille recomposée",  
      
      ENFANT == "1" ~ "Enfant sans parent dans la famille"))

freq(indiv_fam$MOCO_DET_ENF)
indiv_fam$SEXE_PARENT
table(indiv_fam$SEXE_PARENT, indiv_fam$MOCO_DET_ENF)

indiv_fam <- indiv_fam %>%
      mutate(
      MOCO_DET_ENF_SEXE = case_when(
        MOCO_DET_ENF == "Enfant d’une famille traditionnelle" ~ MOCO_DET_ENF,
        MOCO_DET_ENF == "Enfant d’une famille monoparentale"
        & SEXE_PARENT == "Vit avec sa mère" ~ "Enfant d’une mère célibataire",
        MOCO_DET_ENF == "Enfant d’une famille monoparentale"
        & SEXE_PARENT == "Vit avec son père" ~ "Enfant d’un père célibataire",
        MOCO_DET_ENF == "Enfant du couple dans une famille recomposée" ~ MOCO_DET_ENF,
        MOCO_DET_ENF == "Enfant d’un seul des adultes de la famille recomposée" 
        & SEXE_PARENT == "Vit avec sa mère" ~ "Enfant de la mère de famille recomposée", 
        MOCO_DET_ENF == "Enfant d’un seul des adultes de la famille recomposée" 
        & SEXE_PARENT == "Vit avec son père" ~ "Enfant du père de famille recomposée",
        ENFANT == "1" ~ "Enfant sans parent dans la famille"))


freq(indiv_fam$MOCO_DET_ENF_SEXE)



indiv_fam <- indiv_fam %>%
  
  mutate(
      #--------------------------------------------------------------
      # ADULTES
      #--------------------------------------------------------------
      
  MOCO_DET_ADU = case_when(
    ENFANT == "2" &
      NCONJOINT==1 &
      NENFANTS==0 &
      NBEAUX_ENFANTS== 0
    ~"Adulte d'un couple sans enfant",
    
    ENFANT == "2" &
      NCONJOINT==0 &
      NENFANTS==0 
    ~"Adulte célibataire et sans enfant", 
    
    ENFANT == "2" &
      NCONJOINT==0 &
      NENFANTS>0
    ~"Adulte d’une famille monoparentale",
    
    ENFANT == "2"&
      NCONJOINT==1 &
      NENFANTS>0 &
      NBEAUX_ENFANTS>0 
      ~"Adulte d’une famille recomposée (beau-parent avec enfants)",
    
    ENFANT == "2"&
      NCONJOINT==1 &
      NENFANTS==0 &
      NBEAUX_ENFANTS>0 
      ~"Adulte d’une famille recomposée (beau-parent sans enfants)",
    
    ENFANT == "2"&
      NCONJOINT==1 &
      NENFANTS>0 &
      NBEAUX_ENFANTS==0 &
      NBEAUX_ENFANTS_CONJOINT > 0
      ~"Adulte d’une famille recomposée (parent sans beaux-enfants)",
    
    
    ENFANT == "2" &
        NCONJOINT==1 &
        NENFANTS>0 &
      NBEAUX_ENFANTS==0 &
      NBEAUX_ENFANTS_CONJOINT == 0
      ~"Adulte d’une famille traditionnelle", 
    
    ENFANT == "2" & 
      NCONJOINT==0 &
      NENFANTS==0 &
      NPARENTS==0 ~ "Autre adulte sans lien familial direct dans le ménage"))

freq(indiv_fam$MOCO_DET_ADU)

indiv_fam <- indiv_fam %>%
  mutate(MOCO_DET_ADU_SEXE = case_when(
    (!is.na(MOCO_DET_ADU)) & SEXE == "1" ~ str_replace(MOCO_DET_ADU, "Adulte", "Homme"),
    (!is.na(MOCO_DET_ADU)) & SEXE == "2" ~ str_replace(MOCO_DET_ADU, "Adulte", "Femme"))) %>%
  mutate(MOCO_DET = case_when(
    ENFANT == "1" ~ MOCO_DET_ENF,
    ENFANT == "2" ~ MOCO_DET_ADU)) %>%
  mutate(MOCO_DET_SEXE = case_when(
    ENFANT == "1" ~ MOCO_DET_ENF_SEXE,
    ENFANT == "2" ~ MOCO_DET_ADU_SEXE))
 
    
    
freq(indiv_fam$MOCO_DET)
freq(indiv_fam$MOCO_DET_ADU_SEXE)
freq(indiv_fam$MOCO_DET_SEXE)

tbl_summary(indiv_fam, 
            include = MOCO_DET_SEXE)

anomalie <- indiv_fam[indiv_fam$MOCO_DET == "Autre adulte", ]

freq(indiv_fam$MOCO_DET_SEXE)




freq(indiv_fam$NENFANTS_COMMUNS)
freq(indiv_fam$N_DEMI_FRERES_SOEURS)
freq(indiv_fam$SEXE_CONJOINT)
class(indiv_fam$SEXE_CONJOINT)

##############################################################
# des variables socio_demo pour tous : individus, conjoint, parents, beaux parents
##############################################################

indiv_socdem <- indiv_socdem %>%
  select(IDENT_MEN, NOI, any_of(variables_socdem))

names(indiv_fam)
names(indiv_socdem)

indiv_fam_enrichie <- indiv_fam %>%
  select(-any_of(variables_socdem)) %>%
  # individu
  left_join(indiv_socdem, by = c("IDENT_MEN", "NOI")) %>%
  # conjoint eventuel
  left_join(indiv_socdem, by = c("IDENT_MEN", "CONJOINT" = "NOI"), 
            suffix = c("", "_CONJOINT")) %>%
  # Parents eventuel
  #left_join(indiv %>% select(IDENT_MEN, NOI, PER2E, MER2E), by = c("IDENT_MEN", "NOI")) %>%
  left_join(indiv_socdem, by = c("IDENT_MEN", "PER2E" = "NOI"), 
            suffix = c("", "_PERE")) %>%
  left_join(indiv_socdem, by = c("IDENT_MEN", "MER2E" = "NOI"), 
            suffix = c("", "_MERE")) %>%
  # Beau parent eventuel
  left_join(indiv_socdem, by = c("IDENT_MEN", "BEAUPAR2E" = "NOI"), 
            suffix = c("", "_BEAUPARENT")) 


###############################################################
# 7. CONSTRUCTION DES NOYAUX FAMILIAUX
###############################################################
#
# On travaille maintenant au niveau famille
# et non ménage
# Un noyau = un couple ou un parent + enfant 
#
###############################################################

menages <- readRDS("Data_output/menages.Rds")
names(menages)
# On va coder le type de famille a partir de la situation des adultes car il y a toujours un adulte dans un noyaux familial (couple ou parent + enfant)

adultes <- indiv_fam_enrichie %>% 
  filter(ENFANT == "2") %>%
  mutate(TDM8 = case_when(
    
    MOCO_DET_SEXE %in% c(
      "Femme célibataire et sans enfant",
      "Homme célibataire et sans enfant"
    ) ~ "Personne seule sans enfant",
    
    MOCO_DET_SEXE %in% c(
      "Femme d'un couple sans enfant",
      "Homme d'un couple sans enfant"
    ) ~ "Couple sans enfant",
    
    MOCO_DET_SEXE %in% c(
      "Femme d’une famille recomposée (beau-parent sans enfants)",
      "Homme d’une famille recomposée (beau-parent sans enfants)"
    ) ~ "Couple sans enfant du couple, et avec au moins un enfant d'un seul des deux membres du couple",
    
    MOCO_DET_SEXE %in% c(
      "Femme d’une famille traditionnelle",
      "Homme d’une famille traditionnelle"
    ) ~ "Couple avec uniquement enfant(s) du couple",
    
    MOCO_DET_SEXE %in% c(
      "Homme d’une famille monoparentale"
    ) ~ "Famille monoparentale (père + enfant(s))",
    
    MOCO_DET_SEXE %in% c(
      "Femme d’une famille monoparentale"
    ) ~ "Famille monoparentale (mère + enfant(s))",
    
    MOCO_DET_SEXE %in% c(
      "Femme d’une famille recomposée (parent sans beaux-enfants)",
      "Homme d’une famille recomposée (parent sans beaux-enfants)", 
      "Femme d’une famille recomposée (beau-parent avec enfants)",
      "Homme d’une famille recomposée (beau-parent avec enfants)")
    & NENFANTS_COMMUNS > 0 
    ~ "Couple avec enfant(s) du couple, et avec au moins un enfant d'un seul des deux membres du couple",
    
    MOCO_DET_SEXE %in% c(
      "Femme d’une famille recomposée (parent sans beaux-enfants)",
      "Homme d’une famille recomposée (parent sans beaux-enfants)", 
      "Femme d’une famille recomposée (beau-parent avec enfants)",
      "Homme d’une famille recomposée (beau-parent avec enfants)")
    & NENFANTS_COMMUNS == 0 
    ~ "Couple sans enfant du couple, et avec au moins un enfant d'un seul des deux membres du couple",
    
    TRUE ~ "Autre type de configuration"
  ))



adultes <- adultes %>%
  mutate(
    TDM8_SEXE = case_when(
      
      MOCO_DET_SEXE == "Femme célibataire et sans enfant" ~ "Femme seule sans enfant", 
      MOCO_DET_SEXE == "Homme célibataire et sans enfant" ~ "Homme seul sans enfant", 
      MOCO_DET_SEXE %in% c(
        "Femme d'un couple sans enfant",
        "Homme d'un couple sans enfant"
      ) ~ "Couple sans enfant",
      MOCO_DET_SEXE %in% c(
        "Femme d’une famille recomposée (beau-parent sans enfants)",
        "Homme d’une famille recomposée (beau-parent sans enfants)"
      )  & SEXE_CONJOINT == "1" 
      ~ "Couple sans enfant du couple, et avec au moins un enfant du père",
      MOCO_DET_SEXE %in% c(
        "Femme d’une famille recomposée (beau-parent sans enfants)",
        "Homme d’une famille recomposée (beau-parent sans enfants)"
      )  & SEXE_CONJOINT == "2" 
      ~ "Couple sans enfant du couple, et avec au moins un enfant de la mère",
      MOCO_DET_SEXE %in% c(
        "Femme d’une famille traditionnelle",
        "Homme d’une famille traditionnelle"
      ) ~ "Couple avec uniquement enfant(s) du couple",
      
      
      MOCO_DET_SEXE %in% c(
        "Homme d’une famille monoparentale"
      ) ~ "Famille monoparentale (père + enfant(s))",
      
      MOCO_DET_SEXE %in% c(
        "Femme d’une famille monoparentale"
      ) ~ "Famille monoparentale (mère + enfant(s))",
      
      MOCO_DET_SEXE == "Femme d’une famille recomposée (parent sans beaux-enfants)" 
      & NENFANTS_COMMUNS > 0
      ~ "Couple avec enfant(s) du couple, et avec au moins un enfant de la mère",
      
      MOCO_DET_SEXE == "Femme d’une famille recomposée (parent sans beaux-enfants)" 
      & NENFANTS_COMMUNS == 0
      ~ "Couple sans enfant du couple, et avec au moins un enfant de la mère",
      
      MOCO_DET_SEXE == "Homme d’une famille recomposée (parent sans beaux-enfants)" 
      & NENFANTS_COMMUNS > 0
      ~ "Couple avec enfant(s) du couple, et avec au moins un enfant du père",
      
      MOCO_DET_SEXE == "Homme d’une famille recomposée (parent sans beaux-enfants)" 
      & NENFANTS_COMMUNS == 0
      ~ "Couple sans enfant du couple, et avec au moins un enfant du père",
      MOCO_DET_SEXE %in% c(
        "Femme d’une famille recomposée (beau-parent avec enfants)",
        "Homme d’une famille recomposée (beau-parent avec enfants)")
      & NENFANTS_COMMUNS == 0 & NBEAUX_ENFANTS_CONJOINT > 0
      ~ "Couple sans enfant du couple, et avec au moins un enfant de chacun des membres du couple",
      
      MOCO_DET_SEXE %in% c(
        "Femme d’une famille recomposée (beau-parent avec enfants)",
        "Homme d’une famille recomposée (beau-parent avec enfants)")
      & NENFANTS_COMMUNS > 0 & NBEAUX_ENFANTS_CONJOINT > 0
      ~ "Couple avec enfant(s) du couple, et avec au moins un enfant de chacun des membres du couple",
      
      MOCO_DET_SEXE %in% c(
        "Femme d’une famille recomposée (beau-parent avec enfants)",
        "Homme d’une famille recomposée (beau-parent avec enfants)")
      & SEXE_CONJOINT == 2
      & NENFANTS_COMMUNS > 0 & NBEAUX_ENFANTS_CONJOINT == 0
      ~ "Couple avec enfant(s) du couple, et avec au moins un enfant de la mère",
    
      
      MOCO_DET_SEXE %in% c(
        "Femme d’une famille recomposée (beau-parent avec enfants)",
        "Homme d’une famille recomposée (beau-parent avec enfants)")
      & SEXE_CONJOINT == 1
      & NENFANTS_COMMUNS > 0 & NBEAUX_ENFANTS_CONJOINT == 0
      ~ "Couple avec enfant(s) du couple, et avec au moins un enfant du père",
      
      TRUE ~ "Autre type de configuration"
    )
  )
  
anomalies <- adultes %>%
  filter(TDM8_SEXE == "Autre type de configuration")

menages_from_indiv <- adultes %>%
  group_by(IDENT_MEN) %>%
  reframe(TDM8_SEXE = unique(TDM8_SEXE))

anomalies <- menages_from_indiv %>%
  filter(IDENT_MEN %in% menages_from_indiv$IDENT_MEN[duplicated(menages_from_indiv$IDENT_MEN)]) %>% 
  mutate(value = 1) %>%
  #pivot_wider(names_from = TDM8_agreg, id_cols = IDENT_MEN, values_fill = 0) %>%
  left_join(menages %>%
              select(IDENT_MEN, NCOUPLES, NPERS, NENFANTS, TYPMEN5) %>%
              rec_TYPMEN5())
freq(anomalies$TYPMEN5) # toute les anomalies sont lié a des ménages complexes (souvent + de deux familles dedans)

menages_from_indiv <- adultes %>%
  group_by(IDENT_MEN) %>%
  summarise(TDM8_SEXE = list(unique(TDM8_SEXE)), 
             TDM8 = list(unique(TDM8)))
menages_from_indiv[menages_from_indiv$IDENT_MEN %in% anomalies$IDENT_MEN, ]$TDM8 <- list("Autre ménage (complexe)")
menages_from_indiv[menages_from_indiv$IDENT_MEN %in% anomalies$IDENT_MEN, ]$TDM8_SEXE <- list("Autre ménage (complexe)")

menages_from_indiv <- menages_from_indiv %>%
  mutate(TDM8 = as.character(TDM8),
         TDM8_SEXE = as.character(TDM8_SEXE))

freq(menages_from_indiv$TDM8)
freq(menages_from_indiv$TDM8_SEXE)

#################################################
## COUPLE_SEXE : Couples het, femmes, hommes ### 
################################################
menages_from_indiv2 <- adultes %>%
  group_by(IDENT_MEN) %>%
  reframe(COUPLE_SEXE = unique(COUPLE_SEXE))

anomalies <- menages_from_indiv2 %>%
  filter(IDENT_MEN %in% menages_from_indiv2$IDENT_MEN[duplicated(menages_from_indiv2$IDENT_MEN)]) %>% 
  mutate(value = 1) %>%
  #pivot_wider(names_from = TDM8_agreg, id_cols = IDENT_MEN, values_fill = 0) %>%
  left_join(menages %>%
              select(IDENT_MEN, NCOUPLES, NPERS, NENFANTS, TYPMEN5) %>%
              rec_TYPMEN5())

freq(anomalies$TYPMEN5) # toute les anomalies sont lié a des ménages complexes (souvent + de deux familles dedans)

# dans une majorité de cas, c'est un couple qui vit avec des célibataires, ou plussieurs couples de sexes différents qui vivent ensemble, donc on peut considérer que la variable est ok pour tout le ménage (de toute facon c'es des complexe, donc on regardera pas souvent)

menages_from_indiv2 <- adultes %>%
  #filter(FAMPRINC == "1") %>%
  group_by(IDENT_MEN) %>%
  reframe(COUPLE_SEXE = unique(na.omit(COUPLE_SEXE)))

anomalies <- menages_from_indiv2 %>%
  filter(IDENT_MEN %in% menages_from_indiv2$IDENT_MEN[duplicated(menages_from_indiv2$IDENT_MEN)]) %>% 
  mutate(value = 1) %>%
  #pivot_wider(names_from = TDM8_agreg, id_cols = IDENT_MEN, values_fill = 0) %>%
  left_join(menages %>%
              select(IDENT_MEN, NCOUPLES, NPERS, NENFANTS, TYPMEN5) %>%
              rec_TYPMEN5())
# On a deux ménages pour lesquels on a deux couples 

ano1 <- indiv_fam %>%
  filter(IDENT_MEN == "11_03045")
# Un couple H/F qui vivent avec leur fille et la copine de leur fille (environ 20 ans)

ano2 <- indiv_fam %>%
  filter(IDENT_MEN == "11_04570")
# Un couple H/F qui vivent avec leur 4 enfant un couple hommes agés (+70) (lien familial indéterminé)

# On va recoder ces deux situations en "couple de sexes différents, car ce sont eux qui ont des enfants dans le ménage, donc on niveua du ménage, on privilégie cette configuration. Par ailleurs, au vus des ages qui nous interesssent, ces deux couples de jeunes femmes et de viels hommes sont un peu hors cadre. 

menages_from_indiv2 <- adultes %>%
  mutate(COUPLE_SEXE = if_else(IDENT_MEN %in% c("11_03045", "11_04570"), "Couple de sexes différents", COUPLE_SEXE)) %>%
  filter(!is.na(COUPLE_SEXE)) %>%
  group_by(IDENT_MEN) %>%
  summarise(COUPLE_SEXE = unique(na.omit(COUPLE_SEXE)))
# Dans les cas ou il y a vait plusieurs couples avec des configurations de sexe différentes, on a privilégiié le couple qui avait des enfnats dans le ménage pour classer le ménage. Donc mécaniquement on donne la prio aux couples Hommes/femmes (pour rappel aucun n'enfant n'a deux pères ou deux mères dans l'enquête)

menages_from_indiv <- left_join(menages_from_indiv, menages_from_indiv2, by = "IDENT_MEN")

#####################################
### variables socio démo genrées ####
#####################################

names(adultes)
ref <- menages %>%
  select(IDENT_MEN, PREF) 
  # pivot_longer(cols = c("PREF", "PCONJ"), 
  #              names_to = "REF",
  #              values_to = "NOI") 

adultes
ref <- pad_2digits(ref, "PREF") 
ref
adultesref <- inner_join(adultes, ref, by = c("IDENT_MEN", "NOI" = "PREF"))

adultesref <- adultesref %>%
  mutate(
    MENAGE_RAPPORT_IMMIGR = case_when(
      
      # Immigré seul ou deux immigrés
      IMMIGR == "Immigré(e)" &
        (IMMIGR_CONJOINT == "Immigré(e)" | is.na(IMMIGR_CONJOINT)) ~
        "Immigré-e-s",
      
      # Couple mixte avec un immigré
      (xor(IMMIGR == "Immigré(e)",
          IMMIGR_CONJOINT == "Immigré(e)") &
        !is.na(IMMIGR_CONJOINT) ) 
      | (IMMIGR == "Né(e) en France d'une autre nationalité (naturalisé ou non)" |
           IMMIGR_CONJOINT == "Né(e) en France d'une autre nationalité (naturalisé ou non)")
      ~"Couple mixte avec un-e immigré-e et personnes nées en France d'une autre nationalité",
      
      # DOM
      (IMMIGR == "Né(e) français(e) né(e) dans les DOM-TOM" |
         IMMIGR_CONJOINT == "Né(e) français(e) né(e) dans les DOM-TOM") ~
        "Français-es né-e-s dans les DOM",
      
      # Français né à l'étranger
      (IMMIGR == "Né(e) français(e) né(e) à l'étranger" |
         IMMIGR_CONJOINT == "Né(e) français(e) né(e) à l'étranger") ~
        "Français-es né-e-s à l'étranger",
      
      # France hexagonale
      IMMIGR == "Né(e) français(e) né(e) en France hexagonale" &
        (IMMIGR_CONJOINT == "Né(e) français(e) né(e) en France hexagonale" |
           is.na(IMMIGR_CONJOINT)) ~
        "Français-es né-e-s en France hexagonale",
      
      TRUE ~ NA_character_
    ), 
    
    MENAGE_RAPPORT_IMMIGR = factor(
      MENAGE_RAPPORT_IMMIGR ,
      levels = c(
        "Français-es né-e-s en France hexagonale",
        "Français-es né-e-s dans les DOM",
        "Français-es né-e-s à l'étranger",
        "Couple mixte avec un-e immigré-e et personnes nées en France d'une autre nationalité",
        "Immigré-e-s"
      )
    )
  )
  

ano <- adultesref %>%
  filter(is.na(MENAGE_RAPPORT_IMMIGR))

freq(adultesref$MENAGE_RAPPORT_IMMIGR)

# Rapport au public / privé 
table(adultesref$STATUT, adultesref$STATUT_CONJOINT)
public <- c("Salarié-e de l'Etat", "Salarié-e d'une collectivité locale, des HLM ou des hôpitaux publics")

adultesref <- adultesref %>%
  mutate(
    MENAGE_PUBLICPRIVE = case_when(
      STATUT %in% public | STATUT_CONJOINT %in% public ~ "Au moins une personne travaille dans le public", 
      !is.na(STATUT) | !is.na(STATUT) ~ "Personne ne travaille dans le public", 
      TRUE ~ NA_character_))
freq(adultesref$MENAGE_PUBLICPRIVE)
freq(adultesref$STATUT)
freq(adultesref$STATUT_CONJOINT)

# Variables socio-démo genrées 
menages_from_indiv3 <- reduce(
    c("NOI", variables_socdem),
    .init = adultesref,
    .f = \(df, v) {
      mutate(
        df,
        !!paste0(v, "_H") :=
          if_else(
            SEXE == "1",
            .data[[v]],
            .data[[paste0(v, "_CONJOINT")]]
          ),
        !!paste0(v, "_F") :=
          if_else(
            SEXE == "2",
            .data[[v]],
            .data[[paste0(v, "_CONJOINT")]]
          )
      )
    }
  )


tab <- table(menages_from_indiv3$COUPLE_SEXE, menages_from_indiv3$SEXE_H, menages_from_indiv3$SEXE_F, useNA = "ifany") %>%
  as.data.frame()
# Pour les personnes qui vivent en couple de meme sexe, si la presonne de référence est un homme alors sont conjoint est en "_F", si la personne de ref est une femme alors sa conjointe est "_H"

anomalies<- menages_from_indiv3 %>%
    filter(IDENT_MEN %in% menages_from_indiv3$IDENT_MEN[duplicated(menages_from_indiv3$IDENT_MEN)]) 
# Normal car on a qu'une seule personne de référence par ménage

# Revenus individuels
freq(menages_from_indiv3$n_REVENUS_indiv_cut_F)
freq(menages_from_indiv3$n_REVENUS_indiv_cut_H)

menages_from_indiv3 <- menages_from_indiv3 %>%
  mutate(
    n_RevenusContribF = case_when(
      n_REVENUS_indiv_cut_F == "Sans revenus" & n_REVENUS_indiv_cut_H == "Sans revenus" ~ 50, 
      n_REVENUS_indiv_cut_F == "Sans revenus" & !is.na(n_REVENUS_indiv_H) ~ 0, 
      n_REVENUS_indiv_cut_H == "Sans revenus" & !is.na(n_REVENUS_indiv_F) ~ 100,
      !is.na(n_REVENUS_indiv_F)&!is.na(n_REVENUS_indiv_H) ~ 
        (n_REVENUS_indiv_F/(n_REVENUS_indiv_F+n_REVENUS_indiv_H))*100))
summary(menages_from_indiv3$n_RevenusContribF)

# Epargne et placements individuels 
freq(menages_from_indiv3$n_PATRIMOINE_indiv_cut_F)
freq(menages_from_indiv3$n_PATRIMOINE_indiv_cut_H)
menages_from_indiv3 <- menages_from_indiv3 %>%
  mutate(
    n_EpargnePartF = case_when(
      n_PATRIMOINE_indiv_cut_F == "Sans économies" & n_PATRIMOINE_indiv_cut_H == "Sans économies" ~ 50, 
      n_PATRIMOINE_indiv_cut_F == "Sans économies" & !is.na(n_PATRIMOINE_indiv_H) ~ 0, 
      n_PATRIMOINE_indiv_cut_H == "Sans économies" & !is.na(n_PATRIMOINE_indiv_F) ~ 100,
      !is.na(n_PATRIMOINE_indiv_F)&!is.na(n_PATRIMOINE_indiv_H) ~ (n_PATRIMOINE_indiv_F/(n_PATRIMOINE_indiv_F+n_PATRIMOINE_indiv_H))*100))
summary(menages_from_indiv3$n_EpargnePartF)

menages_from_indiv <- left_join(menages_from_indiv, 
                                menages_from_indiv3 %>%
                                  select(IDENT_MEN, ends_with("_H"), ends_with("_F"), 
                                         n_RevenusContribF, n_EpargnePartF, MENAGE_RAPPORT_IMMIGR, MENAGE_PUBLICPRIVE)) 




######################################
### TAF : Type agregée de famille ####
######################################

menages_from_indiv$TAF <- menages_from_indiv$TDM8 |>
  fct_recode(
    "Famille recomposée (avec enfant(s) du couple)" = "Couple avec enfant(s) du couple, et avec au moins un enfant d'un seul des deux membres du couple",
    "Famille traditionnelle" = "Couple avec uniquement enfant(s) du couple",
    "Famille recomposée (sans enfant du couple)" = "Couple sans enfant du couple, et avec au moins un enfant d'un seul des deux membres du couple",
    "Famille monoparentale" = "Famille monoparentale (mère + enfant(s))",
    "Famille monoparentale" = "Famille monoparentale (père + enfant(s))"
  )

freq(menages_from_indiv$TAF)

names(menages_from_indiv)
table(menages_from_indiv$TAF, menages_from_indiv$COUPLE_SEXE)

###############################################################################
## AJOUT SUR LES TABLES ####
##############################################################################

menages <- readRDS("Data_output/menages.Rds")
names(menages)
menages[menages == ""] <- NA 
menages <- menages %>%
  left_join(menages_from_indiv, by = "IDENT_MEN")
names(menages)


indiv_fam_enrichie <- indiv_fam_enrichie %>%
  left_join(menages_from_indiv %>% select(-COUPLE_SEXE), by = "IDENT_MEN")
freq(indiv_fam_enrichie$TDM8)
freq(indiv_fam_enrichie$TDM8_SEXE)
freq(indiv_fam_enrichie$TAF)

tab <- table(indiv_fam_enrichie$TAF, indiv_fam_enrichie$MOCO_DET) %>%
  data.frame()
indiv_fam_enrichie %>%
  as_survey_design(weights = PONDIND, 
                   ids = IDENT_MEN) %>%
  tbl_svysummary(
            include = c("MOCO_DET_ENF", "MOCO_DET_ADU",  "ANNEE_BDF"), 
            by = "ANNEE_BDF", 
            percent = "column") %>%
  add_overall(last = T)



tbl_summary(indiv_fam_enrichie, 
            include = c("MOCO_DET_ADU", "COUPLE_SEXE"), 
            by = "COUPLE_SEXE", 
            percent = "row")

tbl_summary(menages, 
            include = c("TAF", "TDM8_SEXE", "APART"), 
            by = "APART", 
            percent = "row")

freq(enfants2$POSITION_FRATERIE)
freq(indiv_fam_enrichie$POSITION_FRATERIE)

dir.create("Data_output/data_recode")
saveRDS(indiv_fam_enrichie, "Data_output/data_recode/indiv.Rds")
saveRDS(menages, "Data_output/data_recode/menages.Rds")

