##############################
# 0. Packages
##############################

library(dplyr)
library(tidyr)

indiv <- readRDS("Data_output/indiv.Rds") %>%
  mutate(NOI = if_else(str_length(NOI) == 1, str_glue("0{NOI}"), as.character(NOI))) %>%
  mutate(CONJOINT = if_else(str_length(CONJOINT) == 1, str_glue("0{CONJOINT}"), as.character(CONJOINT))) 



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

##############################
# 1. Passage du TCM en format long
##############################

# Chaque individu devient une ligne par relation avec un autre membre du ménage
liens_long <- indiv %>%
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
    starts_with("LIEN_")
  ) %>%
  left_join(indiv %>% 
              select(IDENT_MEN, NOI, SEXE) %>%
              rename(SEXE_CONJOINT = SEXE),
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

test <- indiv %>%
  group_by(IDENT_MEN)%>%
  summarise(NBS = unique(BS))



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
    SEXE, 
    CONJOINT,
    SEXE_CONJOINT
  ) %>%
  mutate(COUPLE_SEXE = case_when(
    SEXE != SEXE_CONJOINT ~ "Couple de sexes différents", 
    SEXE == SEXE_CONJOINT & SEXE == "1" ~ "Couple d'hommes", 
    SEXE == SEXE_CONJOINT & SEXE == "2" ~ "Couple de femmes"
  ))

str(couples)
freq(couples$COUPLE_SEXE)

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
  )

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
#--------------------------------------------------------------
# Nombre d'enfants
#--------------------------------------------------------------

nb_enfants <- enfants %>%
  
  group_by(
    IDENT_MEN,
    PARENT
  ) %>%
  
  summarise(
    NENFANTS = n_distinct(ENFANT),
    .groups="drop"
  ) %>%
  
  rename(
    NOI = PARENT
  )


str(nb_enfants)
freq(nb_enfants$NENFANTS)


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
freq(nb_freres_soeurs$N_FRERESSOEURS_delca)


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
    BEAUPARENT = CONJOINT,
    SEXE_BEAUPARENT = SEXE_CONJOINT
  ) 



beaux_parents <- beaux_parents %>%
  
  anti_join(
    parents %>%
      rename(
        BEAUPARENT=PARENT
      ),
    by=c(
      "IDENT_MEN",
      "ENFANT",
      "BEAUPARENT"
    )
  )

nb_beaux_parents <- beaux_parents %>%
  
  group_by(
    IDENT_MEN,
    ENFANT
  ) %>%
  
  summarise(
    NBEAUX_PARENTS =
      n_distinct(BEAUPARENT),
    SEXE_BEAUPARENT = list(na.omit(SEXE_BEAUPARENT)),
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
        length(x) == 0 ~ NA,
        TRUE ~ NA_character_
      )
      
    })
  )

freq(nb_beaux_parents$SEXE_BEAUPARENT)

# ####################NBEAUX_PARENTS# ###############################################################
# # 5. FRATRIES
# ###############################################################
# #
# # On compare tous les enfants du ménage entre eux
# #
# ###############################################################

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
              rename(NBEAUX_PARENTS_ENFANT1 = NBEAUX_PARENTS), by = c("IDENT_MEN", "ENFANT1" = "NOI")) %>%
  left_join(nb_beaux_parents %>%
              rename(NBEAUX_PARENTS_ENFANT2 = NBEAUX_PARENTS), by = c("IDENT_MEN", "ENFANT2" = "NOI"))

 
# freq(parents_communs$NPARENTS_COMMUNS)
# # On a bcp d'enfant qui ont un seul parent en commun mais prbablement lié aux familles monoparentales
# 
###############################################################
# 6. FRERES / SOEURS TOUS CONFONDUS
###############################################################


freres_soeurs <- parents_communs


nb_freres_soeurs <- bind_rows(

  freres_soeurs %>%
    select(
      IDENT_MEN,
      NOI=ENFANT1,
      AUTRE=ENFANT2
    ),

  freres_soeurs %>%
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
    nb_beaux_enfants,
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
  
  mutate(
    
    across(
      c(
        NPARENTS,
        NENFANTS,
        NCONJOINT,
        NBEAUX_ENFANTS,
        NBEAUX_ENFANTS_CONJOINT,
        NBEAUX_PARENTS,
        N_FRERES_SOEURS_TOUS,
        N_DEMI_FRERES_SOEURS,
        N_QUASI_FRERES_SOEURS
      ),
      ~replace_na(.x,0)
    )
    
  )


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
      NENFANTS==0 &
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
      NPARENTS==0 ~ "Autre adulte sans lien direct dans le ménage"))




indiv_fam <- indiv_fam %>%
  mutate(MOCO_DET = case_when(
    ENFANT == "1" ~ MOCO_DET_ENF,
    ENFANT == "2" ~ MOCO_DET_ADU)) %>%
  mutate(MOCO_DET_ADU_SEXE = case_when(
    (!is.na(MOCO_DET_ADU)) & SEXE == "1" ~ str_replace(MOCO_DET_ADU, "Adulte", "Homme"),
    (!is.na(MOCO_DET_ADU)) & SEXE == "2" ~ str_replace(MOCO_DET_ADU, "Adulte", "Femme"))) %>%
 
    
    
freq(indiv_fam$MOCO_DET)
freq(indiv_fam$MOCO_DET_ADU_SEXE)

anomalie <- indiv_fam[indiv_fam$MOCO_DET == "Autre adulte", ]

###############################################################
# 7. CONSTRUCTION DES NOYAUX FAMILIAUX
###############################################################
#
# On travaille maintenant au niveau famille
# et non ménage
#
###############################################################


# Couples présents

familles_couples <- couples %>%
  
  left_join(
    enfants,
    by=c(
      "IDENT_MEN",
      "NOI"="PARENT"
    )
  ) %>%
  
  rename(
    PARENT1=NOI,
    ENFANT=ENFANT
  )



# enfants communs du couple

enfants_communs <- familles_couples %>%
  
  inner_join(
    enfants,
    by=c(
      "IDENT_MEN",
      "CONJOINT"="PARENT",
      "ENFANT"
    )
  ) %>%
  
  distinct(
    IDENT_MEN,
    PARENT1,
    ENFANT
  )



# nombre enfants communs

nb_enfants_communs <- enfants_communs %>%
  
  group_by(
    IDENT_MEN,
    PARENT1
  ) %>%
  
  summarise(
    ENFANTS_COMMUNS=n_distinct(ENFANT),
    .groups="drop"
  )



###############################################################
# 8. CLASSIFICATION DES FAMILLES
###############################################################


familles <- couples %>%
  
  left_join(
    nb_enfants_communs,
    by=c(
      "IDENT_MEN",
      "NOI"="PARENT1"
    )
  ) %>%
  
  mutate(
    ENFANTS_COMMUNS=
      replace_na(ENFANTS_COMMUNS,0)
  )



###############################################################
# 9. TAF ET RECOMPOSEE
###############################################################


familles <- familles %>%
  
  mutate(
    
    RECOMPOSEE = case_when(
      
      ENFANTS_COMMUNS>0 &
        NBEAUX_ENFANTS>0
      ~1,
      
      
      ENFANTS_COMMUNS==0 &
        NBEAUX_ENFANTS>0
      ~2,
      
      
      TRUE
      ~0
      
    ),
    
    
    TAF = case_when(
      
      NBEAUX_ENFANTS>0
      ~3,
      
      
      ENFANTS_COMMUNS>0
      ~2,
      
      
      TRUE
      ~0
      
    )
    
  )



###############################################################
# 10. TDM8
###############################################################
#
# Cette variable est construite à partir des familles
# identifiées
#
###############################################################


familles <- familles %>%
  
  mutate(
    
    TDM8 = case_when(
      
      
      RECOMPOSEE==2
      ~302,
      
      
      RECOMPOSEE==0 &
        ENFANTS_COMMUNS>0
      ~310,
      
      
      RECOMPOSEE==1
      ~311,
      
      
      TRUE
      ~200
      
    )
    
  )



###############################################################
# FIN
#
# indiv_fam :
#   classification individuelle MOCO_DET
#
# familles :
#   classification familiale TDM8 / TAF / RECOMPOSEE
#
###############################################################