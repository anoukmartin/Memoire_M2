
infosBDF <- readRDS("Data_output/infosBDF.Rds")

couples <- readRDS("Data_output/familles_parents.Rds")

couples %>%
  filter(n_TYPMEN_new %in% c("Couple sans enfant", "Traditionnelle", "Recomposée")) %>%
  mutate(PONDMEN = PONDMEN/mean(couples$PONDMEN),
         n_TYPMEN_new = droplevels(n_TYPMEN_new)) %>%
  as_survey_design(weights = PONDMEN) %>%
  tbl_svysummary(
  by = n_TYPMEN_new, 
  include = c("n_TYPMEN_new", "hetero")
) %>%
  add_overall()
couples$n_TYPMEN_new


coupleshet <- readRDS("Data_output/familles_parents.Rds") %>%
  filter(hetero == "Hetero") %>%
  mutate(
  #DIP7_F = DIP7_F %>%
  #   fct_recode(
  #     NULL = "NULL",
  #     "Doctorat,\ningénieur,\ngrande école" = "Diplôme universitaire du 3eme cycle, ingénieur, grande école",
  #     "Master" = "Diplôme universitaire de 2eme cycle",
  #     "Licence, BTS, DUT, diplôme santé social" = "Diplôme universitaire de 1er cycle, BTS, DUT, diplôme santé social (niveau bac + 2)"
  #   ), 
  #   DIP7_H = DIP7_H %>%
  #     fct_recode(
  #       NULL = "NULL",
  #       "Doctorat, ingénieur, grande école" = "Diplôme universitaire du 3eme cycle, ingénieur, grande école",
  #       "Master" = "Diplôme universitaire de 2eme cycle",
  #       "Licence, BTS, DUT, diplôme santé social" = "Diplôme universitaire de 1er cycle, BTS, DUT, diplôme santé social (niveau bac + 2)"
  #     ), 
    CS12_F = CS12_F %>%
      fct_recode(
        "Cadre/chef−fe d'etp, prof. lib." = "Cadre/chef-fe d'entreprise, profession libérale",
        NULL = "NULL"
      ), 
    CS12_H = CS12_H %>%
      fct_recode(
        "Cadre/chef−fe d'etp, prof. lib." = "Cadre/chef-fe d'entreprise, profession libérale",
        NULL = "NULL"
      ))
    

freq(coupleshet$DIP7_F) 
freq(coupleshet$CS12_F)
#irec(coupleshet$CS12_F)

# HEATMAP homogame de diplome ###############################################
VAR <- "DIP7"

## Tous les couples ######################################################### 
couples <- coupleshet

tabcouples <- wtd.table(couples[[paste0(VAR, "_H")]], couples[[paste0(VAR, "_F")]], weights = couples$PONDMEN)
tabcouplesh <- lprop(tabcouples, total = T)
margesf <- tabcouplesh %>%
  as.data.frame() %>%
  filter(Var1 == "All") %>%
  filter(Var2 != "Total") %>%
  rename(FreqVar2 = "Freq")
tabcouplesf <- cprop(tabcouples, total = T)
margesh <- tabcouplesf %>%
  as.data.frame() %>%
  filter(Var2 == "All") %>%
  filter(Var1 != "Total") %>%
  rename(FreqVar1 = "Freq")
tabcouplesh <- lprop(tabcouples, total = F) %>%
  as.data.frame() %>%
  mutate(FreqRound = round(Freq, 0)) %>%
  left_join(margesh[, c("Var1", "FreqVar1")], by = "Var1")  %>%
  left_join(margesf[, c("Var2", "FreqVar2")], by = "Var2")  

# tabcouplesh$Var1 <- paste0(tabcouplesh$Var1, " (", round(tabcouplesh$FreqVar1, 0), "%)")
# tabcouplesh$Var2 <- paste0(tabcouplesh$Var2, " (", round(tabcouplesh$FreqVar2, 0), "%)")

tabcouplesh1 <- tabcouplesh

# Plot
gg <- tabcouplesh %>%
  mutate(Var2 = fct_rev(Var2), 
         Var1 = fct_rev(Var1)) %>%
  ggplot( 
       aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = FreqRound), color = "black", size = 3.2) +
  xlab("Femme") + ylab("Homme") +
  scale_fill_gradient(low = "white", high = "darkgreen") + 
  theme_memoire() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
gg

# saveTableau(gg, 
#             type = "tileplot", 
#             label = "homogamieTous", 
#             description = "Diplôme des femmes en fonction de celui des hommes", 
#             champ = paste0("Couples vivants en ", infosBDF$champ), 
#             ponderation = T,
#             n = nrow(couples))


## Familles recomposées ########################################################
couples <- coupleshet %>%
  filter(n_TYPMEN_new == "Recomposée")

tabcouples <- wtd.table(couples[[paste0(VAR, "_H")]], couples[[paste0(VAR, "_F")]], weights = couples$PONDMEN)
tabcouplesh <- lprop(tabcouples, total = T)
margesf <- tabcouplesh %>%
  as.data.frame() %>%
  filter(Var1 == "All") %>%
  filter(Var2 != "Total") %>%
  rename(FreqVar2 = "Freq")
tabcouplesf <- cprop(tabcouples, total = T)
margesh <- tabcouplesf %>%
  as.data.frame() %>%
  filter(Var2 == "All") %>%
  filter(Var1 != "Total") %>%
  rename(FreqVar1 = "Freq")
tabcouplesh <- lprop(tabcouples, total = F) %>%
  as.data.frame() %>%
  mutate(FreqRound = round(Freq, 0)) %>%
  left_join(margesh[, c("Var1", "FreqVar1")], by = "Var1")  %>%
  left_join(margesf[, c("Var2", "FreqVar2")], by = "Var2")  
# tabcouplesh$Var1 <- paste0(tabcouplesh$Var1, " (", round(tabcouplesh$FreqVar1, 0), "%)")
# tabcouplesh$Var2 <- paste0(tabcouplesh$Var2, " (", round(tabcouplesh$FreqVar2, 0), "%)")
tabcouplesh2 <- tabcouplesh


# Plot
gg <- ggplot(tabcouplesh, 
       aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = FreqRound), color = "black", size = 3.2) +
  xlab("Femme") + ylab("Homme") +
  scale_fill_gradient(low = "white", high = "darkgreen") + 
  theme_memoire() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
gg


## Couples avec enfants  #######################################################

couples <- coupleshet %>%
  filter(TYPMEN5 == "Couple avec au moins un enfant")

tabcouples <- wtd.table(couples[[paste0(VAR, "_H")]], couples[[paste0(VAR, "_F")]], weights = couples$PONDMEN)
tabcouplesh <- lprop(tabcouples, total = T)
margesf <- tabcouplesh %>%
  as.data.frame() %>%
  filter(Var1 == "All") %>%
  filter(Var2 != "Total") %>%
  rename(FreqVar2 = "Freq")
tabcouplesf <- cprop(tabcouples, total = T)
margesh <- tabcouplesf %>%
  as.data.frame() %>%
  filter(Var2 == "All") %>%
  filter(Var1 != "Total") %>%
  rename(FreqVar1 = "Freq")
tabcouplesh <- lprop(tabcouples, total = F) %>%
  as.data.frame() %>%
  mutate(FreqRound = round(Freq, 0)) %>%
  left_join(margesh[, c("Var1", "FreqVar1")], by = "Var1")  %>%
  left_join(margesf[, c("Var2", "FreqVar2")], by = "Var2")  
# tabcouplesh$Var1 <- paste0(tabcouplesh$Var1, " (", round(tabcouplesh$FreqVar1, 0), "%)")
# tabcouplesh$Var2 <- paste0(tabcouplesh$Var2, " (", round(tabcouplesh$FreqVar2, 0), "%)")


# Plot
gg <- ggplot(tabcouplesh, 
             aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = FreqRound), color = "black", size = 3.2) +
  xlab("Femme") + ylab("Homme") +
  scale_fill_gradient(low = "white", high = "darkgreen") + 
  theme_memoire() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
gg

## Assemblage ##################################################################


### Données #####
tabcouplesh$pop <- "Ensemble des\ncouples avec enfant(s)"
tabcouplesh2$pop <- "Couples avec enfant(s)\nissus d'union(s)\nprécédante(s)"
tabcouplesh1$pop <- "Ensemble des couples"

tabcouplesh <- bind_rows(tabcouplesh1, tabcouplesh, tabcouplesh2)

### Plot ####
gg <- tabcouplesh %>%
  mutate(Var1 = fct_rev(Var1), 
         Var2 = fct_rev(Var2), 
         pop = factor(pop, 
                      levels = c("Couples avec enfant(s)\nissus d'union(s)\nprécédante(s)",
                                 "Ensemble des\ncouples avec enfant(s)", 
                                 "Ensemble des couples"))) %>%
  ggplot( 
             aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = FreqRound), color = "black", size = 3.2) +
  xlab("Femme") + ylab("Homme") +
  scale_fill_gradient(low = "white", high = "darkgreen") + 
  facet_grid(. ~ pop) +
  theme_memoire() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
gg

### Enregistrement du plot ####
saveTableau(gg, 
            type = "tileplot", 
            label = "homogamiedeuxgraphs", 
            description = "Diplôme des femmes en fonction de celui des hommes", 
            champ = c(
              paste0("Couples de sexes différents vivants en ", infosBDF$champ),
              paste0("Couple unis secondairement vivants en ", infosBDF$champ)), 
            ponderation = T,
            n = nrow(couples))


# HEATMAP homogamie de CSP ######################################################
VAR <- "CS12"

## Tous les couples ######################################################### 
couples <- coupleshet  

tabcouples <- wtd.table(couples[[paste0(VAR, "_H")]], couples[[paste0(VAR, "_F")]], weights = couples$PONDMEN)
tabcouplesh <- lprop(tabcouples, total = T)
margesf <- tabcouplesh %>%
  as.data.frame() %>%
  filter(Var1 == "All") %>%
  filter(Var2 != "Total") %>%
  rename(FreqVar2 = "Freq")
tabcouplesf <- cprop(tabcouples, total = T)
margesh <- tabcouplesf %>%
  as.data.frame() %>%
  filter(Var2 == "All") %>%
  filter(Var1 != "Total") %>%
  rename(FreqVar1 = "Freq")
tabcouplesh <- lprop(tabcouples, total = F) %>%
  as.data.frame() %>%
  mutate(FreqRound = round(Freq, 0)) %>%
  left_join(margesh[, c("Var1", "FreqVar1")], by = "Var1")  %>%
  left_join(margesf[, c("Var2", "FreqVar2")], by = "Var2")  

# tabcouplesh$Var1 <- paste0(tabcouplesh$Var1, " (", round(tabcouplesh$FreqVar1, 0), "%)")
# tabcouplesh$Var2 <- paste0(tabcouplesh$Var2, " (", round(tabcouplesh$FreqVar2, 0), "%)")

tabcouplesh1 <- tabcouplesh

# Plot
gg <- ggplot(tabcouplesh, 
             aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = FreqRound), color = "black", size = 3.2) +
  xlab("Femme") + ylab("Homme") +
  scale_fill_gradient(low = "white", high = "darkgreen") + 
  theme_memoire() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
gg

# saveTableau(gg, 
#             type = "tileplot", 
#             label = "homogamieTous", 
#             description = "Diplôme des femmes en fonction de celui des hommes", 
#             champ = paste0("Couples vivants en ", infosBDF$champ), 
#             ponderation = T,
#             n = nrow(couples))


## Familles recomposées ########################################################
couples <- coupleshet %>%
  filter(n_TYPMEN_new == "Recomposée")

tabcouples <- wtd.table(couples[[paste0(VAR, "_H")]], couples[[paste0(VAR, "_F")]], weights = couples$PONDMEN)
tabcouplesh <- lprop(tabcouples, total = T)
margesf <- tabcouplesh %>%
  as.data.frame() %>%
  filter(Var1 == "All") %>%
  filter(Var2 != "Total") %>%
  rename(FreqVar2 = "Freq")
tabcouplesf <- cprop(tabcouples, total = T)
margesh <- tabcouplesf %>%
  as.data.frame() %>%
  filter(Var2 == "All") %>%
  filter(Var1 != "Total") %>%
  rename(FreqVar1 = "Freq")
tabcouplesh <- lprop(tabcouples, total = F) %>%
  as.data.frame() %>%
  mutate(FreqRound = round(Freq, 0)) %>%
  left_join(margesh[, c("Var1", "FreqVar1")], by = "Var1")  %>%
  left_join(margesf[, c("Var2", "FreqVar2")], by = "Var2")  
# tabcouplesh$Var1 <- paste0(tabcouplesh$Var1, " (", round(tabcouplesh$FreqVar1, 0), "%)")
# tabcouplesh$Var2 <- paste0(tabcouplesh$Var2, " (", round(tabcouplesh$FreqVar2, 0), "%)")
tabcouplesh2 <- tabcouplesh


# Plot
gg <- ggplot(tabcouplesh, 
             aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = FreqRound), color = "black", size = 3.2) +
  xlab("Femme") + ylab("Homme") +
  scale_fill_gradient(low = "white", high = "darkgreen") + 
  theme_memoire() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
gg


## Couples avec enfants  #######################################################
couples <- coupleshet %>%
  filter(TYPMEN5 == "Couple avec au moins un enfant")

tabcouples <- wtd.table(couples[[paste0(VAR, "_H")]], couples[[paste0(VAR, "_F")]], weights = couples$PONDMEN)
tabcouplesh <- lprop(tabcouples, total = T)
margesf <- tabcouplesh %>%
  as.data.frame() %>%
  filter(Var1 == "All") %>%
  filter(Var2 != "Total") %>%
  rename(FreqVar2 = "Freq")
tabcouplesf <- cprop(tabcouples, total = T)
margesh <- tabcouplesf %>%
  as.data.frame() %>%
  filter(Var2 == "All") %>%
  filter(Var1 != "Total") %>%
  rename(FreqVar1 = "Freq")
tabcouplesh <- lprop(tabcouples, total = F) %>%
  as.data.frame() %>%
  mutate(FreqRound = round(Freq, 0)) %>%
  left_join(margesh[, c("Var1", "FreqVar1")], by = "Var1")  %>%
  left_join(margesf[, c("Var2", "FreqVar2")], by = "Var2")  
# tabcouplesh$Var1 <- paste0(tabcouplesh$Var1, " (", round(tabcouplesh$FreqVar1, 0), "%)")
# tabcouplesh$Var2 <- paste0(tabcouplesh$Var2, " (", round(tabcouplesh$FreqVar2, 0), "%)")


# Plot
gg <- ggplot(tabcouplesh, 
             aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = FreqRound), color = "black", size = 3.2) +
  xlab("Femme") + ylab("Homme") +
  scale_fill_gradient(low = "white", high = "darkgreen") + 
  theme_memoire() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
gg

## Assemblage ##################################################################


### Données #####
tabcouplesh$pop <- "Ensemble des\ncouples avec enfant(s)"
tabcouplesh2$pop <- "Couples avec enfant(s)\nissus d'union(s)\nprécédante(s)"
tabcouplesh1$pop <- "Ensemble des couples"

tabcouplesh <- bind_rows(tabcouplesh1, tabcouplesh, tabcouplesh2)

### Plot ####
gg <- tabcouplesh %>%
  mutate(Var1 = fct_rev(Var1), 
         Var2 = fct_rev(Var2), 
         pop = factor(pop, 
                      levels = c("Couples avec enfant(s)\nissus d'union(s)\nprécédante(s)",
                                 "Ensemble des\ncouples avec enfant(s)", 
                                 "Ensemble des couples"))) %>%
  ggplot(aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = FreqRound), color = "black", size = 3.2) +
  xlab("Femme") + ylab("Homme") +
  scale_fill_gradient(low = "white", high = "darkred") + 
  facet_grid(. ~ pop)+
  theme_memoire() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
gg

### Enregistrement du plot ####
saveTableau(gg, 
            type = "tileplot", 
            label = "homogamieCSPtroisgraphs", 
            description = "CSP des femmes en fonction de celle des hommes", 
            champ = c(
              paste0("Couples de sexes différents en vivants", infosBDF$champ),
              paste0("Couple unis secondairement vivants en ", infosBDF$champ)), 
            ponderation = T,
            n = nrow(couples))

### on nettoie

rm(couples, coupleshet, familles, femmes, gg, hommes, indiv, margesf, margesh, souspop, tabcouplesh, tabcouplesh1, tabcouplesh2, tabcouples, tabcouplesf, VAR)
