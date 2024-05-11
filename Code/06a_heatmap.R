
infosBDF <- readRDS("Data_output/infosBDF.Rds")
indiv <- readRDS("Data_output/parents.Rds") %>%
  rec_DIP(Var = "DIP14", NewVar = "DIPL") %>%
  rec_CSP6(Var = "CS24", NewVar = "CS6") %>%
  rec_DIP14(Var = "DIP14") %>%
  rec_AG6() %>%
  select(IDENT_MEN, n_IdentIndiv, n_IdentConjoint, SEXE, DIPL, DIP14, n_REVENUS, n_PATRIMOINE, CS6, CS24, AG6) %>%
  filter(!is.na(n_IdentConjoint))

femmes <- indiv %>% 
  filter(SEXE == "2") %>%
  select(-n_IdentConjoint)

hommes <- indiv %>% 
  filter(SEXE == "1") %>%
  select(-n_IdentIndiv)

coupleshet <- full_join(hommes, femmes, 
                        by = c("n_IdentConjoint" = "n_IdentIndiv"), 
            suffix = c("_H", "_F")) %>% 
  select(-IDENT_MEN_F) %>%
  rename(IDENT_MEN = "IDENT_MEN_H")


familles <- readRDS("Data_output/familles.Rds") %>%
  select(IDENT_MEN, n_config, TYPMEN5, PONDMEN) %>%
  rec_TYPMEN5() 



# HEATMAP homogame de diplome ###############################################
VAR <- "DIPL"

## Tous les couples ######################################################### 
couples <- coupleshet %>%
  left_join(familles, by = "IDENT_MEN") 

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
souspop <- familles %>%
  filter(TYPMEN5 == "Couple avec au moins un enfant") %>%
  filter(n_config == "Recomposée")

couples <- coupleshet %>%
  left_join(familles, by = "IDENT_MEN") %>%
  filter(IDENT_MEN %in% souspop$IDENT_MEN)

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
souspop <- familles %>%
  filter(TYPMEN5 == "Couple avec au moins un enfant")

couples <- coupleshet %>%
  left_join(familles, by = "IDENT_MEN") %>%
  filter(IDENT_MEN %in% souspop$IDENT_MEN)

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
tabcouplesh$pop <- "Couples avec enfants"
tabcouplesh2$pop <- "Nouvelles unions"
tabcouplesh1$pop <- "Ensemble des couples"

tabcouplesh <- bind_rows(tabcouplesh1, tabcouplesh, tabcouplesh2)

### Plot ####
gg <- ggplot(tabcouplesh, 
             aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = FreqRound), color = "black", size = 3.2) +
  xlab("Femme") + ylab("Homme") +
  scale_fill_gradient(low = "white", high = "darkgreen") + 
  facet_grid(. ~ pop)+
  theme_memoire() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
gg

### Enregistrement du plot ####
saveTableau(gg, 
            type = "tileplot", 
            label = "homogamiedeuxgraphs", 
            description = "Diplôme des femmes en fonction de celui des hommes", 
            champ = c(
              paste0("Couples vivants en ", infosBDF$champ),
              paste0("Couple unis secondairement vivants en ", infosBDF$champ)), 
            ponderation = T,
            n = nrow(couples))


# HEATMAP homogamie de CSP ######################################################
VAR <- "CS6"

## Tous les couples ######################################################### 
couples <- coupleshet %>%
  left_join(familles, by = "IDENT_MEN") 

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
souspop <- familles %>%
  filter(TYPMEN5 == "Couple avec au moins un enfant") %>%
  filter(n_config == "Recomposée")

couples <- coupleshet %>%
  left_join(familles, by = "IDENT_MEN") %>%
  filter(IDENT_MEN %in% souspop$IDENT_MEN)

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
souspop <- familles %>%
  filter(TYPMEN5 == "Couple avec au moins un enfant")

couples <- coupleshet %>%
  left_join(familles, by = "IDENT_MEN") %>%
  filter(IDENT_MEN %in% souspop$IDENT_MEN)

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
tabcouplesh$pop <- "Couples avec enfants"
tabcouplesh2$pop <- "Nouvelles unions"
tabcouplesh1$pop <- "Ensemble des couples"

tabcouplesh <- bind_rows(tabcouplesh1, tabcouplesh, tabcouplesh2)

### Plot ####
gg <- ggplot(tabcouplesh, 
             aes(x = Var2, y = Var1, fill = Freq)) +
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
              paste0("Couples vivants en ", infosBDF$champ),
              paste0("Couple unis secondairement vivants en ", infosBDF$champ)), 
            ponderation = T,
            n = nrow(couples))

### on nettoie

rm(couples, coupleshet, familles, femmes, gg, hommes, indiv, margesf, margesh, souspop, tabcouplesh, tabcouplesh1, tabcouplesh2, tabcouples, tabcouplesf, VAR)
