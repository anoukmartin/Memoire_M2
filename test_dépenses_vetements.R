# STats sur les dépenses individualisables 

depind <- readRDS(file = "Data_output/DepIndiv.Rds") %>%
  var_IDENTIFIANT(IdentIndiv = "NOI", IdentMenage = "IDENT_MEN", "n_IdentIndiv")
depind$n_IdentIndiv
depind$MVET_D
depind$MVETEMENTS_D
summary(depind$MVETEMENTS_D)
hist(depind$MVETEMENTS_D , breaks = 500)

enfants <- readRDS(file = "Data_output/enfantsDuMenage.Rds")

menages <- readRDS(file = "Data_output/menages_parents.Rds")
menages2 <- readRDS(file = "Data_output/menages.Rds")
menages3 <- readRDS(file = "Data_output/familles_parents.Rds")
enfants <- left_join(enfants, menages)
enfants <- left_join(enfants, menages2, by = "IDENT_MEN")
enfants <- left_join(enfants, menages3)
enfants <- left_join(enfants, depind)

names(enfants)
enfants$n_TYPMEN_new





parents <- readRDS(file = "Data_output/parents.Rds")
parents <- parents %>%
  select(n_IdentIndiv, SEXE, n_REVENUSmens)

parents$n_REVENUSmens


enfants <- enfants %>%
  left_join(parents %>%
              rename(n_IdentMere = n_IdentIndiv, 
                     n_REVENUSmens_Mere = n_REVENUSmens)) %>%
  left_join(parents %>%
              rename(n_IdentPere = n_IdentIndiv, 
                     n_REVENUSmens_Pere = n_REVENUSmens)) 

plot(enfants$n_REVENUSmens_Pere, enfants$n_REVENUSmens_Mere)

enfants <- enfants %>%
  mutate(
    n_REVENUSmens_Parents =
      coalesce(n_REVENUSmens_Pere, 0) +
      coalesce(n_REVENUSmens_Mere, 0)
  ) %>%
  group_by(IDENT_MEN, SEXE) %>%
  mutate(
    n_rang_sexe = row_number(desc(AG))
  ) %>%
  ungroup()

summary(enfants$n_REVENUSmens_Parents)


enfants$n_TYPMEN_new
data <- filter(enfants, n_TYPMEN_new == "Recomposée") %>%
  mutate(n_REVENUSmens_Parents = n_REVENUSmens_Parents/100) %>%
  mutate(NIVIE = NIVIE/100) %>% 
  mutate(REVDISP = REVDISP/1200) %>%
  mutate(n_REVENUS_F = n_REVENUS_F/1200) %>%
  mutate(n_REVENUS_H = n_REVENUS_H/1200) %>%
  mutate(MVETEMENTS_D = case_when(is.na(MVETEMENTS_D) ~ 0, 
                                  TRUE ~ MVETEMENTS_D)) %>%
  mutate(part_femmes = n_REVENUS_F / (n_REVENUS_F + n_REVENUS_H))
enfants$n_FractionClasse
enfants$NENFANTS
enfants$NEN
data$n_NPARENTS <- relevel(factor(data$n_NPARENTS), ref = "les deux")
reg1 <- glm(MVETEMENTS_D>0 ~ REVDISP+ n_FractionClasse + NENFANTS +  AG + n_rang_sexe + SEXE + n_NPARENTS:n_REVENUSmens_Parents,
            family = quasibinomial,
          data = data, 
          weights = PONDIND)

tbl_regression(reg1) %>%
  add_significance_stars(thresholds = c(0.001, 0.01, 0.05, 0.1), hide_ci = F, hide_p = F) 


reg2 <- glm(MVETEMENTS_D ~ REVDISP+ n_FractionClasse + NENFANTS +  AG + n_rang_sexe + SEXE + n_NPARENTS:n_REVENUSmens_Parents,
            family = Gamma(link = "log"),
            subset = MVETEMENTS_D  > 0, 
            data = data, 
            weights = PONDIND)

tbl_regression(reg2) %>%
  add_significance_stars(thresholds = c(0.001, 0.01, 0.05, 0.1), hide_ci = F, hide_p = F) 

cor(data$REVDISP,
    data$n_REVENUSmens_Parents,
    use = "complete.obs")

reg2 <- lm(log(MVETEMENTS_D+1) ~ REVDISP + n_FractionClasse + NENFANTS +  AG +  n_rang_sexe + SEXE,
          data = data, 
          weights = PONDIND)

anova(reg1, reg2)
summary(reg1)


tbl_regression(reg2)

summary(reg1)$adj.r.squared
summary(reg2)$adj.r.squared
AIC(reg1, reg2)
BIC(reg1, reg2)
library(fixest)

data$PONDIND

reg_fe <- feols(
  log(MVETEMENTS_D + 1) ~
    REVDISP +
    # part_femmes +
    # n_NPARENTS +
    part_femmes:n_NPARENTS +
    AG +
    SEXE:n_rang_sexe +
    NENFANTS +
    n_FractionClasse,
  weights = ~PONDIND,
  cluster = ~IDENT_MEN,
  data = data
)

reg_fe <- feols(
  MVETEMENTS_D ~ AG + n_rang_sexe:SEXE +  n_NPARENTS*n_REVENUSmens_Parents | IDENT_MEN,
  data = data, 
  weights = ~PONDIND,
  cluster = ~IDENT_MEN
)

summary(data$n_REVENUSmens_Parents)
summary(data$n_NPARENTS)
summary(data$MVETEMENTS_D)

freq(data$n_NPARENTS)
summary(reg_fe)
tbl_regression(reg_fe)
