################################################################################-
##################  Tests regression dépenses ##################################
################################################################################-

familles <- readRDS("Data_output/familles.Rds")
dic_fam<- look_for(familles)

conso <- readRDS("Data_output/conso.Rds")
dic_conso <- look_for(conso)


# depIndiv <- readRDS("Data_output/DepIndiv.Rds")
# dic_depIndiv <- look_for(depIndiv)

# calcul du montant de dépenses gloable en lien avec la scolarité 
depScolMen <- conso |> 
  filter(IDENT_MEN %in% familles$IDENT_MEN) %>%
  select(c("IDENT_MEN"), starts_with("C10")) 

sum<-apply(depScolMen[, -1],1,sum,na.rm=TRUE)
familles <- bind_cols(familles, sum)
names(familles)[622] <- "n_DepEns"
familles$n_DepEns <- labelled(familles$n_DepEns, 
                               label = "Depenses d'enseignement")
rm(depScolMen, sum)

names(familles)
vars <- c("n_genreFam", "n_enfantNewUnion", "n_config", "NIVIE", "NENFANTS")
data <- familles |>
  select(n_DepEns, all_of(vars))

data %>%
  tbl_continuous(
    variable = "n_DepEns", 
    statistic = everything() ~ c("{mean} ({p25} - {p75})"))

vars
hist(data$n_DepEns, breaks = 100)

data$NIVIE <- labelled(data$NIVIE/100, 
                       label = "Niveau de vie du ménage (en centaine d'euros)")
reg <- lm(n_DepEns ~ NIVIE + NENFANTS + n_config, 
          data = data)


tbl_regression(reg, intercept = T )%>%
  add_glance_source_note()







# On va chercher à identifer tout ce qui est dépenses socolaires : 
# # - MENS_D
# dic_depenses <- dic_depIndiv |>
#   filter(variable %in% c("MENS_D", "MINSC_D", "MREPET_D" ))
# 

