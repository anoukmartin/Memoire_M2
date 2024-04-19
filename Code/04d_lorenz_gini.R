install.packages("ineq")
library("ineq")
# Compute the Lorenz curve Lc{ineq}

#Ménages 
CL_menages <- Lc(famillesToutes$NIVIE, n = famillesToutes$PONDMEN)
CL_menages_df <- data.frame(p = CL_menages$p,L = CL_menages$L)

adultes <- indiv %>%
  filter(ENFANT != 1) %>%
  mutate(REVTOT = REVINDEP + SALAIRES + RETRAITES + CHOMAGE)
adultes$PONDIND <- adultes$PONDIND / mean(adultes$PONDIND)
CL_indiv <- Lc(adultes$REVTOT, 
               n = adultes$PONDIND)

CL_indiv_df <- data.frame(p = CL_indiv$p,L = CL_indiv$L)

# plot
ggplot() +
  geom_line(aes(x=p, y=L), color="#990000", data=CL_menages_df) +
  geom_line(aes(x=p, y=L), color="blue", data=CL_indiv_df) +
  scale_x_continuous(name="Cumulative share of X", limits=c(0,1)) + 
  scale_y_continuous(name="Cumulative share of Y", limits=c(0,1)) +
  geom_abline() + 
  theme_minimal()
