source('src/data_source.R')

cobb_douglas_pf <- formula(
  #Cobb-Douglas is a simple Y(L,K) = C(L^a)(K^b) + C
  #Reframed as Log(Y) = Log(C) + aLog(L) + bLog(K)
  'log(admitted_pathways) ~ log(consultant) + log(total_diagnostic_tests) + (1|trust_code)'
)

#Mixed Ef#Mixed Effects Cobb-Douglas Production Function; exponent approach?
model3 <- lme4::lmer(
  formula = cobb_douglas_pf,
  data = final_data |> 
    filter(admitted_pathways >= 1 & total_diagnostic_tests >= 1),
  REML = T)

summary(model3)
