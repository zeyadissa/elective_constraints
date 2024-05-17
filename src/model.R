source('src/data_source.R')

library(betareg)
library(marginaleffects)
library(brms)

data <- FINAL_regression_data %>%
  mutate(
    backlog = low + moderate + high + significant,
    serious = significant + high,
    non_serious = low + moderate,
    ratio = ( serious / (non_serious) )
  )

#Mixed effects model (model 3)
model3 <- lme4::lmer(
  formula = 
    total_pathways ~
    scale(total_ftes) +
    scale(nurses_ratio) *
    scale(senior_doctors_ratio) +
    scale(operating_theatres)+ 
    scale(occupied_beds)+
    scale(covid_beds)  +
    ratio +
    years_since_covid + 
    (1|trust_code), 
  offset = scale(trust_total_catchment),
  data = data,
  REML = T)


model3 %>% summary()
model3 %>% performance::check_model()
model3 %>% performance::check_autocorrelation()
model3 %>% performance::check_collinearity()
model3 %>% performance::check_heteroscedasticity()  
model3 %>% performance::check_outliers()
model3 %>% performance::model_performance()