source('src/data_source.R')

# Regression --------------------------------------------------------------

#Simple linear regression using all variables available

#Simple linear regression (model 1)
model1 <- lm(formula = 
               total_pathways ~
               (managers) +
               (occupied_beds) +
               (nurses_ratio)*
               (senior_doctors_ratio)+
               (operating_theatres)+ 
               (sickness_rate)+
               (all_diagnostics_tests) +
               (covid_beds) +
               (total_ftes)+
               covid_flag,
             offset = (trust_total_catchment),
             data = FINAL_regression_data)

model1 %>% anova()
model1 %>% summary()
model1 %>% performance::check_autocorrelation()

plot(model1)
#Simple Gaussian GAM (model 2)
model2 <- gam::gam(
  total_pathways ~
    s(managers) +
    s(occupied_beds) +
    s(doctors_ratio) *
    s(nurses_ratio) +
    s(operating_theatres)+ 
    s(sickness_rate)+
    s(all_diagnostics_tests) +
    s(covid_beds) +
    covid_flag,
  offset = (trust_total_catchment),
  data = FINAL_regression_data
)

#Note: results be come insignificant when offset included.
#Why? is it the lack of a fixed effects?
#I don't know but too problematic doe noqo
summary(model2)

#Mixed effects model (model 3)
model3 <- lme4::lmer(
  formula = 
    total_pathways ~
    scale(total_ftes) +
    scale(nurses_ratio) *
    scale(senior_doctors_ratio)+
    scale(managers)+
    scale(operating_theatres)+ 
    scale(all_diagnostics_tests) +
    scale(occupied_beds)+
    scale(covid_beds) +
    covid_flag + 
    (1|trust_code), 
  offset = scale(trust_total_catchment),
  data = FINAL_regression_data,
  REML = T)


model3 %>% summary()
model3 %>% check_model()
model3 %>% performance::check_autocorrelation()
model3 %>% performance::check_collinearity()
model3 %>% performance::check_heteroscedasticity()  
model3 %>% performance::check_outliers()
model3 %>% performance::check_predictions()
model3 %>% performance::model_performance()
x<-check_autocorrelation(model1)
plot(x)
summary(mixed_lmer)

compare_performance(model1,model3,model2) %>% plot()

# Diagnostics and further investigation ---------

#how does the data look like? skewed but normalish?
ggplot(FINAL_tidy_data %>% 
         ungroup %>% 
         select(metric,values) %>% 
         drop_na() %>% 
         gather(key='metric',value='values'), aes(values)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~metric, scales = 'free_x')

#residuals v fitted
#Don't notice a difference. Seem evenly distributed with a handful of
#outliers. need to remove but there are a lot of data points so should be OK
plot(mixed_lmer)

#normality assumption: not a fan of thi
qqnorm(residuals(mixed_lmer))

#scale location seems homoskedastic enough 
plot(mixed_lmer,
     sqrt(abs(resid(.)))~fitted(.),
     type=c("p","smooth"), col.line=1)

# ouch
plot(mixed_lmer, rstudent(.) ~ hatvalues(.))
cooks.distance(mixed_lmer)

#oh someone made a package to automate this apparently?!
performance::check_model(mixed_lmer)
#ok something deeply wrong here: what is going on with the residual normality stuff?
#worth investigating further

#worth noting op theatres and diag have high f-val, not enough out of group var
#probaly worth doing something about it at some point..
mixed_lmer %>% anova()

#model for ae
#Mixed effects model (model 3)
model3 <- lme4::lmer(
  formula = 
    admit_ratio ~
    scale(total_ftes) +
    scale(occupied_beds)+
    covid_flag + 
    (1|trust_code), 
  offset = scale(trust_total_catchment),
  data = FINAL_regression_data,
  REML = T)

summary(model3)

