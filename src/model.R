source('src/data_source.R')

library(betareg)
library(marginaleffects)
library(brms)


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
    scale(covid_beds)  +
    (significant_ratio) +
    years_since_covid + 
    (1|trust_code), 
  offset = scale(trust_total_catchment),
  data = FINAL_regression_data,
  REML = T)


model3 %>% summary()
model3 %>% performance::check_model()
model3 %>% performance::check_autocorrelation()
model3 %>% performance::check_collinearity()
model3 %>% performance::check_heteroscedasticity()  
model3 %>% performance::check_outliers()
model3 %>% performance::check_predictions()
model3 %>% performance::model_performance()
x<-check_autocorrelation(model1)
plot(x)

# Diagnostics and further investigation ---------

#how does the data look like? skewed but normalish?
ggplot2::ggplot(FINAL_tidy_data %>% 
         ungroup %>% 
         select(metric,values) %>% 
         drop_na() %>% 
         gather(key='metric',value='values'), aes(values)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~metric, scales = 'free_x')

#normality assumption: not a fan of thi
qqnorm(residuals(model3))

#scale location seems homoskedastic enough 
plot(model3,
     sqrt(abs(resid(.)))~fitted(.),
     type=c("p","smooth"), col.line=1)

# ouch
plot(model3, rstudent(.) ~ hatvalues(.))
cooks.distance(model3)

#oh someone made a package to automate this apparently?!
performance::check_model(model3)
#ok something deeply wrong here: what is going on with the residual normality stuff?
#worth investigating further

#worth noting op theatres and diag have high f-val, not enough out of group var
#probaly worth doing something about it at some point..
model3 %>% anova()
