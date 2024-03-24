ae_regression_data <- FINAL_regression_data %>%
  dplyr::mutate(percent_breach = (ae_breaches+admission_breaches)/(ae_attendances+ae_admissions),
         breaches = (ae_breaches+admission_breaches)) %>% %>%
  select(occupied_ratio,percent_breach) %>%
  drop_na()

#model
ae_model_1 <- brm(
  #set up parameters: coi and zoi represents one and zero models respectively
  bf(percent_breach ~  occupied_ratio,
     phi ~  occupied_ratio,
     zoi ~ occupied_ratio,
     coi ~ occupied_ratio),
  data = ae_regression_data,
  #we have full occupancy and no occupancies, so zoib needed
  family = zero_one_inflated_beta(),
  init = 0,
  chains = 4, 
  iter = 1000, 
  warmup = 500,
  cores = 4, 
  seed = 1234, 
  #save because this takes ages to run
  file = "ae_model"
)

ae_beta_1 <- ae_model_1 %>%
  marginaleffects::avg_comparisons(variables = "occupied_ratio") 


yy%>% 
  posterior_draws()

beta_bayes_pred_1 <- model_beta_bayes_1 %>% 
  epred_draws(newdata = expand_grid(quota = FALSE,
                                    polyarchy = seq(0, 100, by = 1)))