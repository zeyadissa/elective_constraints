## Mapping workforce and RTT specialties

library(readxl)
Matching_workforce_and_RTT_names <- read_excel("const/Matching workforce and RTT names.xlsx")

#Converting all characters to lowercase
Matching_workforce_and_RTT_names2 <- Matching_workforce_and_RTT_names %>%
  mutate(across(where(is.character), tolower))

Workforce_names<-Workforce_names %>% 
  mutate(across(where(is.character), tolower))


## Workforce

# Converting all characters to lowercase
workforce_data4 <- workforce_data %>%
  mutate(across(where(is.character), tolower))

# Rename 'specialty' to 'Workforce' in workforce_data
workforce_data4 <- workforce_data4 %>%
  rename(Workforce = specialty)

workforce_data4 <- workforce_data4 %>%
  left_join(select(Workforce_names, Workforce, Combined), 
            by = "Workforce")

## RTT 

# Convert all character columns in a data frame to lowercase
rtt_data2 <- rtt_data %>%
  mutate(across(where(is.character), tolower))

rtt_data2<-rtt_data2 %>% 
  rename (RTT = treatment_function_name)

rtt_data2 <- rtt_data2 %>%
  left_join(Matching_workforce_and_RTT_names2, by = "RTT", relationship = "many-to-many")
