## Mapping workforce and RTT specialties

library(readxl)
library(dplyr)

#Converting all characters to lowercase

Workforce_names<-Workforce_names %>% 
  mutate(across(where(is.character), tolower))

## Workforce

# Converting all characters to lowercase
workforce_data4 <- workforce_data %>%
  mutate(across(where(is.character), tolower))

# Rename 'specialty' to 'Workforce' in workforce_data
workforce_data4 <- workforce_data4 %>%
  rename(Workforce = specialty)

workforce_data<-workforce_data %>% 
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

RTT_names <- RTT_names %>%
  mutate(across(where(is.character), tolower))

rtt_data2$RTT <- tolower(rtt_data2$RTT)

rtt_data2 <- rtt_data2 %>%
  left_join(RTT_names, by = "RTT", relationship = "many-to-many")
