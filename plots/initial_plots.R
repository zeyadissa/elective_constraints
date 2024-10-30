
####################### Exploratory descriptive data analysis ###########################

## Libraries ##
library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)
library(Gini)

########## High level RTT graphs ##############

#### Overall completed pathways for admitted patients ####

completed_pathways<-rtt_data %>% 
  group_by(date) %>% 
  summarise(completed_pathways_for_admitted_patients = sum(completed_pathways_for_admitted_patients, na.rm = TRUE))

completed_pathways_graph<-ggplot(completed_pathways,aes(x=date,y=completed_pathways_for_admitted_patients))+
  geom_line()+
  labs(title = "Total completed pathways for admitted patients",
       x = "Date",
       y = "Total completed pathways for admitted patients",
       color = "Organization Code") +  
  scale_y_continuous(labels = label_number()) +
  theme_minimal()

ggsave(filename = "completed_pathways.jpeg", plot = completed_pathways_graph, width = 10, height = 6, dpi = 300)

#### Incomplete pathways ####

incomplete_pathways<-rtt_data %>% 
  group_by(date) %>% 
  summarise(incomplete_pathways = sum(incomplete_pathways, na.rm = TRUE))

incomplete_pathways_graph<-ggplot(incomplete_pathways,aes(x=date,y=incomplete_pathways))+
  geom_line()+
  labs(title = "Total incompleted pathways",
       x = "Date",
       y = "Total incomplete pathways")+
  scale_y_continuous(labels = label_number()) +
  theme_minimal()

ggsave(filename = "incomplete_pathways.jpeg", plot = incomplete_pathways_graph, width = 10, height = 6, dpi = 300)

#### Overall completed pathways for non-admitted patients ####

nonadmitted_completed_pathways<-rtt_data %>% 
  group_by(date) %>% 
  summarise(nonadmitted_completed_pathways = sum(completed_pathways_for_non_admitted_patients, na.rm = TRUE))

nonadmitted_completed_pathways_graph<-ggplot(nonadmitted_completed_pathways,aes(x=date,y=nonadmitted_completed_pathways))+
  geom_line()+
  labs(title = "Total completed patients for non admitted",
       x = "Date",
       y = "Total completed patients for non admitted")+
  scale_y_continuous(labels = label_number()) +
  theme_minimal()

ggsave(filename = "nonadmitted_completed_pathways_graph.jpeg", plot = nonadmitted_completed_pathways_graph, width = 10, height = 6, dpi = 300)

#### Completed pathways for admitted patients by specialties ####

completed_pathways_specialty <- rtt_data2 %>% 
  group_by(date, Combined) %>% 
  summarise(completed_pathways_for_admitted_patients = sum(completed_pathways_for_admitted_patients, na.rm = TRUE))

completed_pathways_specialty <- completed_pathways_specialty %>%
  filter(!is.na(Combined))

completed_pathways_specialty_graph <- ggplot(completed_pathways_specialty, aes(x = date, y = completed_pathways_for_admitted_patients, color = Combined, group = Combined)) +
  geom_line() +
  labs(
    title = "Total Completed Patients for Admitted",
    x = "Date",
    y = "Total Completed Patients for Admitted",
    color = "Combined"  # Legend title
  ) +
  scale_y_continuous(labels = label_number()) +  # Change y-axis to regular number format
  theme_minimal()

ggsave(filename = "completed_pathways_specialty_graph.jpeg", plot = completed_pathways_specialty_graph, width = 10, height = 6, dpi = 300)

#### Completed pathways for admitted patients index by specialty ####

reference_values <- completed_pathways_specialty %>%
  filter(date == as.Date("2018-04-01")) %>% 
  group_by(Combined) %>%
  summarise(reference_value = sum(completed_pathways_for_admitted_patients, na.rm = TRUE))

completed_pathways_specialty_index <- completed_pathways_specialty_index %>%
  left_join(reference_values, by = "Combined") %>%  
  mutate(index = (completed_pathways_for_admitted_patients / reference_value) * 100) %>%
  select(-reference_value)  

completed_pathways_specialty_index_graph <- ggplot(completed_pathways_specialty_index, aes(x = date, y = index, color = Combined, group = Combined)) +
  geom_line() +
  labs(
    title = "Index of Total Admitted Patients with Completed Pathways (Base: April 2018 = 100)",
    x = "Date",
    y = "Index (April 2018 = 100)",
    color = "Combined"
  ) +
  scale_y_continuous(labels = label_number()) +
  theme_minimal()  

ggsave(filename = "completed_pathways_specialty_index_graph.jpeg", plot = completed_pathways_specialty_index_graph, width = 10, height = 6, dpi = 300)

#### Incomplete pathways by specialty ####

incomplete_pathways_specialty <- rtt_data2 %>% 
  group_by(date, Combined) %>% 
  summarise(incomplete_pathways = sum(incomplete_pathways, na.rm = TRUE))

incomplete_pathways_specialty <- incomplete_pathways_specialty %>%
  filter(!is.na(Combined))

incomplete_pathways_specialty_graph <- ggplot(incomplete_pathways_specialty, aes(x = date, y = incomplete_pathways, color = Combined, group = Combined)) +
  geom_line() +
  labs(
    title = "Total incompleted pathways by specialty",
    x = "Date",
    y = "Total incompleted pathways",
    color = "Combined" 
  ) +
  scale_y_continuous(labels = label_number()) +  # Change y-axis to regular number format
  theme_minimal()

ggsave(filename = "incomplete_pathways_specialty_graph.jpeg", plot = incomplete_pathways_specialty_graph, width = 10, height = 6, dpi = 300)

#### Index of incomplete pathways by specialty ####

reference_values2 <- incomplete_pathways_specialty %>%
  filter(date == as.Date("2018-04-01")) %>% 
  group_by(Combined) %>%
  summarise(reference_value = sum(incomplete_pathways, na.rm = TRUE))

incomplete_pathways_specialty_index <- incomplete_pathways_specialty %>%
  left_join(reference_values2, by = "Combined") %>%  
  mutate(index = (incomplete_pathways / reference_value) * 100) %>%  
  select(-reference_value)  

incomplete_pathways_specialtyindex_graph <- ggplot(incomplete_pathways_specialty_index, aes(x = date, y = index, color = Combined, group = Combined)) +
  geom_line() +
  labs(
    title = "Index of total incompleted pathways by specialty (Base 100 = April 2018)",
    x = "Date",
    y = "Index of total incompleted pathways",
    color = "Combined" 
  ) +
  scale_y_continuous(labels = label_number()) +  # Change y-axis to regular number format
  theme_minimal()

ggsave(filename = "incomplete_pathways_specialtyindex_graph.jpeg", plot = incomplete_pathways_specialtyindex_graph, width = 10, height = 6, dpi = 300)

############## High level diagnostics ###################

diagnostics_overview<-diagnostic_data %>% 
  group_by(date) %>% 
  summarise(total = sum(total, na.rm = TRUE))

operating_theatres_overview<-operating_theatre_data %>% 
  group_by(date) %>% 
  summarise(operating_theatres = sum(operating_theatres, na.rm = TRUE))

operating_theatres_graph<-ggplot(operating_theatres_overview,aes(x=date,y=operating_theatres))+
  geom_line()+
  labs(title = "Total operating theatres",
       x = "Date",
       y = "Total operating theatres")+
  scale_y_continuous(labels = label_number()) +
  theme_minimal()

ggsave(filename = "operating_theatres_graph.jpeg", plot = operating_theatres_graph, width = 10, height = 6, dpi = 300)

operating_theatres_graph<-ggplot(operating_theatre_data,aes(x=date,y=operating_theatres, group = trust_code))+
  geom_line()+
  labs(title = "Total operating theatres",
       x = "Date",
       y = "Total operating theatres")+
  scale_y_continuous(labels = label_number()) +
  theme_minimal()

########### Ratio of completed pathways to incomplete pathways #############

# Corrected code to calculate completed pathways for admitted and non-admitted patients by date
elective_ratio <- rtt_data %>%
  group_by(date) %>%
  summarise(
    completed_pathways_admitted = sum(completed_pathways_for_admitted_patients, na.rm = TRUE),
    completed_pathways_nonadmitted = sum(completed_pathways_for_non_admitted_patients, na.rm = TRUE),
    incomplete_pathways = sum(incomplete_pathways,na.rm = TRUE)) %>% 
  mutate(complete_admitted_divided_incomplete_pathways = completed_pathways_admitted/incomplete_pathways)

elective_ratio_graph<-ggplot(elective_ratio,aes(x=date,y=complete_admitted_divided_incomplete_pathways))+
  geom_line()+
  labs(title = "Total complete admitted pathways / incomplete pathways",
       x = "Date",
       y = "Ratio of complete admitted pathways / incomplete pathways")+
  scale_y_continuous(labels = label_number()) +
  theme_minimal()

ggsave(filename = "elective_ratio_graph.jpeg", plot = elective_ratio_graph, width = 10, height = 6, dpi = 300)

elective_ratio_2 <- rtt_data %>%
  group_by(date,trust_code) %>% 
  summarise(
    completed_pathways_admitted = sum(completed_pathways_for_admitted_patients, na.rm = TRUE),
    completed_pathways_nonadmitted = sum(completed_pathways_for_non_admitted_patients, na.rm = TRUE),
    incomplete_pathways = sum(incomplete_pathways,na.rm = TRUE)) %>% 
  mutate(complete_admitted_divided_incomplete_pathways = completed_pathways_admitted/incomplete_pathways)

elective_ratio_2_filtered <- elective_ratio_2 %>%
  filter(completed_pathways_admitted != 0) %>% 
  filter(complete_admitted_divided_incomplete_pathways >= 50) %>%
  filter(is.finite(complete_admitted_divided_incomplete_pathways)) 


elective_ratio_trusts_graph <- ggplot(elective_ratio_2_filtered, aes(x = date, 
                                                                     y = complete_admitted_divided_incomplete_pathways, 
                                                                     group = trust_code)) +
  geom_line(show.legend = FALSE) +  # Remove the legend
  labs(
    title = "Ratio of Complete Admitted Pathways to Incomplete Pathways by Trust",
    x = "Date",
    y = "Ratio of Complete Admitted / Incomplete Pathways"
  ) +
  scale_y_continuous(labels = label_number()) +
  theme_minimal()

ggsave(filename = "elective_ratio_trusts_graph.jpeg", plot = elective_ratio_trusts_graph, width = 10, height = 6, dpi = 300)



########## Gini index #######


Gini = function (x, corr = FALSE, na.rm = TRUE) 
{
  if (!na.rm && any(is.na(x))) 
    return(NA_real_)
  x <- as.numeric(na.omit(x))
  l <- length(x)
  x <- sort(x)
  gi <- sum(x * 1L:l)
  gi <- 2 * gi/sum(x) - (l + 1L)
  if (corr) 
    gi/(l - 1L)
  else gi/l
}

gini_by_trust_specialty <- elective_workforce %>%
  group_by(org_code, Workforce) %>%
  summarize(gini = Gini(total_fte, na.rm = TRUE))