#packages
library(tidyverse)
library (data.table)
library(xml2)
library(rvest)
library(janitor)
library(lubridate)
library(purrr)
library(lme4)
library(lmerTest)
library(lmtest)
library(performance)
library(see)
library(Gini)
library(betareg)
library(marginaleffects)
library(brms)

#Globals
source('const/glob.R')

#Dependencies
source('src/create_data/create_rtt_data.R')
source('src/create_data/create_org_mapping_data.R')
source('src/create_data/create_diagnostics_data.R')
source('src/create_data/create_operating_theatre_data.R')
source('src/create_data/create_workforce_data.R')

final_rtt_data <- rtt_data |> 
  dplyr::mutate(trust_code = substr(trust_code,1,3),
                total_pathways = completed_pathways_for_admitted_patients + completed_pathways_for_non_admitted_patients) |>
  dplyr::filter(treatment_function_name %in% c("Gastroenterology","Gastroenterology Service")) |> 
  dplyr::group_by(date,trust_code) |> 
  dplyr::summarise(
    admitted_pathways = sum(completed_pathways_for_admitted_patients,na.rm=T),
    non_admitted_pathways = sum(completed_pathways_for_non_admitted_patients,na.rm=T),
    new_pathways = sum(new_rtt_periods_all_patients,na.rm=T),
    total_pathways = sum(total_pathways,na.rm=T)
  ) 

#Cardiology = cardiology
final_workforce_data <- workforce_data |> 
  dplyr::filter(specialty %in% c("Gastro-enterology","Gastroenterology" )) |> 
  dplyr::mutate(trust_code = substr(org_code,1,3),
                date = lubridate::make_date(
                  year = lubridate::year(date),
                  month = lubridate::month(date),
                  day = 1
                )) |> 
  dplyr::group_by(trust_code,date,grade) |> 
  dplyr::summarise(total_fte = sum(total_fte,na.rm=T)) |> 
  tidyr::pivot_wider(names_from=grade,values_from=total_fte) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(total_ftes = rowSums(across(!c(trust_code,date)),na.rm=T)) |> 
  janitor::clean_names()
  
final_diagnostic_data <- raw_diagnostics_data |> 
  dplyr::mutate(trust_code = substr(trust_code,1,3),
                all_tests = dplyr::case_when(
                  is.na(total) == T ~ all_diagnostics_tests,
                  T ~ total
                )) |> 
  group_by(trust_code,date) |> 
  summarise(total_diagnostic_tests = sum(all_tests,na.rm=T))

# Final data output ---------------------------------------------------------------------

final_data <- purrr::reduce(list(
  final_rtt_data,
  final_workforce_data,
  final_diagnostic_data),
  dplyr::left_join, by = c('date','trust_code')) |> 
  dplyr::left_join(trust_pop,by=c('trust_code'))

write.csv(data, 'output/raw_data.csv')

#REMOVE ALL that aren't final. 
rm(list=ls()[!grepl("FINAL_regression", ls())])