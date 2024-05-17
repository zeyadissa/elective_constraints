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

# Globals -------

source('https://raw.githubusercontent.com/zeyadissa/open_data/main/const/global_var.R')
source('https://raw.githubusercontent.com/zeyadissa/open_data/main/src/functions.R')

# Data -------

source('https://raw.githubusercontent.com/zeyadissa/open_data/main/src/diagnostics.R')
source('https://raw.githubusercontent.com/zeyadissa/open_data/main/src/operating_theatres.R')
source('https://raw.githubusercontent.com/zeyadissa/open_data/main/src/covid_beds.R')

#deflator
deflator <- data.table::fread('https://raw.githubusercontent.com/zeyadissa/eric_analysis/main/const/deflator.csv')
#population
FINAL_trust_pop <- read.csv('const/trust_pop.csv') %>%
  rename('trust_code'=TrustCode) %>%
  filter(CatchmentYear == 2020)
#workforce
FINAL_workforce <- read.csv('const/FINAL_workforce.csv') %>%
  dplyr::mutate(date = zoo::as.yearqtr(date)) %>%
  janitor::clean_names() %>%
  dplyr::mutate(nurses = nurses_health_visitors,
         senior_doctors = consultant+associate_specialist+staff_grade+specialty_doctor,
         managers = managers + senior_managers,
         all_ftes = total,
         doctors = hchs_doctors- senior_doctors,
         ratio_doctors = senior_doctors / hchs_doctors) %>%
  select(date,ratio_doctors,trust_code,doctors,senior_doctors,nurses,managers,all_ftes)
#Overnight beds
FINAL_overnight_beds <- read.csv('const/FINAL_overnight_beds.csv') %>%
  mutate(date = zoo::as.yearqtr(date))
#sickness rates
FINAL_sickness <- read.csv('const/FINAL_sickness.csv') %>%
  mutate(date = zoo::as.yearqtr(date))
#los
FINAL_adjusted_los <- read.csv('const/los_age_complexity_adjusted.csv') %>%
  rename('trust_code'=PROCODE) %>%
  mutate(quarter = substr(cal_quarter,1,2),
         year = substr(cal_quarter,nchar(cal_quarter)-4,nchar(cal_quarter)),
         date = zoo::as.yearqtr(paste0(year,' ',quarter))) %>%
  select(trust_code,date,standard_los)
#los unadjusted
FINAL_unadjusted_los <- read.csv('const/los_unadjusted.csv') %>%
  rename('trust_code'=PROCODE) %>%
  mutate(quarter = substr(cal_quarter,1,2),
         year = substr(cal_quarter,nchar(cal_quarter)-4,nchar(cal_quarter)),
         date = zoo::as.yearqtr(paste0(year,' ',quarter))) %>%
  select(trust_code,date,los_unadjusted)
#emergency proportions
FINAL_proportions <- read.csv('const/elective_em_proportions.csv') %>%
  rename('trust_code'=PROCODE) %>%
  mutate(quarter = substr(cal_quarter,1,2),
         year = substr(cal_quarter,nchar(cal_quarter)-4,nchar(cal_quarter)),
         date = zoo::as.yearqtr(paste0(year,' ',quarter))) %>%
  select(trust_code,date,elective_proportion,emergency_proportion)

#backlog and investment
source('https://raw.githubusercontent.com/zeyadissa/open_data/main/src/eric_backlog.R')
source('https://raw.githubusercontent.com/zeyadissa/open_data/main/src/eric_investment.R')
FINAL_backlog_data <- site_data %>%
  dplyr::group_by(date,trust_code,risk) %>%
  dplyr::summarise(cost = sum(cost,na.rm=T)) %>%
  dplyr::ungroup()%>%
  tidyr::pivot_wider(names_from=risk,values_from=cost) %>%
  dplyr::left_join(.,trust_data,site_date,by=c('date','trust_code')) %>%
  dplyr::mutate(date = date) %>%
  dplyr::group_by(date,trust_code)%>%
  dplyr::summarise(low = sum(low,na.rm=T),
            moderate = sum(moderate,na.rm=T),
            high = sum(high,na.rm=T),
            significant = sum(significant,na.rm=T),
            investment = sum(investment,na.rm=T)) %>%
  replace(is.na(.), 0) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(cost = low + high + moderate + significant) %>%
  dplyr::group_by(trust_code) %>% 
  dplyr::left_join(.,deflator,by='date') %>%
  # Add GDP deflator
  dplyr::mutate(cost = cost * (deflator),
         investment = investment * deflator) %>%
  dplyr::mutate(total = cost + dplyr::lag(investment,n=1,order_by=date)) %>%
  dplyr::mutate(total_growth = (total -dplyr::lag(total,n=1,order_by=date))/dplyr::lag(total,n=1,order_by=date),
         invest_growth = (investment - lag(investment,n=1,order_by=date))/dplyr::lag(investment,n=1,order_by=date),
         cost_growth = (cost -dplyr::lag(cost,n=1,order_by=date))/dplyr::lag(cost,n=1,order_by=date)) %>%
  dplyr::select(date,trust_code,low,moderate,high,significant,investment,cost,total)

# Org mapping Data ----------------------------------------------------------

#org to icb mapping file location
org_url <- 'https://files.digital.nhs.uk/assets/ods/current/etr.zip'

#wrangling to get a final mapping file
temp <- tempfile()
download.file(org_url,temp)
FINAL_org_mapping <- read.csv(unz(temp,'etr.csv'),header = F,fill=T)
names(FINAL_org_mapping)[1:4] <- c('trust_code','org_name','region_code','org')
FINAL_org_mapping <- FINAL_org_mapping %>%
  dplyr::select(org,trust_code)

# RTT Data ----------------------------------------------------------------

#Urls for datasets
rtt_urls <- c('https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2022-23/',
              'https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2023-24/',
              'https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2021-22/',
              'https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2019-20/',
              'https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2018-19/',
              'https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2020-21/')

rtt_files <- GetLinks(rtt_urls,'Full-CSV-data')
rtt_data <- sapply(rtt_files,
                   function(x){
                     UnzipCSV(x)}
)

#Cleans the underlying data to get what we want
FINAL_rtt <- lapply(rtt_data,
                           function(x){
                             x %>%
                               dplyr::mutate(all_rtt = total_all,
                                      date = zoo::as.yearqtr(zoo::as.yearmon(substr(period,5,99),'%B-%Y'))) %>%
                               #Remove all the columns that are 'waiting 18 to 20 weeks' or whaever,
                               #We aren't using them and there is mismatch after 2019/20
                               dplyr::select(-starts_with('Gt.')) %>%
                               #Select ALL specialties
                               #dplyr::filter(treatment_function_name == 'Total') %>%
                               dplyr::rename( 'trust_code' = provider_org_code)
                           }) %>%
  #collapse the list
  data.table::rbindlist(fill=T) %>%
  #remove treatment_function_name if needed?
  dplyr::group_by(date,treatment_function_code,treatment_function_name,rtt_part_description,trust_code) %>%
  dplyr::summarise(all_rtt = sum(all_rtt,na.rm=T)) %>%
  #Useful for left_joins
  tidyr::pivot_wider(.,names_from=rtt_part_description,values_from=all_rtt)

#Clean names
names(FINAL_rtt) <- names(FINAL_rtt) %>% janitor::make_clean_names()

# Final data output ---------------------------------------------------------------------

FINAL_data <- purrr::reduce(list(
  #remove all other specs from rtt dataset
  FINAL_rtt %>%
    dplyr::ungroup()%>%
    dplyr::filter(treatment_function_name == 'Total') %>%
    dplyr::select(!c(treatment_function_name,treatment_function_code)),
  FINAL_diagnostic_data,
  FINAL_op_th_data,
  FINAL_overnight_beds,
  FINAL_covid,
  FINAL_sickness,
  FINAL_workforce,
  FINAL_adjusted_los,
  FINAL_unadjusted_los,
  FINAL_proportions), 
  dplyr::left_join, by = c('date','trust_code')) %>%
  dplyr::left_join(.,FINAL_trust_pop,by=c('trust_code')) %>%
  #turn to numeric
  dplyr::mutate_at(vars(-trust_code,-date),
                   ~as.numeric(.)) %>%
  #covid flag
  dplyr::mutate(covid_flag = dplyr::case_when(lubridate::year(date) == 2020 ~ 'covid',
                                       lubridate::year(date) < 2020 ~ 'pre-covid',
                                       lubridate::year(date) > 2020 ~ 'post-covid',
                                       TRUE ~ NA),
                date_yr = lubridate::year(date),
                #Add 0 for pre-covid (which will be na)
                covid_beds = dplyr::case_when(is.na(covid_beds)==TRUE ~ 0,
                                       TRUE ~ covid_beds),
                trust_code = substr(trust_code,1,3))%>%
  #Important as a lot of these are curiously at site level?
  dplyr::group_by(date,trust_code,covid_flag) %>%
  dplyr::summarise_all(sum,na.rm=T) %>%
  dplyr::mutate(total_ftes2=total_ftes*sickness_rate) %>%
  dplyr::left_join(.,FINAL_backlog_data,by=c('trust_code','date_yr'='date'))

FINAL_regression_data <- FINAL_data %>% 
  dplyr::filter(covid_flag != 'covid') %>%
  dplyr::mutate(
    total_pathways = completed_pathways_for_admitted_patients + completed_pathways_for_non_admitted_patients,
    admitted_pathways = completed_pathways_for_admitted_patients,
    non_admitted_pathways = completed_pathways_for_non_admitted_patients,
    nurses_ratio = nurses/total_ftes,
    doctors_ratio = doctors/total_ftes,
    senior_doctors_ratio = senior_doctors/total_ftes,
    occupied_ratio = occupied_beds / bed_capacity,
    significant_ratio = (high+significant)/(low+moderate)) %>%
  dplyr::filter(nurses * doctors *total_ftes* non_admitted_pathways * admitted_pathways * trust_total_catchment *occupied_beds != 0) %>%
  dplyr::filter(admitted_pathways > 1000 & non_admitted_pathways > 1000) %>%
  filter(!trust_code %in% c('RRJ','RL1','RP6')) %>%
  ungroup() %>%
  mutate(
    years_since_covid = case_when(
      lubridate::year(date) - 2019 < 0 ~ '0_yrs',
      T ~ paste0((lubridate::year(date) - 2020),'_yrs'))
  )

#REMOVE ALL that aren't final. 
rm(list=ls()[!grepl("FINAL_regression", ls())])