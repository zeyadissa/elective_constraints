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

# Functions -----

source('https://raw.githubusercontent.com/zeyadissa/open_data/main/const/global_var.R')
source('https://raw.githubusercontent.com/zeyadissa/open_data/main/src/functions.R')
source('https://raw.githubusercontent.com/zeyadissa/open_data/main/src/ae.R')

#Static catchment population data per trust: a better way to do this is to use the total admitted (?) or total
#Served population from hes, but for now this will have to do for the offset.
FINAL_trust_pop <- read.csv('const/trust_pop.csv') %>%
  rename('trust_code'=TrustCode) %>%
  filter(CatchmentYear == 2020)

# source eric ----------------------------------------------------------

# Functions -------

library(tidyverse)
library(data.table)
library(rvest)
library(stringr)

thf <- '#dd0031'

#Gets links from any single url; string matches
GetLinks <- function(url_name,string){
  files <- c()
  #this is inefficient and bad practice but it's a small vector.
  for(i in seq_along(url_name)){
    pg <- rvest::read_html(url_name[i])
    pg<-(rvest::html_attr(rvest::html_nodes(pg, "a"), "href"))
    files <- c(files,pg[grepl(string,pg,ignore.case = T)])
    files <- files %>% unique()
  }
  return(files)
}

#Read all csvs from urls; unz for zips
UnzipCSV <- function(files){
  #creates temp file to read in the data
  temp <- tempfile()
  download.file(files,temp)
  #This is needed because a zip file may have multiple files
  file_names <- unzip(temp,list=T)$Name
  data<- lapply(file_names,
                function(x){
                  da <- data.table::fread(unzip(temp,x))
                  #janitor to clean unruly names
                  names(da) <- names(da) %>% janitor::make_clean_names()  
                  return(da)
                })
  #unlink the temp file, important to do
  unlink(temp)
  data}

eric_url <- 'https://digital.nhs.uk/data-and-information/publications/statistical/estates-returns-information-collection'
eric_links <- GetLinks(eric_url,'estates-returns-information-collection/')
deflator <- data.table::fread('https://raw.githubusercontent.com/zeyadissa/eric_analysis/main/const/deflator.csv')

# Site level data -------

site_links <- sapply(eric_links,
                     function(x){
                       if(x == "/data-and-information/publications/statistical/estates-returns-information-collection/england-historical-data-files-1999-2000-to-2013-14"){
                         filter_dat <- '.xls|.XLS'
                       } else if(x == '/data-and-information/publications/statistical/estates-returns-information-collection/estates-returns-information-collection-eric-england-2014-15'){
                         filter_dat <- 'est-ret-info-col-2014-2015-dat.csv'
                       } else {
                         filter_dat <- 'site'
                       }
                       test<-GetLinks(paste0('https://digital.nhs.uk',x),
                                      filter_dat)
                       test<-test[grepl('.csv|xls|XLS',test)]
                       #remove double counting
                       test<-test[!grepl('eric-201617',test)]
                       return(test)
                     })  %>%
  unlist(use.names=F)

site_data <- lapply(site_links[1:19],
                    function(x){
                      if(grepl('xls|XLS',x) == TRUE){
                        temp = tempfile(fileext = ".xls")
                        download.file(x, destfile=temp, mode='wb',skip=2)
                        output <- readxl::read_xls(temp, sheet ='Site Data',skip=1)
                      } else if(x %in% c('https://files.digital.nhs.uk/publicationimport/pub18xxx/pub18726/est-ret-info-col-2014-2015-dat.csv',
                                         "https://files.digital.nhs.uk/publicationimport/pub21xxx/pub21992/est-ret-info-col-2015-2016-site-data.csv")){
                        output <- data.table::fread(x,skip=1)
                      } else{
                        output <- data.table::fread(x)
                      }
                      output <- output %>% 
                        janitor::clean_names() %>%
                        dplyr::mutate(date=match(x,site_links)) %>%
                        dplyr::rename(.,any_of(c(trust_code='organisation_code'))) %>%
                        select(date,
                               trust_code,
                               site_code,
                               site_type,
                               contains(c('low','high','significant','moderate')) & contains('cost')& contains('backlog')) %>%
                        pivot_longer(cols=!c(date,trust_code,site_code,site_type),names_to='risk',values_to='cost') %>%
                        mutate(risk = str_extract(pattern='high|low|significant|moderate',risk))
                      return(output)
                    }) %>%
  data.table::rbindlist(fill=T) %>%
  dplyr::filter(trust_code != 'Trust Code') %>%
  dplyr::mutate(cost = as.numeric(gsub(",","",cost))) %>%
  drop_na() %>%
  dplyr::filter(cost!=0)

# Trust level data -------

trust_links <- sapply(eric_links,
                      function(x){
                        if(x == "/data-and-information/publications/statistical/estates-returns-information-collection/england-historical-data-files-1999-2000-to-2013-14"){
                          filter_dat <- '.xls|.XLS'
                          test<-GetLinks(paste0('https://digital.nhs.uk',x),
                                         filter_dat)
                          test<-test[grepl('.csv|xls|XLS',test)]
                          #remove double counting
                          test<-test[!grepl('eric-201617',test)]
                        } else if(x == '/data-and-information/publications/statistical/estates-returns-information-collection/estates-returns-information-collection-eric-england-2014-15'){
                          filter_dat <- 'est-ret-info-col-2014-2015-tru-lev'
                          test<-GetLinks(paste0('https://digital.nhs.uk',x),
                                         filter_dat)
                          test<-test[grepl('.csv',test)]
                          #remove double counting
                          test<-test[!grepl('eric-201617',test)]
                        } else {
                          filter_dat <- 'trust'
                          test<-GetLinks(paste0('https://digital.nhs.uk',x),
                                         filter_dat)
                          test<-test[grepl('.csv',test)]
                          #remove double counting
                          test<-test[!grepl('eric-201617',test)]
                        }
                        return(test)
                      })  %>%
  unlist(use.names=F)

trust_data <- lapply(trust_links[1:19],
                     function(x){
                       if(grepl('xls|XLS',x) == TRUE){
                         temp = tempfile(fileext = ".xls")
                         download.file(x, destfile=temp, mode='wb',skip=2)
                         output <- readxl::read_xls(temp, sheet ='Trust Data',skip=1)
                       } else if(x %in% c( "https://files.digital.nhs.uk/publicationimport/pub18xxx/pub18726/est-ret-info-col-2014-2015-tru-lev-dat-v2.csv",
                                           "https://files.digital.nhs.uk/publicationimport/pub21xxx/pub21992/est-ret-info-col-2015-2016-trust-data.csv")){
                         output <- data.table::fread(x,skip=1)
                       } else{
                         output <- data.table::fread(x)
                       }
                       output <- output %>% 
                         janitor::clean_names() %>%
                         dplyr::mutate(date=match(x,trust_links)) %>%
                         dplyr::rename(.,any_of(c(trust_code='organisation_code'))) %>%
                         select(date,
                                trust_code,
                                starts_with('investment'),
                                any_of(starts_with('capital_investment_for_maintaining'))) %>%
                         rename(.,any_of(c(capital_investment_for_maintaining_lifecycle_existing_buildings='investment')))
                       return(output)
                     }) %>%
  data.table::rbindlist(fill=T) %>%
  filter(trust_code != 'Trust Code') %>%
  dplyr::mutate(investment1 = as.numeric(gsub(",","",investment_to_reduce_backlog_maintenance)),
                investment2 = as.numeric(gsub(",","",capital_investment_for_maintaining_lifecycle_existing_buildings))) %>%
  replace(is.na(.), 0) %>%
  dplyr::mutate(investment = investment1+investment2)%>%
  dplyr::select(!c(investment1,investment2,starts_with('investment_to'),capital_investment_for_maintaining_lifecycle_existing_buildings)) %>%
  drop_na() %>%
  dplyr::filter(investment!=0)

# Merge -------

FINAL_backlog_data <- site_data %>%
  group_by(date,trust_code,risk) %>%
  summarise(cost = sum(cost,na.rm=T)) %>%
  ungroup()%>%
  pivot_wider(names_from=risk,values_from=cost) %>%
  left_join(.,trust_data,site_date,by=c('date','trust_code')) %>%
  mutate(date = 2023 - date) %>%
  group_by(date,trust_code)%>%
  summarise(low = sum(low,na.rm=T),
            moderate = sum(moderate,na.rm=T),
            high = sum(high,na.rm=T),
            significant = sum(significant,na.rm=T),
            investment = sum(investment,na.rm=T)) %>%
  replace(is.na(.), 0) %>%
  ungroup() %>%
  mutate(cost = low + high + moderate + significant) %>%
  group_by(trust_code) %>% 
  left_join(.,deflator,by='date') %>%
  # Add GDP deflator
  mutate(cost = cost * (deflator),
         investment = investment * deflator) %>%
  mutate(total = cost + lag(investment,n=1,order_by=date)) %>%
  mutate(total_growth = (total -lag(total,n=1,order_by=date))/lag(total,n=1,order_by=date),
         invest_growth = (investment - lag(investment,n=1,order_by=date))/lag(investment,n=1,order_by=date),
         cost_growth = (cost -lag(cost,n=1,order_by=date))/lag(cost,n=1,order_by=date)) %>%
  select(date,trust_code,low,moderate,high,significant,investment,cost,total)


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

# Workforce -------------------------------------------------------------

#All the stuff here was deleted. I'm using a presaved version because
#I don't have the energy to re-write it before DAP purges it again
FINAL_workforce <- read.csv('const/FINAL_workforce.csv') %>%
  mutate(date = zoo::as.yearqtr(date))

#clean names
names(FINAL_workforce) <- janitor::make_clean_names(names(FINAL_workforce)) 

#Clean this up a bit
FINAL_workforce <- FINAL_workforce %>%
  mutate(nurses = nurses_health_visitors,
         senior_doctors = consultant+associate_specialist+staff_grade+specialty_doctor,
         managers = managers + senior_managers,
         all_ftes = total,
         doctors = hchs_doctors- senior_doctors,
         ratio_doctors = senior_doctors / hchs_doctors) %>%
  select(date,ratio_doctors,trust_code,doctors,senior_doctors,nurses,managers,all_ftes)
  
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

# Operating theatres Data ----------------------------------------------------------

#operating table links. We have to get rid of 2013 to 2015 as:
#1) they're not needed
#2) change in formatting messes up the read. 
#This is a hackish approach i am aware :(
op_url <- 'https://www.england.nhs.uk/statistics/statistical-work-areas/cancelled-elective-operations/supporting-facilities-data/'
op_links <- GetLinks(op_url,'Operating-Theatres')
op_links <- op_links[!grepl("transparency", op_links)]
op_links<-op_links[!grepl("-2013|-2012|-2014|-2015", op_links)]


op_raw_data <- lapply(op_links,
                  function(x){
                    data <- openxlsx::read.xlsx(x)
                    data <- data %>%
                      #Extracts year and date from string
                      mutate(date = paste0(stringr::str_sub(data[2,2],nchar(data[2,2])-4,nchar(data[2,2])-1),
                                           ' Q',
                                           stringr::str_sub(data[2,2],9,9)
                                           )
                             )
                    #fix rows; names are row 12 everything prior is useless
                    names(data) <- data[12,]
                    data <- data[-c(1:14),]
                    #new row called date that we mutated.
                    names(data)[7] <- 'date'
                    #fix names
                    names(data) <- names(data) %>% janitor::make_clean_names()
                    data})

#Final output file
FINAL_op_data <- op_raw_data %>%
  #merge together
  data.table::rbindlist() %>%
  mutate(date = zoo::as.yearqtr(date)) %>%
  rename( 'trust_code' = organisation_code) %>%
  group_by(date,trust_code) %>%
  summarise(operating_theatres = sum(as.numeric(number_of_operating_theatres),na.rm=T))

# Discharge Data ----------------------------------------------------------

#NOTE: This is very messy. I don't suggest we use it. It only exists across a 3 year period,
# Would suggest we drop it. 

# #Pull links and discharge data
# discharge_url <- 'https://www.england.nhs.uk/statistics/statistical-work-areas/discharge-delays-acute-data/'
# discharge_links <- GetLinks(discharge_url,'Daily-discharge-sitrep-monthly')
# 
# #Discharge data final output
# discharge_data <- lapply(discharge_links,
#        function(x){
#          #Read
#          raw_discharge_data <- openxlsx::read.xlsx(x,sheet='Table 2')
#          #Fix names and all that jazz
#          names(raw_discharge_data) <- raw_discharge_data[3,]
#          names(raw_discharge_data)[1:3] <- c('region','org','icb_name')
#          raw_discharge_data<-raw_discharge_data[-c(1:12),]
#          #Format to appropriate tidy standard
#          discharge_data <- raw_discharge_data %>% 
#            tidyr::pivot_longer(cols=!c(org,icb_name,region),names_to='type',values_to='values') %>%
#            dplyr::filter(grepl('INTEGRATED CARE',icb_name)) %>%
#            #Note this is a moronic way to do it but,I don't want to deal with it anymore. It *will* break if someone uploads the wrong file name.
#            #Probably a smarter way to do it is to use upload date (unless they upload irregularly) or use a regex to identify 'Apr, May, etc..'
#            dplyr::mutate(date = zoo::as.yearqtr(zoo::as.yearmon(str_sub(sub('*.revised',"",sub('.*webfile-',"",x)),0,-6),format='%B%Y'))) %>%
#            dplyr::group_by(org,type,date) %>%
#            dplyr::summarise(values = sum(as.numeric(values),na.rm=T)) %>%
#            dplyr::filter(type == 'Number of patients remaining in hospital who no longer meet the criteria to reside') %>%
#            dplyr::filter(org != 'Org Code')
#          discharge_data
#          }) %>%
#   data.table::rbindlist() %>%
#   dplyr::group_by(org,date) %>%
#   dplyr::summarise(delayed_discharges = mean(values,na.rm=T))

 
# # Diagnostics Data ----------------------------------------------------------

#Pull links and discharge data
diagnostic_urls <- c('https://www.england.nhs.uk/statistics/statistical-work-areas/diagnostics-waiting-times-and-activity/monthly-diagnostics-waiting-times-and-activity/monthly-diagnostics-data-2023-24/',
                     'https://www.england.nhs.uk/statistics/statistical-work-areas/diagnostics-waiting-times-and-activity/monthly-diagnostics-waiting-times-and-activity/monthly-diagnostics-data-2022-23/',
                     'https://www.england.nhs.uk/statistics/statistical-work-areas/diagnostics-waiting-times-and-activity/monthly-diagnostics-waiting-times-and-activity/monthly-diagnostics-data-2021-22/',
                     'https://www.england.nhs.uk/statistics/statistical-work-areas/diagnostics-waiting-times-and-activity/monthly-diagnostics-waiting-times-and-activity/monthly-diagnostics-data-2020-21/',
                     'https://www.england.nhs.uk/statistics/statistical-work-areas/diagnostics-waiting-times-and-activity/monthly-diagnostics-waiting-times-and-activity/monthly-diagnostics-data-2019-20/',
                     'https://www.england.nhs.uk/statistics/statistical-work-areas/diagnostics-waiting-times-and-activity/monthly-diagnostics-waiting-times-and-activity/monthly-diagnostics-data-2018-19/')

#Get the full extracts
diagnostic_links <-GetLinks(diagnostic_urls,'full-extract')
#Remove CDCs (should these be included?)
diagnostic_links <- diagnostic_links[!grepl("CDC", diagnostic_links)]

#Download everything
raw_diagnostics_data <- sapply(diagnostic_links,
                function(x){
                  UnzipCSV(x)
                    })

#Fix up data to be ready for use
FINAL_diagnostic_data <- lapply(raw_diagnostics_data,
                          function(x){
                            x %>%
                              #Extract data
                              dplyr::mutate(date = zoo::as.yearqtr(zoo::as.yearmon(substr(period,6,99),format='%B-%Y')),
                                            diagnostic_tests = case_when(
                                              diagnostic_tests == 'TOTAL' ~ 'all_diagnostics_tests',
                                              TRUE ~ diagnostic_tests)) %>%
                              #filter(diagnostic_tests == 'TOTAL') %>%
                              dplyr::select(diagnostic_tests,commissioner_parent_org_code,date,total_activity,provider_org_code) %>%
                              dplyr::rename('org' = commissioner_parent_org_code,
                                            'trust_code' = `provider_org_code`)
                          }) %>%
  #Bind everything together and group and summarise
  data.table::rbindlist() %>%
  dplyr::group_by(trust_code,date,diagnostic_tests) %>%
  dplyr::summarise(tests = sum(total_activity,na.rm=T),.groups='keep') %>%
  tidyr::pivot_wider(.,names_from = diagnostic_tests,values_from=tests)

#Fix names
names(FINAL_diagnostic_data) <- names(FINAL_diagnostic_data) %>% janitor::make_clean_names()

# Bed occupancy --------------------------------------------

#Same issue as workforce.
FINAL_overnight_beds <- read.csv('const/FINAL_overnight_beds.csv') %>%
  mutate(date = zoo::as.yearqtr(date))

# Sickness Rates ------

#Same issue as above, copied from Open Data Collection Code
FINAL_sickness <- read.csv('const/FINAL_sickness.csv') %>%
  mutate(date = zoo::as.yearqtr(date))

#Covid bed occupancy -----

covid_url <- 'https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/'

covid_links <- GetLinks(covid_url,'Covid-Publication')
covid_links <- covid_links[!grepl("Supplementary", covid_links)]

FINAL_covid <- lapply(covid_links,
                      function(x){
                        da <- openxlsx::read.xlsx(x,sheet='Total Beds Occupied Covid')
                        names(da) <- da[9,]
                        da <- da[-c(1:17),]
                        da <- da %>%
                          dplyr::select(!c(Name,`NHS England Region`)) %>%
                          tidyr::pivot_longer(cols=!c(Code),names_to='date',values_to='covid_beds') %>%
                          dplyr::mutate(date = zoo::as.yearqtr(as.Date(as.numeric(date), origin = "1899-12-30"))) %>%
                          dplyr::rename('trust_code' = Code)
                        return(da)
                      }) %>%
  data.table::rbindlist() %>%
  #Note the summarise is outside the loop as there will be overlap in the quarterly date aggregations 
  #(wasted 2 hours debugging before I realised this...)
  dplyr::group_by(date,trust_code) %>%
  dplyr::summarise(covid_beds = sum(covid_beds))

#LoS -----

FINAL_adjusted_los <- read.csv('const/los_age_complexity_adjusted.csv') %>%
  rename('trust_code'=PROCODE) %>%
  mutate(quarter = substr(cal_quarter,1,2),
         year = substr(cal_quarter,nchar(cal_quarter)-4,nchar(cal_quarter)),
         date = zoo::as.yearqtr(paste0(year,' ',quarter))) %>%
  select(trust_code,date,standard_los)

FINAL_unadjusted_los <- read.csv('const/los_unadjusted.csv') %>%
  rename('trust_code'=PROCODE) %>%
  mutate(quarter = substr(cal_quarter,1,2),
         year = substr(cal_quarter,nchar(cal_quarter)-4,nchar(cal_quarter)),
         date = zoo::as.yearqtr(paste0(year,' ',quarter))) %>%
  select(trust_code,date,los_unadjusted)

FINAL_proportions <- read.csv('const/elective_em_proportions.csv') %>%
  rename('trust_code'=PROCODE) %>%
  mutate(quarter = substr(cal_quarter,1,2),
         year = substr(cal_quarter,nchar(cal_quarter)-4,nchar(cal_quarter)),
         date = zoo::as.yearqtr(paste0(year,' ',quarter))) %>%
  select(trust_code,date,elective_proportion,emergency_proportion)

#AE

FINAL_ae_data <- FINAL_ae_data %>%
  mutate(date = zoo::as.yearqtr(period)) %>%
  group_by(date,trust_code) %>%
  summarise(ae_breaches = sum(ae_breaches,na.rm=T),
            ae_attendances = sum(ae_attendances,na.rm=T),
            ae_admissions = sum(ae_admissions,na.rm=T),
            admission_breaches = sum(admission_breaches,na.rm=T)) %>%
  mutate(admit_ratio = ae_admissions/(ae_attendances+ae_admissions),
         breach_attend = ae_breaches/ae_attendances,
         breach_admit=admission_breaches/ae_admissions)

# Final data output ---------------------------------------------------------------------

FINAL_data <- purrr::reduce(list(
  #remove all other specs from rtt dataset
  FINAL_rtt %>%
    dplyr::ungroup()%>%
    dplyr::filter(treatment_function_name == 'Total') %>%
    dplyr::select(!c(treatment_function_name,treatment_function_code)),
  FINAL_ae_data,
  FINAL_diagnostic_data,
  FINAL_op_data,
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
  dplyr::mutate(covid_flag = case_when(year(date) == 2020 ~ 'covid',
                                       year(date) < 2020 ~ 'pre-covid',
                                       year(date) > 2020 ~ 'post-covid',
                                       TRUE ~ NA),
                #Add 0 for pre-covid (which will be na)
                covid_beds = case_when(is.na(covid_beds)==TRUE ~ 0,
                                       TRUE ~ covid_beds),
                trust_code = substr(trust_code,1,3))%>%
  #Important as a lot of these are curiously at site level?
  group_by(date,trust_code,covid_flag) %>%
  summarise_all(sum,na.rm=T) %>%
  dplyr::mutate(total_ftes2=total_ftes*sickness_rate)

<<<<<<< HEAD

FINAL_datayr <- FINAL_data %>%
  mutate(date = year(date))%>%
  group_by(date,trust_code,covid_flag) %>%
  summarise_all(sum,na.rm=T) %>%
  left_join(.,FINAL_backlog_data,by=c('trust_code','date'))
  
=======
>>>>>>> 5c6e4cc1dab4f5e36db7264a8406ec3e04ca1ee0
#tidy format
FINAL_tidy_data <- FINAL_data %>%
  tidyr::pivot_longer(cols=!c(date,trust_code,covid_flag),names_to='metric',values_to='values')

FINAL_regression_data <- FINAL_data %>% 
  dplyr::filter(covid_flag != 'covid') %>%
  dplyr::mutate(
    total_pathways = completed_pathways_for_admitted_patients + completed_pathways_for_non_admitted_patients,
    admitted_pathways = completed_pathways_for_admitted_patients,
    non_admitted_pathways = completed_pathways_for_non_admitted_patients,
    nurses_ratio = nurses/total_ftes,
    doctors_ratio = doctors/total_ftes,
    senior_doctors_ratio = senior_doctors/total_ftes,
    occupied_ratio = occupied_beds / bed_capacity) %>%
  dplyr::filter(nurses * doctors *total_ftes* non_admitted_pathways * admitted_pathways * trust_total_catchment *occupied_beds != 0) %>%
  dplyr::filter(admitted_pathways > 1000 & non_admitted_pathways > 1000) %>%
  filter(!trust_code %in% c('RRJ','RL1','RP6')) %>%
  ungroup()

FINAL_regression_datayr <- FINAL_datayr %>% 
  dplyr::filter(covid_flag != 'covid') %>%
  dplyr::mutate(
    total_pathways = completed_pathways_for_admitted_patients + completed_pathways_for_non_admitted_patients,
    admitted_pathways = completed_pathways_for_admitted_patients,
    non_admitted_pathways = completed_pathways_for_non_admitted_patients,
    nurses_ratio = nurses/total_ftes,
    doctors_ratio = doctors/total_ftes,
    senior_doctors_ratio = senior_doctors/total_ftes,
    occupied_ratio = occupied_beds / bed_capacity) %>%
  dplyr::filter(nurses * doctors *total_ftes* non_admitted_pathways * admitted_pathways * trust_total_catchment *occupied_beds != 0) %>%
  dplyr::filter(admitted_pathways > 1000 & non_admitted_pathways > 1000) %>%
  filter(!trust_code %in% c('RRJ','RL1','RP6')) %>%
  ungroup() %>%
  group_by(trust_code) %>%
  mutate(growth = (cost -lag(cost,n=1,order_by=date))/lag(cost,n=1,order_by=date),
         high_backlog = high + significant,
         low_backlog = low + moderate,
         ratio = high_backlog/low_backlog,
         backlog = case_when(
           (high+significant+moderate)/low >= 0 ~ (high+significant+moderate)/low,
           low == 0 ~ 0,
           TRUE ~ 0),
         backlog_per_bed = (high+significant+low+moderate)/bed_capacity)
  

#REMOVE ALL that aren't final. 
rm(list=ls()[!grepl("FINAL_", ls())])
