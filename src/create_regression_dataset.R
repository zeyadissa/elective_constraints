# DEPENDANCIES --------

source('const/glob.R')
source('src/functions.R')

#RAW DATA (read for ease of access)
#source('src/create_data/create_rtt_data.R')
#source('src/create_data/create_diagnostics_data.R')
#source('src/create_data/create_workforce_data.R')
#source('src/create_data/create_operating_theatre_data.R')
source('src/create_data/create_eric_data.R')

rtt_dataset <- readRDS('const/results/rtt_data.rds')
diagnostic_data <- readRDS('const/results/diagnostic_data.rds')
workforce_data <- readRDS('const/results/workforce_data.rds')
op_theatre_data <- readRDS('const/results/operating_threatre_data.rds')

# Final data output ---------------------------------------------------------------------

diagnostics_data <- diagnostic_data |> 
  dplyr::filter(lubridate::year(date) >= 2019) |> 
  dplyr::mutate(trust_code = substr(trust_code,1,3)) |> 
  tidyr::pivot_longer(cols = -c(trust_code,date),names_to='metric',values_to='values') |>
  dplyr::mutate(treatment_function_code = 'C_999') |> 
  dplyr::mutate_all(~replace(., is.na(.), 0)) |> 
  dplyr::ungroup() |> 
  dplyr::filter(date > min(date,na.rm=T)) |> 
  SummariseAcrossTrusts() |> 
  CreateIndex()

medical_workforce_data <- workforce_data[['medical_workforce']] |> 
  dplyr::filter(lubridate::year(date) >= 2019) |> 
  dplyr::rename(
    trust_code = org_code,
    treatment_function_code = specialty_group,
    metric = grade,
    values = total_fte
  ) |> 
  dplyr::select(date,trust_code,treatment_function_code,metric,values) |> 
  dplyr::mutate(
    metric = dplyr::case_when(
      metric == 'Consultant (including Directors of Public Health)' ~ 'Consultant',
      T ~ metric
    )
  ) |> 
  SummariseAcrossSpecialties() |> 
  SummariseAcrossTrusts() |> 
  CreateIndex()

non_medical_workforce_data <- workforce_data[['non_medical_workforce']] |> 
  dplyr::filter(lubridate::year(date) >= 2019) |> 
  dplyr::rename(
    trust_code = org_code,
    treatment_function_code = care_setting,
    metric = staff_group_1,
    values = total_fte
  ) |> 
  dplyr::select(date,trust_code,treatment_function_code,metric,values) |> 
  dplyr::mutate(metric = substr(metric,5,nchar(metric))) |> 
  SummariseAcrossSpecialties() |> 
  SummariseAcrossTrusts() |> 
  CreateIndex()

rtt_data <- rtt_dataset |> 
  dplyr::filter(lubridate::year(date) >= 2019) |> 
  dplyr::ungroup() |> 
  #Wrangle for consistency; all files need to be in same format to be used
  dplyr::select(date,
                treatment_function_code,
                trust_code,
                completed_pathways_for_admitted_patients,
                completed_pathways_for_non_admitted_patients) |> 
  tidyr::pivot_longer(cols=-c(date,treatment_function_code,trust_code),names_to='metric',values_to='values') |> 
  dplyr::mutate(trust_code = substr(trust_code,1,3),
                date = lubridate::as_date(date)) |> 
  dplyr::group_by(date,treatment_function_code,trust_code,metric) |> 
  dplyr::summarise(values = sum(values,na.rm=T)) |> 
  dplyr::ungroup() |> 
  #from functions.R; note the behaviour.
  SummariseAcrossTrusts() |> 
  CreateIndex()

quarter_date <- data.frame(
  quarter = c(rep(1,3),rep(2,3),rep(3,3),rep(4,3)),
  month = c(1:12)
) |> 
  dplyr::mutate(quarter = paste0('Q',quarter))

operating_theatre_data <- op_theatre_data |> 
  dplyr::mutate(trust_code = substr(trust_code,1,3)) |> 
  dplyr::group_by(date,trust_code,metric) |> 
  dplyr::summarise(values = sum(values,na.rm=T)) |> 
  dplyr::mutate(treatment_function_code = 'all_specialties') |> 
  dplyr::ungroup() |> 
  SummariseAcrossTrusts() |> 
  dplyr::mutate(
    date = as.character(zoo::as.yearqtr(date)),
    quarter = substr(date,nchar(date)-1,nchar(date))) |> 
  dplyr::left_join(quarter_date,by='quarter',relationship = 'many-to-many') |> 
  dplyr::mutate(
    date = lubridate::make_date(
      year = substr(date,1,4),
      month = month,
      day = 1
    )
  ) |> 
  dplyr::select(-month,-quarter) |> 
  dplyr::filter(lubridate::year(date) >= 2019) |> 
  dplyr::ungroup() |> 
  CreateIndex() 

eric_dataset <- eric_data |> 
  SummariseAcrossTrusts() |> 
  CreateIndex()

final_dataset <- rbind(
  rtt_data,
  diagnostics_data,
  medical_workforce_data,
  non_medical_workforce_data,
  operating_theatre_data,
  eric_dataset
)

saveRDS(final_dataset,'const/results/regression_dataset.rds')
