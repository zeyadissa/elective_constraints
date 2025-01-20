# DEPENDANCIES --------

source('const/glob.R')
source('src/functions.R')

#RAW DATA (read for ease of access)
#source('src/create_data/create_rtt_data.R')
#source('src/create_data/create_diagnostics_data.R')
#source('src/create_data/create_workforce_data.R')
#source('src/create_data/create_operating_theatre_data.R')
source('src/create_data/create_eric_data.R')
source('src/create_data/create_day_bed_data.R')

rtt_dataset <- readRDS('const/results/rtt_data.rds')
diagnostic_data <- readRDS('const/results/diagnostic_data.rds')
workforce_data <- readRDS('const/results/workforce_data.rds')
op_theatre_data <- readRDS('const/results/operating_threatre_data.rds')

quarter_date <- data.frame(
  quarter = c(rep(1,3),rep(2,3),rep(3,3),rep(4,3)),
  month = c(1:12)
) |> 
  dplyr::mutate(quarter = paste0('Q',quarter))

# DATA ---------------------------------------------------------------------

## Diagnostics ---------------------------------------------------------------------

diagnostics_data_final <- diagnostic_data |> 
  dplyr::filter(lubridate::year(date) >= 2019) |> 
  dplyr::mutate(trust_code = substr(trust_code,1,3)) |> 
  tidyr::pivot_longer(cols = -c(trust_code,date),names_to='metric',values_to='values') |>
  dplyr::mutate(treatment_function_code = 'C_999') |> 
  dplyr::mutate_all(~replace(., is.na(.), 0)) |> 
  dplyr::ungroup() |> 
  dplyr::filter(date >= index_baseline_date) |> 
  SummariseAcrossTrusts() |> 
  CreateIndex()

diagnostics_data_final <- CreatePOD(diagnostics_data_final)

## Workforce ---------------------------------------------------------------------

medical_workforce_data_final <- workforce_data[['medical_workforce']] |> 
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
  dplyr::filter(date >= index_baseline_date) |> 
  CreateIndex() 

medical_workforce_data_final <- CreatePOD(medical_workforce_data_final)

non_medical_workforce_data_final <- workforce_data[['non_medical_workforce']] |> 
  dplyr::filter(date >= index_baseline_date) |> 
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

non_medical_workforce_data_final <- CreatePOD(non_medical_workforce_data_final)

## RTT Elective ---------------------------------------------------------------------

rtt_data_final <- rtt_dataset |> 
  dplyr::filter(date >= index_baseline_date) |> 
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

rtt_data_final <- CreatePOD(rtt_data_final)

## Beds ---------------------------------------------------------------------

day_bed_data_2 <- day_bed_data |> 
  #turn to data.frame (from list!!)
  dplyr::bind_rows() |> 
  dplyr::left_join(quarter_date,by='month') |> 
  dplyr::select(!month) |> 
  dplyr::left_join(quarter_date,by='quarter',relationship = 'many-to-many') |> 
  dplyr::mutate(
    date = lubridate::make_date(
      year = year,
      month = month,
      day = 1
    )
  ) |> 
  dplyr::select(!c(year,month,quarter))

day_bed_data_final <- day_bed_data_2 |> 
  rbind(
    day_bed_data_2 |> 
      dplyr::group_by(trust_code,treatment_function_code,date) |> 
      dplyr::summarise(values = sum(values,na.rm=T)) |> 
      dplyr::mutate(metric = 'total')
  ) |> 
  dplyr::filter(treatment_function_code !='total') |> 
  SummariseAcrossTrusts() |> 
  SummariseAcrossSpecialties() |> 
  dplyr::filter(date >= index_baseline_date) |> 
  CreateIndex() |> 
  dplyr::mutate(
    metric = paste0('day_beds_',metric)
  )

day_bed_data_final <- CreatePOD(day_bed_data_final)

## Operating Theatres ---------------------------------------------------------------------

operating_theatre_data_final <- op_theatre_data |> 
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
  dplyr::filter(date >= index_baseline_date) |> 
  dplyr::ungroup() |> 
  CreateIndex() 

operating_theatre_data_final <- CreatePOD(operating_theatre_data_final)

## Maintenance & Estates ---------------------------------------------------------------------

eric_data_final <- eric_data |> 
  dplyr::rowwise() |> 
  dplyr::mutate(
    serious_backlog = sum(high,significant,na.rm=T),
    total_backlog = sum(high,significant,low,moderate,na.rm=T)
  ) |> 
  dplyr::filter(
    date >= 2019
    ) |> 
  tidyr::pivot_longer(
    cols = -c(trust_code,date,treatment_function_code),
    names_to='metric',
    values_to='values'
  ) |> 
  SummariseAcrossTrusts() |> 
  dplyr::mutate(date = lubridate::make_date(
    year = date,
    month = 3,
    day = 1
  )) |> 
  dplyr::filter(date >= index_baseline_date) |> 
  CreateIndex()

eric_data_final <- CreatePOD(eric_data_final)

# FINAL DATASET ---------

final_dataset <- rbind(
  rtt_data_final,
  diagnostics_data_final,
  medical_workforce_data_final,
  non_medical_workforce_data_final,
  operating_theatre_data_final,
  eric_data_final,
  day_bed_data_final
)

saveRDS(final_dataset,'const/results/regression_dataset.rds')
