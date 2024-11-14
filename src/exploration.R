# DEPENDANCIES ------

source('src/functions.R')
rtt_data <- read.csv('const/raw_data/rtt_data.csv')

test_dataset <- rtt_data |> 
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
  CreateTimeSeriesIndex()

# VISUALISATION 1: TIME SERIES TRENDS -------

CreateTimeSeriesGraph(
  x = test_dataset,
  selected_metric = 'completed_pathways_for_non_admitted_patients',
  selected_specialty = 'C_999',
  select_title = 'Completed non-admitted activity over time',
  select_subtitle = 'Changes in completed non-admitted elective activity (2018 = 100)',
  select_y_axis = 'Completed activity index (2018 = 100)'
  )

# VISUALISATION 2: CHANGE IN GINI INDEX -------

CreateGiniGraph(
  x = test_dataset |> 
    CreateGiniData(),
  selected_metric = c('completed_pathways_for_non_admitted_patients','completed_pathways_for_admitted_patients'),
  selected_specialty = 'C_999',
  select_title = 'GINI Index',
  select_subtitle = 'Change in GINI Index across all trusts and specialties',
  select_y_axis = 'GINI Index'
)

# VISUALISATION 3: DISTRIBUTION OF TRUSTS -------

# VISUALISATION 4: ADDITIONAL EXPLORATION OF INTERESTS -------
