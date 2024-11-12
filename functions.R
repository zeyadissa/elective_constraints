write.csv(rtt_data,'~/Desktop/rtt_data.csv')
rtt_data <- read.csv('~/Desktop/rtt_data.csv')

test_dataset <- rtt_data |> 
  dplyr::ungroup() |> 
  dplyr::select(date,
                treatment_function_code,
                trust_code,
                completed_pathways_for_admitted_patients,
                completed_pathways_for_non_admitted_patients) |> 
  tidyr::pivot_longer(cols=-c(date,treatment_function_code,trust_code),names_to='metric',values_to='values')

CreateTimeSeriesIndex <- function(x){
  
  total_data <- x |> 
    dplyr::group_by(date,treatment_function_code,metric) |> 
    dplyr::summarise(values = sum(values,na.rm=T)) |> 
    dplyr::mutate(trust_code = 'All Trusts')
  
  final_data1 <- rbind(x,
                      total_data)
  
  baseline_data <- final_data1 |> 
    dplyr::filter(date == min(date,na.rm=T)) |> 
    dplyr::rename(baseline_values = 'values') |> 
    dplyr::select(!date)
  
  output <- final_data1 |> 
    dplyr::left_join(baseline_data) |> 
    dplyr::mutate(index = values / baseline_values)
  
  return(output)
  
}

CreateTimeSeriesGraph <- function(x){
  
}
time_series_data <- CreateTimeSeriesIndex(test_dataset)


ggplot2::ggplot()+
  ggplot2::geom_line(
    data = time_series_data |> 
      dplyr::filter(metric == 'completed_pathways_for_non_admitted_patients') |> 
      dplyr::filter(treatment_function_code == 'C_999'),
    ggplot2::aes(x = date,y=index,group = trust_code),
    alpha = 0.2
  ) +
  ggplot2::geom_line(
    data = time_series_data |> 
      dplyr::filter(metric == 'completed_pathways_for_non_admitted_patients') |> 
      dplyr::filter(treatment_function_code == 'C_999' &
                      trust_code == 'All Trusts'),
    ggplot2::aes(x = date,y=index),
    col = 'red'
  ) +
  ggplot2::ylim(0,2)
  