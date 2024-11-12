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

GetGiniIndex = function (x, corr = FALSE, na.rm = TRUE) {
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
  
  #Needed for geom_ribbon
  output_quartiles <- output |> 
    dplyr::filter(trust_code != 'All_Trusts') |> 
    dplyr::group_by(date,treatment_function_code,metric) |> 
    dplyr::summarise(
      upper_quartile = quantile(index,0.75,na.rm=T),
      lower_quartile = quantile(index,0.25,na.rm=T),
      median = quantile(index,0.5,na.rm=T))
  
  final_output <- output |> 
    dplyr::left_join(output_quartiles)
    
  return(final_output)
  
}

CreateTimeSeriesGraph <- function(x,selected_metric,selected_specialty,select_title,select_subtitle = '',select_y_axis = ''){
  
  dat <- x |> 
    dplyr::filter(
      metric == selected_metric &
        treatment_function_code == selected_specialty)
  
  graph <- ggplot2::ggplot(data = dat) +
    ggplot2::geom_line(
      #Filter data for metric and treatment function code
      ggplot2::aes(x = date,
                   y = index,
                   group = trust_code),
      col = 'gray',
      alpha = 0.15) + 
    ggplot2::geom_line(
      #Filter data for metric and treatment function code
      ggplot2::aes(x = date,
                   y = upper_quartile),
      col = 'red',
      linetype = 2) + 
    ggplot2::geom_line(
      #Filter data for metric and treatment function code
      ggplot2::aes(x = date,
                   y = lower_quartile),
      col = 'red',
      linetype = 2)+
    ggplot2::geom_line(
      data = dat |> 
        dplyr::filter(trust_code == 'All Trusts'),
      #Filter data for metric and treatment function code
      ggplot2::aes(x = date,
                   y = index),
      col = 'red',
      linewidth = 0.75,
      linetype = 1)+
    ggplot2::ylim(0,2) +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(
      label = select_title,
      subtitle = select_subtitle
    ) +
    ggplot2::xlab('')+
    ggplot2::ylab(select_y_axis)
  
  return(graph)
}

time_series_data <- CreateTimeSeriesIndex(test_dataset)

CreateTimeSeriesGraph(x= time_series_data,
                      selected_metric = 'completed_pathways_for_non_admitted_patients',
                      selected_specialty = 'C_999',
                      select_title = 'Completed Non-Admitted Elective Activity Trends',
                      select_subtitle = 'Index of completed non-admitted elective activity from 2018 to present across all trusts',
                      select_y_axis='Index of completed activity (2018 = 100)')
  