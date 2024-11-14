#Functions -----

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

GetGini <- function (x, corr = FALSE, na.rm = TRUE) {
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

CreateGiniData <- function(x){
  
  output <- x |> 
    dplyr::filter(trust_code != 'All Trusts') |> 
    dplyr::group_by(date,metric, treatment_function_code) |> 
    dplyr::summarise(gini = GetGini(values))
  
  return(output)
  
}

CreateGiniGraph <- function(x,selected_metric,selected_specialty,select_title,select_subtitle = '',select_y_axis = ''){
  
  ggplot2::ggplot(
    data = x |> 
      dplyr::filter(metric %in% selected_metric &
               treatment_function_code == selected_specialty)
  )+
    ggplot2::geom_line(
      ggplot2::aes(x = date,
                   y = gini,
                   col = metric)) + 
    ggplot2::xlab('') + 
    ggplot2::ylab(select_y_axis) +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(
      label = select_title,
      subtitle = select_subtitle
    ) 
}
