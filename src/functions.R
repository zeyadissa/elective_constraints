#Functions -----

CreatePOD <- function(x){

  pod_name <- deparse(substitute(x))
  
  out <- x |> 
    dplyr::mutate(pod = pod_name)
  
  return(out)
  
} 

SummariseAcrossTrusts <- function(x){
  
  intermed <- x |> 
    dplyr::group_by(date,treatment_function_code,metric) |> 
    dplyr::summarise(values = sum(values,na.rm=T)) |> 
    dplyr::mutate(trust_code = 'all_trusts')
  
  out <- x |> 
    rbind(intermed)
  
  return(out)
  
}

SummariseAcrossSpecialties <- function(x){
  
  intermed <- x |> 
    dplyr::group_by(date,trust_code,metric) |> 
    dplyr::summarise(values = sum(values,na.rm=T)) |> 
    dplyr::mutate(treatment_function_code = 'all_specialties')
  
  out <- x |> 
    rbind(intermed)
  
  return(out)
  
}

CreateIndex <- function(x,index_year,groups = c('treatment_function_code','trust_code','metric')){
  
  baseline <- x |> 
    dplyr::filter(date == min(date,na.rm=T)) |> 
    dplyr::group_by(date,metric,trust_code,treatment_function_code) |> 
    dplyr::summarise(baseline = sum(values,na.rm=T)) |> 
    dplyr::ungroup() |> 
    dplyr::select(!date)
  
  out <- x |> 
    dplyr::left_join(baseline,
                     by=groups) |> 
    dplyr::mutate(index = values / baseline) |> 
    dplyr::select(!baseline)
    
  return(out)
  
}

CreateTimeSeriesGraph <- function(x,
                                  selected_metric,
                                  selected_trust,
                                  selected_specialty,
                                  select_title,
                                  index_flag = T,
                                  select_subtitle = '',
                                  select_y_axis = ''){
  
  dat <- x |> 
    dplyr::filter(
      metric %in% selected_metric &
        treatment_function_code %in% selected_specialty &
        trust_code %in% selected_trust)
  
  graph <- ggplot2::ggplot(data = dat) +
    ggplot2::geom_line(
      #Filter data for metric and treatment function code
      ggplot2::aes(x = date,
                   y = index,
                   col = metric,
                   linetype = trust_code),
      alpha = 1) +
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
