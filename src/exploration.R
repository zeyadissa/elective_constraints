# DEPENDANCIES ------

#FOR ASHLEY:
#This is the raw dataset that we are going to be using for the RTT file.
#I want to do the exact same for workforce with the same specialty mapping. 
#If you can create a workforce file (as below) in the exact same format
#as the rtt data (important same columns + types so the functions can be used!)
#That would be great

source('src/functions.R')
#source('src/create_regression_dataset.R')
final_dataset<-readRDS('const/results/regression_dataset.rds')

# VISUALISATION 1: TIME SERIES TRENDS, WORKFORCE -------

#Time series graph for all trusts. You can choose to do whatever. Note strike impact
#Really interesting! Range between best and worst has also increased! Again, fascinating
CreateTimeSeriesGraph(
  x = final_dataset,
  selected_trust = 'all_trusts',
  selected_metric = c(
    'Consultant',
    "Nurses & health visitors"),
  selected_specialty = c('all_specialties','C_999'),
  select_title = 'Elective Constraints',
  select_subtitle = 'Labour determinants and constraints to elective activity (2019 = 100)',
  select_y_axis = 'Index (2019 = 100)'
)+
  ggplot2::geom_hline(yintercept=1,linetype=2) +
  ggplot2::ylim(0.75,1.5)

# VISUALISATION 1: TIME SERIES TRENDS, CAPITAL -------

#Time series graph for all trusts. You can choose to do whatever. Note strike impact
#Really interesting! Range between best and worst has also increased! Again, fascinating
CreateTimeSeriesGraph(
  x = final_dataset,
  selected_trust = 'all_trusts',
  selected_metric = c(
    'all_diagnostics_tests',
    'operating_theatres',
    'serious_backlog',
    'day_beds_total'),
  selected_specialty = c('all_specialties','C_999'),
  select_title = 'Elective Constraints',
  select_subtitle = 'Capital determinants and constraints to elective activity (2019 = 100)',
  select_y_axis = 'Index (2019 = 100)'
)+
  ggplot2::geom_hline(yintercept=1,linetype=2) +
  ggplot2::ylim(0.75,1.5)

# VISUALISATION 3: CHANGE SINCE BASELINE -------

test<-final_dataset |> 
  dplyr::filter(treatment_function_code %in% c('all_specialties','C_999') &
                  trust_code %in% c('all_trusts')) |> 
  dplyr::group_by(pod,trust_code,treatment_function_code,metric) |> 
  dplyr::filter(date %in% c(max(date,na.rm=T),min(date,na.rm=T))) |> 
  dplyr::select(date,metric,index)
  
test2 <- test |> 
  dplyr::filter(date %in% c(min(date,na.rm=T))) |> 
  dplyr::rename('first_date' = 'date') |> 
  dplyr::select(!index) |> 
  dplyr::left_join(test |> 
                     dplyr::filter(date %in% c(max(date,na.rm=T))) |> 
                     dplyr::rename('last_date' = 'date')) |> 
  dplyr::mutate(diff_mon = lubridate::interval(start=first_date,
                                               end=last_date) %/% months(1),
                change_cagr = 100*((index ^ (1/diff_mon)) - 1),
                pod2 = dplyr::case_when(
                  pod %in% c('operating_theatre_data_final','day_bed_data_final','diagnostics_data_final','eric_data_final') ~ 'Capital',
                  pod == 'rtt_data_final' ~ 'Outcome',
                  T ~ 'Labour'
                ))

ggplot2::ggplot() +
  ggplot2::geom_col(
    data = test2 |> 
      dplyr::filter(metric %in% c(
        'all_diagnostics_tests',
        'operating_theatres',
        'serious_backlog',
        'day_beds_total',
        'Consultant',
        'completed_pathways_for_admitted_patients',
        'completed_pathways_for_non_admitted_patients',
        "Nurses & health visitors")),
    ggplot2::aes(y = forcats::fct_reorder(metric,.x = change_cagr),
                 x = change_cagr,
                 fill = pod2),
    col = 'black') +
  ggplot2::theme_bw() +
  ggplot2::labs(fill = 'Input') +
  ggplot2::ylab('') + 
  ggplot2::xlab('Change since 2018/19') +
  ggplot2::scale_x_continuous(labels = scales::percent) +
  ggplot2::ggtitle(
    label = 'Elective Constraints since COVID-19',
    subtitle = 'Percent change in key Capital and Labour constraints since 2018/19'
  )

# VISUALISATION 2: CHANGE IN GINI INDEX -------


# VISUALISATION 3: DISTRIBUTION OF TRUSTS -------

#FOR ASHLEY:
#I'm struggling to think of what visualisation to put here. I really think it would be
#helpful to see the distribution of how much some trusts have recovered since COVID-19
#a trust and the value on x is activity in 2018/19 and y is activity in 2024/25
#But the standardisation is confusing. Could also do a bar chart? I don't know.
#If possible, have a think about what to do here and what visualisations are best. 

# VISUALISATION 4: SUMMARY VISUALISATIONS (ASHLEY) -------

#FOR ASHLEY:
#Your previous work should be added here (the stuff you shared in the ppt)
#I think we should start curating it and thinking about where to put it in the grand scheme
#of things: i.e: what story are we saying? I really want to bring in the stuff you've
#done front and centre - although it won't be possible to put every chart in there,
#I am thinking this can be a dashboard of sorts that people can look at because the
#information is genuinly useful.

# VISUALISATION 5: ADDITIONAL EXPLORATION OF INTERESTS -------

#Additionally, if you find anything else of interest feel free to add your explorations here.
#Eg: we talked about the downwards spikes due to strikes. Does strike action
#Impact different specialties differently? etc... this type of stuff can be of interest
#and you have full reign.

#SAM: Population density; cities and rural; 