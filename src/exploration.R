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

# VISUALISATION 1: TIME SERIES TRENDS -------

#Time series graph for all trusts. You can choose to do whatever. Note strike impact
#Really interesting! Range between best and worst has also increased! Again, fascinating
CreateTimeSeriesGraph(
  x = final_dataset,
  selected_trust = 'all_trusts',
  selected_metric = c('Consultant',"Nurses & health visitors",'all_diagnostics_tests','operating_theatres'),
  selected_specialty = c('all_specialties','C_999'),
  select_title = 'Completed non-admitted activity over time',
  select_subtitle = 'Changes in completed non-admitted elective activity (2018 = 100)',
  select_y_axis = 'Completed activity index (2018 = 100)'
  )+
  ggplot2::geom_hline(yintercept=1,linetype=2)

# VISUALISATION 2: CHANGE IN GINI INDEX -------

#Big story here, but I have no idea what.
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