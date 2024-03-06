source('src/data_source.R')

# EDA -------------------------------------------------------------------

#This is a mess; completely exploratory no sense in mind. Accept it as it is
baseline <- FINAL_data %>%
  select(date,trust_code,completed_pathways_for_admitted_patients,completed_pathways_for_non_admitted_patients) %>%
  drop_na() %>%
  filter(year(date) %in% c(2018)) %>%
  group_by(trust_code) %>%
  summarise(
    baseline_non = mean(completed_pathways_for_non_admitted_patients,na.rm=T),
    baseline_admit = mean(completed_pathways_for_admitted_patients,na.rm=T)
  ) %>%
  unique()

#Time series dataset
time_series_total <- 
  FINAL_data %>%
  select(date,trust_code,completed_pathways_for_admitted_patients,completed_pathways_for_non_admitted_patients) %>%
  left_join(.,baseline,
            by=c('trust_code')) %>%
  ungroup()%>%
  mutate(
    pathways_admit = completed_pathways_for_admitted_patients / baseline_admit,
    flag = case_when(
      pathways_admit > 1 ~ 'Above baseline',
      TRUE ~ 'Below baseline'
    )
  ) %>%
  group_by(flag,date) %>%
  summarise(pathways=n()) %>%
  filter(year(date) >= 2019) %>%
  mutate(pathways = case_when(
    flag == 'Below baseline' ~ pathways*-1,
    TRUE ~ pathways
  )) 

#Clustered dataset
cluster_data <- FINAL_data %>%
  filter(covid_flag == 'post-covid') %>%
  group_by(trust_code) %>%
  mutate(occupancy = occupied_beds/bed_capacity,
         pathways = (completed_pathways_for_admitted_patients),
         total_ftes = scale(total_ftes),
         occupied_beds = scale(occupied_beds),
         nurses_to_doctors = scale(nurses/all_doctors)) %>%
  ungroup()%>%
  select(pathways,occupancy) %>% 
  drop_na()

#time_series_trend
ggplot(data=time_series_total) +
  geom_col(aes(x=date,y=pathways,fill=flag)) +
  THFstyle::scale_fill_THF()+
  ylab('Number of trusts above or below baseline') +
  xlab('')+
  scale_y_continuous(labels = abs) +
  THFstyle::theme_THF() +
  theme(
    axis.text = element_text(size=(10)),
    axis.text.y = element_text(size=(10))
  )


#clustering
kmeans_output <- kmeans(cluster_data,centers=4)
cluster_data$cluster_id <- factor(kmeans_output$cluster)
ggplot(cluster_data, aes(occupancy, pathways, color = cluster_id)) +
  geom_point()+
  THFstyle::theme_THF()+
  THFstyle::scale_fill_THF()

#Gini coeff function
Gini = function (x, corr = FALSE, na.rm = TRUE) 
{
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


#split by group
rtt_filtered <- FINAL_rtt %>%
  filter(completed_pathways_for_admitted_patients > 1000 | completed_pathways_for_non_admitted_patients > 1000) %>%
  ungroup()

rtt_2018 <- rtt_filtered %>%
  filter(year(date) == 2019) %>%
  ungroup()%>%
  select(!date) %>%
  group_by(treatment_function_code,trust_code) %>%
  summarise_all(mean,na.rm=T) %>%
  mutate(admitted_2018 = completed_pathways_for_admitted_patients,
         non_admitted_2018 = completed_pathways_for_non_admitted_patients) %>%
  select(trust_code,treatment_function_code,admitted_2018,non_admitted_2018) %>%
  ungroup()

rtt_gini <- rtt_filtered %>%
  filter(year(date) == 2023) %>%
  ungroup()%>%
  select(!date) %>%
  group_by(treatment_function_name,treatment_function_code,trust_code) %>%
  summarise_all(mean,na.rm=T) %>%
  ungroup()%>%
  left_join(.,rtt_2018,by=c('treatment_function_code','trust_code')) %>%
  mutate(admitted = completed_pathways_for_admitted_patients / admitted_2018,
         non_admitted = completed_pathways_for_non_admitted_patients / non_admitted_2018) %>%
  select(trust_code,treatment_function_name,admitted,non_admitted) %>%
  drop_na()

rtt_list<-split(rtt_gini,rtt_gini$treatment_function_name)

#create table
gini_table <- lapply(rtt_list,function(x){
  data <- data.frame(
    'admitted_gini' = Gini(x$admitted),
    'non_admitted_gini' = Gini(x$non_admitted),
    'specialty' = unique(x$treatment_function_name)
  )
}) %>%
  rbindlist()

#AE stuff

tet<-FINAL_overnight_beds %>%
  left_join(.,FINAL_ae_data,by=c('trust_code','date'))

test <- tet %>%
  summarise(total = sum(total,na.rm=T),
            breach = sum(breach,na.rm=T),
            occupied_beds = sum(occupied_beds,na.rm=T),
            bed_capacity = sum(bed_capacity,na.rm=T)) %>%
  mutate(prop = breach/total,
         beds = 1-(occupied_beds/bed_capacity))

ggplot()+
  geom_point(data=tet %>% filter(prop != 0),
            aes(x=1-(occupied_beds/bed_capacity),y=prop), col = 'black',alpha=0.2) +
  geom_vline(xintercept=test$beds,col='red')+
  geom_hline(yintercept = test$prop,col='red')+
  theme_bw() +
  ylab('Proportion of AE breaches') +
  xlab('Percent open overnight bed capacity')

ggplot()+
  geom_point(data=test,
             aes(x=beds,y=prop), col = 'black',alpha=1) +
  theme_bw() +
  ylab('Proportion of AE breaches') +
  xlab('Percent open overnight bed capacity')

ggplot()+
  geom_line(data=tet %>% filter(prop != 0),
             aes(x=date,y=1-(occupied_beds/bed_capacity),group=trust_code), col = 'black',alpha=0.1) +
  theme_bw() +
  ylab('Proportion of free beds') +
  xlab('Date')
