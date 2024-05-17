## eda data ------

source('src/data_source.R')

eda_data <- data %>%
  mutate(
    backlog = low + moderate + high + significant,
    serious = significant + high,
    non_serious = low + moderate) %>%
  select(date,
         total_pathways,
         total_ftes,
         nurses,
         senior_doctors,
         operating_theatres,
         covid_beds,
         serious,
         non_serious,
         occupied_beds
         ) %>%
  group_by(date)%>%
  dplyr::summarise_all(sum,na.rm=T) %>%
  mutate(nurses_ratio = nurses / total_ftes,
         senior_doctors = senior_doctors / total_ftes,
         ratio = serious/non_serious) %>%
  select(!c(nurses,senior_doctors,serious,non_serious))
  
## plot 1 data ------

plot_data <- eda_data %>%
  mutate(
    pathways = total_pathways / (eda_data %>% filter(date == min(date)))$total_pathways,
    total_ftes = total_ftes / (eda_data %>% filter(date == min(date)))$total_ftes
  )

plot_1 <- ggplot()+
  geom_line(data=plot_data,aes(x=date,y=pathways),col='red') + 
  geom_line(data=plot_data,aes(x=date,y=total_ftes),col='blue') +
  geom_hline(yintercept=1)

## plot 2 ------

plot_2a <- eda_data %>%
  pivot_longer(cols=!date,names_to='metric',values_to='values')

plot_2b <- eda_data %>%
  pivot_longer(cols=!date,names_to='metric',values_to='base_values') %>%
  filter(date == min(date)) %>%
  select(!date)

plot_2 <- plot_2a %>%
  left_join(plot_2b,by=c('metric')) %>%
  mutate(index = values/base_values)

ggplot()+
  geom_line(data=plot_2,aes(x=date,y=index,col=metric))

