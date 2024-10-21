# Data -------

#backlog and investment
source('https://raw.githubusercontent.com/zeyadissa/open_data/main/src/eric_backlog.R')
source('https://raw.githubusercontent.com/zeyadissa/open_data/main/src/eric_investment.R')

FINAL_backlog_data <- site_data %>%
  dplyr::group_by(date,trust_code,risk) %>%
  dplyr::summarise(cost = sum(cost,na.rm=T)) %>%
  dplyr::ungroup()%>%
  tidyr::pivot_wider(names_from=risk,values_from=cost) %>%
  dplyr::left_join(.,trust_data,site_date,by=c('date','trust_code')) %>%
  dplyr::mutate(date = date) %>%
  dplyr::group_by(date,trust_code)%>%
  dplyr::summarise(low = sum(low,na.rm=T),
                   moderate = sum(moderate,na.rm=T),
                   high = sum(high,na.rm=T),
                   significant = sum(significant,na.rm=T),
                   investment = sum(investment,na.rm=T)) %>%
  replace(is.na(.), 0) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(cost = low + high + moderate + significant) %>%
  dplyr::group_by(trust_code) %>% 
  dplyr::left_join(.,deflator,by='date') %>%
  # Add GDP deflator
  dplyr::mutate(cost = cost * (deflator),
                investment = investment * deflator) %>%
  dplyr::mutate(total = cost + dplyr::lag(investment,n=1,order_by=date)) %>%
  dplyr::mutate(total_growth = (total -dplyr::lag(total,n=1,order_by=date))/dplyr::lag(total,n=1,order_by=date),
                invest_growth = (investment - lag(investment,n=1,order_by=date))/dplyr::lag(investment,n=1,order_by=date),
                cost_growth = (cost -dplyr::lag(cost,n=1,order_by=date))/dplyr::lag(cost,n=1,order_by=date)) %>%
  dplyr::select(date,trust_code,low,moderate,high,significant,investment,cost,total)
