
GetData <- function(){
  
day_beds_links <- Rpublic::extract_links(
  url_name = day_bed_url,
  pattern = '.xls'
)

#NOTE: This opts to take the last fifty quarters. I think prior to 2010
#the data changes shape / is not useful. If you want to expand it go ahead but
#make sure to check if the data format is consistent.

day_bed_data <- purrr::map(
  .x = day_beds_links[1:50],
  .f = function(.x){
    
    #download data
    dat <- Rpublic::extract_sheets(
      files = .x,
      pattern = 'Trust') |> 
      as.data.frame()
    
    names(dat) <- dat[13,]
    dat2 <- dat[-c(1:15),]
    
    dat3 <- dat2 |> 
      #Clean names and turn into long format
      janitor::clean_names() |> 
      dplyr::mutate(across(!any_of(c('year','period', 'region_code','org_code','org_name')),~as.numeric(.x))) |> 
      tidyr::pivot_longer(
        cols = !any_of(c('year','period', 'region_code','org_code','org_name')),
        names_to = 'treatment_function_code',
        values_to = 'values'
      ) |> 
      dplyr::select(!any_of(c('region_code','org_name'))) |> 
      #Main header has occupied, available, %occupied, turn to new metric called metric
      dplyr::mutate(
        metric = dplyr::case_when(
          substr(treatment_function_code,nchar(treatment_function_code)-1,nchar(treatment_function_code)) == '_2' ~ 'occupied',
          substr(treatment_function_code,nchar(treatment_function_code)-1,nchar(treatment_function_code)) == '_3' ~ 'percent_occupied',
          T ~ 'available'
        )
      ) |> 
      #Clean by removing nas and the _1 / _2 from same names
      dplyr::filter(substr(treatment_function_code,1,2) != 'na') |> 
      dplyr::filter(metric != 'percent_occupied') |> 
      dplyr::mutate(treatment_function_code = dplyr::case_when(
        metric != 'available' ~ substr(treatment_function_code,1,nchar(treatment_function_code)-2),
        T ~ treatment_function_code),
        #Create date in quarter format
        month = match(period, month.name),
        year = as.numeric(substr(year,1,4))
        ) |> 
      dplyr::select(!period) |> 
      dplyr::rename('trust_code'='org_code')

    return(dat3)
    
    }
) 

return(day_bed_data)

}

day_bed_data <- GetData()
rm(GetData)
