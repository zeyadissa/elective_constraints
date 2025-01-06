# Data -------

GetData <- function(){
  
#backlog and investment
eric_data <- read.csv('https://raw.githubusercontent.com/zeyadissa/eric_analysis/refs/heads/main/output/final_data.csv?token=GHSAT0AAAAAAC43UJ22TZ7BBV6Q4LZ4OXGWZ3344TA')

out <- eric_data |> 
  dplyr::select(trust_code,date,high,low,moderate,significant,total_investment) |> 
  dplyr::mutate(treatment_function_code = 'all_specialties') |> 
  tidyr::pivot_longer(
    cols = -c(trust_code,date,treatment_function_code),
    names_to='metric',
    values_to='values'
  )

return(out)

}

eric_data <- GetData()
rm(GetData)
