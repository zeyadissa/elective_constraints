# Data -------

GetData <- function(){
  
#backlog and investment
eric_data <- read.csv('https://raw.githubusercontent.com/zeyadissa/eric_analysis/refs/heads/main/output/final_data.csv?token=GHSAT0AAAAAAC43UJ234JGYN5ZC275NS6AWZ335ONQ')

out <- eric_data |> 
  dplyr::select(trust_code,date,high,low,moderate,significant,total_investment) |> 
  dplyr::mutate(treatment_function_code = 'all_specialties')

return(out)

}

eric_data <- GetData()
rm(GetData)
