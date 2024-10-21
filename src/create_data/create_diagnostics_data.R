GetData <- function(){
  
  CleanDiagData <- function(x){
    data <- x |> 
      janitor::clean_names() |> 
      #Extract data and create date
      dplyr::mutate(date = as.Date(
        x= paste0('01-',substr(period,6,nchar(period))),
        format = '%d-%B-%Y'),
        diagnostic_tests = dplyr::case_when(
          diagnostic_tests == 'TOTAL' ~ 'all_diagnostics_tests',
          TRUE ~ diagnostic_tests)) |> 
      dplyr::select(diagnostic_tests,commissioner_parent_org_code,date,total_activity,provider_org_code) |> 
      dplyr::rename('org' = commissioner_parent_org_code,
                    'trust_code' = `provider_org_code`) |> 
      dplyr::group_by(trust_code,date,diagnostic_tests) |> 
      dplyr::summarise(tests = sum(total_activity,na.rm=T),.groups='keep') |> 
      tidyr::pivot_wider(names_from = diagnostic_tests,values_from=tests) |> 
      #this is so stupid but im lazy
      janitor::clean_names()

    return(data)
  }
  
  #Source the main diagnostics dataset
  #Get the full extracts
  diag_links1 <- Rpublic::extract_links(diag_url,'monthly-diagnostics-waiting-times-')
  
  #Get ones that are only full extracts
  diag_links2 <- purrr::map(
    .x = diag_links1,
    .f = Rpublic::extract_links,
    pattern = 'full-extract') |> 
    unlist()
    
  #Remove CDCs (should these be included?)
  diag_links <- diag_links2[!grepl("CDC", diag_links2)]
  
  #Download everything
  raw_diagnostics_data <- purrr::map(
    .x = diag_links,
    .f = function(.x){
      Rpublic::extract_zipped(
        files = .x,
        pattern = '') |> 
        dplyr::bind_rows() |> 
        CleanDiagData()}) |> 
    dplyr::bind_rows()
  
  return(raw_diagnostics_data)}

diagnostic_data <- GetData()
rm(GetData)
