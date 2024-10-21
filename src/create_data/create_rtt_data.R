GetData <- function() {
  
  #Get RTT file names
  rtt_files <- purrr::map(
    .f = Rpublic::extract_links,
    .x = rtt_urls,
    pattern = "Full-CSV-data"
  ) |>
    unlist()

  #Function to clean all rtt datasets
  CleanRTTData <- function(x) {
    data <- x |>
      janitor::clean_names() |>
      dplyr::mutate(
        all_rtt = total_all,
        date = as.Date( x= paste0('01-',substr(period, 5, 99)),
                       format = "%d-%B-%Y")
      ) |>
      # Remove all the columns that are 'waiting 18 to 20 weeks' or whaever,
      # We aren't using them and there is mismatch after 2019/20
      dplyr::select(all_rtt, date, treatment_function_code, treatment_function_name, rtt_part_description, provider_org_code) |>
      # Select ALL specialties
      # dplyr::filter(treatment_function_name == 'Total') |>
      dplyr::rename("trust_code" = provider_org_code)

    return(data)
  }

  #Download, merge, and clean all files
  rtt_data <- purrr::map(
    .x = rtt_files,
    .f = function(.x) {
      Rpublic::extract_zipped(.x, pattern = "") |>
        dplyr::bind_rows() |>
        CleanRTTData()
    }) |>
    # Merge and clean all lists together
    dplyr::bind_rows() |>
    dplyr::group_by(date, treatment_function_code, treatment_function_name, rtt_part_description, trust_code) |>
    dplyr::summarise(all_rtt = sum(all_rtt, na.rm = T)) |>
    # Useful for left_joins
    tidyr::pivot_wider(names_from = rtt_part_description, values_from = all_rtt) |>
    janitor::clean_names()

  #Return output
  return(rtt_data)
}

rtt_data <- GetData()
rm(GetData)
