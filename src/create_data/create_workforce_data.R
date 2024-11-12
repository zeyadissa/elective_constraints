GetData <- function() {
  
  # read links
  # Note there is a very annoying amount of cleaning to do here
  # This is primarily due to the nature of what we're trying to get which is the med
  # files from 2016 on - work thru it for the logic.

  workforce_url <- "https://digital.nhs.uk/data-and-information/publications/statistical/nhs-workforce-statistics"

  workforce_links1 <- Rpublic::extract_links(workforce_url, "nhs-workforce-statistics")
  
  workforce_links2 <- purrr::map(
    .x = paste0("https://digital.nhs.uk/", workforce_links1),
    .f = Rpublic::extract_links,
    pattern = ".zip"
  ) |>
    unlist()


  workforce_links3 <- workforce_links2[grepl(pattern = "%20csv%20files.zip|nhs-work-stat-|nhs_workforce_statistics_", workforce_links2)]

  workforce_links <- workforce_links3[!grepl(pattern = "medical|med|nonmed|quart|2013|2014|2015", workforce_links3)]

  CleanWorkforceData <- function(x) {
    x |>
      dplyr::bind_rows() |>
      dplyr::mutate(Date = as.Date(Date)) |>
      dplyr::filter(lubridate::year(Date) >= 2010) |>
      janitor::clean_names() |>
      dplyr::select(date, specialty, specialty_group, total_fte, grade, org_code, grade_sort_order)
  }

  # read data
  workforce_data <- purrr::map(
    .x = workforce_links,
    .f = function(.x) {
      Rpublic::extract_zipped(.x, pattern = "medical staff") |>
        CleanWorkforceData()
    }
  ) 
  
  workforce_data_final <- workforce_data |> 
    purrr::keep(function(x) nrow(x) <= 20000 & nrow(x) != 0) |> 
    dplyr::bind_rows()

  return(workforce_data_final)
}

workforce_data <- GetData()
rm(GetData)
