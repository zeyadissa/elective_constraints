GetData <- function() {
  
  # operating table links. We have to get rid of 2013 to 2015 as:
  # 1) they're not needed
  # 2) change in formatting messes up the read.
  # This is a hackish approach i am aware :(
  op_th_links1 <- Rpublic::extract_links(op_th_url, "Operating-Theatres")
  op_th_links2 <- op_th_links1[!grepl("transparency", op_th_links1)]
  op_th_links <- op_th_links2[!grepl("-2013|-2012|-2014|-2015", op_th_links2)]

  CleanOTData <- function(x) {
      data <- x |> 
        # Extracts year and date from string in random cell.
        dplyr::mutate(date = paste0(
          stringr::str_sub(x[2, 2], nchar(x[2, 2]) - 4, nchar(x[2, 2]) - 1),
          " Q",
          stringr::str_sub(x[2, 2], 9, 9)
        ))
      # fix rows; names are row 12 everything prior is useless
      names(data) <- data[12, ]
      data <- data[-c(1:14), ]
      # new row called date that we mutated.
      names(data)[7] <- "date"
      # fix names
      data <- data |> 
        janitor::clean_names() |> 
        dplyr::mutate(date = zoo::as.yearqtr(date)) |> 
        rename("trust_code" = organisation_code)
      return(data)
    }

  #Download everything
  raw_ot_data <- purrr::map(
    .x = op_th_links,
    .f = function(.x){
      #Read file
      openxlsx::read.xlsx(
        xlsxFile = .x,
        sheet = 1) |> 
        CleanOTData()}) |> 
    #summarise for ease of access
    dplyr::bind_rows() |> 
    dplyr::group_by(date, trust_code) |> 
    dplyr::summarise(operating_theatres = sum(as.numeric(number_of_operating_theatres), na.rm = T))
  
  #return for GetData()
  return(raw_ot_data)
  
}

operating_theatre_data <- GetData()
rm(GetData)