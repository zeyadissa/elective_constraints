GetData <- function(x) {
  # org to icb mapping file location

  # wrangling to get a final mapping file
  temp <- tempfile()
  download.file(org_url, temp)
  FINAL_org_mapping <- read.csv(unz(temp, "etr.csv"), header = F, fill = T)
  names(FINAL_org_mapping)[1:4] <- c("trust_code", "org_name", "region_code", "org")
  org_mapping_data <- FINAL_org_mapping |> 
    dplyr::select(org, trust_code)
  
  return(org_mapping_data)
  
}

org_mapping_data <- GetData()
rm(GetData)