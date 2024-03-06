#Functions

#Gets links from any single url; string matches
GetLinks <- function(url_name,string){
  files <- c()
  #this is inefficient and bad practice but it's a small vector.
  for(i in seq_along(url_name)){
    pg <- rvest::read_html(url_name[i])
    pg<-(rvest::html_attr(rvest::html_nodes(pg, "a"), "href"))
    files <- c(files,pg[grepl(string,pg,ignore.case = T)])
    files <- files %>% unique()
  }
  return(files)
}

#Read all csvs from urls; unz for zips
UnzipCSV <- function(files){
  #creates temp file to read in the data
  temp <- tempfile()
  download.file(files,temp)
  #This is needed because a zip file may have multiple files
  file_names <- unzip(temp,list=T)$Name
  data<- lapply(file_names,
                function(x){
                  da <- data.table::fread(unzip(temp,x))
                  #janitor to clean unruly names
                  names(da) <- names(da) %>% janitor::make_clean_names()  
                  return(da)
                })
  #unlink the temp file, important to do
  unlink(temp)
  data}