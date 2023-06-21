#'gets tghe time of each record of the tibble retrieved by drive_get()
#'@param gtibble tibble returned by drive_get
#'@returns a tibble with all the files returned by drive_get() and an extra column
#'with the time of each file

get_time <- function(gtibble){
  
  #get time of files this ifo comes in a list form for each row
  time <- sapply(gtibble$drive_resource, function(x){
    
    x$modifiedByMeTime
    
  })
  
  #creates a column with the time of each file
  gtibble$time <- lubridate::ymd_hms(time)
  
  
  return(gtibble)
  
}
