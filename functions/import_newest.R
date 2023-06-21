#' Import the newest file  from Gdrive
#' @param gtibble tibble created by get_time()
#' @param temp_file temporary file to store the downloads from gdrive

import_newest <- function(gtibble, temp_file){
  # identify the newest file
  latest <- which(gtibble$time == max(gtibble$time) )
  
  #get the id of the newest file
  f_id <- gtibble$id[latest]
  
  #download file
  cli::cli_alert_info('Downloading file from Gdrive')
  googledrive::drive_download(f_id, path = my_txt)
  
  #read it
  cli::cli_alert_info('importing file')
  raw <- rwhatsapp::rwa_read(my_txt)
  
  return(raw)
  
}

