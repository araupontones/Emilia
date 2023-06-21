

remove_old_files <- function(gtibble){
  #identify newest/latest file
  latest <- which(gtibble$time == max(gtibble$time) )
  
  #remove old files, every time that data is exported to drive, the old files arent
  # replaced.  Thus, lets remove them
  lapply(gtibble$id[-latest], function(x){
    
    googledrive::drive_rm(x)
    
  })
  
  #remove contacts file. In case they exist
  contacts <- drive_get('Emilia/2 contacts.vcf')
  
  lapply(contacts$id, function(x){
    googledrive::drive_rm(x)
  })
  
  
  
  

  }
