install.packages("googledrive")
library(googledrive)

library(httr)
drive_find(n = 1)
?drive_auth
drive_auth()

drive_find(pattern = "txt")
https://drive.google.com/file/d/1F8GjvtDmIerjg_Z1y9kNrEJAuX1Ruj4i/view?usp=drive_link

#get ID 
f <- drive_get("Emilia/WhatsApp Chat with Marti España.txt")
f_id <- f$id
#download file
tempfile()
drive_download(f_id)
