
gmdacr::load_functions('functions')
#'Downloads data from gdrive ==================================================

#authenticate ------------------------------------------------------------------
#drive_auth(email = 'araupontones@gmail.com')

#get raw data ------------------------------------------------------------------

#define tempfile
my_txt <- tempfile(fileext = '.txt')

#Download data ----------------------------------------------------------------

#get list of files with this pattern
f <- drive_get("Emilia/WhatsApp Chat with Marti España.txt")
#get the time of each file
#get_time(f) is created in functions 
f_time <- get_time(f)

#download and import the latest file from Gdrive
#import_newest() is creted in functions
raw <- import_newest(f_time, my_txt)

#remove old files -------------------------------------------------------------
#every time that data is exported to drive, the old files arent
# replaced.  Thus, lets remove them
remove_old_files(f_time)
#remove old files, every time that data is exported to drive, the old files arent



#get sleep and awake times =====================================================

cli::cli_alert_info("Limpiando los datos....")
sleep <- get_horas(raw, new_var = duerme,my_emoji = 'raised fist')



#awake data -----------------------------------------------------------
awake <- get_horas(raw,new_var = despierta, "eyes")
#add missing dates, sometimes we forget to record this
awake_all <- add_missing_dates(awake, c("2023-06-02 12:20:00",
                                        "2023-06-10 15:30:00"),
                               col_date = despierta)


#join data -------------------------------------------------------------
all <- join_sleep_awake(sleep, awake_all)

rio::export(all, 'data/sleep.rds')

last_refreshed <- Sys.time()
rio::export(last_refreshed, 'data/last_refreshed.rds')
