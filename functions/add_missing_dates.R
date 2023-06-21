#' add awake dates that were missing
#' @description  sometimes, we forget to add the time when she wakes up
#' this function allows us to fix that problem
#' @param add_to data base to add the missing date (awake, or sleep)
#' @param dates dates missing (Y-m-d H:M:s)
#' @param col_date name of the time column (despierta or duerme)

add_missing_dates <- function(add_to, dates, col_date = despierta){
  
  #get fecha of the event
  fecha = str_sub(dates, 1,10)
  

  add_dates <- tibble(

    {{col_date}} := lubridate::ymd_hms(dates),

    fecha = fecha,
    
    text = "added"

  )

  # 
  # 
  all <-  rbind(add_to, add_dates) %>%
    arrange({{col_date}})
  # 
  
  
}
