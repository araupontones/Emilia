
#'@param new_var name given to the new variable created
#'@param my_omoji the emoji that identifies the event:
#'@return a table with the time and date when the event occured
get_horas <- function(.data,new_var,my_emoji){
  
  .data %>%
    #keep records that have selected emoji
    dplyr::filter(str_detect(emoji_name, my_emoji)) %>% 
    #keep only events that have a time in the message
    dplyr::filter(str_detect(text, "[0-9]")) %>%
    #   #remove redundancies
    dplyr:: filter(!str_detect(emoji_name, "watch")) %>%
    #   #create the new var and transform date into ymd
       mutate(
         fecha = lubridate::ymd(str_sub(time, 1, 10)),
         #get hora from text
         hora = lapply( str_extract_all(text, '[0-9]'), function(x){paste(x, collapse = '')}),
         
         
         hora = case_when(str_length(hora) == 2 ~ glue('{hora}:00:00'),
                          str_length(hora) == 4 ~ glue('{str_sub(hora,1,2)}:{str_sub(hora,3,4)}:00'),
                          str_length(hora) == 3 ~ glue('0{str_sub(hora,1,1)}:{str_sub(hora,2,3)}:00'),
                          T ~ paste(hora, "XXX")
         ),
         {{new_var}} := lubridate::ymd_hms(paste(fecha, hora))
     )%>%
     #fecha = lubridate::ymd(str_sub(time, 1, 10))) %>%
     #keep only variables of interest
     select({{new_var}}, fecha, text)  %>%
     arrange({{new_var}})
}


