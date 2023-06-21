
#font_import(paths = "C:/Users/andre/AppData/Local/Microsoft/Windows/Fonts")
#loadfonts(device = "win")
#get raw data ---------------------------------------------------------------------
drive_auth(email = 'araupontones@gmail.com')
#define tempfile
my_txt <- tempfile(fileext = '.txt')

#get files
f <- drive_get("Emilia/WhatsApp Chat with Marti España.txt")


#get time of files this ifo comes in a list form for each row
time <- sapply(f$drive_resource, function(x){
  
  x$modifiedByMeTime
  
})


f$time <- lubridate::ymd_hms(time)

#get latest file
latest <- which(f$time == max(f$time) )
f_id <- f$id[latest]
drive_download(f_id, path = my_txt)
raw <- rwa_read(my_txt)

#remove old files, every time that data is exported to drive, the old files arent
# replaced.  Thus, lets remove them
lapply(f$id[-latest], function(x){
  
  drive_rm(x)
  
})

#remove contacts file. In case they exist
contacts <- drive_get('Emilia/2 contacts.vcf')
lapply(contacts$id, function(x){
  drive_rm(x)
})




# to get the name of the person who sent the message ---------------------------
#I am doing this because some times the two send a message at the exact same time
names <-raw %>%
  filter(str_detect(text, "✊"))

#Get the horas when sleeping and waking up =====================================

#'@param new_var name given to the new variable created
#'@param my_omoji the emoji that identifies the event:
#'✊: sleeps
#'👀: wakes up
#'@return a table with the time and date when the event occured
get_horas <- function(.data,new_var,my_emoji){
  
  .data %>%
    filter(str_detect(text, my_emoji))%>% 
    #keep only events that have a time in the message
    filter(str_detect(text, "[0-9]")) %>%
    #remove redundancies
    filter(!str_detect(text, "⌚")) %>%
    #create the new var and transform date into ymd
    mutate(
           fecha = lubridate::ymd(str_sub(time, 1, 10)),
          #get hora from text
          hora = lapply( str_extract_all(text, '[0-9]'), function(x){paste(x, collapse = '')}),
        
          
          hora = case_when(str_length(hora) == 4 ~ glue('{str_sub(hora,1,2)}:{str_sub(hora,3,4)}:00'),
                           str_length(hora) == 3 ~ glue('0{str_sub(hora,1,1)}:{str_sub(hora,2,3)}:00')
                           ),
          {{new_var}} := lubridate::ymd_hms(paste(fecha, hora))
    )%>%
           #fecha = lubridate::ymd(str_sub(time, 1, 10))) %>%
    #keep only variables of interest
    select({{new_var}}, fecha)  %>%
    arrange({{new_var}})
    }
  


#mutate(fecha = lubridate::ymd(fecha))

#get sleep and awake times ----------------------------------------------------

sleep <- get_horas(raw,new_var = duerme,"✊")


#awake data -----------------------------------------------------------
awake <- get_horas(raw,new_var = despierta, "👀")

add_awake <- tibble(
  
  despierta = lubridate::ymd_hms(c("2023-06-02 12:20:00",
                                   "2023-06-10 15:30:00")),
  
  fecha = c("2023-06-02",
            "2023-06-10")
  
)


awake_all <- rbind(awake, add_awake) %>% 
  arrange(despierta)





#Join sleep and awake ==========================================================
all <- sleep %>%
  full_join(awake_all, by = c("fecha")) %>%
  #just to arrange data
  mutate(hora = ifelse(is.na(duerme), despierta, duerme)) %>%
  arrange(fecha, hora) %>%
  relocate(fecha) %>%
  #count nap time
  mutate(duracion = difftime(despierta, duerme, units = 'mins')) %>%
  #keep only feasible value
  filter(duracion >0 | is.na(duracion)) %>%
  #identify the correct nap time
  group_by(duerme) %>%
  mutate(correcto = (duracion == min(duracion) | is.na(duracion))) %>%
  ungroup() %>%
  filter(correcto) %>%
  select(-hora) %>%
  #get the author of the nao
  left_join(select(names, time, author),
            by = c('duerme' = 'time')) %>%
  #estp es para el plot
  mutate(hora_plot_duerme = lubridate::ymd_hms(paste("2025-01-01", str_sub(duerme, 11,19))),
         hora_plot_despierta = lubridate::ymd_hms(paste("2025-01-01", str_sub(despierta, 11,19)))
         ) %>%
  group_by(fecha) %>%
  mutate(tiempo_entre = as.numeric(difftime(duerme,lag(despierta,1), units = 'mins' )))
  


rio::export(all, 'sleep.rds')


   

#Seguimiento de siestas ========================================================

data_plot <- all %>%
  select(fecha, duerme, despierta) %>%
  pivot_longer(-c(fecha),
               names_to = "indicador",
               values_to = 'hora') %>%
  dplyr::filter(!is.na(hora)) %>%
  mutate(hora_plot = lubridate::ymd_hms(paste("2025-01-01", str_sub(hora, 11,19))))





ggplot(data_plot,
       aes(y = fecha,
           x = hora_plot,
           color = indicador
           ))+
 
  geom_segment(data= all %>% mutate(indicador = "despierta"),
               aes(x = hora_plot_duerme,
                   xend = hora_plot_despierta,
                   y = fecha,
                   yend = fecha),
               show.legend = F
               ) +
  #points
  geom_point(size = 3) +
  lapply(c('09','12','15'), function(x){
    
    time <- glue::glue('2025-01-01 {x}:00:00')
    geom_vline(xintercept = lubridate::ymd_hms(time),
               linetype = 'dotted')
  }) +
  scale_color_manual(breaks = c("duerme", "despierta"),
                     values = c('#D34D71', '#4000A0'),
                     name ="") +
  scale_y_date(breaks = '1 days',
               labels = function(x)format(x, "%d %b")) +
  scale_x_datetime(breaks = '1 hours',
                   labels = function(x)format(x, "%H:%M")) +
  labs(x = '',
       y = '',
      title = "Seguimiento de siestas de Emilia",
      caption = "Data: Mensajes de WhatsApp con Martina") +
   theme_emilia()
  



ggsave('plots/track_naps.png')

#Duracion de las siestas =======================================================
data_duracion <- all %>%
  mutate(duracion = as.numeric(duracion)) %>%
  group_by(fecha) %>%
  summarise(duracion_media = mean(duracion, na.rm = T),
            duracion_total = sum(duracion, na.rm = T),
            duracion_min = min(duracion, na.rm = T),
            duracion_max = max(duracion, na.rm = T),
            .groups = 'drop') %>%
  mutate(across(c(duracion_min, duracion_max),function(x)ifelse(x %in% c(Inf, -Inf), NA_real_, x)))

ggplot(data_duracion,
       aes(x = fecha,
           y = duracion_media)
       ) +
  #Columns ---------------------------------------------------------------------
  geom_col(width = .8,
           fill= '#B870CB') +
  #points ----------------------------------------------------------------------
  geom_point(aes(y = duracion_min,
                 color = 'Min'),
             size = 2) +
  geom_point(aes(y = duracion_max,
                 color = "Max"),
             size = 2) +
  
  scale_color_manual(
    breaks = c("Min", "Max"),
    values = c( '#07A6ED', '#F14D40'),
    name = ""
  ) +
  
  #hlines ---------------------------------------------------------------------
  lapply(c(40, 80, 120), function(x){
    
    geom_hline(yintercept = x,
               linetype = 'dotted')
  }) +
 
 #scale axis -------------------------------------------------------------------
  scale_x_date(breaks = '1 day',
               labels = function(x)format(x,"%d %b")) +
  scale_y_continuous(breaks = seq(0,150,10)) +
  
  #labels & theme --------------------------------------------------------------
  labs(y = 'Duración (minutos)',
       x = '',
       title = "Duración promedio de siestas de Emilia",
       caption = "Data: Mensajes de WhatsApp con Martina") +
theme_emilia() +
  theme(axis.text.x = element_text(angle = 90))

ggsave('plots/average_nap_time.png')






#Numero de siestas ============================================================

promedio <- mean(data_duracion$duracion_total, na.rm = T)
ggplot(data_duracion,
       aes(x = fecha,
           y = duracion_total)
) +
  geom_col(width = .8,
           fill= '#B870CB') + #labels & theme --------------------------------------------------------------

#hlines ---------------------------------------------------------------------
lapply(promedio, function(x){
  
  geom_hline(yintercept = x,
             linetype = 'dotted',
             size= 1)
}) +
  
  scale_x_date(breaks = '1 day',
               labels = function(x)format(x,"%d %b")) +
labs(y = 'Duración (minutos)',
     x = '',
     title = "Duración TOTAL de siestas de Emilia",
     caption = "Data: Mensajes de WhatsApp con Martina") +
  theme_emilia()  +
  theme(axis.text.x = element_text(angle = 90))

ggsave('plots/total_nap_time.png')


#duracion entre siestas ======================================================

promedio <- mean(all$tiempo_entre, na.rm = T)

all %>%
  filter(!is.na(tiempo_entre), tiempo_entre < 160) %>%
  ggplot(aes(x = duerme,
             y = tiempo_entre)) +
  geom_line(color = '#B870CB') +
  geom_point(color = '#B870CB') +
  geom_hline(yintercept = promedio,
             linetype = 'dotted',
             size = 1) +
  #scale axis -------------------------------------------------------------------
scale_x_datetime(breaks = '1 day',
             labels = function(x)format(x,"%d %b")) +
  labs(x = '',
       y = 'Minutos',
       title = 'Tiempo entre siestas Emilia',
       caption = "Data: Mensajes de WhatsApp con Martina") +
  theme_emilia() +
  theme(axis.text.x = element_text(angle = 90))

ggsave('plots/in_between_time.png')

