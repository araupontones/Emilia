
join_sleep_awake <- function(data_sleep, data_awake){
  data_sleep %>%
  full_join(data_awake, by = c("fecha")) %>%
  #just to arrange data
  mutate(hora = ifelse(is.na(duerme), despierta, duerme)) %>%
  arrange(fecha, hora) %>%
  relocate(fecha) %>%
  #count nap time
  mutate(duracion = difftime(despierta, duerme, units = 'mins')) %>%
  #keep only feasible value
  dplyr::filter(duracion >0 | is.na(duracion)) %>%
  #identify the correct nap time
  group_by(duerme) %>%
  mutate(correcto = (duracion == min(duracion) | is.na(duracion))) %>%
  ungroup() %>%
  dplyr::filter(correcto) %>%
  select(-hora) %>%
  #esto es para el plot, creamos una fecha de manera artificial para poder
    #graficar las horas del dia
  
  mutate(hora_plot_duerme = lubridate::ymd_hms(paste("2025-01-01", str_sub(duerme, 11,19))),
         hora_plot_despierta = lubridate::ymd_hms(paste("2025-01-01", str_sub(despierta, 11,19)))
  ) %>%
    #calcular tiempo entre siestas
  group_by(fecha) %>%
  mutate(
    tiempo_entre = as.numeric(difftime(duerme, dplyr::lag(despierta,1), units = 'mins'))
    )
  
}


