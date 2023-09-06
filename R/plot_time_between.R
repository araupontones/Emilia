
plot_time_between <- function(data_){
  
  promedio <- mean(data_$tiempo_entre, na.rm = T)
  
  data_ %>%
    dplyr::filter(!is.na(tiempo_entre), tiempo_entre < 160) %>%
    ggplot(aes(x = duerme,
               y = tiempo_entre)) +
    geom_line(color = '#B870CB') +
    geom_point(color = '#B870CB') +
    geom_hline(yintercept = promedio,
               linetype = 'dotted',
               size = 1) +
    #scale axis -------------------------------------------------------------------
  scale_x_datetime(breaks = '10 days',
                   labels = function(x)format(x,"%d %b")) +
    labs(x = '',
         y = 'Minutos',
         title = 'Tiempo entre siestas Emilia',
         caption = "Data: Mensajes de WhatsApp con Martina") +
    theme_emilia() +
    theme(axis.text.x = element_text(angle = 90))
  
  
  
}