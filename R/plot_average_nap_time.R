
plot_average_nap_time <- function(data_){
  ggplot(data_,
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
  scale_x_date(breaks = '10 day',
               labels = function(x)format(x,"%d %b")) +
    scale_y_continuous(breaks = seq(0,150,10)) +
    
    #labels & theme --------------------------------------------------------------
  labs(y = 'Duración (minutos)',
       x = '',
       title = "Duración promedio de siestas de Emilia",
       caption = "Data: Mensajes de WhatsApp con Martina") +
    theme_emilia() +
    theme(axis.text.x = element_text(angle = 90))
  
}

