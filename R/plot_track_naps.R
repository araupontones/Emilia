#'plot track naps

plot_track_naps <- function(.data, data_all){
  
  .data %>%
  ggplot(aes(y = fecha,
             x = hora_plot,
             color = indicador
         )) +
    
    geom_segment(data= data_all %>% mutate(indicador = "despierta"),
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
  
}


