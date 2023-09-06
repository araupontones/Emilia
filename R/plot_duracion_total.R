
plot_duracion_total <- function(data_){
  
#get average
promedio <- mean(data_$duracion_total, na.rm = T)

data_ %>%
ggplot(aes(x = fecha,
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
  
  scale_x_date(breaks = '10 days',
               labels = function(x)format(x,"%d %b")) +
  labs(y = 'Duración (minutos)',
       x = '',
       title = "Duración TOTAL de siestas de Emilia",
       caption = "Data: Mensajes de WhatsApp con Martina") +
  theme_emilia()  +
  theme(axis.text.x = element_text(angle = 90))

}