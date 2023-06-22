
theme_emilia <- function(){
  theme_minimal() +
  theme(legend.position = 'bottom',
      text = element_text(family = "Comfortaa"),
      plot.title.position = 'plot',
      plot.title = element_text(size = 24, margin = margin(b = 15)),
      panel.background = element_rect(fill = 'white', color = NA),
      plot.background = element_rect(fill = 'white', colour = NA)
      
      )
  
}
