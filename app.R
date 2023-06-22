library(shiny)
library(shinycssloaders)
library(rio)
library(rwhatsapp)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(extrafont)
library(glue)
library(googledrive)

# options(
#   # whenever there is one account token found, use the cached token
#   gargle_oauth_email = 'araupontones@gmail.com',
#   # specify auth tokens should be stored in a hidden directory ".secrets"
#   gargle_oauth_cache = ".secrets"
# )

options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = "araupontones@gmail.com"
)

# scopes <- "https://www.googleapis.com/auth/drive"
# token <- gargle::credentials_user_oauth2(scopes, client = gargle_client())


ui <- fluidPage(
  tags$head(
    
    tags$link(rel = 'stylesheet', href = 'styles.css'),
    
    tags$link(rel="preconnect",href="https://fonts.googleapis.com"),
    tags$link( rel="preconnect",href="https://fonts.gstatic.com"),
    tags$link( href="https://fonts.googleapis.com/css2?family=Pathway+Extreme:ital,wght@0,100;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap",
               rel="stylesheet"),
    tags$link( href="https://fonts.googleapis.com/css2?family=Comfortaa:wght@300;400;500;600;700&display=swap", rel="stylesheet")
    
    
  ),
  
  fluidRow(class = 'purple refresh-bar',
  
  uiOutput('refresh_text', class = "inline-block vc-line-50"),
  actionButton('boton', "Refresh data", class = "btn-info"),
  
  ),
  br(),
  sidebarLayout(
    
    sidebarPanel(width = 2,
      tags$head(tags$style("#plot{height:85vh !important;}")),
      selectInput('chart_type',
                  'Select analysis',
                  choices = c('Nap track', 
                              "Total nap time",
                              "Average nap time",
                              "Time between naps")
      )
      
    ), #side panel
    
    mainPanel(
      
      withSpinner(plotOutput("plot"))
    )
  ) #sidebar layoyt
  
)

server <- function(input, output, session) {
  
  
  #update data
  observeEvent(input$boton,{
    
    
    
    source('update_data.R', encoding = 'utf-8')
    
    
  })
  
  observeEvent(input$boton, {
    
    withProgress(message = 'updating data', value = 0, {
      
      n <- 10
      for(i in 1:n){
        
        incProgress(1/n, detail = paste(i))
        
        Sys.sleep(0.5)
        
      }
      
      
    })
    
  })
  
  
  data_app <- reactive({
    
    
    rio::import('data/sleep.rds')
    
  })
  
  
  refresh_text <- reactive({
    
    date <- rio::import('data/last_refreshed.rds')
    
    format(date, "%d %B %Y at %H:%M")
    
  })
  
  
  output$refresh_text <- renderUI({
    
    text <- glue::glue('Last data update: {refresh_text()}')
    
  })
  
  
  
  #reactive data ==============================================================
  
  data_plot <- reactive({
    
    if(input$chart_type == 'Nap track'){
      
      data_plot <- data_app() %>%
        select(fecha, duerme, despierta) %>%
        pivot_longer(-c(fecha),
                     names_to = "indicador",
                     values_to = 'hora') %>%
        dplyr::filter(!is.na(hora)) %>%
        mutate(hora_plot = lubridate::ymd_hms(paste("2025-01-01", str_sub(hora, 11,19))))
      
      
    } else if(input$chart_type %in% c("Total nap time",
                                      "Average nap time"
                                      )) {
      
      data_plot <-  data_app() %>%
        mutate(duracion = as.numeric(duracion)) %>%
        group_by(fecha) %>%
        summarise(duracion_media = mean(duracion, na.rm = T),
                  duracion_total = sum(duracion, na.rm = T),
                  duracion_min = min(duracion, na.rm = T),
                  duracion_max = max(duracion, na.rm = T),
                  .groups = 'drop') %>%
        mutate(across(c(duracion_min, duracion_max),function(x)ifelse(x %in% c(Inf, -Inf), NA_real_, x)))
      
      
    }
    
    
    data_plot
    
    
  })
  
  
  #plot ========================================================================
  
  output$plot <- renderPlot({
    
    
    if(input$chart_type == "Nap track"){
      
      plot_track_naps(data_plot(), data_app())
      
    } else if( input$chart_type == "Total nap time"){
      
      plot_duracion_total(data_plot())
      
    } else if(input$chart_type == "Average nap time"){
      
      plot_average_nap_time(data_plot())
    } else if(input$chart_type == "Time between naps"){
      
      print(names(data_app()))
      plot_time_between(data_app())
      
    }
   
    
    
    
  })
  
  
  
}

shinyApp(ui, server)