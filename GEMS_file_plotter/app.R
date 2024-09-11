library(shiny)
library(tidyverse)
library(dygraphs)
source("../R/gems_functions.R")

get_data <- function(filename) {
  read_gems(filename) %>% 
    mutate(experiment = "test") %>% 
    rga_wider() %>% 
    select(-experiment)
}

ui <- fluidPage(
  titlePanel("Current GEMS data"),
  fileInput("file", "Choose a RGA file"),
  dygraphOutput("gemsPlot", width = "100%"),
)

server <- function(input, output, session) {
  
  file_data <- reactive({
    req(input$file)
    input$file
  })
  
  file_reader <- reactive({
    req(file_data())
    reactiveFileReader(5000, session, file_data()$datapath, get_data)
  })
  
  output$gemsPlot <- renderDygraph({
    file_reader()() %>% 
      dygraph() %>% 
      dyOptions(logscale = TRUE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
