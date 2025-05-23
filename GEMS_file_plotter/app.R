library(shiny)
library(tidyverse)
library(dygraphs)
library(shinyFiles)
source("../R/gems_functions.R")

get_data <- function(filename) {
  read_gems_isots(filename) %>%
    mutate(experiment = "test") %>% 
    rga_wider() %>% 
    select(-experiment)
}

ui <- fluidPage(
  titlePanel("Current GEMS data"),
  shinyFilesButton('file', label='File select', title='Choose a RGA file', multiple=FALSE),
  dygraphOutput("gemsPlot", width = "100%"),
)

server <- function(input, output, session) {
  
  roots = getVolumes()
  
  file_data <- reactive({
    req(input$file)
    shinyFileChoose(input, 'file', root=roots, filetypes=c('', 'txt'))
    parseFilePaths(root=roots, input$file)
  })
  
  file_reader <- reactive({
    req(file_data()$datapath)
    reactiveFileReader(5000, session, file_data()$datapath[[1]], get_data)
  })
  
  output$gemsPlot <- renderDygraph({
    req(file_data()$datapath)
    file_reader()() %>% 
      dygraph() %>% 
      dyOptions(logscale = TRUE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
