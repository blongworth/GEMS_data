library(shiny)
library(tidyverse)
library(dygraphs)
source("../R/gems_functions.R")

filename <- "../data/GEMS_tests/SerialCapture/GEMS_2024-08-19_chamber_sed_closed.txt"

get_data <- function(filename) {
  read_gems(filename) %>% 
    mutate(experiment = "test") %>% 
    rga_wider()
}

# Define UI for application that plots gems data
ui <- fluidPage(
  
  # Application title
  titlePanel("Current GEMS data"),
  
  # Show a plot of the generated distribution
  dygraphOutput("gemsPlot", width = "100%"),
  
  # Set filename
  #shinyFilesButton("filename", "Choose File", "Choose a file", multiple = FALSE)
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  fileData <- reactiveFileReader(5000, session, filename, get_data)
  
  output$gemsPlot <- renderDygraph({
    
    fileData() |> 
      #select(timestamp, mass_28, mass_29, mass_30, mass_40) %>% 
      select(-experiment) %>% 
      dygraph() %>% 
      dyOptions(logscale = TRUE)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
