library(shiny)
library(tidyverse)

filename <- "C:/Users/brett/Desktop/GEMS_NF2_2023-09-18 17-25-19.txt"

read_GEMS <- function(filename) {
  raw_file <- read_lines(filename)
  data_file <- raw_file[str_starts(raw_file, "\\d+,")]
  read_csv(I(data_file),
           col_names = c("hour", "min", "sec", "month", "day", "year", "mass", "current")) |> 
  mutate(hour = ifelse(str_sub(hour, 1) == "R", as.numeric(str_sub(hour, 3)), hour),
         mass = as.factor(mass),
         current = current*1E-16,
         pressure = current/0.0801,
         timestamp = lubridate::make_datetime(year, month, day, hour, min, sec, tz = "America/New_York"))
}

# Define UI for application that plots gems data
ui <- fluidPage(
  
  # Application title
  titlePanel("Current GEMS data"),
  
  # Show a plot of the generated distribution
  plotOutput("gemsPlot", width = "100%"),
  
  # Set filename
  shinyFilesButton("filename", "Choose File", "Choose a file", multiple = FALSE)
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  fileData <- reactiveFileReader(1000, session, filename, read_GEMS)
  
  output$gemsPlot <- renderPlot({
    
    fileData() |> 
      ggplot(aes(timestamp, current, color = mass)) +
      geom_line() +
      scale_y_log10()
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
