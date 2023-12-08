library(shiny)
library(tidyverse)
library(dygraphs)
library(xts)

filename <- "C:/Users/whoim/Documents/RGAData/PressurevsTime/GEMS_test_NF0_SMURP_label_open_2023-12-08.txt"

read_GEMS <- function(filename) {
  raw_file <- read_lines(filename)
  data_file <- raw_file[str_starts(raw_file, "\\d+,")]
  df <- read_csv(
    I(data_file),
    col_names = c("hour", "min", "sec", "month", "day", "year", "mass", "current")
  ) |>
    mutate(
      #loop = cumsum
      hour = ifelse(str_sub(hour, 1) == "R", as.numeric(str_sub(hour, 3)), hour),
      mass = as.factor(mass),
      current = current * 1E-16,
      pressure = current / 0.0801,
      timestamp = lubridate::make_datetime(year, month, day, hour, min, sec, 
                                           tz = "America/New_York")
    ) |>
    select(timestamp, mass, pressure) |>
    pivot_wider(
      names_from = mass,
      names_prefix = "mass_",
      values_from = pressure
    )
  xts(df[, -1], order.by = df$timestamp)
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
  
  fileData <- reactiveFileReader(5000, session, filename, read_GEMS)
  
  output$gemsPlot <- renderDygraph({
    
    fileData() |>
      dygraph() |>
      dyOptions(logscale = TRUE) |>
      dyRoller(rollPeriod = 5) |>
      dyRangeSelector()
      
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
