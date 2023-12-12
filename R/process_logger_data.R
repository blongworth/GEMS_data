# Process GEMS Logger data and save to csv

library(shiny)
library(tidyverse)

filename <- "C:/Users/whoim/Documents/RGAData/PressurevsTime/GEMS_test_NF0_SMURP_label_open_2023-12-08.txt"

read_GEMS <- function(filename) {
  raw_file <- read_lines(filename)
  data_file <- raw_file[str_starts(raw_file, "\\d+,")]
  read_csv(I(data_file),
           col_names = c("hour", "min", "sec", "month", "day", "year", "mass", "current")) |> 
    mutate(hour = ifelse(str_sub(hour, 1) == "R", as.numeric(str_sub(hour, 3)), hour),
           mass = as.factor(mass),
           current = current*1E-16,
           pressure = current/0.0801,
           timestamp = lubridate::make_datetime(year, month, day, hour, min, sec)) %>% 
    select(timestamp, mass, current, pressure)
}

read_GEMS(filename) %>% 
  write_csv("data/GEMS_SMURP_label_open_2023-12-08.csv")
