# Functions for reading and plotting GEMS data

read_gems <- function(file) {
  raw_file <- read_lines(file)
  raw_file <- str_remove(raw_file, "^R:")
  data_file <- raw_file[str_starts(raw_file, "\\d+,")]
  read_csv(I(data_file),
           col_names = c("hour", "min", "sec", "month", "day", "year", "mass", "current")) |> 
  mutate(current = current*1E-16,
         pressure = current/0.0801,
         timestamp = lubridate::make_datetime(year, month, day, hour, min, sec, tz = "America/New_York"))
}

plot_gems <- function(data, log = TRUE) {
  stopifnot(is.logical(log))
  p <- data |> 
    ggplot(aes(timestamp, current, color = as.factor(mass))) +
    geom_line()
  
  if (log) {
    p +
      scale_y_log10()
  } else {
    p
  }
}