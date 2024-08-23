# Functions for reading and plotting GEMS data

process_gems <- function(df, bg_et_range, nit_sat_umol) {
  # get mean timestamps and widen data
  df_wide <- rga_wider(df)
  # determine backgrounds
  df_bg <- df_wide |> 
    filter(et > bg_et_range[1],
           et < bg_et_range[2])
  
  bg_29 <- df_bg |> 
    pull(mass_29) |> 
    mean()
  
  bg_30 <- df_bg |> 
    pull(mass_30) |> 
    mean()
  
  # add ratios and concentrations
  norm_rga(df_wide, bg_29, bg_30, nit_sat_umol)
  
}

#' Calculate rate based on slope of linear fit
#'
#' @param df a dataframe with 2 columns: elapsed time (sec) and the measurement of interest
#'
#' @return A rate in units of measurement per day
#' @export
calc_rate <- function(df){
  coef(lm(df[[2]] ~ df[[1]]))[2] * 3600 * 24
}

rga_wider <- function(df) {
  df %>% 
    mutate(cycle = cumsum(mass == 18)) %>% 
    group_by(cycle) %>% 
    mutate(cycle_ts = mean(timestamp)) %>% 
    ungroup() %>% 
    select(timestamp = cycle_ts, mass, pressure, experiment) %>% 
    pivot_wider(names_from = mass, names_prefix = "mass_",
                values_from = pressure)
}

norm_rga <- function(df, bg_29, bg_30, nit_sat_umol) {
  df %>% 
    mutate(et = timestamp - timestamp[1],
           mass_28_18 = mass_28 / mass_18,
           mass_29_18 = mass_29 / mass_18,
           mass_30_18 = mass_30 / mass_18,
           mass_28_40 = mass_28 / mass_40,
           mass_29_40 = mass_29 / mass_40,
           mass_30_40 = mass_30 / mass_40,
           mass_40_28 = (mass_40 / mass_28), # include mass 40 bg subtraction?
           mass_29_28 = ( mass_29 - bg_29 ) / mass_28,
           mass_30_28 = ( mass_30 - bg_30 ) / mass_28,
           # molar concentration based on nitrogen saturation
           umol_29 = mass_29_28 * nit_sat_umol,
           umol_30 = mass_30_28 * nit_sat_umol)
}

read_gems <- function(file) {
  raw_file <- read_lines(file)
  gems_raw <- raw_file[str_starts(raw_file, "(R:)*\\d+,")]
  gems_data <- str_remove(gems_raw, "^R:")
  read_csv(I(gems_data),
           col_names = c("hour", "min", "sec", "month", 
                         "day", "year", "mass", "current")) |> 
    mutate(mass = as.factor(mass),
           current = current*1E-16,
           pressure = current/0.0801,
           timestamp = lubridate::make_datetime(year, month, day, hour, min, sec, 
                                                tz = "America/New_York")) |> 
    select(timestamp, mass, current, pressure)
}

plot_gems <- function(data, log = TRUE) {
  stopifnot(is.logical(log))
  p <- data |> 
    ggplot(aes(timestamp, pressure, color = mass)) +
    geom_line() +
    ylab("Pressure (Torr)") +
    theme(axis.title.x = element_blank())
  
  if (log) {
    p +
      scale_y_log10()
  } else {
    p
  }
}

#' Read SRS RGASoft files
#' 
#' Old RGA software tabular format. Watch out for missing spaces causing NA's
#'
#' @param filename File to read.
#'
#' @return A dataframe of RGA data in long format.
#' @export
#'
read_rgasoft <- function(filename) {
  
  # get timestamp
  ts <- read_lines(filename, n_max = 1)[[1]] |> 
    lubridate::mdy_hms()
  #masses <- read_table(file)
  # rowlist <- read_lines(filename, skip = 30, n_max = 1) |> 
  #   str_split("\\s+") 
  # column_names <- rowlist[[1]]
  #read_lines(filename, skip = 32) |> 
  #  str_replace("(\\d)(-)", "$1 $2") |> 
  read_csv(filename,
           col_names = c("time_s", "mass_18", "mass_28", "mass_29", 
                         "mass_30", "mass_32", "mass_40", "mass_44", 
                         "mass_45", "mass_46", "X"), 
           col_types = cols(
             time_s = col_number(),
             mass_18 = col_double(),
             mass_28 = col_double(),
             mass_29 = col_double(),
             mass_30 = col_double(),
             mass_32 = col_double(),
             mass_40 = col_double(),
             mass_44 = col_double(),
             mass_45 = col_double(),
             mass_46 = col_double(),
             X = col_logical()
           ), skip = 32) |> 
    select(-X) |> 
    mutate(timestamp = ts + time_s) |> 
    select(!time_s) |> 
    pivot_longer(cols = starts_with("mass"),
                 names_to = "mass",
                 names_pattern = "mass_(.+)",
                 values_to = "pressure") |> 
    mutate(mass = as.factor(mass))
}

#' Read fixed width RGASoft files
#'
#' @param filename 
#'
#' @return
#' @export
#'
#' @examples
read_rgasoft_ascii <- function(filename) {
  
  # get timestamp
  ts <- read_lines(filename, n_max = 1)[[1]] |> 
    mdy_hms()
  #masses <- read_table(file)
  # rowlist <- read_lines(filename, skip = 30, n_max = 1) |> 
  #   str_split("\\s+") 
  # column_names <- rowlist[[1]]
  #read_lines(filename, skip = 32) |> 
  #  str_replace("(\\d)(-)", "$1 $2") |> 
  read_table(filename,
             col_names = c("time_s", "mass_18", "mass_28", "mass_29", "mass_30", "mass_32", "mass_40", "mass_44", "mass_45", "mass_46", "X"), 
             col_types = cols(
  time_s = col_number(),
  mass_18 = col_double(),
  mass_28 = col_double(),
  mass_29 = col_double(),
  mass_30 = col_double(),
  mass_32 = col_double(),
  mass_40 = col_double(),
  mass_44 = col_double(),
  mass_45 = col_double(),
  mass_46 = col_double(),
  X = col_logical()
), skip = 32) |> 
    select(-X) |> 
    mutate(timestamp = ts + time_s)
}
