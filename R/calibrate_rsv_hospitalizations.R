# Calibrate RSV hospitalizations using observed national level data 
# --------------------------------------------------------------------------
calibrate_rsv_hospitalizations <- function(df) {
  packages <- c("tidyverse","here")
  install.packages(setdiff(packages, rownames(installed.packages())))
  invisible(lapply(packages, library, character.only = TRUE))
  suppressMessages(here::i_am("R/calibrate_rsv_hospitalizations.R"))
  print("---a. calibrate_rsv_hospitalizations.R")
  
  hospitalizations_national_model <- df %>% 
    filter(state_name=='United States' & declining_coverage_among_new_births==0) %>% 
    group_by(time_horizon) %>%
    summarise(hospitalizations_national_model = sum(hospitalizations))
  df <- left_join(df, hospitalizations_national_model, by = c("time_horizon" = "time_horizon"))
  calibration_factor <- df$observed_national_hospitalizations / df$hospitalizations_national_model
  df$hospitalizations <- calibration_factor * df$hospitalizations
  df$hospitalizations_per_100k <- df$hospitalizations / df$age_group_population * 100000
  df <- df %>% select(-hospitalizations_national_model)
  return(df)
}
