# Calibrate Hib cases using observed national level data 
# --------------------------------------------------------------------------
calibrate_hib_cases <- function(df) {
  packages <- c("tidyverse","here")
  install.packages(setdiff(packages, rownames(installed.packages())))
  invisible(lapply(packages, library, character.only = TRUE))
  suppressMessages(here::i_am("R/calibrate_hib_cases.R"))
  print("---a. calibrate_hib_cases.R")
  
  infections_national_model <- df %>% 
    filter(state_name=='United States' & declining_coverage_among_new_births==0) %>% 
    group_by(time_horizon) %>%
    summarise(infections_national_model = sum(infections))
  df <- left_join(df, infections_national_model, by = c("time_horizon" = "time_horizon"))
  calibration_factor <- df$observed_national_cases / df$infections_national_model
  df$cases <- calibration_factor * df$infections
  df$cases_per_100k <- df$cases / df$age_group_population * 100000
  df <- df %>% select(-infections_national_model)
  return(df)
}
