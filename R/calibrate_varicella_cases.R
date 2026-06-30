# Calibrate varicella cases model parameters using observed national level data 
# --------------------------------------------------------------------------

calibrate_varicella_cases <- function(df) {
  
  # Install & load required libraries
  # --------------------------------------------------------------------------
  packages <- c("tidyverse","here")
  install.packages(setdiff(packages, rownames(installed.packages())))
  invisible(lapply(packages, library, character.only = TRUE))
  
  # Set file location relative to current project
  # --------------------------------------------------------------------------
  suppressMessages(here::i_am("R/calibrate_varicella_cases.R"))
  print("---a. calibrate_varicella_cases.R")
  
  ## Run varicella cases calibration based on observed data
  # --------------------------------------------------------------------------
  
  # Sum model infections for the United States at baseline
  # --------------------------------------------------------------------------
  infections_national_model <- df %>% 
    filter(state_name=='United States' & 
             declining_coverage_among_new_births==0) %>% 
    group_by(time_horizon) %>%
    summarise(infections_national_model = sum(infections))
  
  # Join the summed data back onto the dataframe
  # --------------------------------------------------------------------------
  df <- left_join(df, infections_national_model, by = c("time_horizon" = "time_horizon"))
  
  # Determine calibration factor for modeled estimates based on observed national data
  # --------------------------------------------------------------------------
  calibration_factor <- df$observed_national_cases / df$infections_national_model
  
  # Apply calibration factor to modeled cases
  # --------------------------------------------------------------------------
  df$cases <- calibration_factor * df$infections
  df$cases_per_100k <- df$cases / df$age_group_population * 100000
  
  df <- df %>% select(-infections_national_model)
  
  return(df)
  
}
