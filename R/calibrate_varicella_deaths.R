# Calibrate varicella deaths model parameters using observed national level data 
# --------------------------------------------------------------------------

calibrate_varicella_deaths <- function(df) {
  
  # Install & load required libraries
  # --------------------------------------------------------------------------
  packages <- c("tidyverse","here")
  install.packages(setdiff(packages, rownames(installed.packages())))
  invisible(lapply(packages, library, character.only = TRUE))
  
  # Set file location relative to current project
  # --------------------------------------------------------------------------
  suppressMessages(here::i_am("R/calibrate_varicella_deaths.R"))
  print("---c. calibrate_varicella_deaths.R")
  
  ## Run varicella deaths calibration based on observed data
  # --------------------------------------------------------------------------
  
  # Sum model deaths for the United States at baseline
  # --------------------------------------------------------------------------
  deaths_national_model <- df %>% 
    filter(state_name=='United States' & 
             declining_coverage_among_new_births==0) %>% 
    group_by(time_horizon) %>%
    summarise(deaths_national_model = sum(deaths))
  
  # Join the summed data back onto the dataframe
  # --------------------------------------------------------------------------
  df <- left_join(df, deaths_national_model, by = c("time_horizon" = "time_horizon"))
  
  # Determine calibration factor for modeled estimates based on observed national data
  # --------------------------------------------------------------------------
  calibration_factor <- df$observed_national_deaths / df$deaths_national_model
  
  # Apply calibration factor to modeled deaths
  # --------------------------------------------------------------------------
  df$deaths <- calibration_factor * df$deaths
  df$deaths_per_100k <- df$deaths / df$age_group_population * 100000
  
  df <- df %>% select(-deaths_national_model)
  
  return(df)
  
}
