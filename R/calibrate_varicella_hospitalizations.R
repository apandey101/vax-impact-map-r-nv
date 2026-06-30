# Calibrate varicella hospitalization model parameters using observed national level data 
# --------------------------------------------------------------------------

calibrate_varicella_hospitalizations <- function(df) {
  
  # Install & load required libraries
  # --------------------------------------------------------------------------
  packages <- c("tidyverse","here")
  install.packages(setdiff(packages, rownames(installed.packages())))
  invisible(lapply(packages, library, character.only = TRUE))
  
  # Set file location relative to current project
  # --------------------------------------------------------------------------
  suppressMessages(here::i_am("R/calibrate_varicella_hospitalizations.R"))
  print("---b. calibrate_varicella_hospitalizations.R")
  
  ## Run varicella hospitalization calibration based on observed data
  # --------------------------------------------------------------------------
  
  # Sum model hospitalizations for the United States at baseline
  # --------------------------------------------------------------------------
  hospitalizations_national_model <- df %>% 
    filter(state_name=='United States' & 
             declining_coverage_among_new_births==0) %>% 
    group_by(time_horizon) %>%
    summarise(hospitalizations_national_model = sum(hospitalizations))
  
  # Join the summed data back onto the dataframe
  # --------------------------------------------------------------------------
  df <- left_join(df, hospitalizations_national_model, by = c("time_horizon" = "time_horizon"))
  
  # Determine calibration factor for modeled estimates based on observed national data
  # --------------------------------------------------------------------------
  calibration_factor <- df$observed_national_hospitalizations / df$hospitalizations_national_model
  
  # Apply calibration factor to modeled hospitalizations
  # --------------------------------------------------------------------------
  df$hospitalizations <- calibration_factor * df$hospitalizations
  df$hospitalizations_per_100k <- df$hospitalizations / df$age_group_population * 100000
  
  df <- df %>% select(-hospitalizations_national_model)
  
  return(df)
  
}
