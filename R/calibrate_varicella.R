# Calibrate varicella model parameters using observed national level data 
# --------------------------------------------------------------------------

calibrate_varicella <- function(df) {
  
  # Install & load required libraries
  # --------------------------------------------------------------------------
  packages <- c("tidyverse","here")
  install.packages(setdiff(packages, rownames(installed.packages())))
  invisible(lapply(packages, library, character.only = TRUE))
  
  # Set file location relative to current project
  # --------------------------------------------------------------------------
  suppressMessages(here::i_am("R/calibrate_varicella.R"))
  print("--4. calibrate_varicella.R")
  
  ## Filter the model data for just varicella
  # --------------------------------------------------------------------------
  df <- df %>% filter(disease=='Varicella')
  
  ## Run varicella hospitalization calibration based on observed data (NIS, <20y)
  # --------------------------------------------------------------------------
  read_path_calibrate_varicella_hospitalizations_r <- here("R/calibrate_varicella_hospitalizations.R")
  source(read_path_calibrate_varicella_hospitalizations_r)
  df <- calibrate_varicella_hospitalizations(df)
  
  ## Run varicella case calibration (derived from calibrated hospitalizations)
  # --------------------------------------------------------------------------
  read_path_calibrate_varicella_cases_r <- here("R/calibrate_varicella_cases.R")
  source(read_path_calibrate_varicella_cases_r)
  df <- calibrate_varicella_cases(df)
  
  ## Run varicella death calibration (derived from calibrated cases)
  # --------------------------------------------------------------------------
  read_path_calibrate_varicella_deaths_r <- here("R/calibrate_varicella_deaths.R")
  source(read_path_calibrate_varicella_deaths_r)
  df <- calibrate_varicella_deaths(df)
  
  return(df)
  
}