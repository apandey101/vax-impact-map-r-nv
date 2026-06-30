# Calibrate model using observed national level data 
# --------------------------------------------------------------------------

calibrate <- function(df) {
  
  # Install & load required libraries
  # --------------------------------------------------------------------------
  packages <- c("tidyverse","here")
  install.packages(setdiff(packages, rownames(installed.packages())))
  invisible(lapply(packages, library, character.only = TRUE))
  
  # Set file location relative to current project
  # --------------------------------------------------------------------------
  suppressMessages(here::i_am("R/calibrate.R"))
  print("-F. calibrate.R")
  
  ## Run rotavirus calibration based on observed data
  # --------------------------------------------------------------------------
  read_path_calibrate_rota_r <- here("R/calibrate_rota.R")
  source(read_path_calibrate_rota_r)
  df_rota_calibrated <- calibrate_rota(df)
  
  ## Run pertussis calibration based on observed data
  # --------------------------------------------------------------------------
  read_path_calibrate_pertussis_r <- here("R/calibrate_pertussis.R")
  source(read_path_calibrate_pertussis_r)
  df_pertussis_calibrated <- calibrate_pertussis(df)
  
  ## Run pneumococcal calibration based on observed data
  # --------------------------------------------------------------------------
  read_path_calibrate_pneumo_r <- here("R/calibrate_pneumo.R")
  source(read_path_calibrate_pneumo_r)
  df_pneumo_calibrated <- calibrate_pneumo(df)
  
  ## Run varicella calibration based on observed data
  # --------------------------------------------------------------------------
  read_path_calibrate_varicella_r <- here("R/calibrate_varicella.R")
  source(read_path_calibrate_varicella_r)
  df_varicella_calibrated <- calibrate_varicella(df)
  
  # Union the calibrated results for each disease
  df_calibrated <- union(df_rota_calibrated, 
                         df_pertussis_calibrated) %>%
                   union(df_pneumo_calibrated) %>%
                   union(df_varicella_calibrated)
  
  return(df_calibrated)
  
}