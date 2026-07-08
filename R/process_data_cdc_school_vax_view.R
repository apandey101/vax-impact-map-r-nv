# Function that runs all functions for processing CDC school vax view data
# --------------------------------------------------------------------------
process_data_cdc_school_vax_view <- function() {
  
  # Install & load required libraries
  # --------------------------------------------------------------------------
  packages <- c("tidyverse","here")
  install.packages(setdiff(packages, rownames(installed.packages())))
  invisible(lapply(packages, library, character.only = TRUE))
  
  # Set file location relative to current project
  # --------------------------------------------------------------------------
  suppressMessages(here::i_am("R/process_data_cdc_school_vax_view.R"))
  print("---b. process_data_cdc_school_vax_view.R")
  
  # Source and run functions for processing cdc school vax view data
  # --------------------------------------------------------------------------
  
  # process_data_cdc_school_vax_view_dtap.R
  read_path_process_data_cdc_school_vax_view_dtap_r <- here("R/process_data_cdc_school_vax_view_dtap.R")
  source(read_path_process_data_cdc_school_vax_view_dtap_r)
  process_data_cdc_school_vax_view_dtap()
  
  # process_data_cdc_school_vax_view_varicella.R
  read_path_process_data_cdc_school_vax_view_varicella_r <- here("R/process_data_cdc_school_vax_view_varicella.R")
  source(read_path_process_data_cdc_school_vax_view_varicella_r)
  process_data_cdc_school_vax_view_varicella()

}