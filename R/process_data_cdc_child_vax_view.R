# Function that runs all functions for processing CDC child vax view data
# --------------------------------------------------------------------------
process_data_cdc_child_vax_view <- function() {
  
  # Install & load required libraries
  # --------------------------------------------------------------------------
  packages <- c("tidyverse","here")
  install.packages(setdiff(packages, rownames(installed.packages())))
  invisible(lapply(packages, library, character.only = TRUE))
  
  # Set file location relative to current project
  # --------------------------------------------------------------------------
  suppressMessages(here::i_am("R/process_data_cdc_child_vax_view.R"))
  print("---a. process_data_cdc_child_vax_view.R")
  
  # Source and run functions for processing data
  # --------------------------------------------------------------------------
  
  # process_data_cdc_child_vax_view_rotavirus.R
  read_path_process_data_cdc_child_vax_view_rotavirus_r <- here("R/process_data_cdc_child_vax_view_rotavirus.R")
  source(read_path_process_data_cdc_child_vax_view_rotavirus_r)
  process_data_cdc_child_vax_view_rotavirus()
  
  # process_data_cdc_child_vax_view_pcv.R
  read_path_process_data_cdc_child_vax_view_pcv_r <- here("R/process_data_cdc_child_vax_view_pcv.R")
  source(read_path_process_data_cdc_child_vax_view_pcv_r)
  process_data_cdc_child_vax_view_pcv()

}