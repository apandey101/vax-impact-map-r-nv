# Function that runs all functions for getting data
# --------------------------------------------------------------------------
get_data <- function() {
  
  # Install & load required libraries
  # --------------------------------------------------------------------------
  packages <- c("tidycensus","tidyverse","here","tigris","sf")
  install.packages(setdiff(packages, rownames(installed.packages())))
  invisible(lapply(packages, library, character.only = TRUE))
  
  # Set file location relative to current project
  # --------------------------------------------------------------------------
  suppressMessages(here::i_am("R/get_data.R"))
  print("-A. get_data.R")
  
  # Source and run functions for getting data
  # --------------------------------------------------------------------------
  
  # Get CDC data
  read_path_get_data_cdc_r <- here("R/get_data_cdc.R")
  source(read_path_get_data_cdc_r)
  get_data_cdc()

  # Get CDC coverage time series data (baseline coverage over time)
  read_path_get_data_cdc_coverage_timeseries_r <- here("R/get_data_cdc_coverage_timeseries.R")
  source(read_path_get_data_cdc_coverage_timeseries_r)
  get_data_cdc_coverage_timeseries(refresh_raw = FALSE)  # reuse the data-raw pulls just saved by get_data_cdc()

  # Get CDC school exemption rate time series data (by state, over time)
  read_path_get_data_cdc_school_exemptions_timeseries_r <- here("R/get_data_cdc_school_exemptions_timeseries.R")
  source(read_path_get_data_cdc_school_exemptions_timeseries_r)
  get_data_cdc_school_exemptions_timeseries(refresh_raw = FALSE)  # reuse the data-raw pull just saved by get_data_cdc()

  # Get census data
  read_path_get_data_census_r <- here("R/get_data_census.R")
  source(read_path_get_data_census_r)
  get_data_census()
  
  # Get model input parameter data
  read_path_get_data_model_input_parameters_r <- here("R/get_data_model_input_parameters.R")
  source(read_path_get_data_model_input_parameters_r)
  get_data_model_input_parameters()
  
  # Get tigris state data
  read_path_get_data_tigris_states <- here("R/get_data_tigris_states.R")
  source(read_path_get_data_tigris_states)
  get_data_tigris_states()

}