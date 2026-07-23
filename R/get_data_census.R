# Function that runs all functions for getting census data
# --------------------------------------------------------------------------
get_data_census <- function() {
  
  # Install & load required libraries
  # --------------------------------------------------------------------------
  packages <- c("tidycensus","tidyverse","here")
  install.packages(setdiff(packages, rownames(installed.packages())))
  invisible(lapply(packages, library, character.only = TRUE))
  
  # Set file location relative to current project
  # --------------------------------------------------------------------------
  suppressMessages(here::i_am("R/get_data_census.R"))
  print("--2. get_data_census.R")
  
  # Source and run functions for getting census data
  # --------------------------------------------------------------------------
  
  # get_data_census_acs_states.R
  read_path_get_data_census_acs_states_r <- here("R/get_data_census_acs_states.R")
  source(read_path_get_data_census_acs_states_r)
  get_data_census_acs_states()
  
  # get_data_census_acs_state_population_0_4_years.R
  read_path_get_data_census_acs_state_population_0_4_years_r <- here("R/get_data_census_acs_state_population_0_4_years.R")
  source(read_path_get_data_census_acs_state_population_0_4_years_r)
  get_data_census_acs_state_population_0_4_years()
  
  # get_data_census_acs_state_population_0_14_years.R
  read_path_get_data_census_acs_state_population_0_14_years_r <- here("R/get_data_census_acs_state_population_0_14_years.R")
  source(read_path_get_data_census_acs_state_population_0_14_years_r)
  get_data_census_acs_state_population_0_14_years()
  
   # get_data_census_acs_state_population_0_19_years.R
  read_path_get_data_census_acs_state_population_0_19_years_r <- here("R/get_data_census_acs_state_population_0_19_years.R")
  source(read_path_get_data_census_acs_state_population_0_19_years_r)
  get_data_census_acs_state_population_0_19_years()

  # get_data_census_acs_state_population.R
  read_path_get_data_census_acs_state_population_r <- here("R/get_data_census_acs_state_population.R")
  source(read_path_get_data_census_acs_state_population_r)
  get_data_census_acs_state_population()

}