# Function to read all data collected and processed from get_and_process_data.R
# --------------------------------------------------------------------------
read_data <- function() {
  
  # Install & load required libraries
  # --------------------------------------------------------------------------
  packages <- c("here")
  install.packages(setdiff(packages, rownames(installed.packages())))
  invisible(lapply(packages, library, character.only = TRUE))
  
  # Set file location relative to current project
  # --------------------------------------------------------------------------
  suppressMessages(here::i_am("R/read_data.R"))
  print("-A. read_data.R")
  
  # Read data
  # --------------------------------------------------------------------------

  ## Census data
  
  # Read census_acs_states.rds from the project `data-raw` folder
  read_path_census_acs_states_rds <- here("data-raw/census_acs_states.rds")
  census_acs_states_df <- readRDS(read_path_census_acs_states_rds)
  
  # Read census_acs_state_population.rds from the project `data-raw` folder
  read_path_census_acs_state_population_rds <- here("data-raw/census_acs_state_population.rds")
  census_acs_state_population_df <- readRDS(read_path_census_acs_state_population_rds)
  
  # Read census_acs_state_population_0_4_years.rds from the project `data-raw` folder
  read_path_census_acs_state_population_0_4_years_rds <- here("data-raw/census_acs_state_population_0_4_years.rds")
  census_acs_state_population_0_4_years_df <- readRDS(read_path_census_acs_state_population_0_4_years_rds)
  
  # Read census_acs_state_population_0_14_years.rds from the project `data-raw` folder
  read_path_census_acs_state_population_0_14_years_rds <- here("data-raw/census_acs_state_population_0_14_years.rds")
  census_acs_state_population_0_14_years_df <- readRDS(read_path_census_acs_state_population_0_14_years_rds)
  
  ## CDC data
  
  # Read cdc_school_vax_view_dtap.rds from the project `data-raw` folder
  read_path_cdc_school_vax_view_dtap_rds <- here("data-raw/cdc_school_vax_view_dtap.rds")
  cdc_school_vax_view_dtap_df <- readRDS(read_path_cdc_school_vax_view_dtap_rds)
  
  # Read cdc_school_vax_view_varicella.rds from the project `data-raw` folder
  read_path_cdc_school_vax_view_varicella_rds <- here("data-raw/cdc_school_vax_view_varicella.rds")
  cdc_school_vax_view_varicella_df <- readRDS(read_path_cdc_school_vax_view_varicella_rds)
  
  # Read cdc_child_vax_view_rotavirus.rds from the project `data-raw` folder
  read_path_cdc_child_vax_view_rotavirus_rds <- here("data-raw/cdc_child_vax_view_rotavirus.rds")
  cdc_child_vax_view_rotavirus_df <- readRDS(read_path_cdc_child_vax_view_rotavirus_rds)
  
  # Read cdc_child_vax_view_pcv.rds from the project `data-raw` folder
  read_path_cdc_child_vax_view_pcv_rds <- here("data-raw/cdc_child_vax_view_pcv.rds")
  cdc_child_vax_view_pcv_df <- readRDS(read_path_cdc_child_vax_view_pcv_rds)
  
  # Read cdc_child_vax_view_varicella.rds from the project `data-raw` folder
  read_path_cdc_child_vax_view_varicella_rds <- here("data-raw/cdc_child_vax_view_varicella.rds")
  cdc_child_vax_view_varicella_df <- readRDS(read_path_cdc_child_vax_view_varicella_rds)
  
  ## Model input parameters
  
  # Read model_input_parameters.rds from the project `data-raw` folder
  read_path_model_input_parameters_rds <- here("data-raw/model_input_parameters.rds")
  model_input_parameters_df <- readRDS(read_path_model_input_parameters_rds)
  
  ## Tigris state geography
  
  # Read tigris_states.rds from the project `data-raw` folder
  read_path_tigris_states_rds <- here("data-raw/tigris_states.rds")
  tigris_states_df <- readRDS(read_path_tigris_states_rds)
  
  ## Create list of dataframes to return
  data_frames_list <- list(census_acs_states_df = census_acs_states_df,
                           census_acs_state_population_df=census_acs_state_population_df,
                           census_acs_state_population_0_4_years_df=census_acs_state_population_0_4_years_df,
                           census_acs_state_population_0_14_years_df=census_acs_state_population_0_14_years_df,
                           cdc_school_vax_view_dtap_df=cdc_school_vax_view_dtap_df,
                           cdc_child_vax_view_rotavirus_df=cdc_child_vax_view_rotavirus_df,
                           cdc_child_vax_view_pcv_df=cdc_child_vax_view_pcv_df,
                           cdc_school_vax_view_varicella_df=cdc_school_vax_view_varicella_df,
                           model_input_parameters_df=model_input_parameters_df,
                           tigris_states_df=tigris_states_df)
  
  ## Get dataframes from list and add then to the global env
  list2env(data_frames_list, envir=.GlobalEnv)

}