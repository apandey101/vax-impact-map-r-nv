# Function to run all project code
# --------------------------------------------------------------------------
main <- function() {
  
  ## Install & load required libraries
  # --------------------------------------------------------------------------
  packages <- c("tidyverse","here")
  install.packages(setdiff(packages, rownames(installed.packages())))
  invisible(lapply(packages, library, character.only = TRUE))
  
  ## Set file location relative to current project
  # --------------------------------------------------------------------------
  suppressMessages(here::i_am("R/main.R"))
  print("main.R")
  
  ## Source and run code
  # --------------------------------------------------------------------------
  
  ## Get and process data - only needs to be run when refreshing data
  read_path_get_and_process_data_r <- here("R/get_and_process_data.R")
  source(read_path_get_and_process_data_r)
  get_and_process_data()
  
  ## Run the model
  read_path_run_model_r <- here("R/run_model.R")
  source(read_path_run_model_r)
  run_model()
  
  ## Curate model output
  read_path_curate_model_output_r <- here("R/curate_model_output.R")
  source(read_path_curate_model_output_r)
  curate_model_output()
  
  ## Merge the age-structured producers (Hib, Varicella) into the single curated
  ## output file. Requires the ACS band populations in data-raw/csv (produced by
  ## get_data_census_acs_state_population_{hib,vzv}_bands(); see runbook).
  read_path_combine_model_output_r <- here("R/combine_model_output.R")
  source(read_path_combine_model_output_r)
  combine_model_output()
  
}

main()
