# Compute incidence at endemic equilibrium by scenario for the diseases of interest
# --------------------------------------------------------------------------

run_model <- function() {
  
  # Install & load required libraries
  # --------------------------------------------------------------------------
  packages <- c("tidyverse","here")
  install.packages(setdiff(packages, rownames(installed.packages())))
  invisible(lapply(packages, library, character.only = TRUE))
  
  # Set file location relative to current project
  # --------------------------------------------------------------------------
  suppressMessages(here::i_am("R/run_model.R"))
  print("II. run_model.R")
  
  ## Read the model input data
  # --------------------------------------------------------------------------
  read_path_read_data_r <- here("R/read_data.R")
  source(read_path_read_data_r)
  read_data()
  
  ## Compile the model input data
  # --------------------------------------------------------------------------
  read_path_compile_model_input_data_r <- here("R/compile_model_input_data.R")
  source(read_path_compile_model_input_data_r)
  df_model_input_data <- compile_model_input_data()
  
  ## Calculate structural vaccine coverage (and effective structural vaccine coverage)
  # --------------------------------------------------------------------------
  read_path_calculate_structural_vaccine_coverage_r <- here("R/calculate_structural_vaccine_coverage.R")
  source(read_path_calculate_structural_vaccine_coverage_r)
  df_model_data <- calculate_structural_vaccine_coverage(df_model_input_data)
  
  ## Compute incidence at endemic equilibrium
  # --------------------------------------------------------------------------
  read_path_compute_ee_incidence_r <- here("R/compute_ee_incidence.R")
  source(read_path_compute_ee_incidence_r)
  df_model_data <- compute_ee_incidence(df_model_data)
  
  ## Calculate infections from incidence
  # --------------------------------------------------------------------------
  read_path_calculate_infections_r <- here("R/calculate_infections.R")
  source(read_path_calculate_infections_r)
  df_model_data <- calculate_infections(df_model_data)
  
  ## Calculate disease burden
  # --------------------------------------------------------------------------
  read_path_calculate_disease_burden_r <- here("R/calculate_disease_burden.R")
  source(read_path_calculate_disease_burden_r)
  df_model_data <- calculate_disease_burden(df_model_data)
  
  ## Calibrate model using observed national data
  # --------------------------------------------------------------------------
  read_path_calibrate_r <- here("R/calibrate.R")
  source(read_path_calibrate_r)
  df_model_data <- calibrate(df_model_data)
  
  ## Calculate economic impact
  # --------------------------------------------------------------------------
  read_path_calculate_economic_impact_r <- here("R/calculate_economic_impact.R")
  source(read_path_calculate_economic_impact_r)
  df_model_data <- calculate_economic_impact(df_model_data)
  
  ## Calculate additional disease burden from declining vaccine coverage relative to current baseline vaccination coverage
  # --------------------------------------------------------------------------
  read_path_calculate_additional_disease_burden_r <- here("R/calculate_additional_disease_burden.R")
  source(read_path_calculate_additional_disease_burden_r)
  df_model_data <- calculate_additional_disease_burden(df_model_data)
  
  ## Save the results
  # --------------------------------------------------------------------------
  # Write data as a rds called vax_impact_map_model_output.rds to the project `data` folder
  write_path_rds <- here("data/vax_impact_map_model_output.rds")
  saveRDS(df_model_data, file = write_path_rds)
  
  # Message specifying where data was written
  # print(paste0("Saved model output to ",write_path_rds))
  
  # Write data as a csv called vax_impact_map_model_output.csv to the project `data` folder
  write_path_csv <- here("data/csv/vax_impact_map_model_output.csv")
  write.csv(df_model_data, file = write_path_csv)
  
  # Message specifying where data was written
  # print(paste0("Saved model output to ",write_path_csv))
  
}
