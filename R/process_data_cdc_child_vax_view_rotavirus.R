# Create function process_data_cdc_child_vax_view_rotavirus for processing rotavirus vaccine data from CDC Child Vax View
# --------------------------------------------------------------------------
process_data_cdc_child_vax_view_rotavirus <- function() {
  
  # Install & load required libraries
  # --------------------------------------------------------------------------
  packages <- c("here","tidyverse")
  install.packages(setdiff(packages, rownames(installed.packages())))
  invisible(lapply(packages, library, character.only = TRUE))
  
  # Set file location relative to current project
  # --------------------------------------------------------------------------
  suppressMessages(here::i_am("R/process_data_cdc_child_vax_view_rotavirus.R"))
  print("----i. process_data_cdc_child_vax_view_rotavirus.R")
  
  # Create function process_data_cdc_child_vax_view_rotavirus by reading cdc_child_vax_view.rds in data-raw
  # --------------------------------------------------------------------------
  
  # Read cdc_child_vax_view.rds from the project `data-raw` folder
  read_path_rds <- here("data-raw/cdc_child_vax_view.rds")
  df <- readRDS(read_path_rds)
  
  # Filter the data for rotavirus
  df_processed <- df %>% 
                    filter(Vaccine=='Rotavirus' & 
                           Birth.Year.Birth.Cohort=='2022' &
                           Dimension.Type=='Age' & 
                           Dimension=='8 Months' &
                           !Geography %in% c('Guam',
                                             'IL-City of Chicago',
                                             'IL-Rest of state',
                                             'NY-City of New York',
                                             'NY-Rest of state',
                                             'PA-Philadelphia',
                                             'PA-Rest of state',
                                             'TX-Bexar County',
                                             'TX-City of Houston',
                                             'TX-Dallas County',
                                             'TX-El Paso County',
                                             'TX-Hidalgo County',
                                             'TX-Rest of state',
                                             'TX-Tarrant County',
                                             'TX-Travis County',
                                             'U.S. Virgin Islands',
                                             'Region 1',
                                             'Region 2',
                                             'Region 3',
                                             'Region 4',
                                             'Region 5',
                                             'Region 6',
                                             'Region 7',
                                             'Region 8',
                                             'Region 9',
                                             'Region 10')) %>%
                      select(Vaccine, Geography, Birth.Year.Birth.Cohort, Estimate....) %>%
                        rename(vaccine = Vaccine,
                               state_name = Geography,
                               birth_year = Birth.Year.Birth.Cohort,
                               vaccine_coverage_estimate = Estimate....) %>%
                      select(-birth_year)
  
  # Write data as a rds called cdc_child_vax_view_rotavirus.rds to the project `data-raw` folder
  write_path_rds <- here("data-raw/cdc_child_vax_view_rotavirus.rds")
  saveRDS(df_processed, file = write_path_rds)
  
  # Message specifying where data was written
  # print(paste0("Saved state data to ",write_path_rds))
  
  # Write data as a csv called cdc_child_vax_view_rotavirus.csv to the project `data-raw` folder
  write_path_csv <- here("data-raw/csv/cdc_child_vax_view_rotavirus.csv")
  write.csv(df_processed, file = write_path_csv)
  
  # Message specifying where data was written
  # print(paste0("Saved state data to ",write_path_csv))
  
}