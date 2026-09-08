# Create function process_data_cdc_school_vax_view_dtap for processing cdc_school_vax_view.rds
# --------------------------------------------------------------------------
process_data_cdc_school_vax_view_dtap <- function() {
  
  # Install & load required libraries
  # --------------------------------------------------------------------------
  packages <- c("here","tidyverse")
  install.packages(setdiff(packages, rownames(installed.packages())))
  invisible(lapply(packages, library, character.only = TRUE))
  
  # Set file location relative to current project
  # --------------------------------------------------------------------------
  suppressMessages(here::i_am("R/process_data_cdc_school_vax_view_dtap.R"))
  print("----i. process_data_cdc_school_vax_view_dtap.R")
  
  # Create function process_data_cdc_school_vax_view_dtap by reading cdc_school_vax_view.rds in data-raw
  # --------------------------------------------------------------------------
  
  # Read cdc_child_vax_view.rds from the project `data-raw` folder
  read_path_rds <- here("data-raw/cdc_school_vax_view.rds")
  df <- readRDS(read_path_rds)
  
  # Filter the data for DTaP
  school_year <- '2025-26'
  df_processed <- df %>% 
                    filter(Vaccine.Exemption=='DTP, DTaP, or DT' & 
                           Geography.Type %in% c('States','National') &
                           School.Year==school_year &
                           !Geography %in% c('NY-City of New York',
                                             'TX-City of Houston',
                                             'U.S. Median')) %>%
                      select(Vaccine.Exemption, Geography, School.Year, Estimate....) %>%
                        rename(vaccine = Vaccine.Exemption,
                               state_name = Geography,
                               school_year = School.Year,
                               vaccine_coverage_estimate = Estimate....) %>%
                      mutate(vaccine_coverage_estimate = suppressWarnings(as.numeric(vaccine_coverage_estimate)))
  
  missing_states <- df_processed %>%
                      filter(is.na(vaccine_coverage_estimate)) %>%
                      pull(state_name)
  
  if (length(missing_states) > 0) {
    stop(sprintf("Missing DTaP coverage for School.Year %s in: %s",
                 school_year,
                 paste(missing_states, collapse = ", ")))
  }
  
  df_processed <- df_processed %>%
                    select(-school_year)
  
  # Write data as a rds called cdc_school_vax_view_dtap.rds to the project `data-raw` folder
  write_path_rds <- here("data-raw/cdc_school_vax_view_dtap.rds")
  saveRDS(df_processed, file = write_path_rds)
  
  # Message specifying where data was written
  # print(paste0("Saved state data to ",write_path_rds))
  
  # Write data as a csv called cdc_school_vax_view_dtap.csv to the project `data-raw` folder
  write_path_csv <- here("data-raw/csv/cdc_school_vax_view_dtap.csv")
  write.csv(df_processed, file = write_path_csv)
  
  # Message specifying where data was written
  # print(paste0("Saved state data to ",write_path_csv))
  
}