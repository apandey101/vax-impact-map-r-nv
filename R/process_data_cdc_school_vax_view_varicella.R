# Create function process_data_cdc_school_vax_view_varicella for processing cdc_school_vax_view.rds
# --------------------------------------------------------------------------
process_data_cdc_school_vax_view_varicella <- function() {
  
  # Install & load required libraries
  # --------------------------------------------------------------------------
  packages <- c("here","tidyverse")
  install.packages(setdiff(packages, rownames(installed.packages())))
  invisible(lapply(packages, library, character.only = TRUE))
  
  # Set file location relative to current project
  # --------------------------------------------------------------------------
  suppressMessages(here::i_am("R/process_data_cdc_school_vax_view_varicella.R"))
  print("----ii. process_data_cdc_school_vax_view_varicella.R")
  
  # Create function process_data_cdc_school_vax_view_varicella by reading cdc_school_vax_view.rds in data-raw
  # --------------------------------------------------------------------------
  
  # Read cdc_school_vax_view.rds from the project `data-raw` folder
  read_path_rds <- here("data-raw/cdc_school_vax_view.rds")
  df <- readRDS(read_path_rds)
  
  # Filter the data for varicella, up-to-date (UTD) dose among kindergartners.
  # UTD is used (rather than 1 dose or 2 dose) because the required number of
  # doses varies by state; UTD reflects each state's own requirement.
  df_processed <- df %>% 
                    filter(Vaccine.Exemption=='Varicella' & 
                           Dose=='UTD (unknown disease history)' &
                           Geography.Type %in% c('States','National') &
                           School.Year=='2023-24' &
                           !Geography %in% c('NY-City of New York',
                                             'TX-City of Houston',
                                             'U.S. Median')) %>%
                      select(Vaccine.Exemption, Geography, School.Year, Estimate....) %>%
                        rename(vaccine = Vaccine.Exemption,
                               state_name = Geography,
                               school_year = School.Year,
                               vaccine_coverage_estimate = Estimate....) %>%
                      select(-school_year)
  
  # Write data as a rds called cdc_school_vax_view_varicella.rds to the project `data-raw` folder
  write_path_rds <- here("data-raw/cdc_school_vax_view_varicella.rds")
  saveRDS(df_processed, file = write_path_rds)
  
  # Message specifying where data was written
  # print(paste0("Saved state data to ",write_path_rds))
  
  # Write data as a csv called cdc_school_vax_view_varicella.csv to the project `data-raw` folder
  write_path_csv <- here("data-raw/csv/cdc_school_vax_view_varicella.csv")
  write.csv(df_processed, file = write_path_csv)
  
  # Message specifying where data was written
  # print(paste0("Saved state data to ",write_path_csv))
  
}
