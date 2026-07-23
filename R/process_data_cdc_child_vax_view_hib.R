# Create function process_data_cdc_child_vax_view_hib for processing Hib vaccine data from CDC Child Vax View
# --------------------------------------------------------------------------
process_data_cdc_child_vax_view_hib <- function() {
  
  # Install & load required libraries
  # --------------------------------------------------------------------------
  packages <- c("here","tidyverse")
  install.packages(setdiff(packages, rownames(installed.packages())))
  invisible(lapply(packages, library, character.only = TRUE))
  
  # Set file location relative to current project
  # --------------------------------------------------------------------------
  suppressMessages(here::i_am("R/process_data_cdc_child_vax_view_hib.R"))
  print("----iv. process_data_cdc_child_vax_view_hib.R")
  
  # Read cdc_child_vax_view.rds from the project `data-raw` folder
  read_path_rds <- here("data-raw/cdc_child_vax_view.rds")
  df <- readRDS(read_path_rds)
  
  # Filter for Hib, PRIMARY SERIES (relevant protection for the infant at-risk window;
  # the booster/full series comes after peak Hib risk), coverage by 24 months, 2022 cohort.
  df_processed <- df %>% 
                    filter(Vaccine=='Hib' & 
                           Dose=='Primary Series' &
                           Dimension.Type=='Age' & 
                           Dimension=='24 Months' &
                           Birth.Year.Birth.Cohort=='2022' &
                           !Geography %in% c('Guam','IL-City of Chicago','IL-Rest of state',
                                             'NY-City of New York','NY-Rest of state',
                                             'PA-Philadelphia','PA-Rest of state',
                                             'TX-Bexar County','TX-City of Houston','TX-Dallas County',
                                             'TX-El Paso County','TX-Hidalgo County','TX-Rest of state',
                                             'TX-Tarrant County','TX-Travis County','U.S. Virgin Islands',
                                             'Region 1','Region 2','Region 3','Region 4','Region 5',
                                             'Region 6','Region 7','Region 8','Region 9','Region 10')) %>%
                      select(Vaccine, Geography, Birth.Year.Birth.Cohort, Estimate....) %>%
                        rename(vaccine = Vaccine,
                               state_name = Geography,
                               birth_year = Birth.Year.Birth.Cohort,
                               vaccine_coverage_estimate = Estimate....) %>%
                      select(-birth_year) %>%
                      mutate(vaccine = 'Hib')
  
  # Write rds + csv to data-raw
  saveRDS(df_processed, file = here("data-raw/cdc_child_vax_view_hib.rds"))
  write.csv(df_processed, file = here("data-raw/csv/cdc_child_vax_view_hib.csv"))
  
}
