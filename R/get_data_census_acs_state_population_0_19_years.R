# Create function get_data_census_acs_state_population_0_19_years for retrieving Census ACS 2019-2023 5 year population estimates for children age 0-19 years
# --------------------------------------------------------------------------
get_data_census_acs_state_population_0_19_years <- function() {
  
  # Install & load required libraries
  # --------------------------------------------------------------------------
  packages <- c("tidycensus","tidyverse","here")
  install.packages(setdiff(packages, rownames(installed.packages())))
  invisible(lapply(packages, library, character.only = TRUE))
  
  # Set file location relative to current project
  # --------------------------------------------------------------------------
  suppressMessages(here::i_am("R/get_data_census_acs_state_population_0_19_years.R"))
  print("---c. get_data_census_acs_state_population_0_19_years.R")
  
  # Create function get_data_census_acs_state_population_0_19_years by calling the census ACS API from tidycensus
  # --------------------------------------------------------------------------
  
  # Get state data from Census ACS
  df_state <- suppressMessages(
                get_acs(geography = "state", 
                            variables = c("B01001_003E","B01001_004E","B01001_005E","B01001_006E","B01001_007E", # Male population age 0-4y, 5-9y, 10-14y, 15-17y, 18-19y
                                          "B01001_027E","B01001_028E","B01001_029E","B01001_030E","B01001_031E"), # Female population age 0-4y, 5-9y, 10-14y, 15-17y, 18-19y
                            year = 2023, 
                            geometry = FALSE)
                ) %>% 
                group_by(GEOID, NAME) %>%
                summarise(.groups="keep", age_group_population = sum(estimate)) %>%
                mutate(age_group = '0-19 years',
                       age_group_length = 20) %>%
                rename(state_fips_code = GEOID,
                       state_name = NAME) %>%
                ungroup()
  
  # Get national data from Census ACS
  df_nation <- suppressMessages(
                get_acs(geography = "us", 
                            variables = c("B01001_003E","B01001_004E","B01001_005E","B01001_006E","B01001_007E", # Male population age 0-4y, 5-9y, 10-14y, 15-17y, 18-19y
                                          "B01001_027E","B01001_028E","B01001_029E","B01001_030E","B01001_031E"), # Female population age 0-4y, 5-9y, 10-14y, 15-17y, 18-19y
                            year = 2023, 
                            geometry = FALSE)
                ) %>% 
                group_by(GEOID, NAME) %>%
                summarise(.groups="keep", age_group_population = sum(estimate)) %>%
                mutate(age_group = '0-19 years',
                       age_group_length = 20) %>%
                rename(state_fips_code = GEOID,
                       state_name = NAME) %>%
                ungroup()
  
  # Union state and nation data
  df <- union(df_state,df_nation)
  
  # Write data as a rds called census_acs_state_population_0_19_years.rds to the project `data-raw` folder
  write_path_rds <- here("data-raw/census_acs_state_population_0_19_years.rds")
  saveRDS(df, file = write_path_rds)
  
  # Write data as a csv called census_acs_state_population_0_19_years.csv to the project `data-raw` folder
  write_path_csv <- here("data-raw/csv/census_acs_state_population_0_19_years.csv")
  write.csv(df, file = write_path_csv)
  
}
