# Function to compile all data read from read_data.R and collected and processed from get_and_process_data.R
# --------------------------------------------------------------------------

compile_model_input_data <- function() {
  
  # Install & load required libraries
  # --------------------------------------------------------------------------
  packages <- c("tidyverse","here")
  install.packages(setdiff(packages, rownames(installed.packages())))
  invisible(lapply(packages, library, character.only = TRUE))
  
  # Set file location relative to current project
  # --------------------------------------------------------------------------
  suppressMessages(here::i_am("R/compile_model_input_data.R"))
  print("-B. compile_model_input_data.R")
  
  # Compile model input data data
  # --------------------------------------------------------------------------
  
  # Create rotavirus data table
  df_census <- left_join(census_acs_states_df, census_acs_state_population_df %>% select(-state_name), by = c("state_fips_code" = "state_fips_code"))
  df_census_0_4 <- left_join(df_census, census_acs_state_population_0_4_years_df %>% select(-state_name), by = c("state_fips_code" = "state_fips_code")) # join on state population 0-4 years from census
  df_census_0_4_rota <- left_join(df_census_0_4, cdc_child_vax_view_rotavirus_df, by = c("state_name" = "state_name")) # Add on rotavirus vaccine coverage data
  df_census_0_4_rota_w_model_input_params <- left_join(df_census_0_4_rota, model_input_parameters_df, by = c("vaccine" = "vaccine")) %>% select(-ends_with("_source")) %>% filter(disease == 'Rotavirus') # add on model input parameters for rotavirus
  
  # Create PCV data table
  df_census_0_4_pcv <- left_join(df_census_0_4, cdc_child_vax_view_pcv_df, by = c("state_name" = "state_name")) # Add on PCV vaccine coverage data
  df_census_0_4_pcv_w_model_input_params <- left_join(df_census_0_4_pcv, model_input_parameters_df, by = c("vaccine" = "vaccine")) %>% select(-ends_with("_source")) %>% filter(disease == 'Pneumococcal') # add on model input parameters for PCV
  
  # Create pertussis data table
  df_census <- left_join(census_acs_states_df, census_acs_state_population_df %>% select(-state_name), by = c("state_fips_code" = "state_fips_code"))
  df_census_0_14 <- left_join(df_census, census_acs_state_population_0_14_years_df %>% select(-state_name), by = c("state_fips_code" = "state_fips_code")) # join on state population 0-14 years from census
  df_census_0_14_dtap <- left_join(df_census_0_14, cdc_school_vax_view_dtap_df, by = c("state_name" = "state_name")) %>% mutate(vaccine_coverage_estimate = as.numeric(vaccine_coverage_estimate)) # Add on DTaP vaccine coverage data
  df_census_0_14_dtap_w_model_input_params <- left_join(df_census_0_14_dtap, model_input_parameters_df, by = c("vaccine" = "vaccine")) %>% select(-ends_with("_source")) %>% filter(disease == 'Pertussis') # add on model input parameters for pertussis
  
  # Create varicella data table (0-14 years, same age band as pertussis; coverage from SchoolVaxView UTD)
  df_census_0_14_varicella <- left_join(df_census_0_14, cdc_school_vax_view_varicella_df, by = c("state_name" = "state_name")) %>% mutate(vaccine_coverage_estimate = as.numeric(vaccine_coverage_estimate)) # Add on varicella vaccine coverage data
  df_census_0_14_varicella_w_model_input_params <- left_join(df_census_0_14_varicella, model_input_parameters_df, by = c("vaccine" = "vaccine")) %>% select(-ends_with("_source")) %>% filter(disease == 'Varicella') # add on model input parameters for varicella
  
  # Union rotavirus, PCV, pertussis, and varicella data to create the start of the model input data frame
  df_model_input_data <- union(df_census_0_4_rota_w_model_input_params,
                               df_census_0_4_pcv_w_model_input_params) %>%
                         union(df_census_0_14_dtap_w_model_input_params) %>%
                         union(df_census_0_14_varicella_w_model_input_params)
  
  # Next, add rows for declining vaccination coverage among births, ranging from 0 to 100%, and 1 to 5 years as the time horizons of interest
  declining_coverage_among_new_births <- 0:20 # Create vector 0 to 20
  time_horizon <- c(1, 5, 10, 20) # accrual horizons of interest (years)
  df_model_input_data_expanded <- df_model_input_data %>% crossing(declining_coverage_among_new_births, time_horizon)
  
  # Organize dataframe columns (and drop age_group_target from the dataframe, originally sourced from model_input_parameters.csv, as it is not needed)
  df_model_input_data_expanded <- df_model_input_data_expanded %>% select(state_fips_code, 
                                                                          state_name,
                                                                          disease,
                                                                          vaccine,
                                                                          time_horizon,
                                                                          declining_coverage_among_new_births,
                                                                          total_population,
                                                                          age_group,
                                                                          age_group_length,
                                                                          age_group_population,
                                                                          vaccine_coverage_estimate,
                                                                          vaccine_effectiveness,
                                                                          waning_rate_annual,
                                                                          basic_reproduction_number,
                                                                          observed_national_cases,
                                                                          observed_national_hospitalizations,
                                                                          observed_national_deaths,
                                                                          duration_infectious_days,
                                                                          duration_sick_days,
                                                                          cost_wage_daily,
                                                                          proportion_hospitalized_given_case,
                                                                          duration_hospitalized_days,
                                                                          cost_hospitalization_daily,
                                                                          death_rate,
                                                                          model_type,,
                                                                          severe_adverse_event_rate
                                                                            )                                                                          
  
  # Perform minor reformatting to convert vaccine coverage data to percentages
  df_model_input_data_expanded_clean <- df_model_input_data_expanded %>% 
                                          mutate(
                                            declining_coverage_among_new_births = declining_coverage_among_new_births/100,
                                            vaccine_coverage_estimate = vaccine_coverage_estimate/100
                                          )
  
  return(df_model_input_data_expanded_clean)

}