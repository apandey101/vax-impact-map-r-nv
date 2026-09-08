# Function to compile all data read from read_data.R and collected and processed from get_and_process_data.R
# --------------------------------------------------------------------------
# EQUILIBRIUM diseases only: rotavirus, PCV, pertussis, RSV.
# Hib and Varicella are produced by the age-structured producers
# (produce_hib_output / produce_varicella_output) and merged in
# combine_model_output(); they are intentionally not built here.
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
  
  # Compile model input data
  # --------------------------------------------------------------------------
  
  # Base census table (state attributes + total population)
  df_census <- left_join(census_acs_states_df, census_acs_state_population_df %>% select(-state_name), by = c("state_fips_code" = "state_fips_code"))
  
  # Create rotavirus data table (0-4 years)
  df_census_0_4 <- left_join(df_census, census_acs_state_population_0_4_years_df %>% select(-state_name), by = c("state_fips_code" = "state_fips_code")) # join on state population 0-4 years from census
  df_census_0_4_rota <- left_join(df_census_0_4, cdc_child_vax_view_rotavirus_df, by = c("state_name" = "state_name")) # Add on rotavirus vaccine coverage data
  df_census_0_4_rota_w_model_input_params <- left_join(df_census_0_4_rota, model_input_parameters_df, by = c("vaccine" = "vaccine")) %>% select(-ends_with("_source")) %>% filter(disease == 'Rotavirus') # add on model input parameters for rotavirus
  
  # Create PCV data table (0-4 years)
  df_census_0_4_pcv <- left_join(df_census_0_4, cdc_child_vax_view_pcv_df, by = c("state_name" = "state_name")) # Add on PCV vaccine coverage data
  df_census_0_4_pcv_w_model_input_params <- left_join(df_census_0_4_pcv, model_input_parameters_df, by = c("vaccine" = "vaccine")) %>% select(-ends_with("_source")) %>% filter(disease == 'Pneumococcal') # add on model input parameters for PCV

  # Create pertussis data table (0-14 years)
  df_census_0_14 <- left_join(df_census, census_acs_state_population_0_14_years_df %>% select(-state_name), by = c("state_fips_code" = "state_fips_code")) # join on state population 0-14 years from census
  df_census_0_14_dtap <- left_join(df_census_0_14, cdc_school_vax_view_dtap_df, by = c("state_name" = "state_name")) %>% mutate(vaccine_coverage_estimate = as.numeric(vaccine_coverage_estimate)) # Add on DTaP vaccine coverage data
  df_census_0_14_dtap_w_model_input_params <- left_join(df_census_0_14_dtap, model_input_parameters_df, by = c("vaccine" = "vaccine")) %>% select(-ends_with("_source")) %>% filter(disease == 'Pertussis') # add on model input parameters for pertussis
  
  # Create RSV data table (0-1 years). The <1y population is derived as the 0-4y
  # population / 5, consistent with the pipeline's uniform-birth-cohort assumption
  # (no separate <1y census band exists). Coverage is the imputed nirsevimab (mAb)
  # coverage; the maternal RSV vaccine layer is added downstream in
  # calculate_structural_vaccine_coverage (fixed, hardwired). The coverage file's
  # `vaccine` is relabelled "RSV" so it joins to the RSV parameter row.
  df_census_0_1 <- df_census_0_4 %>%
       mutate(age_group_population = age_group_population / 5,
           age_group = "0-1 years",
           age_group_length = 1)
  df_census_0_1_rsv <- left_join(
    df_census_0_1,
    rsv_mab_coverage_imputed_df %>%
      select(state_name, vaccine_coverage_estimate) %>%
      mutate(vaccine = "RSV"),
    by = c("state_name" = "state_name")
  ) %>% mutate(vaccine_coverage_estimate = as.numeric(vaccine_coverage_estimate))
  df_census_0_1_rsv_w_model_input_params <- left_join(
    df_census_0_1_rsv, model_input_parameters_df, by = c("vaccine" = "vaccine")
  ) %>% select(-ends_with("_source")) %>% filter(disease == 'RSV')

  # Union the equilibrium diseases: rotavirus, PCV, pertussis, and RSV.
  df_model_input_data <- union(df_census_0_4_rota_w_model_input_params,
                               df_census_0_4_pcv_w_model_input_params) %>%
                         union(df_census_0_14_dtap_w_model_input_params) %>%
                         union(df_census_0_1_rsv_w_model_input_params)
  
  # Next, add rows for declining vaccination coverage among births. Scenarios are
  # expressed as percentage-point declines from baseline and expanded from 0 to 20
  # in 1pp steps (0.00 to 0.20 after dividing by 100).
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
                                                                          model_type,
                                                                          severe_adverse_event_rate
                                                                            )                                                                          
  
  # Perform minor reformatting to convert vaccine coverage data to proportions
  df_model_input_data_expanded_clean <- df_model_input_data_expanded %>% 
                                          mutate(
                                            declining_coverage_among_new_births = declining_coverage_among_new_births/100,
                                            vaccine_coverage_estimate = vaccine_coverage_estimate/100
                                          )
  
  return(df_model_input_data_expanded_clean)

}