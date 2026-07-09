# Calculate additional disease burden comparing outcomes at baseline vaccine coverage to those incorporating vaccine coverage decline
# --------------------------------------------------------------------------

calculate_additional_disease_burden <- function(df) {
  
  # Install & load required libraries
  # --------------------------------------------------------------------------
  packages <- c("tidyverse","here")
  install.packages(setdiff(packages, rownames(installed.packages())))
  invisible(lapply(packages, library, character.only = TRUE))
  
  # Set file location relative to current project
  # --------------------------------------------------------------------------
  suppressMessages(here::i_am("R/calculate_additional_disease_burden.R"))
  print("-H. calculate_additional_disease_burden.R")
  
  # Start by joining the dataframe onto a version of itself filtered to declining_coverage_among_new_births = 0 and joining by state_fips_code, disease, time_horizon
  # --------------------------------------------------------------------------
  df_joined <- left_join(df, 
                         df %>% 
                           filter(declining_coverage_among_new_births==0) %>% 
                           select(state_fips_code, disease, time_horizon,
                                  infections, cases, hospitalizations, deaths, 
                                  workdays_lost, 
                                  productivity_cost, hospitalization_cost, total_cost,
                                  vaccine_adverse_events) %>%
                           rename(baseline_infections=infections, 
                                  baseline_cases=cases, 
                                  baseline_hospitalizations=hospitalizations, 
                                  baseline_deaths=deaths, 
                                  baseline_workdays_lost=workdays_lost, 
                                  baseline_productivity_cost=productivity_cost, 
                                  baseline_hospitalization_cost=hospitalization_cost, 
                                  baseline_total_cost=total_cost,
                                  baseline_vaccine_adverse_events=vaccine_adverse_events), 
                         by = c("state_fips_code" = "state_fips_code", "disease" = "disease", "time_horizon" = "time_horizon"))
  
  # Calculate additional morbidity, mortality, and economic burden
  # --------------------------------------------------------------------------
  df_joined$additional_infections <- df_joined$infections - df_joined$baseline_infections
  df_joined$additional_infections_per_100k <- df_joined$additional_infections / df_joined$age_group_population * 100000
  
  df_joined$additional_cases <- df_joined$cases - df_joined$baseline_cases
  df_joined$additional_cases_per_100k <- df_joined$additional_cases / df_joined$age_group_population * 100000
  
  df_joined$additional_hospitalizations <- df_joined$hospitalizations - df_joined$baseline_hospitalizations
  df_joined$additional_hospitalizations_per_100k <- df_joined$additional_hospitalizations / df_joined$age_group_population * 100000
  
  df_joined$additional_deaths <- df_joined$deaths - df_joined$baseline_deaths
  df_joined$additional_deaths_per_100k <- df_joined$additional_deaths / df_joined$age_group_population * 100000
  
  df_joined$additional_workdays_lost <- df_joined$workdays_lost - df_joined$baseline_workdays_lost
  df_joined$additional_workdays_lost_per_100k <- df_joined$additional_workdays_lost / df_joined$age_group_population * 100000
  
  df_joined$additional_productivity_cost <- df_joined$productivity_cost - df_joined$baseline_productivity_cost
  df_joined$additional_productivity_cost_per_100k <- df_joined$additional_productivity_cost / df_joined$age_group_population * 100000
  
  df_joined$additional_hospitalization_cost <- df_joined$hospitalization_cost - df_joined$baseline_hospitalization_cost
  df_joined$additional_hospitalization_cost_per_100k <- df_joined$additional_hospitalization_cost / df_joined$age_group_population * 100000
  
  df_joined$additional_total_cost <- df_joined$total_cost - df_joined$baseline_total_cost
  df_joined$additional_total_cost_per_100k <- df_joined$additional_total_cost / df_joined$age_group_population * 100000
  
  # Severe vaccine adverse events AVOIDED under declining coverage (baseline minus scenario;
  # positive because fewer vaccinations means fewer adverse events). This is the counter-
  # weight to the additional disease burden above.
  df_joined$vaccine_adverse_events_avoided <- df_joined$baseline_vaccine_adverse_events - df_joined$vaccine_adverse_events
  df_joined$vaccine_adverse_events_avoided_per_100k <- df_joined$vaccine_adverse_events_avoided / df_joined$age_group_population * 100000
  
  # Organize columns
  # --------------------------------------------------------------------------
  df_joined <- df_joined %>% 
    select(state_fips_code, 
           state_name,
           disease,
           vaccine,
           time_horizon,
           vaccine_coverage_estimate,
           vaccine_effectiveness,
           declining_coverage_among_new_births,
           coverage_with_decline_applied,
           structural_vaccine_coverage,
           effective_structural_vaccine_coverage,
           total_population,
           age_group,
           age_group_length,
           age_group_population,
           waning_rate_annual,
           basic_reproduction_number,
           observed_national_cases,
           duration_infectious_days,
           duration_sick_days,
           cost_wage_daily,
           proportion_hospitalized_given_case,
           duration_hospitalized_days,
           cost_hospitalization_daily,
           death_rate,
           model_type,
           endemic_equilibrium_incidence_rate_annual,
           infections,
           infections_per_100k,
           additional_infections,
           additional_infections_per_100k,
           cases,
           cases_per_100k,
           additional_cases,
           additional_cases_per_100k,
           hospitalizations,
           hospitalizations_per_100k,
           additional_hospitalizations,
           additional_hospitalizations_per_100k,
           deaths,
           deaths_per_100k,
           additional_deaths,
           additional_deaths_per_100k,
           workdays_lost,
           workdays_lost_per_100k,
           additional_workdays_lost,
           additional_workdays_lost_per_100k,
           productivity_cost,
           productivity_cost_per_100k,
           additional_productivity_cost,
           additional_productivity_cost_per_100k,
           hospitalization_cost,
           hospitalization_cost_per_100k,
           additional_hospitalization_cost,
           additional_hospitalization_cost_per_100k,
           total_cost,
           total_cost_per_100k,
           additional_total_cost,
           additional_total_cost_per_100k,
           vaccine_adverse_events,
           vaccine_adverse_events_avoided,
           vaccine_adverse_events_avoided_per_100k
           )
  
  return(df_joined)
  
}