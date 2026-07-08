# Create function curate_model_output for curating model output data in `data` folder
# --------------------------------------------------------------------------
curate_model_output <- function() {
  
  # Install & load required libraries
  # --------------------------------------------------------------------------
  packages <- c("tidyverse","here")
  install.packages(setdiff(packages, rownames(installed.packages())))
  invisible(lapply(packages, library, character.only = TRUE))
  
  # Set file location relative to current project
  # --------------------------------------------------------------------------
  suppressMessages(here::i_am("R/curate_model_output.R"))
  print("III. curate_model_output.R")
  
  # Create function curate_model_output by reading the vax_impact_map_model_output RDS from the `data` folder
  # --------------------------------------------------------------------------
  
  # Get model input parameters from the vax_impact_map_model_output rds in the `data` folder
  read_path_rds <- here("data/vax_impact_map_model_output.rds")
  df <- readRDS(read_path_rds)
  
  # Clean and process the model outputs
  df <- df %>%
        mutate(declining_coverage_among_new_births = declining_coverage_among_new_births*100,
                accrual_label = factor(paste0(time_horizon, ifelse(time_horizon==1, " Year", " Years")),
                                      levels = c("1 Year","5 Years","10 Years","20 Years"))) %>%
        rename(percent_decline = declining_coverage_among_new_births,
               accrual_years = time_horizon,
               baseline_coverage = vaccine_coverage_estimate) %>%
        select(disease,
               state_name,
               age_group,
               age_group_population,
               percent_decline,
               accrual_years,
               accrual_label,
               baseline_coverage,
               cases,
               additional_cases,
               cases_per_100k,
               additional_cases_per_100k,
               hospitalizations,
               additional_hospitalizations,
               hospitalizations_per_100k,
               additional_hospitalizations_per_100k,
               deaths,
               additional_deaths,
               deaths_per_100k,
               additional_deaths_per_100k,
               workdays_lost,
               additional_workdays_lost,
               workdays_lost_per_100k,
               additional_workdays_lost_per_100k,
               productivity_cost,
               additional_productivity_cost,
               productivity_cost_per_100k,
               additional_productivity_cost_per_100k,
               hospitalization_cost,
               additional_hospitalization_cost,
               hospitalization_cost_per_100k,
               additional_hospitalization_cost_per_100k,
               total_cost,
               additional_total_cost,
               total_cost_per_100k,
               additional_total_cost_per_100k
               )
  
  # Write data as a csv called vax_impact_map_model_output_curated.csv to the project `data` folder
  write_path_csv <- here("data/csv/vax_impact_map_model_output_curated.csv")
  write.csv(df, file = write_path_csv)
  
  # Message specifying where data was written
  # print(paste0("Saved curated model output rds to ",write_path_csv))
  
  # Write data as a rds called vax_impact_map_model_output_curated.rds to the project `data` folder
  write_path_rds <- here("data/vax_impact_map_model_output_curated.rds")
  saveRDS(df, file = write_path_rds)
  
  # Message specifying where data was written
  # print(paste0("Saved curated model output csv to ",write_path_rds))
  
}