# Calibrate RSV model parameters using observed national level data 
# --------------------------------------------------------------------------
calibrate_rsv <- function(df) {
  packages <- c("tidyverse","here")
  install.packages(setdiff(packages, rownames(installed.packages())))
  invisible(lapply(packages, library, character.only = TRUE))
  suppressMessages(here::i_am("R/calibrate_rsv.R"))
  print("--6. calibrate_rsv.R")
  
  ## Filter the model data for just RSV
  df <- df %>% filter(disease=='RSV')
  
  ## Anchor hospitalizations to observed national infant RSV hospitalizations (RSV-NET).
  ## Hospitalization is the headline outcome for RSV; cases and deaths are derived.
  source(here("R/calibrate_rsv_hospitalizations.R")); df <- calibrate_rsv_hospitalizations(df)
  source(here("R/calibrate_rsv_cases.R"));            df <- calibrate_rsv_cases(df)
  source(here("R/calibrate_rsv_deaths.R"));           df <- calibrate_rsv_deaths(df)
  
  return(df)
}
