# Calibrate Hib model parameters using observed national level data 
# --------------------------------------------------------------------------
calibrate_hib <- function(df) {
  packages <- c("tidyverse","here")
  install.packages(setdiff(packages, rownames(installed.packages())))
  invisible(lapply(packages, library, character.only = TRUE))
  suppressMessages(here::i_am("R/calibrate_hib.R"))
  print("--5. calibrate_hib.R")
  
  ## Filter the model data for just Hib
  df <- df %>% filter(disease=='Hib')
  
  ## Anchor cases to observed national type-b case count
  source(here("R/calibrate_hib_cases.R")); df <- calibrate_hib_cases(df)
  ## Derive hospitalizations from cases (invasive Hib is near-universally admitted)
  source(here("R/calibrate_hib_hospitalizations.R")); df <- calibrate_hib_hospitalizations(df)
  ## Derive deaths from cases via type-b case fatality ratio
  source(here("R/calibrate_hib_deaths.R")); df <- calibrate_hib_deaths(df)
  
  return(df)
}
