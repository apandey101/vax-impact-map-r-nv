# Calibrate RSV cases (derived from calibrated hospitalizations) 
# --------------------------------------------------------------------------
calibrate_rsv_cases <- function(df) {
  packages <- c("tidyverse","here")
  install.packages(setdiff(packages, rownames(installed.packages())))
  invisible(lapply(packages, library, character.only = TRUE))
  suppressMessages(here::i_am("R/calibrate_rsv_cases.R"))
  print("---b. calibrate_rsv_cases.R")
  
  ## Back-calculate cases from calibrated hospitalizations and P(hospitalized|case).
  ## NOTE: for RSV the headline outcome is HOSPITALIZATIONS. "Cases" here are a derived
  ## quantity for map consistency only and inherit all uncertainty in proportion_hospitalized_given_case.
  df$cases <- df$hospitalizations / df$proportion_hospitalized_given_case
  df$cases_per_100k <- df$cases / df$age_group_population * 100000
  return(df)
}
