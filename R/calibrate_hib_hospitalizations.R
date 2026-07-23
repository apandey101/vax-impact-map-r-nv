# Calibrate Hib hospitalizations (derived from calibrated cases) 
# --------------------------------------------------------------------------
calibrate_hib_hospitalizations <- function(df) {
  packages <- c("tidyverse","here")
  install.packages(setdiff(packages, rownames(installed.packages())))
  invisible(lapply(packages, library, character.only = TRUE))
  suppressMessages(here::i_am("R/calibrate_hib_hospitalizations.R"))
  print("---b. calibrate_hib_hospitalizations.R")
  
  # Invasive Hib disease (bacteremia/meningitis/pneumonia) is almost always hospitalized.
  df$hospitalizations <- df$cases * df$proportion_hospitalized_given_case
  df$hospitalizations_per_100k <- df$hospitalizations / df$age_group_population * 100000
  return(df)
}
