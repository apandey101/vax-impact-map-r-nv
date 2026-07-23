# Calibrate varicella deaths model parameters using observed national level data 
# --------------------------------------------------------------------------

calibrate_varicella_deaths <- function(df) {
  
  # Install & load required libraries
  # --------------------------------------------------------------------------
  packages <- c("tidyverse","here")
  install.packages(setdiff(packages, rownames(installed.packages())))
  invisible(lapply(packages, library, character.only = TRUE))
  
  # Set file location relative to current project
  # --------------------------------------------------------------------------
  suppressMessages(here::i_am("R/calibrate_varicella_deaths.R"))
  print("---c. calibrate_varicella_deaths.R")
  
  ## Back-calculate varicella deaths from calibrated cases and P(death|case)
  # --------------------------------------------------------------------------
  df$deaths <- df$cases * df$death_rate
  df$deaths_per_100k <- df$deaths / df$age_group_population * 100000
  
  return(df)
  
}