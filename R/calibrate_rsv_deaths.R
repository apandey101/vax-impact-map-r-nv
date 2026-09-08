# Calibrate RSV deaths (derived from calibrated cases) 
# --------------------------------------------------------------------------
calibrate_rsv_deaths <- function(df) {
  packages <- c("tidyverse","here")
  install.packages(setdiff(packages, rownames(installed.packages())))
  invisible(lapply(packages, library, character.only = TRUE))
  suppressMessages(here::i_am("R/calibrate_rsv_deaths.R"))
  print("---c. calibrate_rsv_deaths.R")
  
  df$deaths <- df$cases * df$death_rate
  df$deaths_per_100k <- df$deaths / df$age_group_population * 100000
  return(df)
}
