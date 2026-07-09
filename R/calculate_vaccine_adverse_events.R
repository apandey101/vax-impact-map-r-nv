# Calculate severe vaccine adverse events based on the number of vaccinations administered
# --------------------------------------------------------------------------
# Adverse events are NOT a transmission process: they attach directly to the
# children vaccinated each year (the birth cohort reaching vaccination age),
# at the post-decline newborn coverage. So this uses coverage_with_decline_applied
# (X - decline), NOT the band-averaged structural coverage the disease model uses,
# and it does not accrue over the time horizon. The annual vaccinated cohort is
# approximated as age_group_population / age_group_length (uniform-age assumption).

calculate_vaccine_adverse_events <- function(df) {
  
  # Install & load required libraries
  # --------------------------------------------------------------------------
  packages <- c("tidyverse","here")
  install.packages(setdiff(packages, rownames(installed.packages())))
  invisible(lapply(packages, library, character.only = TRUE))
  
  # Set file location relative to current project
  # --------------------------------------------------------------------------
  suppressMessages(here::i_am("R/calculate_vaccine_adverse_events.R"))
  print("-G2. calculate_vaccine_adverse_events.R")
  
  ## Annual number of children vaccinated at the (post-decline) newborn coverage
  # --------------------------------------------------------------------------
  df$annual_vaccinated_cohort <- (df$age_group_population / df$age_group_length) * df$coverage_with_decline_applied
  
  ## Severe vaccine adverse events occurring per year at this coverage
  # --------------------------------------------------------------------------
  df$vaccine_adverse_events <- df$annual_vaccinated_cohort * df$severe_adverse_event_rate
  
  return(df)
  
}
