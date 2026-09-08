# Retrieve Census ACS population estimates in the six ENGAGED model bands used
# by the varicella model and write them to the project data-raw folder.
# --------------------------------------------------------------------------
# The band logic (ACS variable codes for 0-4, 5-17, 18-49, 50-59, 60-74, 75+)
# lives in run_varicella_agestructured.R (vzv_build_acs_band_population), the
# single source of truth kept in sync with ENGAGED_GROUPS. This module only
# wires that builder to the canonical output paths so the file convention
# matches the other get_data_* modules without duplicating the band definition.
#
# The varicella loader (getpop) requires all six band rows per geography; if any
# band is missing for a state, that state is silently skipped. Keep this builder
# and ENGAGED_GROUPS aligned.
#
# Requires: here, tidycensus, dplyr, and a Census API key set for tidycensus
# (see tidycensus::census_api_key). Needs network access; run during data
# refresh, not inside the model pipeline.
# --------------------------------------------------------------------------
get_data_census_acs_state_population_vzv_bands <- function(year = 2023) {

  for (pkg in c("here", "tidycensus", "dplyr")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Install '", pkg,
           "' to pull ACS varicella-band populations (get_data_census_acs_state_population_vzv_bands).")
    }
  }

  suppressMessages(
    here::i_am("R/get_data_census_acs_state_population_vzv_bands.R")
  )
  print("---. get_data_census_acs_state_population_vzv_bands.R")

  # Ensure the builder (and VZV_ACS_MALE_CODES) are available.
  if (!exists("vzv_build_acs_band_population", mode = "function")) {
    source(here::here("R", "run_varicella_agestructured.R"))
  }

  vzv_build_acs_band_population(
    year     = year,
    save_rds = here::here("data-raw",
                          "census_acs_state_population_vzv_bands.rds"),
    save_csv = here::here("data-raw", "csv",
                          "census_acs_state_population_vzv_bands.csv")
  )
}
