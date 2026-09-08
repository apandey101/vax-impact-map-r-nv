# Retrieve Census ACS population estimates in the 13 Hib model reporting bands
# and write them to the project data-raw folder.
# --------------------------------------------------------------------------
# The band logic (ACS variable codes and the split of the ACS 0-4 total across
# the model's seven sub-5 cohorts) lives in run_hib_agestructured.R
# (hib_build_acs_band_population), the single source of truth for the Hib age
# structure. This module only wires that builder to the canonical output paths,
# so the file convention matches the other get_data_* modules without
# duplicating (and risking drift in) the 13-band definition.
#
# Requires: here, tidycensus, dplyr, and a Census API key set for tidycensus
# (see tidycensus::census_api_key). Needs network access; run during data
# refresh, not inside the model pipeline.
# --------------------------------------------------------------------------
get_data_census_acs_state_population_hib_bands <- function(year = 2023) {

  for (pkg in c("here", "tidycensus", "dplyr")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Install '", pkg,
           "' to pull ACS Hib-band populations (get_data_census_acs_state_population_hib_bands).")
    }
  }

  suppressMessages(
    here::i_am("R/get_data_census_acs_state_population_hib_bands.R")
  )
  print("---. get_data_census_acs_state_population_hib_bands.R")

  # Ensure the builder (and its band constants) are available.
  if (!exists("hib_build_acs_band_population", mode = "function")) {
    source(here::here("R", "run_hib_agestructured.R"))
  }

  hib_build_acs_band_population(
    year     = year,
    save_rds = here::here("data-raw",
                          "census_acs_state_population_hib_bands.rds"),
    save_csv = here::here("data-raw", "csv",
                          "census_acs_state_population_hib_bands.csv")
  )
}
