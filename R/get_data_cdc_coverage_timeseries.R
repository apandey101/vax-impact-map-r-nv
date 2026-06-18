# Source and process CDC vaccination coverage TIME SERIES (baseline coverage over time)
# --------------------------------------------------------------------------
# Companion to get_data_cdc.R / process_data_cdc.R.
#
# Those pipelines pull the full CDC datasets but then filter each antigen to a
# SINGLE point in time (child birth cohort 2021; school year 2023-24) and drop
# the time column. This script keeps the time axis instead, producing a tidy
# longitudinal series for the state-profile "coverage trend since pre-COVID"
# view.
#
# Outputs (mirroring the repo layout):
#   - per-antigen intermediate series -> data-raw/ (+ data-raw/csv/)
#   - combined curated series         -> data-raw/ (+ data-raw/csv/) AND
#                                        data/ (+ data/csv/)  for the app
#
# Scope (matches the antigens already used in the app):
#   - Child VaxView : Rotavirus (8 Months), PCV (>=4 Doses, 35 Months)
#   - School VaxView: DTP, DTaP, or DT (kindergarten)
#
# Default start point is just before COVID-19:
#   - child:  birth cohort 2019 onward
#   - school: school year 2019-20 onward
# Adjust via the function arguments below.
#
# Sources (data.cdc.gov Socrata):
#   Child VaxView : https://data.cdc.gov/Child-Vaccinations/Vaccination-Coverage-among-Young-Children-0-35-Mon/fhky-rtsk/about_data
#   School VaxView: https://data.cdc.gov/Vaccinations/Vaccination-Coverage-and-Exemptions-among-Kinderga/ijqb-a7ye/about_data
# --------------------------------------------------------------------------

get_data_cdc_coverage_timeseries <- function(child_min_birth_year = 2019,
                                             school_min_year_start = 2019,
                                             refresh_raw = TRUE) {

  # Install & load required libraries
  # --------------------------------------------------------------------------
  packages <- c("tidyverse", "here")
  install.packages(setdiff(packages, rownames(installed.packages())))
  invisible(lapply(packages, library, character.only = TRUE))

  # Set file location relative to current project
  # --------------------------------------------------------------------------
  suppressMessages(here::i_am("R/get_data_cdc_coverage_timeseries.R"))
  print("--1. get_data_cdc_coverage_timeseries.R")

  # Sub-state / aggregate geographies to drop (same exclusions as the existing
  # cross-sectional child-vax-view processing scripts).
  # --------------------------------------------------------------------------
  child_geo_exclude <- c(
    "Guam",
    "IL-City of Chicago", "IL-Rest of state",
    "NY-City of New York", "NY-Rest of state",
    "PA-Philadelphia", "PA-Rest of state",
    "TX-Bexar County", "TX-City of Houston", "TX-Dallas County",
    "TX-El Paso County", "TX-Hidalgo County", "TX-Rest of state",
    "TX-Tarrant County", "TX-Travis County",
    "U.S. Virgin Islands",
    "Region 1", "Region 2", "Region 3", "Region 4", "Region 5",
    "Region 6", "Region 7", "Region 8", "Region 9", "Region 10"
  )
  school_geo_exclude <- c("NY-City of New York", "TX-City of Houston", "U.S. Median")

  # --------------------------------------------------------------------------
  # 1. SOURCE raw data
  #    Reuse cached pulls in data-raw if available and refresh_raw = FALSE;
  #    otherwise pull fresh from the Socrata CSV API.
  # --------------------------------------------------------------------------
  child_raw_path  <- here("data-raw/cdc_child_vax_view.rds")
  school_raw_path <- here("data-raw/cdc_school_vax_view.rds")

  if (!refresh_raw && file.exists(child_raw_path)) {
    df_child <- readRDS(child_raw_path)
  } else {
    print("---a. pulling Child VaxView from data.cdc.gov ...")
    df_child <- read.csv("https://data.cdc.gov/api/views/fhky-rtsk/rows.csv?accessType=DOWNLOAD&api_foundry=true")
  }

  if (!refresh_raw && file.exists(school_raw_path)) {
    df_school <- readRDS(school_raw_path)
  } else {
    print("---b. pulling School VaxView from data.cdc.gov ...")
    df_school <- read.csv("https://data.cdc.gov/api/views/ijqb-a7ye/rows.csv?accessType=DOWNLOAD&api_foundry=true")
  }

  # --------------------------------------------------------------------------
  # 2. PROCESS child antigens, RETAINING the birth-cohort year as the time axis
  #    Keep single-year birth cohorts only (drop rolling ranges like "2019-2020").
  # --------------------------------------------------------------------------
  process_child <- function(df, vaccine_value, dimension_value, dose_value = NULL) {
    out <- df %>%
      filter(
        Vaccine == vaccine_value,
        Dimension.Type == "Age",
        Dimension == dimension_value,
        str_detect(Birth.Year.Birth.Cohort, "^[0-9]{4}$"),      # single-year cohorts only
        suppressWarnings(as.integer(Birth.Year.Birth.Cohort)) >= child_min_birth_year,
        !Geography %in% child_geo_exclude
      )
    if (!is.null(dose_value)) out <- out %>% filter(Dose == dose_value)

    out %>%
      transmute(
        source       = "child_vax_view",
        vaccine      = Vaccine,
        state_name   = Geography,
        year         = suppressWarnings(as.integer(Birth.Year.Birth.Cohort)),
        year_type    = "birth_cohort",
        vaccine_coverage_estimate = suppressWarnings(as.numeric(Estimate....)),
        ci_95        = X95..CI....,
        sample_size  = suppressWarnings(as.integer(gsub(",", "", Sample.Size)))
      ) %>%
      arrange(state_name, year)
  }

  df_rotavirus <- process_child(df_child, "Rotavirus", "8 Months")
  df_pcv       <- process_child(df_child, "PCV", "35 Months", dose_value = "≥4 Doses")

  # --------------------------------------------------------------------------
  # 3. PROCESS school DTaP, RETAINING school year as the time axis
  # --------------------------------------------------------------------------
  process_school <- function(df, vaccine_value) {
    df %>%
      filter(
        Vaccine.Exemption == vaccine_value,
        Geography.Type %in% c("States", "National"),
        suppressWarnings(as.integer(substr(School.Year, 1, 4))) >= school_min_year_start,
        !Geography %in% school_geo_exclude
      ) %>%
      transmute(
        source       = "school_vax_view",
        vaccine      = Vaccine.Exemption,
        state_name   = Geography,
        year         = suppressWarnings(as.integer(substr(School.Year, 1, 4))),
        year_type    = "school_year",
        school_year  = School.Year,
        vaccine_coverage_estimate = suppressWarnings(as.numeric(Estimate....)),
        ci_95        = NA_character_,
        sample_size  = suppressWarnings(as.integer(gsub(",", "", Population.Size)))
      ) %>%
      arrange(state_name, year)
  }

  df_dtap <- process_school(df_school, "DTP, DTaP, or DT")

  # --------------------------------------------------------------------------
  # 4. Combined tidy long series across all antigens
  # --------------------------------------------------------------------------
  df_combined <- bind_rows(
    df_rotavirus,
    df_pcv,
    df_dtap %>% select(-school_year)
  ) %>%
    arrange(source, vaccine, state_name, year)

  # --------------------------------------------------------------------------
  # 5. Write outputs (rds + csv)
  #    - per-antigen intermediate series -> data-raw/ (+ data-raw/csv/)
  #    - combined curated series         -> data-raw/ AND data/ (+ their csv/)
  # --------------------------------------------------------------------------
  write_raw <- function(obj, name) {
    saveRDS(obj, file = here(paste0("data-raw/", name, ".rds")))
    write.csv(obj, file = here(paste0("data-raw/csv/", name, ".csv")), row.names = FALSE)
  }
  write_curated <- function(obj, name) {
    saveRDS(obj, file = here(paste0("data/", name, ".rds")))
    write.csv(obj, file = here(paste0("data/csv/", name, ".csv")), row.names = FALSE)
  }

  write_raw(df_rotavirus, "cdc_child_vax_view_rotavirus_timeseries")
  write_raw(df_pcv,       "cdc_child_vax_view_pcv_timeseries")
  write_raw(df_dtap,      "cdc_school_vax_view_dtap_timeseries")

  write_raw(df_combined,     "cdc_coverage_timeseries")
  write_curated(df_combined, "cdc_coverage_timeseries")

  print(paste0("Wrote ", nrow(df_combined),
               " rows across ", dplyr::n_distinct(df_combined$vaccine),
               " antigens to data-raw/ and data/ (+ their csv/ subfolders)"))

  invisible(df_combined)
}

# Run when sourced directly (matches the style of the existing get_data_cdc.R)
# --------------------------------------------------------------------------
# get_data_cdc_coverage_timeseries()
