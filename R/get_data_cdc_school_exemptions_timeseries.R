# Source and process CDC SchoolVaxView EXEMPTION rates time series (by state, over time)
# --------------------------------------------------------------------------
# Companion to get_data_cdc_coverage_timeseries.R. Same source dataset
# (SchoolVaxView, ijqb-a7ye) but pulls the kindergarten EXEMPTION rows instead
# of vaccine coverage, retaining the school-year axis for trend display.
#
# The SchoolVaxView dataset encodes exemptions as Vaccine.Exemption == "Exemption"
# with Dose in {"Any Exemption","Medical Exemption","Non-Medical Exemption"}.
# Estimate.... is the exemption rate (%); Number.of.Exemptions and Population.Size
# give the counts behind it.
#
# Outputs (mirroring the repo layout):
#   - long  : one row per state x school-year x exemption type -> data-raw/ (+ csv/)
#   - wide  : one row per state x school-year (3 rate columns)  -> data-raw/ AND data/ (+ csv/)
#
# Default start point is just before COVID-19 (school year 2019-20). Set
# school_min_year_start = 2009 to pull the full available history (back to 2009-10).
#
# Source (data.cdc.gov Socrata):
#   https://data.cdc.gov/Vaccinations/Vaccination-Coverage-and-Exemptions-among-Kinderga/ijqb-a7ye/about_data
# --------------------------------------------------------------------------

get_data_cdc_school_exemptions_timeseries <- function(school_min_year_start = 2019,
                                                      refresh_raw = TRUE) {

  # Install & load required libraries
  # --------------------------------------------------------------------------
  packages <- c("tidyverse", "here")
  install.packages(setdiff(packages, rownames(installed.packages())))
  invisible(lapply(packages, library, character.only = TRUE))

  suppressMessages(here::i_am("R/get_data_cdc_school_exemptions_timeseries.R"))
  print("--1. get_data_cdc_school_exemptions_timeseries.R")

  # Sub-state cities and the median aggregate to drop (keep states/DC + national)
  # --------------------------------------------------------------------------
  geo_exclude <- c("NY-City of New York", "TX-City of Houston",
                   "TX-City of San Antonio", "U.S. Median")

  # --------------------------------------------------------------------------
  # 1. SOURCE raw data: reuse cached pull in data-raw if available and
  #    refresh_raw = FALSE; otherwise pull fresh from the Socrata CSV API.
  # --------------------------------------------------------------------------
  school_raw_path <- here("data-raw/cdc_school_vax_view.rds")
  if (!refresh_raw && file.exists(school_raw_path)) {
    df <- readRDS(school_raw_path)
  } else {
    print("---a. pulling School VaxView from data.cdc.gov ...")
    df <- read.csv("https://data.cdc.gov/api/views/ijqb-a7ye/rows.csv?accessType=DOWNLOAD&api_foundry=true")
  }

  # --------------------------------------------------------------------------
  # 2. Filter to exemption rows, retain school-year axis
  # --------------------------------------------------------------------------
  type_levels <- c("Any Exemption" = "any",
                   "Medical Exemption" = "medical",
                   "Non-Medical Exemption" = "non_medical")

  df_long <- df %>%
    filter(
      Vaccine.Exemption == "Exemption",
      Dose %in% names(type_levels),
      Geography.Type %in% c("States", "National"),
      suppressWarnings(as.integer(substr(School.Year, 1, 4))) >= school_min_year_start,
      !Geography %in% geo_exclude
    ) %>%
    transmute(
      source         = "school_vax_view",
      state_name     = Geography,
      school_year    = School.Year,
      year           = suppressWarnings(as.integer(substr(School.Year, 1, 4))),
      exemption_type = unname(type_levels[Dose]),
      exemption_rate = suppressWarnings(as.numeric(Estimate....)),
      population_size = suppressWarnings(as.integer(gsub(",", "", Population.Size))),
      number_of_exemptions = suppressWarnings(as.integer(gsub(",", "", Number.of.Exemptions)))
    ) %>%
    arrange(state_name, year, exemption_type)

  # --------------------------------------------------------------------------
  # 3. Wide: one row per state x school-year, the three rates side by side
  # --------------------------------------------------------------------------
  df_wide <- df_long %>%
    select(source, state_name, school_year, year, exemption_type,
           exemption_rate, population_size, number_of_exemptions) %>%
    pivot_wider(
      names_from  = exemption_type,
      values_from = c(exemption_rate, number_of_exemptions),
      names_glue  = "{exemption_type}_{.value}"
    ) %>%
    rename_with(~ gsub("exemption_rate", "rate", .x)) %>%
    rename_with(~ gsub("number_of_exemptions", "n", .x)) %>%
    arrange(state_name, year)

  # --------------------------------------------------------------------------
  # 4. Write outputs (rds + csv)
  # --------------------------------------------------------------------------
  write_raw <- function(obj, name) {
    saveRDS(obj, file = here(paste0("data-raw/", name, ".rds")))
    write.csv(obj, file = here(paste0("data-raw/csv/", name, ".csv")), row.names = FALSE)
  }
  write_curated <- function(obj, name) {
    saveRDS(obj, file = here(paste0("data/", name, ".rds")))
    write.csv(obj, file = here(paste0("data/csv/", name, ".csv")), row.names = FALSE)
  }

  write_raw(df_long, "cdc_school_vax_view_exemptions_timeseries")
  write_raw(df_wide, "cdc_school_vax_view_exemptions_timeseries_wide")
  write_curated(df_wide, "cdc_school_vax_view_exemptions_timeseries_wide")

  print(paste0("Wrote ", nrow(df_long), " long rows / ", nrow(df_wide),
               " state-year rows across ",
               dplyr::n_distinct(df_long$state_name), " geographies to data-raw/ and data/"))

  invisible(df_long)
}

# Run when sourced directly (matches the style of the existing get_data_cdc.R)
# --------------------------------------------------------------------------
# get_data_cdc_school_exemptions_timeseries()
