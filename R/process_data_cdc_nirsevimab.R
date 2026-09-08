# Process CDC nirsevimab / clesrovimab coverage into a state-level RSV mAb
# coverage file for the pipeline.
# --------------------------------------------------------------------------
# Source dataset (vhcj-3k53): monthly CUMULATIVE % of children <8 months who
# received >=1 mAb dose, by jurisdiction. Columns: Season, Month, Numerator,
# Population, Jurisdiction, Estimate (a PROPORTION 0-1), Age_group_label.
#
# Handling / assumptions (flag if any should change):
#  * Age group: keep "0-7 months" (the <8-month mAb target).
#  * Late-season estimate: cumulative coverage is monotone over a season, so per
#    jurisdiction we take the LATEST reporting month of the most recent season
#    (the end-of-season cumulative value).
#  * No national row is provided; "United States" is built from sum(Numerator) /
#    sum(Population) over the 50 states + DC that reported.
#  * New York City reports separately from New York State, and Philadelphia
#    separately from Pennsylvania; each is re-aggregated into its state using
#    COUNTS, then the estimate is recomputed. (Chicago is already within Illinois.)
#  * Territories are dropped to match the map's 50 states + DC + US geography.
#  * Non-reporting states ("Not Submitted") are imputed with the national estimate
#    so every state appears in the map.
#  * Output vaccine_coverage_estimate is in PERCENT (Estimate * 100), matching the
#    other coverage files (compile_model_input_data divides by 100).
# --------------------------------------------------------------------------
process_data_cdc_nirsevimab <- function() {
  packages <- c("tidyverse", "here")
  install.packages(setdiff(packages, rownames(installed.packages())))
  invisible(lapply(packages, library, character.only = TRUE))
  suppressMessages(here::i_am("R/process_data_cdc_nirsevimab.R"))
  print("---. process_data_cdc_nirsevimab.R")

  # Read the raw pull. Use the .rds (saved directly by get_data_cdc_nirsevimab),
  # NOT the .csv: write.csv() prepends an unnamed row-number column, and dplyr
  # refuses to operate on a data frame that has a blank ("") column name.
  raw <- readRDS(here("data-raw/cdc_nirsevimab.rds"))

  # -- clean fields --
  raw <- raw %>%
    filter(.data$Age_group_label == "0-7 months") %>%
    mutate(
      numerator  = suppressWarnings(as.numeric(gsub(",", "", .data$Numerator))),
      population = suppressWarnings(as.numeric(.data$Population)),
      estimate   = suppressWarnings(as.numeric(.data$Estimate))
    )

  # -- most recent season (parse the start year of "YYYY-YY") --
  season_start <- suppressWarnings(as.numeric(substr(raw$Season, 1, 4)))
  latest_season <- raw$Season[which.max(season_start)]
  raw <- raw %>% filter(.data$Season == latest_season)

  # -- late-season month per jurisdiction (end-of-season cumulative) --
  month_order <- c(SEP = 1, OCT = 2, NOV = 3, DEC = 4, JAN = 5,
                   FEB = 6, MAR = 7, APR = 8, MAY = 9)
  raw <- raw %>%
    mutate(month_rank = month_order[toupper(.data$Month)]) %>%
    filter(!is.na(.data$estimate), !is.na(.data$month_rank)) %>%
    group_by(.data$Jurisdiction) %>%
    slice_max(order_by = .data$month_rank, n = 1, with_ties = FALSE) %>%
    ungroup()

  # -- re-aggregate sub-state jurisdictions into their states (by counts) --
  merge_into <- function(df, sub, state) {
    s <- df %>% filter(.data$Jurisdiction == sub)
    if (nrow(s) == 0) return(df)
    df %>%
      mutate(
        numerator  = ifelse(.data$Jurisdiction == state,
                            .data$numerator  + sum(s$numerator,  na.rm = TRUE),
                            .data$numerator),
        population = ifelse(.data$Jurisdiction == state,
                            .data$population + sum(s$population, na.rm = TRUE),
                            .data$population)
      ) %>%
      filter(.data$Jurisdiction != sub)
  }
  raw <- merge_into(raw, "New York City", "New York")
  raw <- merge_into(raw, "Philadelphia",  "Pennsylvania")
  raw <- raw %>%
    mutate(estimate = ifelse(!is.na(.data$population) & .data$population > 0,
                             .data$numerator / .data$population, .data$estimate))

  # -- restrict to 50 states + DC (drop territories) --
  states_dc <- c(state.name, "District of Columbia")  #removing dc for now , "District of Columbia"
  reporting <- raw %>% filter(.data$Jurisdiction %in% states_dc)

  # -- national row from reporting states + DC --
# This is a population-weighted aggregate among reporting jurisdictions,
# not a nationally representative estimate.
denom <- sum(reporting$population, na.rm = TRUE)
if (!is.finite(denom) || denom <= 0) {
  stop('Unable to compute national mAb coverage: reporting population denominator is missing/zero.')
}

national_estimate <- sum(reporting$numerator, na.rm = TRUE) / denom
# -- assemble all 50 states + DC + United States --
# Non-reporting states remain NA. No state-level imputation occurs here.
out <- data.frame(
  state_name = c(states_dc, "United States"),
  stringsAsFactors = FALSE
) %>%
  left_join(
    reporting %>% select(Jurisdiction, estimate),
    by = c("state_name" = "Jurisdiction")
  ) %>%
  mutate(
    # Retain the reporting-jurisdiction aggregate for the US row
    estimate = ifelse(
      .data$state_name == "United States",
      national_estimate,
      .data$estimate
    ),
    vaccine = "nirsevimab",
    vaccine_coverage_estimate = 100 * .data$estimate
  ) %>%
  select(
    vaccine,
    state_name,
    vaccine_coverage_estimate
  )

# -- write output in percent units --
saveRDS(
  out,
  here("data-raw/cdc_nirsevimab_coverage.rds")
)

write.csv(
  out,
  here("data-raw/csv/cdc_nirsevimab_coverage.csv"),
  row.names = FALSE
)
# -- load and run the statistical imputation model --
source(
  here("R/impute_rsv_mab_coverage.R"),
  local = environment()
)

imputation_results <- impute_rsv_mab_coverage(
  input_dir = here("data-raw/csv"),
  output_dir = here("data-raw/csv"),
  final_model = "knn7",
  make_plots = TRUE
)

# Return the final dataframe containing observed and predicted coverage
return(imputation_results$coverage)
}
