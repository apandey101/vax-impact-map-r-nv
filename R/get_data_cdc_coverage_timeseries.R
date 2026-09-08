# Source and process CDC vaccination coverage TIME SERIES
# -----------------------------------------------------------------------------
# Produces a single longitudinal coverage file for the six VaxImpactMap
# interventions:
#   Child VaxView : Rotavirus, PCV, and Hib primary series
#   School VaxView: DTaP and varicella
#   IIS RSV data  : infant RSV monoclonal antibody coverage, including the
#                   separately published 2024-25 and 2025-26 CDC datasets
#
# Important definitions
# ---------------------
# * Child VaxView time is the single-year birth cohort.
# * School VaxView time is the starting year of the school year.
# * RSV time is the starting year of the RSV season. The combined file keeps
#   the latest cumulative observation reported by each jurisdiction in each
#   season. The complete monthly RSV series is also written separately.
# * School VaxView currently contains empty "UTD" varicella placeholder rows.
#   Populated data are reported as either "1 Dose (or disease history)" or
#   "2 Doses (or disease history)". For each state-year, this script selects
#   the numeric dose-specific estimate (preferring 2 doses if both are
#   numeric). This recovers the estimate corresponding to the state's school
#   requirement. The United States value is derived by weighting the selected
#   state estimates by the reported kindergarten population.
# * CDC published 2024-25 nirsevimab coverage and 2025-26 RSV monoclonal
#   antibody coverage under separate Socrata dataset IDs. Both are pulled and
#   combined. A separate timeseries cache is also appended/deduplicated so any
#   previously pulled seasons remain available if CDC changes the live feeds.
#
# Outputs
# -------
# Per-intervention intermediate files:
#   data-raw/cdc_child_vax_view_rotavirus_timeseries.{rds,csv}
#   data-raw/cdc_child_vax_view_pcv_timeseries.{rds,csv}
#   data-raw/cdc_child_vax_view_hib_timeseries.{rds,csv}
#   data-raw/cdc_school_vax_view_dtap_timeseries.{rds,csv}
#   data-raw/cdc_school_vax_view_varicella_timeseries.{rds,csv}
#   data-raw/cdc_nirsevimab_coverage_monthly_timeseries.{rds,csv}
#   data-raw/cdc_nirsevimab_coverage_timeseries.{rds,csv}
#
# Combined curated file:
#   data-raw/cdc_coverage_timeseries.{rds,csv}
#   data/cdc_coverage_timeseries.{rds,csv}
#
# CDC sources
# -----------
# Child VaxView : https://data.cdc.gov/resource/fhky-rtsk.csv
# School VaxView: https://data.cdc.gov/resource/ijqb-a7ye.csv
# RSV IIS 2024-25: https://data.cdc.gov/resource/4bdk-kyzv.csv
# RSV IIS 2025-26: https://data.cdc.gov/resource/vhcj-3k53.csv
# -----------------------------------------------------------------------------

get_data_cdc_coverage_timeseries <- function(
    child_min_birth_year = 2019,
    school_min_year_start = 2019,
    rsv_min_season_start = 2023,
    refresh_raw = TRUE,
    preserve_rsv_history = TRUE,
    rsv_vaccine_label = "nirsevimab") {

  # Packages
  # ---------------------------------------------------------------------------
  packages <- c("tidyverse", "here")
  install.packages(setdiff(packages, rownames(installed.packages())))
  invisible(lapply(packages, library, character.only = TRUE))

  suppressMessages(here::i_am("R/get_data_cdc_coverage_timeseries.R"))
  print("--1. get_data_cdc_coverage_timeseries.R")

  # Output directories may not exist in a fresh checkout.
  dir.create(here("data-raw"), recursive = TRUE, showWarnings = FALSE)
  dir.create(here("data-raw/csv"), recursive = TRUE, showWarnings = FALSE)
  dir.create(here("data"), recursive = TRUE, showWarnings = FALSE)
  dir.create(here("data/csv"), recursive = TRUE, showWarnings = FALSE)

  states_dc <- c(state.name, "District of Columbia")
  states_dc_us <- c(states_dc, "United States")

  # Helpers for source files whose displayed column names have changed.
  # ---------------------------------------------------------------------------
  normalize_names <- function(df) {
    nm <- names(df) %>%
      stringr::str_to_lower() %>%
      stringr::str_replace_all("[^a-z0-9]+", "_") %>%
      stringr::str_replace_all("^_+|_+$", "")
    names(df) <- make.unique(nm, sep = "_")
    df
  }

  first_column <- function(df, candidates, default = NA_character_) {
    hit <- intersect(candidates, names(df))
    if (length(hit) == 0L) return(rep(default, nrow(df)))
    as.character(df[[hit[[1L]]]])
  }

  parse_number_safely <- function(x) {
    suppressWarnings(
      readr::parse_number(
        as.character(x),
        na = c("", "NA", "NReq", "Not Submitted", "NULL")
      )
    )
  }

  pull_socrata_csv <- function(dataset_id, limit) {
    url <- paste0(
      "https://data.cdc.gov/resource/", dataset_id,
      ".csv?%24limit=", format(limit, scientific = FALSE, trim = TRUE)
    )
    utils::read.csv(
      url,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      na.strings = character(0)
    )
  }

  standardize_child <- function(df) {
    df <- normalize_names(df)
    tibble::tibble(
      vaccine = first_column(df, c("vaccine")),
      dose = first_column(df, c("dose")),
      geography_type = first_column(df, c("geography_type")),
      geography = first_column(df, c("geography")),
      year_cohort = first_column(
        df,
        c("year_season", "birth_year_birth_cohort")
      ),
      dimension_type = first_column(df, c("dimension_type")),
      dimension = first_column(df, c("dimension")),
      estimate_raw = first_column(
        df,
        c("coverage_estimate", "estimate")
      ),
      ci_95 = first_column(df, c("95_ci", "x95_ci")),
      sample_size_raw = first_column(
        df,
        c("population_sample_size", "sample_size")
      )
    )
  }

  standardize_school <- function(df) {
    df <- normalize_names(df)
    tibble::tibble(
      vaccine = first_column(df, c("vaccine", "vaccine_exemption")),
      dose = first_column(df, c("dose")),
      geography_type = first_column(df, c("geography_type")),
      geography = first_column(df, c("geography")),
      school_year = first_column(df, c("year_season", "school_year")),
      estimate_raw = first_column(
        df,
        c("coverage_estimate", "estimate")
      ),
      sample_size_raw = first_column(
        df,
        c("population_sample_size", "population_size")
      )
    )
  }

  standardize_rsv <- function(df) {
    df <- normalize_names(df)
    tibble::tibble(
      season = first_column(df, c("season")),
      month = stringr::str_to_upper(first_column(df, c("month"))),
      numerator_raw = first_column(df, c("numerator", "numerator_raw")),
      population_raw = first_column(df, c("population", "population_raw")),
      jurisdiction = first_column(df, c("jurisdiction")),
      estimate_raw = first_column(df, c("estimate", "estimate_raw")),
      age_group_label = first_column(df, c("age_group_label"))
    )
  }

  assert_nonempty <- function(df, label) {
    if (nrow(df) == 0L) {
      stop(
        paste0(
          label,
          " returned zero rows. Check the current CDC category labels and ",
          "the minimum-year arguments."
        ),
        call. = FALSE
      )
    }
    invisible(df)
  }

  # 1. Source raw data
  # ---------------------------------------------------------------------------
  child_raw_path <- here("data-raw/cdc_child_vax_view.rds")
  school_raw_path <- here("data-raw/cdc_school_vax_view.rds")
  rsv_raw_path <- here("data-raw/cdc_nirsevimab.rds")
  rsv_history_path <- here("data-raw/cdc_nirsevimab_timeseries_raw.rds")

  if (!refresh_raw && file.exists(child_raw_path)) {
    df_child <- standardize_child(readRDS(child_raw_path))
  } else {
    print("---a. pulling Child VaxView from data.cdc.gov ...")
    df_child <- standardize_child(pull_socrata_csv("fhky-rtsk", 500000L))
  }

  if (!refresh_raw && file.exists(school_raw_path)) {
    df_school <- standardize_school(readRDS(school_raw_path))
  } else {
    print("---b. pulling School VaxView from data.cdc.gov ...")
    df_school <- standardize_school(pull_socrata_csv("ijqb-a7ye", 50000L))
  }

  if (!refresh_raw && file.exists(rsv_history_path)) {
    df_rsv <- standardize_rsv(readRDS(rsv_history_path))
  } else if (!refresh_raw && file.exists(rsv_raw_path)) {
    df_rsv <- standardize_rsv(readRDS(rsv_raw_path))
  } else {
    print("---c. pulling RSV monoclonal antibody coverage from data.cdc.gov ...")

    # CDC stores the two seasons in separate datasets. Standardize and combine
    # them before merging with any locally retained history.
    rsv_2024_25 <- standardize_rsv(
      pull_socrata_csv("4bdk-kyzv", 50000L)
    )
    rsv_2025_26 <- standardize_rsv(
      pull_socrata_csv("vhcj-3k53", 50000L)
    )
    rsv_new <- dplyr::bind_rows(rsv_2025_26, rsv_2024_25) %>%
      dplyr::distinct(
        .data$season,
        .data$month,
        .data$jurisdiction,
        .data$age_group_label,
        .keep_all = TRUE
      )

    if (preserve_rsv_history) {
      old_sources <- list()
      if (file.exists(rsv_history_path)) {
        old_sources[[length(old_sources) + 1L]] <-
          standardize_rsv(readRDS(rsv_history_path))
      }
      if (file.exists(rsv_raw_path)) {
        old_sources[[length(old_sources) + 1L]] <-
          standardize_rsv(readRDS(rsv_raw_path))
      }

      # Fresh rows come first, so they replace older cached versions of the
      # same jurisdiction/season/month observation during distinct().
      df_rsv <- dplyr::bind_rows(c(list(rsv_new), old_sources)) %>%
        dplyr::distinct(
          .data$season,
          .data$month,
          .data$jurisdiction,
          .data$age_group_label,
          .keep_all = TRUE
        )
    } else {
      df_rsv <- rsv_new
    }
  }

  # Save only the dedicated standardized RSV history cache. Do not overwrite
  # cdc_nirsevimab.rds, which may be used by older processing functions that
  # expect the original displayed CDC column names.
  saveRDS(df_rsv, rsv_history_path)

  # 2. Child VaxView series
  # ---------------------------------------------------------------------------
  process_child <- function(
      df,
      vaccine_value,
      dimension_value,
      dose_value = NULL,
      output_vaccine_label = vaccine_value) {

    out <- df %>%
      dplyr::mutate(
        cohort_year = suppressWarnings(as.integer(.data$year_cohort)),
        estimate_num = parse_number_safely(.data$estimate_raw),
        sample_size_num = parse_number_safely(.data$sample_size_raw)
      ) %>%
      dplyr::filter(
        .data$vaccine == vaccine_value,
        .data$dimension_type == "Age",
        .data$dimension == dimension_value,
        stringr::str_detect(.data$year_cohort, "^[0-9]{4}$"),
        .data$cohort_year >= child_min_birth_year,
        .data$geography %in% states_dc_us
      )

    if (!is.null(dose_value)) {
      out <- out %>% dplyr::filter(.data$dose == dose_value)
    }

    out <- out %>%
      dplyr::transmute(
        source = "child_vax_view",
        vaccine = output_vaccine_label,
        state_name = .data$geography,
        year = .data$cohort_year,
        year_type = "birth_cohort",
        vaccine_coverage_estimate = .data$estimate_num,
        ci_95 = dplyr::na_if(.data$ci_95, ""),
        sample_size = .data$sample_size_num
      ) %>%
      dplyr::arrange(.data$state_name, .data$year)

    assert_nonempty(out, paste0("Child VaxView: ", output_vaccine_label))
    out
  }

  df_rotavirus <- process_child(
    df_child,
    vaccine_value = "Rotavirus",
    dimension_value = "8 Months"
  )

  df_pcv <- process_child(
    df_child,
    vaccine_value = "PCV",
    dimension_value = "35 Months",
    dose_value = "≥4 Doses"
  )

  df_hib <- process_child(
    df_child,
    vaccine_value = "Hib",
    dimension_value = "24 Months",
    dose_value = "Primary Series"
  )

  # 3. School VaxView series
  # ---------------------------------------------------------------------------
  process_school_direct <- function(df, vaccine_value) {
    out <- df %>%
      dplyr::mutate(
        year_start = suppressWarnings(
          as.integer(stringr::str_sub(.data$school_year, 1L, 4L))
        ),
        estimate_num = parse_number_safely(.data$estimate_raw),
        sample_size_num = parse_number_safely(.data$sample_size_raw)
      ) %>%
      dplyr::filter(
        .data$vaccine == vaccine_value,
        .data$geography_type %in% c("States", "National"),
        .data$year_start >= school_min_year_start,
        .data$geography %in% states_dc_us
      ) %>%
      dplyr::transmute(
        source = "school_vax_view",
        vaccine = vaccine_value,
        state_name = .data$geography,
        year = .data$year_start,
        year_type = "school_year",
        vaccine_coverage_estimate = .data$estimate_num,
        ci_95 = NA_character_,
        sample_size = .data$sample_size_num
      ) %>%
      dplyr::arrange(.data$state_name, .data$year)

    assert_nonempty(out, paste0("School VaxView: ", vaccine_value))
    out
  }

  df_dtap <- process_school_direct(df_school, "DTP, DTaP, or DT")

  # Varicella requires dose-aware selection because the populated CDC records
  # are dose-specific. In most state-years, the non-required dose is coded
  # NReq. Numeric coverage takes priority; 2 doses is preferred only when both
  # dose rows contain numeric estimates.
  varicella_selected_states <- df_school %>%
    dplyr::mutate(
      year_start = suppressWarnings(
        as.integer(stringr::str_sub(.data$school_year, 1L, 4L))
      ),
      estimate_num = parse_number_safely(.data$estimate_raw),
      sample_size_num = parse_number_safely(.data$sample_size_raw),
      numeric_priority = as.integer(!is.na(.data$estimate_num)),
      dose_priority = dplyr::case_when(
        stringr::str_detect(.data$dose, "^2 Dose") ~ 2L,
        stringr::str_detect(.data$dose, "^1 Dose") ~ 1L,
        TRUE ~ 0L
      )
    ) %>%
    dplyr::filter(
      .data$vaccine == "Varicella",
      .data$geography_type == "States",
      .data$year_start >= school_min_year_start,
      .data$geography %in% states_dc,
      stringr::str_detect(.data$dose, "^(1|2) Dose")
    ) %>%
    dplyr::group_by(.data$geography, .data$year_start, .data$school_year) %>%
    dplyr::arrange(
      dplyr::desc(.data$numeric_priority),
      dplyr::desc(.data$dose_priority),
      .by_group = TRUE
    ) %>%
    dplyr::slice_head(n = 1L) %>%
    dplyr::ungroup()

  assert_nonempty(varicella_selected_states, "School VaxView: Varicella")

  varicella_national <- varicella_selected_states %>%
    dplyr::mutate(
      valid_weight = !is.na(.data$estimate_num) &
        !is.na(.data$sample_size_num) &
        .data$sample_size_num > 0
    ) %>%
    dplyr::group_by(.data$year_start, .data$school_year) %>%
    dplyr::summarise(
      estimate_num = if (any(.data$valid_weight)) {
        stats::weighted.mean(
          .data$estimate_num[.data$valid_weight],
          .data$sample_size_num[.data$valid_weight]
        )
      } else {
        mean(.data$estimate_num, na.rm = TRUE)
      },
      sample_size_num = sum(
        .data$sample_size_num[.data$valid_weight],
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(geography = "United States") %>%
    dplyr::select(
      .data$geography,
      .data$year_start,
      .data$school_year,
      .data$estimate_num,
      .data$sample_size_num
    )

  df_varicella <- dplyr::bind_rows(
    varicella_selected_states %>%
      dplyr::select(
        .data$geography,
        .data$year_start,
        .data$school_year,
        .data$estimate_num,
        .data$sample_size_num
      ),
    varicella_national
  ) %>%
    dplyr::transmute(
      source = "school_vax_view",
      vaccine = "Varicella",
      state_name = .data$geography,
      year = .data$year_start,
      year_type = "school_year",
      vaccine_coverage_estimate = .data$estimate_num,
      ci_95 = NA_character_,
      sample_size = .data$sample_size_num
    ) %>%
    dplyr::arrange(.data$state_name, .data$year)

  # 4. RSV infant monoclonal antibody series
  # ---------------------------------------------------------------------------
  month_order <- c(
    SEP = 1L, OCT = 2L, NOV = 3L, DEC = 4L,
    JAN = 5L, FEB = 6L, MAR = 7L, APR = 8L, MAY = 9L,
    JUN = 10L, JUL = 11L, AUG = 12L
  )

  rsv_clean <- df_rsv %>%
    dplyr::mutate(
      season_start = suppressWarnings(
        as.integer(stringr::str_sub(.data$season, 1L, 4L))
      ),
      month_rank = unname(month_order[.data$month]),
      numerator = parse_number_safely(.data$numerator_raw),
      population = parse_number_safely(.data$population_raw),
      estimate = parse_number_safely(.data$estimate_raw),
      # The CDC estimate is a proportion; preserve it as such internally.
      estimate = dplyr::if_else(.data$estimate > 1, .data$estimate / 100, .data$estimate),
      state_name = dplyr::case_when(
        .data$jurisdiction %in% c(
          "New York City",
          "New York (excluding New York City)"
        ) ~ "New York",
        .data$jurisdiction %in% c(
          "Philadelphia",
          "Pennsylvania (excluding Philadelphia County)"
        ) ~ "Pennsylvania",
        TRUE ~ stringr::str_trim(.data$jurisdiction)
      )
    ) %>%
    dplyr::filter(
      .data$age_group_label == "0-7 months",
      .data$season_start >= rsv_min_season_start,
      !is.na(.data$month_rank),
      .data$state_name %in% states_dc
    )

  assert_nonempty(rsv_clean, "RSV monoclonal antibody coverage")

  # Reaggregate the explicitly separate New York City and Philadelphia rows
  # into their states using counts. Chicago and Houston are not added because
  # their parent-state rows are not labeled as excluding those jurisdictions.
  rsv_state_month <- rsv_clean %>%
    dplyr::group_by(
      .data$season,
      .data$season_start,
      .data$month,
      .data$month_rank,
      .data$state_name
    ) %>%
    dplyr::summarise(
      numerator = if (all(is.na(.data$numerator))) {
        NA_real_
      } else {
        sum(.data$numerator, na.rm = TRUE)
      },
      population = if (all(is.na(.data$population))) {
        NA_real_
      } else {
        sum(.data$population, na.rm = TRUE)
      },
      fallback_estimate = dplyr::first(.data$estimate[!is.na(.data$estimate)], default = NA_real_),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      estimate = dplyr::if_else(
        !is.na(.data$population) & .data$population > 0,
        .data$numerator / .data$population,
        .data$fallback_estimate
      )
    )

  # Keep the complete cumulative monthly series as an intermediate output.
  df_rsv_monthly <- rsv_state_month %>%
    dplyr::transmute(
      source = "cdc_iis",
      vaccine = rsv_vaccine_label,
      state_name = .data$state_name,
      season = .data$season,
      season_start = .data$season_start,
      month = .data$month,
      month_rank = .data$month_rank,
      vaccine_coverage_estimate = 100 * .data$estimate,
      sample_size = .data$population
    ) %>%
    dplyr::arrange(
      .data$state_name,
      .data$season_start,
      .data$month_rank
    )

  # For the combined annual/seasonal file, take the latest nonmissing
  # cumulative estimate within each state and season.
  rsv_latest_observed <- rsv_state_month %>%
    dplyr::filter(!is.na(.data$estimate)) %>%
    dplyr::group_by(.data$season, .data$season_start, .data$state_name) %>%
    dplyr::slice_max(
      order_by = .data$month_rank,
      n = 1L,
      with_ties = FALSE
    ) %>%
    dplyr::ungroup()

  rsv_season_grid <- tidyr::crossing(
    season = sort(unique(rsv_clean$season)),
    state_name = states_dc
  ) %>%
    dplyr::mutate(
      season_start = suppressWarnings(
        as.integer(stringr::str_sub(.data$season, 1L, 4L))
      )
    )

  rsv_states_season_end <- rsv_season_grid %>%
    dplyr::left_join(
      rsv_latest_observed %>%
        dplyr::select(
          .data$season,
          .data$season_start,
          .data$state_name,
          .data$estimate,
          .data$population
        ),
      by = c("season", "season_start", "state_name")
    )

  rsv_national <- rsv_latest_observed %>%
    dplyr::group_by(.data$season, .data$season_start) %>%
    dplyr::summarise(
      numerator = sum(.data$numerator, na.rm = TRUE),
      population = sum(.data$population, na.rm = TRUE),
      estimate = dplyr::if_else(
        .data$population > 0,
        .data$numerator / .data$population,
        NA_real_
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(state_name = "United States") %>%
    dplyr::select(
      .data$season,
      .data$season_start,
      .data$state_name,
      .data$estimate,
      .data$population
    )

  df_rsv_season_end <- dplyr::bind_rows(
    rsv_states_season_end,
    rsv_national
  ) %>%
    dplyr::transmute(
      source = "cdc_iis",
      vaccine = rsv_vaccine_label,
      state_name = .data$state_name,
      year = .data$season_start,
      year_type = "rsv_season",
      vaccine_coverage_estimate = 100 * .data$estimate,
      ci_95 = NA_character_,
      sample_size = .data$population
    ) %>%
    dplyr::arrange(.data$state_name, .data$year)

  # 5. Combined tidy series across all six interventions
  # ---------------------------------------------------------------------------
  df_combined <- dplyr::bind_rows(
    df_rotavirus,
    df_pcv,
    df_dtap,
    df_varicella,
    df_hib,
    df_rsv_season_end
  ) %>%
    dplyr::mutate(
      disease = dplyr::case_when(
        .data$vaccine == "Rotavirus" ~ "Rotavirus",
        .data$vaccine == "PCV" ~ "Pneumococcal",
        .data$vaccine == "DTP, DTaP, or DT" ~ "Pertussis",
        .data$vaccine == "Varicella" ~ "Varicella",
        .data$vaccine == "Hib" ~ "Hib",
        .data$vaccine == rsv_vaccine_label ~ "RSV",
        TRUE ~ NA_character_
      ),
      vaccine_coverage_estimate = dplyr::if_else(
        .data$vaccine_coverage_estimate >= 0 &
          .data$vaccine_coverage_estimate <= 100,
        .data$vaccine_coverage_estimate,
        NA_real_
      )
    ) %>%
    dplyr::relocate(disease, .after = source) %>%
    dplyr::arrange(.data$source, .data$vaccine, .data$state_name, .data$year)

  duplicate_keys <- df_combined %>%
    dplyr::count(
      .data$source,
      .data$vaccine,
      .data$state_name,
      .data$year,
      name = "n"
    ) %>%
    dplyr::filter(.data$n > 1L)

  if (nrow(duplicate_keys) > 0L) {
    stop(
      "The combined coverage series contains duplicate intervention-state-year rows.",
      call. = FALSE
    )
  }

  expected_vaccines <- c(
    "Rotavirus", "PCV", "DTP, DTaP, or DT",
    "Varicella", "Hib", rsv_vaccine_label
  )
  missing_vaccines <- setdiff(expected_vaccines, unique(df_combined$vaccine))
  if (length(missing_vaccines) > 0L) {
    stop(
      paste0(
        "Missing expected coverage series: ",
        paste(missing_vaccines, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (any(is.na(df_combined$disease))) {
    stop(
      "At least one vaccine in the combined series is missing a disease mapping.",
      call. = FALSE
    )
  }

  # 6. Write outputs
  # ---------------------------------------------------------------------------
  write_raw <- function(obj, name) {
    saveRDS(obj, file = here(paste0("data-raw/", name, ".rds")))
    utils::write.csv(
      obj,
      file = here(paste0("data-raw/csv/", name, ".csv")),
      row.names = FALSE
    )
  }

  write_curated <- function(obj, name) {
    saveRDS(obj, file = here(paste0("data/", name, ".rds")))
    utils::write.csv(
      obj,
      file = here(paste0("data/csv/", name, ".csv")),
      row.names = FALSE
    )
  }

  write_raw(df_rotavirus, "cdc_child_vax_view_rotavirus_timeseries")
  write_raw(df_pcv, "cdc_child_vax_view_pcv_timeseries")
  write_raw(df_hib, "cdc_child_vax_view_hib_timeseries")
  write_raw(df_dtap, "cdc_school_vax_view_dtap_timeseries")
  write_raw(df_varicella, "cdc_school_vax_view_varicella_timeseries")
  write_raw(df_rsv_monthly, "cdc_nirsevimab_coverage_monthly_timeseries")
  write_raw(df_rsv_season_end, "cdc_nirsevimab_coverage_timeseries")

  write_raw(df_combined, "cdc_coverage_timeseries")
  write_curated(df_combined, "cdc_coverage_timeseries")

  print(paste0(
    "Wrote ", nrow(df_combined),
    " rows across ", dplyr::n_distinct(df_combined$vaccine),
    " intervention series to data-raw/ and data/ (+ csv/ subfolders)."
  ))

  invisible(df_combined)
}

# Run when sourced directly, if desired:
# get_data_cdc_coverage_timeseries()