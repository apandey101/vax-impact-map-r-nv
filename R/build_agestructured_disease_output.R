# Age-structured disease producers for the vaccine-impact map.
# --------------------------------------------------------------------------
# Hib and Varicella are modelled with the age-structured transmission models in
# run_hib_agestructured.R and run_varicella_agestructured.R rather than the
# equilibrium engine. Each producer below assembles the inputs those models
# need (parameters, coverage, population, contact matrix, and - for Hib -
# surveillance rates), runs the model, and returns the curated ALL-AGES output
# in the canonical 40-column schema used by curate_model_output(). The combiner
# (combine_model_output.R) merges these with the equilibrium diseases.
#
# These functions are side-effect-free: they read inputs and return a
# data.frame; nothing is written here. Assumes tidyverse/here are attached by
# the caller (main.R does this).
# --------------------------------------------------------------------------

# Filter model_input_parameters.csv to one disease and coerce numeric fields.
# The models read params$<field>; the equilibrium CSV stores everything as text
# alongside the *_source columns, so we coerce the fields the models use.
agestructured_read_params <- function(disease, params_csv) {
  pt <- utils::read.csv(params_csv, check.names = FALSE,
                        stringsAsFactors = FALSE)
  row <- pt[!is.na(pt$disease) & pt$disease == disease, , drop = FALSE]
  if (nrow(row) != 1) {
    stop("Expected exactly one '", disease, "' row in ", params_csv,
         " (found ", nrow(row), ").")
  }
  num_fields <- c(
    "basic_reproduction_number", "vaccine_effectiveness", "waning_rate_annual",
    "death_rate", "proportion_hospitalized_given_case",
    "duration_infectious_days", "duration_sick_days", "duration_hospitalized_days",
    "cost_hospitalization_daily", "cost_wage_daily", "observed_national_cases",
    "observed_national_hospitalizations", "observed_national_deaths",
    "severe_adverse_event_rate", "importation_delta", "external_foi_annual",
    "ppv_50plus", "ve_carriage", "booster_given_primary"
  )
  for (f in intersect(num_fields, names(row))) {
    row[[f]] <- suppressWarnings(as.numeric(row[[f]]))
  }
  row
}

# Coverage: the models expect a PROPORTION (0-1) and apply declines additively
# (coverage - decline). The CDC coverage files store PERCENT (e.g. 92, 93.9),
# so divide by 100 - matching what the equilibrium pipeline does internally.
agestructured_read_coverage <- function(coverage_csv) {
  cov <- utils::read.csv(coverage_csv, check.names = FALSE,
                         stringsAsFactors = FALSE)
  if (!all(c("state_name", "vaccine_coverage_estimate") %in% names(cov))) {
    stop("Coverage file ", coverage_csv,
         " must have state_name and vaccine_coverage_estimate columns.")
  }
  cov <- cov[, c("state_name", "vaccine_coverage_estimate"), drop = FALSE]
  cov$vaccine_coverage_estimate <-
    suppressWarnings(as.numeric(cov$vaccine_coverage_estimate)) / 100
  cov
}

# Population in the model's own age bands (produced by the get_data ACS module).
agestructured_read_population <- function(pop_csv) {
  if (!file.exists(pop_csv)) {
    stop("Population file ", pop_csv, " not found. Run the ACS pull first ",
         "(get_data_census_acs_state_population_{hib,vzv}_bands()).")
  }
  utils::read.csv(pop_csv, check.names = FALSE, stringsAsFactors = FALSE)
}


# ---------------------------------------------------------------------------
# Hib producer
# ---------------------------------------------------------------------------
# Seeding term: the age-structured Hib model reads external_foi_annual (default
# 1e-5 via hib_num_param), NOT importation_delta. To control Hib seeding from
# the parameter file, add an `external_foi_annual` column to
# model_input_parameters.csv - it flows through automatically. importation_delta
# is an equilibrium-era term and is IGNORED by this model.
produce_hib_output <- function(
    params_csv          = here::here("data-raw", "csv", "model_input_parameters.csv"),
    coverage_csv        = here::here("data-raw", "csv", "cdc_child_vax_view_hib.csv"),
    pop_csv             = here::here("data-raw", "csv", "census_acs_state_population_hib_bands.csv"),
    contact_matrix_path = here::here("data-raw", "csv", "engaged_contact_matrix.csv"),
    hib_age_rates       = here::here("data-raw", "csv", "hib_age_rates.csv"),
    hib_trend           = here::here("data-raw", "csv", "hib_trend.csv"),
    declines = seq(0,0.2,0.01),
    horizons = c(1, 5, 10, 20),
    verbose  = FALSE) {

  if (!exists("hib_agestructured_main", mode = "function")) {
    source(here::here("R", "run_hib_agestructured.R"))
  }

  params <- agestructured_read_params("Hib", params_csv)

  R0_pop <- suppressWarnings(as.numeric(params$basic_reproduction_number))
  if (length(R0_pop) != 1 || is.na(R0_pop)) R0_pop <- 1.4

  coverage_df <- agestructured_read_coverage(coverage_csv)
  pop_df      <- agestructured_read_population(pop_csv)

  res <- hib_agestructured_main(
    coverage_df         = coverage_df,
    pop_df              = pop_df,
    params              = params,
    contact_matrix_path = contact_matrix_path,
    hib_age_rates       = hib_age_rates,
    hib_trend           = hib_trend,
    declines            = declines,
    horizons            = horizons,
    R0_pop              = R0_pop,
    write               = FALSE,
    verbose             = verbose
  )
  as.data.frame(res$curated, stringsAsFactors = FALSE)
}


# ---------------------------------------------------------------------------
# Varicella producer
# ---------------------------------------------------------------------------
produce_varicella_output <- function(
    params_csv          = here::here("data-raw", "csv", "model_input_parameters.csv"),
    coverage_csv        = here::here("data-raw", "csv", "cdc_school_vax_view_varicella.csv"),
    pop_csv             = here::here("data-raw", "csv", "census_acs_state_population_vzv_bands.csv"),
    contact_matrix_path = here::here("data-raw", "csv", "engaged_contact_matrix.csv"),
    declines = seq(0,0.2,0.01),
    horizons = c(1, 5, 10, 20),
    verbose  = FALSE) {

  if (!exists("varicella_agestructured_main", mode = "function")) {
    source(here::here("R", "run_varicella_agestructured.R"))
  }

  params <- agestructured_read_params("Varicella", params_csv)

  # ppv_50plus is required by the h-calibration (passing NULL breaks the target
  # builder). Default to 1 = no PPV correction if the column is absent.
  ppv <- suppressWarnings(as.numeric(params$ppv_50plus))
  params$ppv_50plus <- if (length(ppv) != 1 || is.na(ppv)) 1 else ppv

  # The model validates against params$observed_current_hospitalizations; the CSV
  # names this observed_national_hospitalizations. Map it (validation only, does
  # not affect calibration or the reported burden).
  if (is.null(params$observed_current_hospitalizations)) {
    params$observed_current_hospitalizations <-
      params$observed_national_hospitalizations
  }

  coverage_df <- agestructured_read_coverage(coverage_csv)
  pop_df      <- agestructured_read_population(pop_csv)

  res <- varicella_agestructured_main(
    coverage_df         = coverage_df,
    pop_df              = pop_df,
    params              = params,
    contact_matrix_path = contact_matrix_path,
    declines            = declines,
    horizons            = horizons,
    write               = FALSE
  )
  as.data.frame(res$curated, stringsAsFactors = FALSE)
}
