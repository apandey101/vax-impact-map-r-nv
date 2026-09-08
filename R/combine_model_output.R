# Combine equilibrium-engine output with the age-structured producers into the
# single curated file the front end consumes.
# --------------------------------------------------------------------------
# Flow: run_model() + curate_model_output() produce the curated CSV for the
# equilibrium diseases (Hib and Varicella are already excluded upstream in
# compile_model_input_data.R). This function reads that file, appends the
# age-structured Hib and Varicella rows (ALL-AGES, canonical schema), and
# overwrites the curated CSV/RDS so every disease sits in one file, one format.
#
# Idempotent: any Hib/Varicella rows already present in the input are dropped
# before the producer rows are appended, and equilibrium rows are defensively
# restricted to the five reported decline scenarios.
# --------------------------------------------------------------------------
combine_model_output <- function(
    curated_csv = here::here("data", "csv", "vax_impact_map_model_output_curated.csv"),
    curated_rds = here::here("data", "vax_impact_map_model_output_curated.rds"),
    agestructured_diseases = c("Hib", "Varicella"),
    reported_declines = seq(0, 20, 1),
    write = TRUE,
    verbose = TRUE) {

  for (pkg in c("here")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Install '", pkg, "' to run combine_model_output().")
    }
  }
  source(here::here("R", "build_agestructured_disease_output.R"))

  if (!file.exists(curated_csv)) {
    stop("Equilibrium curated output not found at ", curated_csv,
         ". Run run_model() and curate_model_output() first.")
  }

  # --- equilibrium diseases -------------------------------------------------
  eq <- utils::read.csv(curated_csv, check.names = FALSE,
                        stringsAsFactors = FALSE)
  # write.csv() writes an unnamed row-name column; drop it if it came back.
  if (names(eq)[1] %in% c("", "X", "X.1")) eq <- eq[, -1, drop = FALSE]

  eq <- eq[!(eq$disease %in% agestructured_diseases), , drop = FALSE]
  eq <- eq[eq$percent_decline %in% reported_declines, , drop = FALSE]
  schema <- names(eq)

  # --- age-structured producers --------------------------------------------
  if (verbose) message("Running Hib age-structured producer ...")
  hib <- produce_hib_output(verbose = verbose)
  if (verbose) message("Running Varicella age-structured producer ...")
  vzv <- produce_varicella_output(verbose = verbose)

  align <- function(df, name) {
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    missing <- setdiff(schema, names(df))
    extra   <- setdiff(names(df), schema)
    if (length(missing) > 0) {
      stop(name, " output is missing expected column(s): ",
           paste(missing, collapse = ", "))
    }
    if (length(extra) > 0 && verbose) {
      message(name, " output has extra column(s) dropped for the map file: ",
              paste(extra, collapse = ", "))
    }
    df[, schema, drop = FALSE]
  }

  combined <- rbind(eq, align(hib, "Hib"), align(vzv, "Varicella"))

  # Order for readability: disease, state, decline, horizon.
  ord <- order(combined$disease, combined$state_name,
               combined$percent_decline, combined$accrual_years)
  combined <- combined[ord, , drop = FALSE]
  rownames(combined) <- NULL

  if (verbose) {
    tab <- table(combined$disease)
    message("Combined output: ", nrow(combined), " rows across ",
            length(tab), " diseases (",
            paste(sprintf("%s=%d", names(tab), as.integer(tab)),
                  collapse = ", "), ").")
  }

  if (write) {
    dir.create(dirname(curated_csv), recursive = TRUE, showWarnings = FALSE)
    # Match the existing curate_model_output() convention (write.csv default,
    # which the front end already reads).
    utils::write.csv(combined, file = curated_csv)
    saveRDS(combined, file = curated_rds)
    if (verbose) message("Wrote combined curated output to ", curated_csv)
  }

  invisible(combined)
}
