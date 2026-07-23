# Validate the model input parameter CSV before running the pipeline
# --------------------------------------------------------------------------
# Catches the ways Excel silently corrupts this file. Run it after ANY manual
# edit, and ideally call it from the top of run_model().
#
#   source("R/validate_model_input.R")
#   validate_model_input()                       # uses the default path
#   validate_model_input(strict = TRUE)          # stop() instead of warn
# --------------------------------------------------------------------------

validate_model_input <- function(path = NULL,
                                 expected_diseases = c("Rotavirus", "Pertussis",
                                                       "Pneumococcal", "Varicella",
                                                       "Hib", "RSV"),
                                 numeric_cols = c("basic_reproduction_number",
                                                  "vaccine_effectiveness",
                                                  "importation_delta",
                                                  "death_rate",
                                                  "proportion_hospitalized_given_case",
                                                  "observed_national_cases",
                                                  "observed_national_hospitalizations",
                                                  "maternal_coverage",
                                                  "maternal_vaccine_effectiveness"),
                                 strict = FALSE) {

  if (is.null(path)) {
    path <- if (requireNamespace("here", quietly = TRUE)) {
      here::here("data-raw/csv/model_input_parameters.csv")
    } else {
      "data-raw/csv/model_input_parameters.csv"
    }
  }
  if (!file.exists(path)) stop("Input file not found: ", path)

  problems <- character(0)
  note <- function(...) problems <<- c(problems, paste0(...))

  cat("Validating:", path, "\n")
  cat(strrep("-", 70), "\n")

  ## 1. Byte-order mark -----------------------------------------------------
  ## A BOM attaches invisible bytes to the FIRST column name, so df$disease
  ## becomes NULL and every disease filter silently returns zero rows.
  con <- file(path, "rb"); first3 <- readBin(con, "raw", 3); close(con)
  if (length(first3) == 3 && all(first3 == as.raw(c(0xEF, 0xBB, 0xBF)))) {
    note("UTF-8 BOM present on the header. Re-save as 'CSV UTF-8' without BOM, ",
         "or read with read.csv(..., fileEncoding = 'UTF-8-BOM').")
  }
  cat("1. BOM check .......................... ",
      if (any(grepl("BOM", problems))) "FAIL\n" else "ok\n", sep = "")

  ## 2. Field counts per line ----------------------------------------------
  ## Catches locale comma-decimals (1,37E-03) and broken quoting around fields
  ## that legitimately contain commas (e.g. the pertussis vaccine field).
  nf <- utils::count.fields(path, sep = ",", quote = "\"")
  expected_n <- nf[1]
  bad_lines <- which(!is.na(nf) & nf != expected_n)
  unbalanced <- which(is.na(nf))
  if (length(unbalanced))
    note("Unbalanced quotes at line(s): ", paste(unbalanced, collapse = ", "))
  if (length(bad_lines))
    note("Wrong field count at line(s): ",
         paste(sprintf("%d (%d vs %d)", bad_lines, nf[bad_lines], expected_n),
               collapse = ", "),
         ". Common cause: a comma decimal separator (1,37E-03) or lost quoting ",
         "around a field containing a comma. This SHIFTS every later column on that row.")
  cat("2. Field counts per line .............. ",
      if (length(bad_lines) || length(unbalanced)) "FAIL\n" else "ok\n", sep = "")

  ## 3. Read the file ------------------------------------------------------
  df <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  cat("3. Parsed ............................. ", nrow(df), " rows, ",
      ncol(df), " columns\n", sep = "")

  ## 4. Blank / filler rows ------------------------------------------------
  ## Excel likes to append rows of bare commas.
  blank <- which(apply(df, 1, function(r) all(is.na(r) | trimws(as.character(r)) == "")))
  if (length(blank))
    note("Blank filler row(s) at data row(s): ", paste(blank, collapse = ", "),
         ". Delete them; they can create phantom groups in joins.")
  cat("4. Blank rows ......................... ",
      if (length(blank)) paste0("FAIL (", length(blank), ")\n") else "ok\n", sep = "")

  ## 5. Disease names ------------------------------------------------------
  ## THE most common silent failure: a trailing space or case change makes
  ## filter(disease == 'Varicella') return zero rows, and the disease simply
  ## vanishes from the output with no error anywhere.
  if (!"disease" %in% names(df)) {
    note("No 'disease' column found. Column names are: ",
         paste(names(df), collapse = ", "))
  } else {
    d <- df$disease
    cat("   disease values seen: ", paste(sprintf("[%s]", unique(d)), collapse = " "), "\n", sep = "")
    ws <- unique(d[d != trimws(d)])
    if (length(ws))
      note("Leading/trailing whitespace in disease name(s): ",
           paste(sprintf("[%s]", ws), collapse = " "),
           ". This silently drops the disease from all output.")
    nbsp <- unique(d[grepl("\u00a0", d)])
    if (length(nbsp))
      note("Non-breaking space in disease name(s): ",
           paste(sprintf("[%s]", nbsp), collapse = " "), " (looks identical to a space).")
    missing <- setdiff(expected_diseases, trimws(d))
    if (length(missing))
      note("Expected disease(s) absent from the input file: ",
           paste(missing, collapse = ", "))
    wrongcase <- expected_diseases[
      tolower(expected_diseases) %in% tolower(trimws(d)) &
        !(expected_diseases %in% trimws(d))]
    if (length(wrongcase))
      note("Case mismatch on disease name(s): expected ",
           paste(wrongcase, collapse = ", "),
           ". Filters are case-sensitive.")
  }
  cat("5. Disease names ...................... ",
      if (any(grepl("disease name|absent from the input", problems))) "FAIL\n" else "ok\n", sep = "")

  ## 6. Numeric columns really are numeric ---------------------------------
  ## Catches thousands separators ("60,000"), stray characters, and any value
  ## Excel wrote as text. A single bad cell turns the whole column character,
  ## which propagates NA through the calibration factor and blanks the disease.
  cat("6. Numeric column integrity:\n")
  for (cl in intersect(numeric_cols, names(df))) {
    raw <- df[[cl]]
    if (is.numeric(raw)) { cat("     ", cl, ": numeric ok\n", sep = ""); next }
    chr <- trimws(as.character(raw))
    coerced <- suppressWarnings(as.numeric(chr))
    offenders <- unique(chr[is.na(coerced) & chr != "" & !is.na(chr) & chr != "NA"])
    if (length(offenders)) {
      note("Column '", cl, "' is not numeric. Non-numeric value(s): ",
           paste(sprintf("[%s]", utils::head(offenders, 5)), collapse = " "))
      cat("     ", cl, ": CHARACTER -> ",
          paste(sprintf("[%s]", utils::head(offenders, 3)), collapse = " "), "\n", sep = "")
    } else {
      cat("     ", cl, ": character but fully coercible (harmless, but re-save cleanly)\n", sep = "")
    }
  }

  ## 7. Scientific notation round-trip ------------------------------------
  ## Confirms small values survived the edit at full precision.
  if ("importation_delta" %in% names(df) && "disease" %in% names(df)) {
    dl <- suppressWarnings(as.numeric(trimws(as.character(df$importation_delta))))
    for (dis in c("Varicella", "Hib")) {
      i <- which(trimws(df$disease) == dis)
      if (length(i)) {
        v <- unique(dl[i])
        cat("7. importation_delta [", dis, "] = ",
            paste(format(v, scientific = TRUE, digits = 6), collapse = ", "), "\n", sep = "")
        if (any(is.na(v)))
          note("importation_delta for ", dis, " is NA. This makes the calibration ",
               "factor NA and blanks every row for that disease.")
        if (any(!is.na(v) & v < 0))
          note("importation_delta for ", dis, " is negative (", v,
               "). Delta must be >= 0; a negative value means R0 is above the ",
               "level the anchor can support.")
      }
    }
  }

  ## 8. Duplicate parameter rows ------------------------------------------
  if (all(c("disease", "vaccine") %in% names(df))) {
    key <- paste(trimws(df$disease), trimws(df$vaccine))
    dup <- unique(key[duplicated(key)])
    if (length(dup))
      note("Duplicate disease/vaccine parameter row(s): ", paste(dup, collapse = ", "),
           ". Joins will fan out and multiply the burden.")
  }

  ## ---- report -----------------------------------------------------------
  cat(strrep("-", 70), "\n")
  if (!length(problems)) {
    cat("PASSED - no input problems detected.\n")
    return(invisible(TRUE))
  }
  msg <- paste0("Input file problems found (", length(problems), "):\n",
                paste0("  ", seq_along(problems), ". ", problems, collapse = "\n"))
  if (strict) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
  cat(msg, "\n")
  invisible(FALSE)
}


# Assert every input disease survived to the output
# --------------------------------------------------------------------------
# The real defect is that a dropped disease is silent. Call this right after
# calibrate() so the pipeline fails loudly instead of quietly omitting a map.
assert_diseases_present <- function(df_out, df_in) {
  want <- sort(unique(trimws(df_in$disease)))
  got  <- sort(unique(trimws(df_out$disease)))
  lost <- setdiff(want, got)
  if (length(lost)) {
    stop("Disease(s) present in the input but MISSING from the calibrated output: ",
         paste(lost, collapse = ", "),
         "\n  Most likely causes:",
         "\n   * disease name mismatch between the CSV and the filter in calibrate_<disease>.R",
         "\n     (check for trailing whitespace: sprintf('[%s]', unique(df$disease)))",
         "\n   * calibrate_<disease>.R not sourced/unioned in calibrate.R",
         "\n   * NA in a required numeric parameter blanking the calibration factor",
         call. = FALSE)
  }
  # all-NA burden is a silent drop in disguise
  for (dis in got) {
    sub <- df_out[trimws(df_out$disease) == dis, ]
    for (cl in intersect(c("cases", "hospitalizations", "deaths"), names(sub))) {
      if (nrow(sub) && all(is.na(sub[[cl]])))
        stop("Every '", cl, "' value is NA for ", dis,
             ". The calibration factor is probably NA - check that the national ",
             "baseline row (state_name == 'United States' & ",
             "declining_coverage_among_new_births == 0) exists and has non-zero ",
             "modeled burden.", call. = FALSE)
    }
  }
  invisible(TRUE)
}
