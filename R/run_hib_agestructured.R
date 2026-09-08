# Hib: deterministic-cohort, age-structured carriage model
# =============================================================================
#
# PURPOSE
# -------
# Estimate the annual all-age health and economic burden of invasive
# Haemophilus influenzae type b (Hib) disease following sustained declines in
# Hib primary-series coverage among new birth cohorts.
#
# Hib transmission is represented through nasopharyngeal carriage. Invasive
# disease is a rare, age-specific outcome of a new Hib carriage acquisition.
# The transmission coefficient is calibrated once nationally to R0 = 1.3 by
# default and then held fixed across states.
#
# IMPORTANT REVISION: DETERMINISTIC COHORT AGING
# ----------------------------------------------
# The previous version moved people between broad age groups using constant
# aging hazards. That makes residence time in an age group exponentially
# distributed and allows some of a newly affected birth cohort to appear in
# chronologically impossible older ages.
#
# This version follows monthly birth cohorts deterministically from birth
# through the twentieth birthday:
#
#   month 0 -> month 1 -> ... -> month 239 -> age 20.
#
# The vaccination-history composition of a cohort therefore advances exactly
# one month per model month. During the supported 20-year projection horizon:
#
#   * a decline beginning at birth cannot directly affect people older than the
#     elapsed time;
#   * older age groups can still change immediately through the force of
#     infection, which is a legitimate indirect effect; and
#   * no exponential/Erlang approximation is used for cohort replacement.
#
# Adults age 20 and older remain five broad transmission groups. Their baseline
# vaccination composition is held fixed during a projection because a cohort
# born after the coverage change does not contribute person-time at age 20 or
# older before the end of the supported 20-year horizon. Extend the
# deterministic cohort grid before using this script beyond 20 years.
#
# MODEL STRUCTURE
# ---------------
# 1. Three vaccination histories:
#      U = does not complete the primary series
#      P = completes the primary series but not the booster
#      B = completes the primary series and the booster
#    The conditional probability of receiving the booster among primary-series
#    completers is fixed, so P and B decline proportionally.
#
# 2. Three carriage/immunity states within each vaccination history:
#      S = susceptible/uncolonized
#      C = colonized
#      R = temporarily protected after carriage
#    Clearance moves C -> R; natural immunity wanes R -> S.
#
# 3. Separate vaccine effects against carriage and invasive disease.
#    The supplied 0.92 efficacy estimate is used against invasive Hib disease,
#    not automatically as the effect against carriage.
#
# 4. The supplied eight-band ENGAGED matrix is retained for transmission:
#      <8mo, 8-19mo, 20mo-4y, 5-17y,
#      18-49y, 50-59y, 60-74y, 75+y.
#    Monthly cohorts are aggregated to these bands for force-of-infection
#    calculations, avoiding a large 245 x 245 contact matrix.
#
# 5. The probability of invasive disease after acquisition is calibrated by
#    age to the HIB COLUMN ONLY of the pooled 2020-2024 rate table. Hia, other
#    non-b serotypes, and nontypeable infections are never used.
#
# 6. "Year 5 burden" means burden during years 4-5. Outputs at years 1, 5, 10,
#    and 20 are trailing 12-month annual burdens, not cumulative burdens.
#
# IDENTIFIABILITY LIMITATION
# --------------------------
# Current invasive incidence cannot separately identify external acquisition,
# carriage prevalence, and the probability of invasion after acquisition.
# R0, carriage duration, vaccine effect on carriage, natural-immunity waning,
# and external force of infection should be treated as sensitivity parameters.
#
# REFERENCES SUPPORTING THE STRUCTURE
# -----------------------------------
# Jackson ML et al. Emerg Infect Dis. 2012;18:13-20.
#   https://wwwnc.cdc.gov/eid/article/18/1/11-0336_article
# Charania NA, Moghadas SM. BMC Public Health. 2017;17:705.
#   https://doi.org/10.1186/s12889-017-4735-9
# Griffiths UK et al. Epidemiol Infect. 2012;140:1343-1355.
#   https://doi.org/10.1017/S0950268812000957
# Barbour ML. Emerg Infect Dis. 1996;2:176-182.
#   https://wwwnc.cdc.gov/eid/article/2/3/96-0303_article
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Constants and utilities
# -----------------------------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

HIB_PARENT_LABELS <- c(
  "<8mo", "8-19mo", "20mo-4y", "5-17y",
  "18-49y", "50-59y", "60-74y", "75+y"
)

# These are the reporting/calibration groups retained from the earlier script.
HIB_INTERNAL_LABELS <- c(
  "0-1mo", "2-3mo", "4-5mo", "6-7mo", "8-11mo", "12-19mo",
  "20mo-4y", "5-17y", "18-49y", "50-59y", "60-64y", "65-74y",
  "75+y"
)
HIB_OUTPUT_LABELS <- HIB_INTERNAL_LABELS

HIB_INTERNAL_EDGES <- list(
  c(0, 2 / 12), c(2 / 12, 4 / 12), c(4 / 12, 6 / 12),
  c(6 / 12, 8 / 12), c(8 / 12, 1), c(1, 20 / 12),
  c(20 / 12, 5), c(5, 18), c(18, 50), c(50, 60),
  c(60, 65), c(65, 75), c(75, 100)
)

# Parent contact group for each reporting group.
HIB_INTERNAL_PARENT <- c(1, 1, 1, 1, 2, 2, 3, 4, 5, 6, 7, 7, 8)
HIB_COARSE_LABELS <- c("0-4", "5-17", "18-49", "50-59", "60-74", "75+")

# Fine model grid: 240 monthly cohorts from 0 through 19y11m, followed by five
# broad adult groups. Adults are present for transmission and disease outcomes,
# but post-decline cohorts do not enter them within the supported horizon.
HIB_CHILD_MONTHS <- 0:239
HIB_CHILD_LABELS <- sprintf("age_%03d_months", HIB_CHILD_MONTHS)
HIB_ADULT_LABELS <- c("20-49y", "50-59y", "60-64y", "65-74y", "75+y")
HIB_CELL_LABELS <- c(HIB_CHILD_LABELS, HIB_ADULT_LABELS)


hib_require_packages <- function() {
  needed <- c("dplyr", "tibble")
  missing <- needed[
    !vapply(needed, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing) > 0) {
    stop(
      "Install the following R packages before running the Hib model: ",
      paste(missing, collapse = ", ")
    )
  }
}


hib_as_params <- function(params) {
  if (is.data.frame(params)) {
    if (nrow(params) != 1) stop("params must contain exactly one Hib row.")
    params <- as.list(params[1, , drop = FALSE])
  }
  if (!is.list(params)) stop("params must be a named list or one-row data.frame.")
  params
}


hib_num_param <- function(params, name, default = NULL, required = FALSE) {
  x <- params[[name]]
  missing <- is.null(x) || length(x) == 0 || all(is.na(x)) ||
    (is.character(x) && all(trimws(x) == ""))
  if (missing) {
    if (required) stop("Missing required parameter: ", name)
    return(default)
  }
  original_names <- names(x)
  out <- suppressWarnings(as.numeric(x))
  if (anyNA(out)) stop("Parameter '", name, "' must be numeric.")
  names(out) <- original_names
  out
}


hib_band_widths <- function(edges = HIB_INTERNAL_EDGES) {
  vapply(edges, function(z) z[2] - z[1], numeric(1))
}


hib_normalize_label <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("\ufeff", "", x, fixed = TRUE)
  x <- gsub("\u2265", ">=", x, fixed = TRUE)
  x <- gsub("\u2013|\u2014", "-", x)
  tolower(gsub("\\s+", "", x))
}


hib_birth_shares <- function(primary_coverage, booster_given_primary) {
  v <- min(max(as.numeric(primary_coverage), 0), 1)
  b <- min(max(as.numeric(booster_given_primary), 0), 1)
  c(
    unvaccinated = 1 - v,
    primary_only = v * (1 - b),
    primary_booster = v * b
  )
}


# -----------------------------------------------------------------------------
# 1. Contact matrix and population mapping
# -----------------------------------------------------------------------------

hib_read_contact_matrix <- function(path) {
  raw <- utils::read.csv(
    path, check.names = FALSE, stringsAsFactors = FALSE,
    fileEncoding = "UTF-8-BOM"
  )
  if (ncol(raw) < 2) stop("Contact matrix CSV has fewer than two columns.")
  C <- as.matrix(raw[, -1, drop = FALSE])
  storage.mode(C) <- "double"
  labs <- trimws(colnames(raw)[-1])
  if (!all(dim(C) == length(HIB_PARENT_LABELS))) {
    stop("The Hib model requires the eight-band ENGAGED contact matrix.")
  }
  if (any(!is.finite(C)) || any(C < 0)) {
    stop("Contact matrix contains missing, non-finite, or negative values.")
  }
  dimnames(C) <- list(labs, labs)
  list(C = C, labels = labs)
}


# Enforce N_i c_ij = N_j c_ji for the modeled population.
hib_balance_contacts <- function(C, N) {
  M <- sweep(C, 1, N, "*")
  M <- 0.5 * (M + t(M))
  out <- sweep(M, 1, N, "/")
  dimnames(out) <- dimnames(C)
  out
}


hib_split_parent_population <- function(parent_pop) {
  if (is.null(names(parent_pop)) ||
      !all(HIB_PARENT_LABELS %in% names(parent_pop))) {
    stop("parent_pop must be named with all eight ENGAGED parent labels.")
  }
  parent_pop <- as.numeric(parent_pop[HIB_PARENT_LABELS])
  widths <- hib_band_widths()
  out <- numeric(length(HIB_INTERNAL_LABELS))
  for (j in seq_along(HIB_PARENT_LABELS)) {
    idx <- which(HIB_INTERNAL_PARENT == j)
    out[idx] <- parent_pop[j] * widths[idx] / sum(widths[idx])
  }
  names(out) <- HIB_INTERNAL_LABELS
  out
}


hib_coarse_to_parent_population <- function(coarse_pop) {
  if (is.null(names(coarse_pop)) ||
      !all(HIB_COARSE_LABELS %in% names(coarse_pop))) {
    stop("coarse_pop must be named with the six supported coarse labels.")
  }
  coarse_pop <- as.numeric(coarse_pop[HIB_COARSE_LABELS])
  names(coarse_pop) <- HIB_COARSE_LABELS
  under5_width <- c(8 / 12, 12 / 12, 5 - 20 / 12)
  under5 <- coarse_pop["0-4"] * under5_width / sum(under5_width)
  out <- c(
    under5,
    coarse_pop["5-17"],
    coarse_pop["18-49"],
    coarse_pop["50-59"],
    coarse_pop["60-74"],
    coarse_pop["75+"]
  )
  names(out) <- HIB_PARENT_LABELS
  out
}


hib_get_internal_population <- function(pop_df, state_name) {
  required <- c("state_name", "age_group", "age_group_population")
  if (!all(required %in% names(pop_df))) {
    stop("pop_df must contain: ", paste(required, collapse = ", "))
  }

  p <- pop_df[pop_df$state_name == state_name, required, drop = FALSE]
  if (nrow(p) == 0) return(NULL)
  p$age_group <- trimws(as.character(p$age_group))
  p$age_group_population <- as.numeric(p$age_group_population)
  if (anyNA(p$age_group_population)) return(NULL)
  vals <- stats::setNames(p$age_group_population, p$age_group)

  if (all(HIB_INTERNAL_LABELS %in% names(vals))) {
    return(as.numeric(vals[HIB_INTERNAL_LABELS]))
  }
  if (all(HIB_PARENT_LABELS %in% names(vals))) {
    return(as.numeric(hib_split_parent_population(vals[HIB_PARENT_LABELS])))
  }

  alias <- c(
    "0-4y" = "0-4", "5-17y" = "5-17", "18-49y" = "18-49",
    "50-59y" = "50-59", "60-74y" = "60-74", "75+y" = "75+"
  )
  for (old in names(alias)) {
    if (old %in% names(vals) && !(alias[[old]] %in% names(vals))) {
      vals[alias[[old]]] <- vals[old]
    }
  }
  if (all(HIB_COARSE_LABELS %in% names(vals))) {
    parent <- hib_coarse_to_parent_population(vals[HIB_COARSE_LABELS])
    return(as.numeric(hib_split_parent_population(parent)))
  }
  NULL
}


# Convert 13 reporting populations to the fine deterministic-cohort grid.
hib_make_cell_grid <- function(N_output) {
  if (length(N_output) != length(HIB_OUTPUT_LABELS)) {
    stop("N_output must contain the 13 Hib reporting groups.")
  }
  if (any(!is.finite(N_output)) || any(N_output <= 0)) {
    stop("All reporting-group populations must be positive and finite.")
  }

  output_index_child <- c(
    rep(1, 2), rep(2, 2), rep(3, 2), rep(4, 2),
    rep(5, 4), rep(6, 8), rep(7, 40), rep(8, 156), rep(9, 24)
  )
  if (length(output_index_child) != 240) {
    stop("Internal error constructing monthly Hib age cohorts.")
  }

  N_child <- numeric(240)
  for (g in 1:8) {
    idx <- which(output_index_child == g)
    N_child[idx] <- N_output[g] / length(idx)
  }
  # Ages 18-19 receive 2/32 of the supplied 18-49 population.
  idx_18_19 <- which(output_index_child == 9)
  N_child[idx_18_19] <- N_output[9] / 32 / 12

  N_adult <- c(
    N_output[9] * 30 / 32,
    N_output[10], N_output[11], N_output[12], N_output[13]
  )
  N_cell <- c(N_child, N_adult)

  parent_child <- ifelse(
    HIB_CHILD_MONTHS < 8, 1,
    ifelse(
      HIB_CHILD_MONTHS < 20, 2,
      ifelse(
        HIB_CHILD_MONTHS < 60, 3,
        ifelse(HIB_CHILD_MONTHS < 216, 4, 5)
      )
    )
  )
  parent_index <- c(parent_child, 5, 6, 7, 7, 8)
  output_index <- c(output_index_child, 9, 10, 11, 12, 13)
  age_mid <- c(
    (HIB_CHILD_MONTHS + 0.5) / 12,
    35, 55, 62.5, 70, 87.5
  )
  age_lower <- c(HIB_CHILD_MONTHS / 12, 20, 50, 60, 65, 75)

  # Confirm that splitting and re-aggregation preserve every input population.
  check <- vapply(
    seq_along(HIB_OUTPUT_LABELS),
    function(g) sum(N_cell[output_index == g]),
    numeric(1)
  )
  if (max(abs(check - N_output)) > 1e-7 * max(N_output)) {
    stop("Fine-grid population does not reproduce reporting-group totals.")
  }

  list(
    labels = HIB_CELL_LABELS,
    N = N_cell,
    output_index = output_index,
    parent_index = parent_index,
    age_mid = age_mid,
    age_lower = age_lower,
    child_index = seq_len(240),
    adult_index = 241:245
  )
}


# Optional ACS helper. It returns the 13 reporting bands expected by the model.
hib_build_acs_band_population <- function(
    year = 2023, save_rds = NULL, save_csv = NULL) {

  if (!requireNamespace("tidycensus", quietly = TRUE)) {
    stop("Install tidycensus to pull ACS population data.")
  }
  hib_require_packages()

  male_codes <- list(
    `0-4` = 3,
    `5-17` = 4:6,
    `18-49` = 7:15,
    `50-59` = 16:17,
    `60-64` = 18:19,
    `65-74` = 20:22,
    `75+` = 23:25
  )

  rows <- list()
  for (geo in c("state", "us")) {
    for (nm in names(male_codes)) {
      m <- male_codes[[nm]]
      vars <- sprintf("B01001_%03dE", c(m, m + 24))
      d <- suppressMessages(
        tidycensus::get_acs(
          geography = geo, variables = vars, year = year, geometry = FALSE
        )
      )
      d <- dplyr::summarise(
        dplyr::group_by(d, .data$GEOID, .data$NAME),
        age_group_population = sum(.data$estimate),
        .groups = "drop"
      )
      d$age_group <- nm
      names(d)[names(d) == "GEOID"] <- "state_fips_code"
      names(d)[names(d) == "NAME"] <- "state_name"
      rows[[length(rows) + 1]] <- d
    }
  }
  coarse <- dplyr::bind_rows(rows)

  out <- list()
  for (st in unique(coarse$state_name)) {
    x <- coarse[coarse$state_name == st, , drop = FALSE]
    fips <- x$state_fips_code[1]
    v <- stats::setNames(x$age_group_population, x$age_group)
    under5_idx <- 1:7
    under5_width <- hib_band_widths()[under5_idx]
    under5_pop <- v["0-4"] * under5_width / sum(under5_width)
    pop <- c(
      under5_pop,
      v["5-17"], v["18-49"], v["50-59"],
      v["60-64"], v["65-74"], v["75+"]
    )
    out[[length(out) + 1]] <- data.frame(
      state_fips_code = fips,
      state_name = st,
      age_group = HIB_INTERNAL_LABELS,
      age_group_population = as.numeric(pop),
      stringsAsFactors = FALSE
    )
  }
  ans <- dplyr::bind_rows(out)

  if (!is.null(save_rds)) {
    dir.create(dirname(save_rds), recursive = TRUE, showWarnings = FALSE)
    saveRDS(ans, save_rds)
  }
  if (!is.null(save_csv)) {
    dir.create(dirname(save_csv), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(ans, save_csv, row.names = FALSE)
  }
  ans
}


# -----------------------------------------------------------------------------
# 2. Hib-only surveillance rates
# -----------------------------------------------------------------------------

hib_read_age_rates <- function(x) {
  if (is.character(x) && length(x) == 1) {
    x <- utils::read.csv(
      x, check.names = FALSE, stringsAsFactors = FALSE,
      fileEncoding = "UTF-8-BOM"
    )
  }
  if (!is.data.frame(x)) stop("hib_age_rates must be a data.frame or CSV path.")

  names(x) <- gsub("\ufeff", "", names(x), fixed = TRUE)
  age_col <- which(tolower(trimws(names(x))) == "age group")
  hib_col <- which(tolower(trimws(names(x))) == "hib")
  if (length(age_col) != 1 || length(hib_col) != 1) {
    stop("Age-rate data must contain exactly one 'Age Group' and one 'Hib' column.")
  }

  # Deliberately retain only these two fields.
  out <- data.frame(
    age_group = as.character(x[[age_col]]),
    hib_rate_per_100k = as.numeric(x[[hib_col]]),
    stringsAsFactors = FALSE
  )
  if (anyNA(out$hib_rate_per_100k) || any(out$hib_rate_per_100k < 0)) {
    stop("The Hib incidence-rate column contains missing or negative values.")
  }
  out
}


hib_internal_rate_targets <- function(hib_age_rates) {
  d <- hib_read_age_rates(hib_age_rates)
  key <- hib_normalize_label(d$age_group)
  rate <- stats::setNames(d$hib_rate_per_100k, key)

  get_rate <- function(label) {
    k <- hib_normalize_label(label)
    if (!(k %in% names(rate))) stop("Missing Hib age-rate row: ", label)
    as.numeric(rate[k])
  }

  r_u1 <- get_rate("<1 year")
  r_1_4 <- get_rate("1-4 years")
  r_5_17 <- get_rate("5-17 years")
  r_18_34 <- get_rate("18-34 years")
  r_35_49 <- get_rate("35-49 years")
  r_50_64 <- get_rate("50-64 years")
  k65 <- hib_normalize_label(">=65 years")
  if (!(k65 %in% names(rate))) stop("Missing Hib age-rate row: >=65 years")
  r_65 <- as.numeric(rate[k65])

  r_18_49 <- (17 * r_18_34 + 15 * r_35_49) / 32
  out <- c(
    rep(r_u1, 5),
    rep(r_1_4, 2),
    r_5_17, r_18_49, r_50_64, r_50_64, r_65, r_65
  )
  names(out) <- HIB_OUTPUT_LABELS
  out
}


hib_recent_allage_rate <- function(hib_trend, years = 2020:2024) {
  if (is.null(hib_trend)) return(NA_real_)
  if (is.character(hib_trend) && length(hib_trend) == 1) {
    hib_trend <- utils::read.csv(
      hib_trend, check.names = FALSE, stringsAsFactors = FALSE,
      fileEncoding = "UTF-8-BOM"
    )
  }
  names(hib_trend) <- gsub("\ufeff", "", names(hib_trend), fixed = TRUE)
  ycol <- which(tolower(trimws(names(hib_trend))) == "year")
  hcol <- which(tolower(trimws(names(hib_trend))) == "hib")
  if (length(ycol) != 1 || length(hcol) != 1) {
    stop("Hib trend data must contain Year and Hib columns.")
  }
  keep <- as.numeric(hib_trend[[ycol]]) %in% years
  mean(as.numeric(hib_trend[[hcol]])[keep], na.rm = TRUE)
}


# -----------------------------------------------------------------------------
# 3. Vaccine effects and parameter expansion
# -----------------------------------------------------------------------------

hib_expand_cell_parameter <- function(
    x, grid, parameter_name, output_labels = HIB_OUTPUT_LABELS) {

  if (length(x) == 1) return(rep(as.numeric(x), length(grid$N)))
  if (!is.null(names(x)) && all(grid$labels %in% names(x))) {
    return(as.numeric(x[grid$labels]))
  }
  if (length(x) == length(grid$N)) return(as.numeric(x))
  if (!is.null(names(x)) && all(output_labels %in% names(x))) {
    x <- as.numeric(x[output_labels])
    return(x[grid$output_index])
  }
  if (length(x) == length(output_labels)) {
    return(as.numeric(x)[grid$output_index])
  }
  stop(
    "Parameter '", parameter_name, "' must be scalar, cell-specific, or have ",
    "one value for each of the 13 Hib reporting age groups."
  )
}


hib_age_parameter <- function(x, labels, parameter_name) {
  if (length(x) == 1) return(rep(as.numeric(x), length(labels)))
  if (!is.null(names(x)) && all(labels %in% names(x))) {
    return(as.numeric(x[labels]))
  }
  if (length(x) == length(labels)) return(as.numeric(x))
  stop(
    "Parameter '", parameter_name, "' must be scalar or have one value for ",
    "each reporting age group."
  )
}


hib_vaccine_effect_matrices <- function(params, grid) {
  params <- hib_as_params(params)
  age_mid <- grid$age_mid

  ve_dis_primary <- hib_num_param(
    params, "vaccine_effectiveness", default = 0.92
  )
  ve_car_primary <- hib_num_param(params, "ve_carriage", default = 0.64)
  ve_dis_booster <- hib_num_param(
    params, "ve_disease_booster", default = max(ve_dis_primary, 0.93)
  )
  ve_car_booster <- hib_num_param(
    params, "ve_carriage_booster", default = ve_car_primary
  )
  w_primary <- hib_num_param(
    params, "waning_rate_primary_annual",
    default = hib_num_param(params, "waning_rate_annual", default = 0.0939)
  )
  w_booster <- hib_num_param(
    params, "waning_rate_booster_annual",
    default = hib_num_param(params, "waning_rate_annual", default = 0.0939)
  )

  ve_values <- c(
    ve_dis_primary, ve_car_primary, ve_dis_booster, ve_car_booster
  )
  if (any(ve_values < 0) || any(ve_values > 1)) {
    stop("All vaccine-effect parameters must be between 0 and 1.")
  }
  if (ve_dis_primary <= 0) {
    stop("vaccine_effectiveness must be greater than zero.")
  }
  if (w_primary < 0 || w_booster < 0) stop("Waning rates cannot be negative.")

  # Approximate dose buildup among eventual series completers.
  buildup <- ifelse(
    age_mid < 2 / 12, 0,
    ifelse(age_mid < 4 / 12, min(1, 0.59 / ve_dis_primary), 1)
  )
  primary_clock <- pmax(age_mid - 0.5, 0)
  booster_clock <- pmax(age_mid - 1.25, 0)

  p_dis <- ve_dis_primary * buildup * exp(-w_primary * primary_clock)
  p_car <- ve_car_primary * buildup * exp(-w_primary * primary_clock)
  b_dis <- p_dis
  b_car <- p_car
  after_booster <- age_mid >= 1.25
  b_dis[after_booster] <- ve_dis_booster *
    exp(-w_booster * booster_clock[after_booster])
  b_car[after_booster] <- ve_car_booster *
    exp(-w_booster * booster_clock[after_booster])

  status <- c("unvaccinated", "primary_only", "primary_booster")
  ve_disease <- cbind(
    unvaccinated = rep(0, length(age_mid)),
    primary_only = p_dis,
    primary_booster = b_dis
  )
  ve_carriage <- cbind(
    unvaccinated = rep(0, length(age_mid)),
    primary_only = p_car,
    primary_booster = b_car
  )
  rownames(ve_disease) <- rownames(ve_carriage) <- grid$labels
  colnames(ve_disease) <- colnames(ve_carriage) <- status

  list(
    ve_disease = pmin(pmax(ve_disease, 0), 1),
    ve_carriage = pmin(pmax(ve_carriage, 0), 1),
    status = status
  )
}


# -----------------------------------------------------------------------------
# 4. Model construction and national transmission calibration
# -----------------------------------------------------------------------------

hib_build_model <- function(
    C_parent_daily, N_output, params, q = NULL, R0_pop = 1.3,
    external_foi_annual = NULL, balance_contacts = TRUE) {

  if (!all(dim(C_parent_daily) == length(HIB_PARENT_LABELS))) {
    stop("C_parent_daily must be the eight-band ENGAGED matrix.")
  }
  grid <- hib_make_cell_grid(N_output)
  params <- hib_as_params(params)

  duration_carriage_days <- hib_num_param(
    params, "duration_carriage_days",
    default = hib_num_param(params, "duration_infectious_days", default = 60)
  )
  if (duration_carriage_days <= 0) stop("Carriage duration must be positive.")
  rho <- 365 / duration_carriage_days
  natural_waning <- hib_num_param(
    params, "natural_immunity_waning_rate_annual", default = 0.0939
  )
  if (natural_waning < 0) {
    stop("natural_immunity_waning_rate_annual cannot be negative.")
  }

  parent_N <- vapply(
    seq_along(HIB_PARENT_LABELS),
    function(j) sum(grid$N[grid$parent_index == j]),
    numeric(1)
  )
  if (balance_contacts) {
    C_parent_daily <- hib_balance_contacts(C_parent_daily, parent_N)
  }
  C_parent_annual <- C_parent_daily * 365

  susceptibility_age <- hib_expand_cell_parameter(
    params$relative_susceptibility %||% 1,
    grid, "relative_susceptibility"
  )
  if (any(susceptibility_age < 0)) {
    stop("relative_susceptibility cannot be negative.")
  }
  susceptibility_parent <- vapply(
    seq_along(HIB_PARENT_LABELS),
    function(j) {
      idx <- grid$parent_index == j
      stats::weighted.mean(susceptibility_age[idx], grid$N[idx])
    },
    numeric(1)
  )

  # Full-susceptibility, unvaccinated NGM. Aging during a roughly 60-day
  # carriage episode is ignored for R0 calibration; deterministic aging is
  # applied in the projection itself.
  K0 <- matrix(0, length(HIB_PARENT_LABELS), length(HIB_PARENT_LABELS))
  for (i in seq_along(HIB_PARENT_LABELS)) {
    for (j in seq_along(HIB_PARENT_LABELS)) {
      K0[i, j] <- susceptibility_parent[i] *
        C_parent_annual[i, j] * parent_N[i] / parent_N[j] / rho
    }
  }
  radius0 <- max(Mod(eigen(K0, only.values = TRUE)$values))
  if (!is.finite(radius0) || radius0 <= 0) {
    stop("Cannot calibrate transmission: invalid next-generation matrix.")
  }
  if (is.null(q)) q <- R0_pop / radius0

  ext <- external_foi_annual %||%
    hib_num_param(params, "external_foi_annual", default = 1e-5)
  ext <- hib_expand_cell_parameter(ext, grid, "external_foi_annual")
  if (any(ext < 0)) stop("external_foi_annual cannot be negative.")

  ve <- hib_vaccine_effect_matrices(params, grid)
  booster_given_primary <- hib_num_param(
    params, "booster_given_primary", default = 1
  )
  if (booster_given_primary < 0 || booster_given_primary > 1) {
    stop("booster_given_primary must be between 0 and 1.")
  }

  list(
    n = length(grid$N),
    k = 3,
    labels = grid$labels,
    status = ve$status,
    N = grid$N,
    N_output = as.numeric(N_output),
    output_labels = HIB_OUTPUT_LABELS,
    output_index = grid$output_index,
    parent_index = grid$parent_index,
    parent_N = parent_N,
    age_mid = grid$age_mid,
    age_lower = grid$age_lower,
    child_index = grid$child_index,
    adult_index = grid$adult_index,
    C_parent = C_parent_annual,
    births = grid$N[1] * 12,
    rho = rho,
    natural_waning = natural_waning,
    q = q,
    R0_national_target = R0_pop,
    R0_with_fixed_q = q * radius0,
    external_foi = ext,
    susceptibility_age = susceptibility_age,
    ve_carriage = ve$ve_carriage,
    ve_disease = ve$ve_disease,
    booster_given_primary = booster_given_primary,
    maximum_projection_years = 20
  )
}


hib_aggregate_cells <- function(x, index, n_group) {
  out <- rowsum(
    matrix(as.numeric(x), ncol = 1),
    group = index, reorder = FALSE
  )[, 1]
  if (length(out) != n_group) {
    ans <- numeric(n_group)
    ans[as.integer(names(out))] <- out
    return(ans)
  }
  as.numeric(out)
}


hib_foi <- function(mod, colonized) {
  colonized_cell <- rowSums(colonized)
  colonized_parent <- hib_aggregate_cells(
    colonized_cell, mod$parent_index, length(HIB_PARENT_LABELS)
  )
  prevalence_parent <- colonized_parent / mod$parent_N
  lambda_parent <- as.numeric(
    mod$q * (mod$C_parent %*% prevalence_parent)
  )
  lambda_parent[mod$parent_index] + mod$external_foi
}


hib_acquisition_flows <- function(mod, uncolonized, colonized) {
  lambda <- hib_foi(mod, colonized)
  base <- lambda * mod$susceptibility_age
  acquisition <- sweep(uncolonized, 1, base, "*") *
    (1 - mod$ve_carriage)
  list(acquisition = acquisition, lambda = lambda)
}


# -----------------------------------------------------------------------------
# 5. State handling and deterministic monthly aging
# -----------------------------------------------------------------------------

hib_pack_state <- function(uncolonized, colonized, protected) {
  c(uncolonized, colonized, protected)
}


hib_unpack_state <- function(y, mod) {
  nk <- mod$n * mod$k
  list(
    uncolonized = matrix(
      y[seq_len(nk)], nrow = mod$n, ncol = mod$k,
      dimnames = list(mod$labels, mod$status)
    ),
    colonized = matrix(
      y[nk + seq_len(nk)], nrow = mod$n, ncol = mod$k,
      dimnames = list(mod$labels, mod$status)
    ),
    protected = matrix(
      y[2 * nk + seq_len(nk)], nrow = mod$n, ncol = mod$k,
      dimnames = list(mod$labels, mod$status)
    )
  )
}


# Epidemiologic transitions only. Demographic aging is a separate exact
# monthly shift, not a continuous hazard.
hib_rhs <- function(mod, y) {
  z <- hib_unpack_state(y, mod)
  flow <- hib_acquisition_flows(mod, z$uncolonized, z$colonized)
  acquisition <- flow$acquisition
  dU <- mod$natural_waning * z$protected - acquisition
  dC <- acquisition - mod$rho * z$colonized
  dR <- mod$rho * z$colonized -
    mod$natural_waning * z$protected
  hib_pack_state(dU, dC, dR)
}


hib_renormalize_state <- function(y, mod) {
  z <- hib_unpack_state(y, mod)
  clean <- function(x) {
    x[x < 0 & x > -1e-7] <- 0
    x
  }
  z$uncolonized <- clean(z$uncolonized)
  z$colonized <- clean(z$colonized)
  z$protected <- clean(z$protected)
  if (any(z$uncolonized < 0) || any(z$colonized < 0) ||
      any(z$protected < 0)) {
    stop("Numerical integration produced materially negative compartments.")
  }

  total <- rowSums(z$uncolonized + z$colonized + z$protected)
  if (any(total <= 0)) stop("A model age cell lost all population.")
  scale <- mod$N / total
  hib_pack_state(
    sweep(z$uncolonized, 1, scale, "*"),
    sweep(z$colonized, 1, scale, "*"),
    sweep(z$protected, 1, scale, "*")
  )
}


hib_set_newborn_history <- function(y, mod, primary_coverage) {
  z <- hib_unpack_state(y, mod)
  shares <- hib_birth_shares(
    primary_coverage, mod$booster_given_primary
  )[mod$status]
  z$uncolonized[1, ] <- mod$N[1] * shares
  z$colonized[1, ] <- 0
  z$protected[1, ] <- 0
  hib_pack_state(z$uncolonized, z$colonized, z$protected)
}


# Move every under-20 cohort exactly one month. Population totals for adjacent
# cells can differ in ACS data, so epidemiologic/vaccination proportions are
# transferred and then applied to the destination cell's fixed population.
hib_age_one_month <- function(y, mod, newborn_primary_coverage) {
  z <- hib_unpack_state(y, mod)
  child <- mod$child_index
  last_child <- max(child)

  shift_matrix <- function(x) {
    out <- x
    source <- child[-length(child)]
    destination <- child[-1]
    proportions <- sweep(x[source, , drop = FALSE], 1, mod$N[source], "/")
    out[destination, ] <- sweep(
      proportions, 1, mod$N[destination], "*"
    )
    out
  }

  U <- shift_matrix(z$uncolonized)
  C <- shift_matrix(z$colonized)
  R <- shift_matrix(z$protected)

  # The oldest post-decline child cell exits at age 20. The broad adult groups
  # remain at their baseline vaccination composition within a <=20-year run.
  if (last_child != 240) stop("Internal child-grid length has changed.")

  shares <- hib_birth_shares(
    newborn_primary_coverage, mod$booster_given_primary
  )[mod$status]
  U[1, ] <- mod$N[1] * shares
  C[1, ] <- 0
  R[1, ] <- 0

  hib_pack_state(U, C, R)
}


# Integrate carriage/immunity over one interval. Returned effective acquisition
# is already multiplied by residual direct disease risk, but not by the
# calibrated probability of invasion.
hib_integrate_interval <- function(mod, y, interval, dt, collect = TRUE) {
  nsub <- max(1, ceiling(interval / dt))
  h <- interval / nsub
  effective_total <- raw_total <- numeric(mod$n)

  rate_vectors <- function(state) {
    z <- hib_unpack_state(state, mod)
    flow <- hib_acquisition_flows(mod, z$uncolonized, z$colonized)
    list(
      effective = rowSums(flow$acquisition * (1 - mod$ve_disease)),
      raw = rowSums(flow$acquisition)
    )
  }

  before <- if (collect) rate_vectors(y) else NULL
  for (step in seq_len(nsub)) {
    k1 <- hib_rhs(mod, y)
    k2 <- hib_rhs(mod, y + h / 2 * k1)
    k3 <- hib_rhs(mod, y + h / 2 * k2)
    k4 <- hib_rhs(mod, y + h * k3)
    y_new <- y + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
    y_new <- hib_renormalize_state(y_new, mod)

    if (collect) {
      after <- rate_vectors(y_new)
      effective_total <- effective_total +
        h * (before$effective + after$effective) / 2
      raw_total <- raw_total + h * (before$raw + after$raw) / 2
      before <- after
    }
    y <- y_new
  }
  list(
    y = y,
    effective_acquisitions = effective_total,
    carriage_acquisitions = raw_total
  )
}


# -----------------------------------------------------------------------------
# 6. Baseline equilibrium and invasive-disease calibration
# -----------------------------------------------------------------------------

# Propagate one cohort through a single age-month under a fixed force of
# acquisition. This is used only by the fast equilibrium fixed-point solver.
hib_propagate_local_month <- function(
    U, C, R, acquisition_rate, rho, omega, nsub = 4) {

  h <- (1 / 12) / nsub
  rhs <- function(u, c, r) {
    acq <- acquisition_rate * u
    list(
      U = omega * r - acq,
      C = acq - rho * c,
      R = rho * c - omega * r
    )
  }
  for (s in seq_len(nsub)) {
    k1 <- rhs(U, C, R)
    k2 <- rhs(
      U + h / 2 * k1$U,
      C + h / 2 * k1$C,
      R + h / 2 * k1$R
    )
    k3 <- rhs(
      U + h / 2 * k2$U,
      C + h / 2 * k2$C,
      R + h / 2 * k2$R
    )
    k4 <- rhs(
      U + h * k3$U,
      C + h * k3$C,
      R + h * k3$R
    )
    U <- U + h / 6 * (k1$U + 2 * k2$U + 2 * k3$U + k4$U)
    C <- C + h / 6 * (k1$C + 2 * k2$C + 2 * k3$C + k4$C)
    R <- R + h / 6 * (k1$R + 2 * k2$R + 2 * k3$R + k4$R)
  }
  list(U = pmax(U, 0), C = pmax(C, 0), R = pmax(R, 0))
}


# Given lambda, construct the deterministic-age baseline cascade.
hib_cascade <- function(mod, lambda, primary_coverage) {
  U <- C <- R <- matrix(
    0, mod$n, mod$k,
    dimnames = list(mod$labels, mod$status)
  )
  shares <- hib_birth_shares(
    primary_coverage, mod$booster_given_primary
  )[mod$status]

  U[1, ] <- mod$N[1] * shares
  for (i in mod$child_index) {
    acquisition_rate <- lambda[i] * mod$susceptibility_age[i] *
      (1 - mod$ve_carriage[i, ])
    end <- hib_propagate_local_month(
      U[i, ], C[i, ], R[i, ], acquisition_rate,
      mod$rho, mod$natural_waning
    )
    if (i < max(mod$child_index)) {
      scale <- mod$N[i + 1] / mod$N[i]
      U[i + 1, ] <- end$U * scale
      C[i + 1, ] <- end$C * scale
      R[i + 1, ] <- end$R * scale
    }
  }

  # Broad adults use their closed S-C-R equilibrium at baseline. Their
  # vaccination-history totals remain fixed over a <=20-year projection.
  for (i in mod$adult_index) {
    acquisition_rate <- lambda[i] * mod$susceptibility_age[i] *
      (1 - mod$ve_carriage[i, ])
    if (mod$natural_waning == 0) {
      # With permanent natural protection, an endemic closed adult group would
      # eventually reside in R. A positive waning rate is required for this
      # reduced S-C-R-S adult equilibrium.
      stop(
        "natural_immunity_waning_rate_annual must be positive for the ",
        "adult equilibrium calculation."
      )
    }
    denom <- 1 + acquisition_rate / mod$rho +
      acquisition_rate / mod$natural_waning
    U[i, ] <- mod$N[i] * shares / denom
    C[i, ] <- acquisition_rate * U[i, ] / mod$rho
    R[i, ] <- acquisition_rate * U[i, ] / mod$natural_waning
  }
  list(uncolonized = U, colonized = C, protected = R)
}


hib_equilibrium <- function(
    mod, primary_coverage, guess = NULL, damp = 0.5,
    iters = 20000, tol = 1e-10, dt = 1 / 52,
    dynamic_spinup_years = 0, dynamic_tol = 1e-7) {

  prevalence_parent <- if (is.null(guess)) {
    rep(1e-7, length(HIB_PARENT_LABELS))
  } else {
    as.numeric(guess)
  }
  if (length(prevalence_parent) != length(HIB_PARENT_LABELS)) {
    stop("Equilibrium guess must have one value per contact group.")
  }

  converged <- FALSE
  for (iteration in seq_len(iters)) {
    lambda_parent <- as.numeric(
      mod$q * (mod$C_parent %*% prevalence_parent)
    )
    lambda <- lambda_parent[mod$parent_index] + mod$external_foi
    cs <- hib_cascade(mod, lambda, primary_coverage)
    colonized_parent <- hib_aggregate_cells(
      rowSums(cs$colonized),
      mod$parent_index, length(HIB_PARENT_LABELS)
    )
    new_prevalence <- colonized_parent / mod$parent_N
    if (max(abs(new_prevalence - prevalence_parent)) < tol) {
      prevalence_parent <- new_prevalence
      converged <- TRUE
      break
    }
    prevalence_parent <- (1 - damp) * prevalence_parent +
      damp * new_prevalence
  }
  if (!converged) warning("Hib equilibrium solver reached its iteration limit.")

  lambda_parent <- as.numeric(
    mod$q * (mod$C_parent %*% prevalence_parent)
  )
  lambda <- lambda_parent[mod$parent_index] + mod$external_foi
  cs <- hib_cascade(mod, lambda, primary_coverage)

  # Refine the fixed-lambda cascade to the periodic equilibrium of the fully
  # coupled monthly projection. The cascade is already close, so this normally
  # requires only a small number of annual iterations.
  y <- hib_pack_state(cs$uncolonized, cs$colonized, cs$protected)
  spinup_converged <- dynamic_spinup_years <= 0
  spinup_iteration <- 0
  state_difference <- function(a, b) {
    za <- hib_unpack_state(a, mod)
    zb <- hib_unpack_state(b, mod)
    max(
      abs(za$uncolonized - zb$uncolonized) / mod$N,
      abs(za$colonized - zb$colonized) / mod$N,
      abs(za$protected - zb$protected) / mod$N
    )
  }
  if (dynamic_spinup_years > 0) {
    for (spinup_iteration in seq_len(dynamic_spinup_years)) {
      y_start <- y
      for (month in seq_len(12)) {
        step <- hib_integrate_interval(
          mod, y, interval = 1 / 12, dt = dt, collect = FALSE
        )
        y <- hib_age_one_month(step$y, mod, primary_coverage)
      }
      if (state_difference(y, y_start) < dynamic_tol) {
        spinup_converged <- TRUE
        break
      }
    }
  }
  if (!spinup_converged) {
    warning("Hib dynamic equilibrium refinement reached its iteration limit.")
  }
  cs <- hib_unpack_state(y, mod)
  colonized_parent <- hib_aggregate_cells(
    rowSums(cs$colonized),
    mod$parent_index, length(HIB_PARENT_LABELS)
  )
  prevalence_parent <- colonized_parent / mod$parent_N
  lambda_parent <- as.numeric(
    mod$q * (mod$C_parent %*% prevalence_parent)
  )
  lambda <- lambda_parent[mod$parent_index] + mod$external_foi

  list(
    uncolonized = cs$uncolonized,
    colonized = cs$colonized,
    protected = cs$protected,
    prevalence_parent = prevalence_parent,
    lambda = lambda,
    converged = converged,
    iterations = iteration,
    dynamic_spinup_converged = spinup_converged,
    dynamic_spinup_years = spinup_iteration
  )
}


hib_baseline_annual_acquisitions <- function(
    mod, equilibrium, primary_coverage, dt = 1 / 52) {

  y <- hib_pack_state(
    equilibrium$uncolonized,
    equilibrium$colonized,
    equilibrium$protected
  )
  effective <- raw <- numeric(mod$n)
  for (month in seq_len(12)) {
    step <- hib_integrate_interval(
      mod, y, interval = 1 / 12, dt = dt, collect = TRUE
    )
    effective <- effective + step$effective_acquisitions
    raw <- raw + step$carriage_acquisitions
    y <- hib_age_one_month(step$y, mod, primary_coverage)
  }
  list(effective = effective, raw = raw, end_state = y)
}


hib_calibrate_invasion <- function(
    mod, equilibrium, target_rates_per_100k, primary_coverage,
    dt = 1 / 52) {

  target_rates_per_100k <- as.numeric(
    target_rates_per_100k[mod$output_labels]
  )
  if (anyNA(target_rates_per_100k)) {
    stop("Age-specific target rates do not cover all reporting groups.")
  }
  target_cases <- target_rates_per_100k / 100000 * mod$N_output
  annual <- hib_baseline_annual_acquisitions(
    mod, equilibrium, primary_coverage, dt
  )
  effective_output <- hib_aggregate_cells(
    annual$effective, mod$output_index, length(mod$output_labels)
  )
  if (any(effective_output <= 0)) {
    stop(
      "At least one age group has no effective carriage acquisitions. ",
      "Increase external_foi_annual or revisit carriage assumptions."
    )
  }

  p_output <- target_cases / effective_output
  names(p_output) <- mod$output_labels
  if (any(p_output > 1)) {
    bad <- mod$output_labels[p_output > 1]
    stop(
      "Calibrated probability of invasive disease exceeds 1 in: ",
      paste(bad, collapse = ", "),
      ". Increase assumed carriage acquisition or revisit target mapping."
    )
  }
  if (any(p_output < 0) || any(!is.finite(p_output))) {
    stop("Invalid calibrated invasion probabilities.")
  }

  list(
    p_invasion_output = p_output,
    p_invasion_cell = p_output[mod$output_index],
    target_cases = target_cases,
    fitted_cases = effective_output * p_output,
    effective_acquisitions = effective_output,
    annual_carriage_acquisitions = hib_aggregate_cells(
      annual$raw, mod$output_index, length(mod$output_labels)
    )
  )
}


# -----------------------------------------------------------------------------
# 7. Dynamic simulation after a birth-cohort coverage decline
# -----------------------------------------------------------------------------

# Track vaccination history alone to demonstrate that direct cohort exposure
# cannot appear above the attained age. This does not constrain indirect
# changes in disease outcomes among older people.
hib_direct_coverage_diagnostic <- function(
    mod, coverage_baseline, coverage_new, horizons) {

  shares0 <- hib_birth_shares(
    coverage_baseline, mod$booster_given_primary
  )[mod$status]
  shares1 <- hib_birth_shares(
    coverage_new, mod$booster_given_primary
  )[mod$status]
  history <- matrix(
    rep(shares0, each = mod$n),
    nrow = mod$n, ncol = mod$k,
    dimnames = list(mod$labels, mod$status)
  )
  history[1, ] <- shares1
  rows <- list()
  max_month <- max(round(horizons * 12))

  for (month in seq_len(max_month)) {
    if (month %in% round(horizons * 12)) {
      h <- month / 12
      primary_cell <- rowSums(
        history[, c("primary_only", "primary_booster"), drop = FALSE]
      )
      primary_output <- vapply(
        seq_along(mod$output_labels),
        function(g) {
          idx <- mod$output_index == g
          stats::weighted.mean(primary_cell[idx], mod$N[idx])
        },
        numeric(1)
      )
      impossible <- mod$age_lower > h + 1e-10 &
        abs(primary_cell - coverage_baseline) > 1e-12
      if (any(impossible)) {
        stop("Deterministic-aging validation detected direct coverage leakage.")
      }
      rows[[length(rows) + 1]] <- data.frame(
        time_horizon = h,
        age_group = mod$output_labels,
        direct_primary_coverage = primary_output,
        direct_change_from_baseline = primary_output - coverage_baseline,
        stringsAsFactors = FALSE
      )
    }
    old <- history
    history[2:240, ] <- old[1:239, , drop = FALSE]
    history[1, ] <- shares1
  }
  dplyr::bind_rows(rows)
}


# Returns annual burden during the final 12 months ending at each horizon.
hib_trajectory_rates <- function(
    mod, coverage_baseline, coverage_new, horizons, p_invasion_cell,
    dt = 1 / 52, baseline_equilibrium = NULL) {

  if (any(horizons < 1)) stop("All horizons must be at least one year.")
  if (any(horizons > mod$maximum_projection_years)) {
    stop(
      "This deterministic cohort grid supports projections through ",
      mod$maximum_projection_years, " years. Extend the monthly grid for ",
      "longer horizons."
    )
  }
  horizon_months <- round(horizons * 12)
  if (any(abs(horizons * 12 - horizon_months) > 1e-8)) {
    stop("Every horizon must resolve to a whole number of months.")
  }

  base <- baseline_equilibrium %||%
    hib_equilibrium(mod, coverage_baseline, dt = dt)
  y <- hib_pack_state(
    base$uncolonized, base$colonized, base$protected
  )

  # The first modeled post-change birth cohort occupies the 0-month cell at
  # t=0. This is a monthly-cohort approximation to continuous births.
  y <- hib_set_newborn_history(y, mod, coverage_new)

  max_month <- max(horizon_months)
  cases_month <- carriage_month <- matrix(
    0, nrow = max_month, ncol = length(mod$output_labels),
    dimnames = list(NULL, mod$output_labels)
  )

  for (month in seq_len(max_month)) {
    step <- hib_integrate_interval(
      mod, y, interval = 1 / 12, dt = dt, collect = TRUE
    )
    case_cell <- step$effective_acquisitions *
      as.numeric(p_invasion_cell)
    cases_month[month, ] <- hib_aggregate_cells(
      case_cell, mod$output_index, length(mod$output_labels)
    )
    carriage_month[month, ] <- hib_aggregate_cells(
      step$carriage_acquisitions,
      mod$output_index, length(mod$output_labels)
    )
    y <- hib_age_one_month(step$y, mod, coverage_new)
  }

  cases <- carriage <- matrix(
    0, nrow = length(horizons), ncol = length(mod$output_labels),
    dimnames = list(as.character(horizons), mod$output_labels)
  )
  for (m in seq_along(horizons)) {
    end_month <- horizon_months[m]
    idx <- (end_month - 11):end_month
    cases[m, ] <- colSums(cases_month[idx, , drop = FALSE])
    carriage[m, ] <- colSums(carriage_month[idx, , drop = FALSE])
  }

  list(
    cases = cases,
    carriage_acquisitions = carriage,
    baseline_equilibrium = base,
    direct_coverage = hib_direct_coverage_diagnostic(
      mod, coverage_baseline, coverage_new, horizons
    )
  )
}


# -----------------------------------------------------------------------------
# 8. State driver and burden calculations
# -----------------------------------------------------------------------------

run_hib_agestructured <- function(
    coverage_df, pop_df, params, contact_matrix_path, hib_age_rates,
    hib_trend = NULL,
    declines = c(0, 0.05, 0.10, 0.15, 0.20),
    horizons = c(1, 5, 10, 20),
    national_name = "United States",
    R0_pop = 1.3,
    dt = 1 / 52,
    verbose = TRUE) {

  hib_require_packages()
  params <- hib_as_params(params)

  cov_required <- c("state_name", "vaccine_coverage_estimate")
  if (!all(cov_required %in% names(coverage_df))) {
    stop("coverage_df must contain: ", paste(cov_required, collapse = ", "))
  }
  coverage_df$vaccine_coverage_estimate <- as.numeric(
    coverage_df$vaccine_coverage_estimate
  )
  if (anyDuplicated(as.character(coverage_df$state_name))) {
    stop("coverage_df must contain at most one row per state_name.")
  }
  if (any(
    coverage_df$vaccine_coverage_estimate < 0 |
      coverage_df$vaccine_coverage_estimate > 1,
    na.rm = TRUE
  )) {
    stop("Vaccine coverage values must be between 0 and 1.")
  }
  if (!(0 %in% declines)) {
    stop("declines must include 0 for baseline additional-burden calculations.")
  }
  if (any(declines < 0) || any(declines > 1)) {
    stop("All coverage declines must be between 0 and 1.")
  }
  if (any(horizons < 1) || any(horizons > 20) ||
      any(!is.finite(horizons))) {
    stop("All horizons must be finite and between 1 and 20 years.")
  }
  if (!is.finite(dt) || dt <= 0 || dt > 1 / 12) {
    stop("dt must be positive and no larger than one month.")
  }

  cm <- hib_read_contact_matrix(contact_matrix_path)
  target_rates <- hib_internal_rate_targets(hib_age_rates)
  getpop <- function(st) hib_get_internal_population(pop_df, st)

  natpop <- getpop(national_name)
  if (is.null(natpop)) {
    state_names <- setdiff(unique(pop_df$state_name), national_name)
    state_pops <- lapply(state_names, getpop)
    complete <- !vapply(state_pops, is.null, logical(1))
    if (!any(complete)) {
      stop("Could not construct a national population from pop_df.")
    }
    natpop <- Reduce(`+`, state_pops[complete])
    warning("No complete national population row; summing complete state rows.")
  }

  natcov <- coverage_df$vaccine_coverage_estimate[
    coverage_df$state_name == national_name
  ]
  if (length(natcov) != 1 || is.na(natcov)) {
    tmp <- coverage_df[
      !is.na(coverage_df$vaccine_coverage_estimate) &
        coverage_df$state_name != national_name,
      , drop = FALSE
    ]
    weights <- vapply(tmp$state_name, function(st) {
      p <- getpop(st)
      if (is.null(p)) NA_real_ else sum(p[1:5])
    }, numeric(1))
    keep <- is.finite(weights) & weights > 0
    if (!any(keep)) {
      natcov <- mean(tmp$vaccine_coverage_estimate, na.rm = TRUE)
      warning("No national coverage row; using the unweighted state mean.")
    } else {
      natcov <- stats::weighted.mean(
        tmp$vaccine_coverage_estimate[keep], weights[keep], na.rm = TRUE
      )
      warning(
        "No national coverage row; using an under-1 population-weighted ",
        "state mean."
      )
    }
  }
  natcov <- as.numeric(natcov)

  ext_default <- hib_num_param(
    params, "external_foi_annual", default = 1e-5
  )

  # National build calibrates q once.
  mod_nat <- hib_build_model(
    cm$C, natpop, params, q = NULL, R0_pop = R0_pop,
    external_foi_annual = ext_default,
    balance_contacts = TRUE
  )
  q_national <- mod_nat$q
  eq_nat <- hib_equilibrium(
    mod_nat, natcov, dt = dt,
    tol = 1e-10, dynamic_spinup_years = 100, dynamic_tol = 1e-7
  )
  cal <- hib_calibrate_invasion(
    mod_nat, eq_nat, target_rates, natcov, dt = dt
  )

  fitted_allage_rate <- sum(cal$fitted_cases) / sum(natpop) * 100000
  recent_trend_rate <- hib_recent_allage_rate(hib_trend)
  prevalence_output <- hib_aggregate_cells(
    rowSums(eq_nat$colonized),
    mod_nat$output_index, length(HIB_OUTPUT_LABELS)
  ) / natpop

  if (verbose) {
    cat("\n-- Hib national calibration --\n")
    cat("  Aging: deterministic monthly cohorts through age 20\n")
    cat(sprintf("  National R0 target: %.3f\n", R0_pop))
    cat(sprintf("  Fixed transmission q: %.8f\n", q_national))
    cat(sprintf("  Primary-series coverage: %.1f%%\n", 100 * natcov))
    cat(sprintf(
      "  Booster given primary: %.1f%%\n",
      100 * mod_nat$booster_given_primary
    ))
    cat(sprintf(
      "  External force of acquisition: %.8g per person-year\n",
      mod_nat$external_foi[1]
    ))
    cat(sprintf(
      "  Natural-immunity waning: %.5f per year\n",
      mod_nat$natural_waning
    ))
    cat(sprintf(
      "  Modeled current all-age carriage prevalence: %.4f%%\n",
      100 * sum(eq_nat$colonized) / sum(mod_nat$N)
    ))
    cat(sprintf(
      "  Fitted all-age Hib rate: %.4f per 100,000\n",
      fitted_allage_rate
    ))
    if (is.finite(recent_trend_rate)) {
      cat(sprintf(
        "  2020-2024 trend mean (validation): %.4f per 100,000\n",
        recent_trend_rate
      ))
    }
    cat(sprintf(
      "  %10s %10s %12s %12s %12s\n",
      "age", "target", "carriage%", "p_invasion", "fit cases"
    ))
    for (i in seq_along(HIB_OUTPUT_LABELS)) {
      cat(sprintf(
        "  %10s %10.4f %11.5f%% %12.6g %12.4f\n",
        HIB_OUTPUT_LABELS[i], target_rates[i],
        100 * prevalence_output[i],
        cal$p_invasion_output[i], cal$fitted_cases[i]
      ))
    }
  }

  build_state_model <- function(pop) {
    hib_build_model(
      cm$C, pop, params, q = q_national, R0_pop = R0_pop,
      external_foi_annual = ext_default,
      balance_contacts = TRUE
    )
  }

  death_rate <- hib_age_parameter(
    hib_num_param(params, "death_rate", default = 0.04),
    HIB_OUTPUT_LABELS, "death_rate"
  )
  proportion_hospitalized <- hib_age_parameter(
    hib_num_param(
      params, "proportion_hospitalized_given_case", default = 0.95
    ),
    HIB_OUTPUT_LABELS, "proportion_hospitalized_given_case"
  )
  duration_sick_days <- hib_age_parameter(
    hib_num_param(params, "duration_sick_days", default = 10),
    HIB_OUTPUT_LABELS, "duration_sick_days"
  )
  duration_hospitalized_days <- hib_age_parameter(
    hib_num_param(params, "duration_hospitalized_days", default = 7),
    HIB_OUTPUT_LABELS, "duration_hospitalized_days"
  )
  cost_hospitalization_daily <- hib_age_parameter(
    hib_num_param(params, "cost_hospitalization_daily", default = 4220),
    HIB_OUTPUT_LABELS, "cost_hospitalization_daily"
  )
  cost_wage_daily <- hib_age_parameter(
    hib_num_param(params, "cost_wage_daily", default = 200),
    HIB_OUTPUT_LABELS, "cost_wage_daily"
  )
  severe_adverse_event_rate <- hib_num_param(
    params, "severe_adverse_event_rate", default = 1e-6
  )

  if (any(death_rate < 0 | death_rate > 1) ||
      any(proportion_hospitalized < 0 | proportion_hospitalized > 1)) {
    stop("Death and hospitalization probabilities must be in [0,1].")
  }
  if (any(duration_sick_days < 0) ||
      any(duration_hospitalized_days < 0) ||
      any(cost_hospitalization_daily < 0) ||
      any(cost_wage_daily < 0) ||
      severe_adverse_event_rate < 0) {
    stop("Durations, costs, and adverse-event rate cannot be negative.")
  }

  states <- coverage_df[
    !is.na(coverage_df$vaccine_coverage_estimate),
    , drop = FALSE
  ]
  rows <- list()

  for (s in seq_len(nrow(states))) {
    st <- as.character(states$state_name[s])
    v0 <- states$vaccine_coverage_estimate[s]
    spop <- getpop(st)
    if (is.null(spop)) {
      warning("Skipping ", st, ": incomplete or unsupported population bands.")
      next
    }
    if (verbose) {
      cat(sprintf(
        "  [%2d/%d] %-24s coverage %.1f%%\n",
        s, nrow(states), st, 100 * v0
      ))
    }
    mod <- build_state_model(spop)
    # The deterministic fixed-lambda cascade is used directly for states.
    # National calibration receives the more expensive dynamic refinement.
    # All decline scenarios within a state begin from the same baseline state.
    state_equilibrium <- hib_equilibrium(
      mod, v0, dt = dt, tol = 1e-8, dynamic_spinup_years = 0
    )

    for (decline in declines) {
      v_new <- max(v0 - decline, 0)
      tr <- hib_trajectory_rates(
        mod, v0, v_new, horizons,
        cal$p_invasion_cell, dt = dt,
        baseline_equilibrium = state_equilibrium
      )

      for (m in seq_along(horizons)) {
        cases <- as.numeric(tr$cases[m, ])
        hospitalizations <- cases * proportion_hospitalized
        deaths <- cases * death_rate

        adverse <- numeric(length(HIB_OUTPUT_LABELS))
        adverse[1] <- mod$births * v_new * severe_adverse_event_rate

        workdays <- cases * duration_sick_days
        productivity_cost <- workdays * cost_wage_daily
        hospitalization_cost <- hospitalizations *
          duration_hospitalized_days * cost_hospitalization_daily

        rows[[length(rows) + 1]] <- tibble::tibble(
          disease = "Hib",
          state_name = st,
          age_group = HIB_OUTPUT_LABELS,
          age_group_population = spop,
          declining_coverage_among_new_births = decline,
          time_horizon = horizons[m],
          vaccine_coverage_estimate = v0,
          cases = cases,
          hospitalizations = hospitalizations,
          deaths = deaths,
          workdays_lost = workdays,
          productivity_cost = productivity_cost,
          hospitalization_cost = hospitalization_cost,
          total_cost = productivity_cost + hospitalization_cost,
          vaccine_adverse_events = adverse
        )
      }
    }
  }

  if (length(rows) == 0) stop("No states produced model output.")
  df <- dplyr::bind_rows(rows)

  base <- dplyr::filter(
    df, .data$declining_coverage_among_new_births == 0
  )
  base <- dplyr::select(
    base,
    .data$state_name, .data$age_group, .data$time_horizon,
    b_cases = .data$cases,
    b_hosp = .data$hospitalizations,
    b_deaths = .data$deaths,
    b_wd = .data$workdays_lost,
    b_pc = .data$productivity_cost,
    b_hc = .data$hospitalization_cost,
    b_tc = .data$total_cost,
    b_ae = .data$vaccine_adverse_events
  )

  df <- dplyr::left_join(
    df, base, by = c("state_name", "age_group", "time_horizon")
  )
  df <- dplyr::mutate(
    df,
    additional_cases = .data$cases - .data$b_cases,
    additional_hospitalizations = .data$hospitalizations - .data$b_hosp,
    additional_deaths = .data$deaths - .data$b_deaths,
    additional_workdays_lost = .data$workdays_lost - .data$b_wd,
    additional_productivity_cost =
      .data$productivity_cost - .data$b_pc,
    additional_hospitalization_cost =
      .data$hospitalization_cost - .data$b_hc,
    additional_total_cost = .data$total_cost - .data$b_tc,
    vaccine_adverse_events_avoided =
      .data$b_ae - .data$vaccine_adverse_events
  )
  df <- dplyr::select(df, -dplyr::starts_with("b_"))
  df <- hib_add_rates(df)

  diagnostic_decline <- min(max(declines), natcov)
  leakage_diagnostic <- hib_direct_coverage_diagnostic(
    mod_nat, natcov, natcov - diagnostic_decline, horizons
  )
  calibration <- list(
    aging_method = "deterministic monthly cohorts through age 20",
    maximum_projection_years = mod_nat$maximum_projection_years,
    R0_national = R0_pop,
    q_national = q_national,
    national_primary_coverage = natcov,
    booster_given_primary = mod_nat$booster_given_primary,
    external_foi_annual = mod_nat$external_foi,
    natural_immunity_waning_rate_annual = mod_nat$natural_waning,
    target_rates_per_100k = target_rates,
    p_invasion = cal$p_invasion_output,
    national_carriage_prevalence = prevalence_output,
    fitted_allage_rate_per_100k = fitted_allage_rate,
    recent_trend_rate_per_100k = recent_trend_rate,
    direct_coverage_diagnostic_decline = diagnostic_decline,
    direct_coverage_diagnostic = leakage_diagnostic
  )
  attr(df, "hib_calibration") <- calibration
  df
}


# -----------------------------------------------------------------------------
# 9. Rate calculation, age collapse, and output formatting
# -----------------------------------------------------------------------------

hib_add_rates <- function(df) {
  per <- function(x, n) ifelse(n > 0, x / n * 100000, NA_real_)
  dplyr::mutate(
    df,
    cases_per_100k = per(.data$cases, .data$age_group_population),
    additional_cases_per_100k =
      per(.data$additional_cases, .data$age_group_population),
    hospitalizations_per_100k =
      per(.data$hospitalizations, .data$age_group_population),
    additional_hospitalizations_per_100k =
      per(.data$additional_hospitalizations, .data$age_group_population),
    deaths_per_100k = per(.data$deaths, .data$age_group_population),
    additional_deaths_per_100k =
      per(.data$additional_deaths, .data$age_group_population),
    workdays_lost_per_100k =
      per(.data$workdays_lost, .data$age_group_population),
    additional_workdays_lost_per_100k =
      per(.data$additional_workdays_lost, .data$age_group_population),
    productivity_cost_per_100k =
      per(.data$productivity_cost, .data$age_group_population),
    additional_productivity_cost_per_100k =
      per(.data$additional_productivity_cost, .data$age_group_population),
    hospitalization_cost_per_100k =
      per(.data$hospitalization_cost, .data$age_group_population),
    additional_hospitalization_cost_per_100k =
      per(.data$additional_hospitalization_cost, .data$age_group_population),
    total_cost_per_100k = per(.data$total_cost, .data$age_group_population),
    additional_total_cost_per_100k =
      per(.data$additional_total_cost, .data$age_group_population),
    vaccine_adverse_events_per_100k =
      per(.data$vaccine_adverse_events, .data$age_group_population),
    vaccine_adverse_events_avoided_per_100k =
      per(.data$vaccine_adverse_events_avoided, .data$age_group_population)
  )
}


collapse_hib_age_groups <- function(df, age_group_label = "All ages") {
  count_cols <- c(
    "cases", "additional_cases",
    "hospitalizations", "additional_hospitalizations",
    "deaths", "additional_deaths",
    "workdays_lost", "additional_workdays_lost",
    "productivity_cost", "additional_productivity_cost",
    "hospitalization_cost", "additional_hospitalization_cost",
    "total_cost", "additional_total_cost",
    "vaccine_adverse_events", "vaccine_adverse_events_avoided"
  )

  out <- dplyr::summarise(
    dplyr::group_by(
      df,
      .data$disease, .data$state_name,
      .data$declining_coverage_among_new_births,
      .data$time_horizon, .data$vaccine_coverage_estimate
    ),
    dplyr::across(
      dplyr::all_of(c("age_group_population", count_cols)),
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )
  out$age_group <- age_group_label
  hib_add_rates(out)
}


hib_curate <- function(df) {
  out <- dplyr::mutate(
    df,
    percent_decline = .data$declining_coverage_among_new_births * 100,
    accrual_label = factor(
      paste0(
        .data$time_horizon,
        ifelse(.data$time_horizon == 1, " Year", " Years")
      ),
      levels = c("1 Year", "5 Years", "10 Years", "20 Years")
    )
  )
  out <- dplyr::rename(
    out,
    accrual_years = .data$time_horizon,
    baseline_coverage = .data$vaccine_coverage_estimate
  )
  dplyr::select(
    out,
    .data$disease, .data$state_name, .data$age_group,
    .data$age_group_population, .data$percent_decline,
    .data$accrual_years, .data$accrual_label, .data$baseline_coverage,
    .data$cases, .data$additional_cases,
    .data$cases_per_100k, .data$additional_cases_per_100k,
    .data$hospitalizations, .data$additional_hospitalizations,
    .data$hospitalizations_per_100k,
    .data$additional_hospitalizations_per_100k,
    .data$deaths, .data$additional_deaths,
    .data$deaths_per_100k, .data$additional_deaths_per_100k,
    .data$workdays_lost, .data$additional_workdays_lost,
    .data$workdays_lost_per_100k,
    .data$additional_workdays_lost_per_100k,
    .data$productivity_cost, .data$additional_productivity_cost,
    .data$productivity_cost_per_100k,
    .data$additional_productivity_cost_per_100k,
    .data$hospitalization_cost, .data$additional_hospitalization_cost,
    .data$hospitalization_cost_per_100k,
    .data$additional_hospitalization_cost_per_100k,
    .data$total_cost, .data$additional_total_cost,
    .data$total_cost_per_100k, .data$additional_total_cost_per_100k,
    .data$vaccine_adverse_events,
    .data$vaccine_adverse_events_per_100k,
    .data$vaccine_adverse_events_avoided,
    .data$vaccine_adverse_events_avoided_per_100k
  )
}


# -----------------------------------------------------------------------------
# 10. Main wrapper
# -----------------------------------------------------------------------------

hib_agestructured_main <- function(
    coverage_df, pop_df, params, contact_matrix_path, hib_age_rates,
    hib_trend = NULL,
    declines = seq(0, 0.20,0.01),
    horizons = c(1, 5, 10, 20),
    national_name = "United States",
    R0_pop = 1.3,
    dt = 1 / 52,
    write = TRUE,
    output_directory = "data",
    csv_directory = file.path("data", "csv"),
    verbose = TRUE) {

  age_df <- run_hib_agestructured(
    coverage_df = coverage_df,
    pop_df = pop_df,
    params = params,
    contact_matrix_path = contact_matrix_path,
    hib_age_rates = hib_age_rates,
    hib_trend = hib_trend,
    declines = declines,
    horizons = horizons,
    national_name = national_name,
    R0_pop = R0_pop,
    dt = dt,
    verbose = verbose
  )
  calibration <- attr(age_df, "hib_calibration")

  out_age <- hib_curate(age_df)
  out_all <- hib_curate(collapse_hib_age_groups(age_df))

  if (write) {
    dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
    dir.create(csv_directory, recursive = TRUE, showWarnings = FALSE)
    saveRDS(
      out_age,
      file.path(output_directory, "hib_agestructured_by_age.rds")
    )
    utils::write.csv(
      out_age,
      file.path(csv_directory, "hib_agestructured_by_age.csv"),
      row.names = FALSE
    )
    saveRDS(
      out_all,
      file.path(output_directory, "hib_agestructured_curated.rds")
    )
    utils::write.csv(
      out_all,
      file.path(csv_directory, "hib_agestructured_curated.csv"),
      row.names = FALSE
    )
    saveRDS(
      calibration,
      file.path(output_directory, "hib_agestructured_calibration.rds")
    )
  }

  list(
    by_age = out_age,
    curated = out_all,
    calibration = calibration
  )
}


# -----------------------------------------------------------------------------
# 11. Example using the supplied inputs
# -----------------------------------------------------------------------------
#
# parameter_table <- read.csv(
#   "model_input_parameters.csv",
#   check.names = FALSE,
#   stringsAsFactors = FALSE
# )
# params <- parameter_table[parameter_table$disease == "Hib", , drop = FALSE]
#
# params$ve_carriage <- 0.64
# params$booster_given_primary <- 1.00
# params$external_foi_annual <- 1e-5
#
# result <- hib_agestructured_main(
#   coverage_df = hib_coverage_df,
#   pop_df = hib_population_df,
#   params = params,
#   contact_matrix_path =
#     "engaged_withgrpone_symmetric_matrix_2026-06-29_deposition_pprasad.csv",
#   hib_age_rates = "HFlu Data for COVE Age Groups.csv",
#   hib_trend = "HFlu Data for COVE.csv",
#   R0_pop = 1.3,
#   write = TRUE
# )
#
# result$by_age
# result$curated
# result$calibration$direct_coverage_diagnostic