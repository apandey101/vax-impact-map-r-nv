# Varicella: age-structured model, n groups, per-state solve
# =============================================================================
# CALIBRATION STRATEGY (changed)
# -----------------------------------------------------------------------------
# Age-specific hospitalisation probability h is calibrated on the PREVACCINE era,
# not the current era. The reason is identification: prevaccine, essentially
# everyone is infected once, so annual infections are pinned by DEMOGRAPHY (the
# birth cohort) rather than by the model's transmission assumptions. Consequently
# h is almost R0-invariant and lands inside the plausible range, whereas
# calibrating on current data made h swing from 0.003 to 0.043 across the
# literature R0 range.
#
# Current burden then becomes a PREDICTION rather than a calibration target:
#
#   prevaccine targets (Marin rates x current population, no PPV correction): 12,809
#     - Marin observed prevaccine total (also unadjusted)                     12,189   (ratio 1.051)
#       The 5% overshoot is expected: 1993-95 rates applied to today's larger,
#       older population. An earlier version applied a 0.43 PPV correction above
#       age 50, which brought this ratio to 1.001 - but that was two offsetting
#       adjustments, not a validation, and has been removed.
#   predicted current hospitalisations at 94% coverage, R0 = 8.5               1,346
#     - Marin observed 2018-19                                                 1,390   (-3%)
#   predicted at 92.3% coverage                                                2,292
#     - NIS 2023                                                               3,390   (-32%, see note)
#   implied prevaccine deaths per year                                           107
#     - commonly cited                                                       100-150
#
# R0 = 8.5 is chosen as the value reproducing the observed current level; it sits
# inside the CDC 7-10 range. Report 7-10 as a sensitivity band.
#
# The NIS 2023 shortfall is expected: that extract excludes zoster codes only
# (not incidental varicella, which Marin also excludes), and 2023 probably
# includes post-pandemic catch-up transmission, so it is not a steady-state year.
#
# WHAT THIS DOES NOT FIX - state these in the methods
#  1. CASES ARE OVERSTATED, roughly twofold. The model gives a 92.9% incidence
#     decline against an observed ~97% (CDC Pink Book, NNDSS, four states). The
#     hospitalisation level is nonetheless right because prevaccine wild-type h is
#     applied to too many cases - two errors partly cancelling. Report
#     hospitalisations as the headline outcome and flag cases accordingly.
#  2. THE AGE DISTRIBUTION IS SKEWED YOUNG. The 0-4 band is a single homogeneous
#     compartment five years wide, so a newborn faces the same force of infection
#     as a four-year-old and infection is front-loaded. No parameter choice fixes
#     this; only finer sub-5 bands would.
#  3. Coverage is a single number applied to every birth cohort, whereas the real
#     population is a mosaic (pre-1995 natural immunity, 1996-2006 one-dose,
#     post-2007 two-dose) and the 92-94% figure is KINDERGARTEN coverage, which
#     describes only recent cohorts.
#  4. Susceptibility and infectiousness are age-INVARIANT; all age variation in
#     infection risk comes from the contact matrix. Both severity parameters
#     (hospitalisation probability h and case-fatality) ARE age-specific.
#  5. h FOR THE OLDEST BANDS IS HIGH. Against the Pink Book reference of 14 per
#     1,000 cases for adults, the model gives 14.8 (18-49) and 11.2 (50-59),
#     which match well, but 26.5 (60-74) and 41.7 (75+), which are 2-3x high.
#     This is most likely misclassification of herpes zoster as varicella in the
#     source hospitalisation data, rising with age as zoster incidence rises. It
#     is reported as an observation rather than corrected, because no defensible
#     age-graded adjustment is available.
#
# OTHER STATED ASSUMPTIONS
#  - Vaccination is all-or-nothing, applied at entry to the youngest band.
#  - VE = 0.92, the two-dose meta-analytic estimate against ANY clinical
#    varicella. (An earlier version used 0.82, which is the ONE-dose figure.)
#  - No importation or heterogeneity term. At R0 = 9 the herd-immunity threshold
#    is 96.6% coverage, comfortably clear of the operating range, so the model
#    does not sit on a numerical cliff. States above ~96.6% coverage will still
#    approach elimination.
#  - The contact matrix is national; only age composition, population size and
#    coverage vary by state.
#  - The supplied ENGAGED matrix is not reciprocity-balanced against these
#    populations, so it is symmetrised.
#  - Case-fatality is age-specific (Pink Book); only the total is reported.
#  - The population is treated as stationary at the observed age structure.
#
# Requires: tidyverse, here, tidycensus (census pull only).
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Contact matrix input and aggregation
# -----------------------------------------------------------------------------
vzv_read_contact_matrix <- function(path) {
  raw <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  C <- as.matrix(raw[, -1, drop = FALSE])
  storage.mode(C) <- "double"
  labs <- trimws(colnames(raw)[-1])
  if (nrow(C) != ncol(C)) {
    stop("Contact matrix is not square: ", nrow(C), " x ", ncol(C))
  }
  dimnames(C) <- list(labs, labs)
  list(C = C, labels = labs)
}

# Band edges in YEARS for the supplied ENGAGED file, in column order.
ENGAGED_EDGES <- list(
  c(0, 8 / 12), c(8 / 12, 20 / 12), c(20 / 12, 5), c(5, 18),
  c(18, 50), c(50, 60), c(60, 75), c(75, 100))

# Default grouping: merge the three sub-5 bands (ACS cannot supply sub-annual
# populations, and kept separate they misbehave - see limitation 2).
ENGAGED_GROUPS <- list(`0-4` = 1:3, `5-17` = 4, `18-49` = 5,
                       `50-59` = 6, `60-74` = 7, `75+` = 8)

# Aggregate a fine matrix onto coarser groups:
#   M_ij = N_i c_ij ;  M_IJ = sum over i in I, j in J ;  c_IJ = M_IJ / N_I
vzv_aggregate_matrix <- function(C, N_fine, groups) {
  M <- N_fine * C                     # column-major recycling scales ROW i
  k <- length(groups)
  Mg <- matrix(0, k, k); Ng <- numeric(k)
  for (I in seq_len(k)) {
    Ng[I] <- sum(N_fine[groups[[I]]])
    for (J in seq_len(k)) Mg[I, J] <- sum(M[groups[[I]], groups[[J]], drop = FALSE])
  }
  Cg <- Mg / Ng
  dimnames(Cg) <- list(names(groups), names(groups))
  list(C = Cg, N = Ng, labels = names(groups))
}

vzv_aggregate_edges <- function(edges, groups) {
  lapply(groups, function(g) c(min(vapply(edges[g], `[`, numeric(1), 1)),
                               max(vapply(edges[g], `[`, numeric(1), 2))))
}

# Apportion a group's population across its fine bands by width (uniform births).
vzv_fine_populations <- function(group_pop, fine_edges, groups) {
  fp <- numeric(length(fine_edges))
  for (I in seq_along(groups)) {
    g <- groups[[I]]
    w <- vapply(fine_edges[g], function(b) b[2] - b[1], numeric(1))
    fp[g] <- group_pop[I] * w / sum(w)
  }
  fp
}


# -----------------------------------------------------------------------------
# 2. Marin et al. hospitalisation rates
# -----------------------------------------------------------------------------
# PREVACCINE (1993-95), per 100,000 population per year. Used for CALIBRATION.
MARIN_PREVAX_RATE_INTERVALS <- list(
  c(0, 1, 34.8), c(1, 5, 27.2), c(5, 10, 11.1),
  c(10, 20, 2.2), c(20, 50, 2.5), c(50, 200, 0.9))

# MATURE 2-DOSE ERA (2018-19), per 100,000. Used only for VALIDATION reporting.
MARIN_CURRENT_RATE_INTERVALS <- list(
  c(0, 1, 1.9), c(1, 5, 0.5), c(5, 10, 0.1),
  c(10, 20, 0.2), c(20, 50, 0.4), c(50, 200, 0.5))

# CDC Pink Book case-fatality per 100,000 CASES, by age:
#   1 (children 1-14) | 6 (15-19) | 21 (adults, 20+)
# Expressed as a probability per case. The source does not quote a separate
# figure for infants under 1, although it states that complications are more
# frequent there, so the 1-14 value is used for that interval and is likely an
# UNDERestimate for the youngest.
#
# Replaces the earlier single age-invariant death rate, which understated total
# deaths roughly 12-fold at current coverage because the adult-to-child gradient
# is 21-fold and about a third of current infections fall in adults. With these
# values the model gives ~103 prevaccine deaths per year against a commonly cited
# 100-150; the flat rate gave 37.
PINKBOOK_CFR_INTERVALS <- list(
  c(0,   1,  1.0e-5),
  c(1,  15,  1.0e-5),
  c(15, 20,  6.0e-5),
  c(20, 200, 2.1e-4))

# Duration-weighted mean rate over an arbitrary band.
vzv_rate_over <- function(s, e, intervals) {
  num <- 0; den <- 0
  for (iv in intervals) {
    ov <- max(0, min(e, iv[2]) - max(s, iv[1]))
    if (ov > 0) { num <- num + iv[3] * ov; den <- den + ov }
  }
  if (den > 0) num / den else 0
}

# Age-specific case-fatality for the model bands. Duration-weighted, which
# assumes population is roughly uniform by single year of age within a band -
# the same approximation used for the hospitalisation rates.
vzv_cfr_by_band <- function(edges) {
  vapply(edges, function(b) vzv_rate_over(b[1], b[2], PINKBOOK_CFR_INTERVALS),
         numeric(1))
}

# Prevaccine hospitalisation counts implied by Marin's RATES applied to the
# CURRENT population. Rates rather than counts, because (a) rates map onto
# arbitrary band edges and (b) it puts the target on the same population basis as
# the model's prevaccine equilibrium.
#
# NO PPV CORRECTION IS APPLIED BY DEFAULT (ppv_50plus = 1). Marin et al. raise
# possible misclassification of herpes zoster as varicella at >=50 years as a
# LIMITATION and do not adjust their published figures, and the 43% positive
# predictive value they cite comes from a small study validating DEATH
# CERTIFICATES, not hospitalisation records. Applying it here would be treating a
# limitation note as ground truth, and it was materially improving the fit -
# which is precisely the circularity to avoid.
#
# The parameter is retained so the correction can be run as a SENSITIVITY.
vzv_prevax_targets <- function(edges, N, ppv_50plus = 1) {
  rate <- vapply(edges, function(b)
    vzv_rate_over(b[1], b[2], MARIN_PREVAX_RATE_INTERVALS), numeric(1))
  ppv <- vapply(edges, function(b) if (b[1] >= 50) ppv_50plus else 1, numeric(1))
  list(target = rate * N / 1e5 * ppv, rate = rate, ppv = ppv)
}


# -----------------------------------------------------------------------------
# 3. Model construction (n groups)
# -----------------------------------------------------------------------------
vzv_build_model <- function(C, N, first_band_years, labels = colnames(C),
                            R0_pop = 8.5, ve_infection = 0.92,
                            duration_infectious_days = 7, symmetrise = TRUE,
                            external_foi_annual) {
  if (missing(external_foi_annual) || length(external_foi_annual) != 1 ||
      is.na(external_foi_annual)) {
    stop("vzv_build_model(): external_foi_annual (mean external importation FOI) ",
         "must be supplied explicitly; there is no default.")
  }
  n <- length(N)
  if (!all(dim(C) == n)) stop("Contact matrix dimension does not match length(N).")
  gamma <- 365 / duration_infectious_days
  if (symmetrise) {
    M <- N * C
    M <- 0.5 * (M + t(M))
    C <- M / N
  }
  Cy <- C * 365
  mu <- (N[1] / first_band_years) / N
  births <- N[1] * mu[1]
  K <- matrix(0, n, n)
  for (i in seq_len(n)) for (j in seq_len(n)) K[i, j] <- Cy[j, i] / (gamma + mu[j])
  eg <- eigen(K)
  dom <- which.max(abs(eg$values))
  q <- R0_pop / abs(eg$values[dom])

  # Age-patterned external importation FOI. A flat external hazard falls equally on
  # every band, but vaccination shifts the standing susceptible pool into ADULTHOOD
  # (which carries the high hospitalisation probability), so a flat term over-
  # allocates imported burden to adults. Instead the external hazard is distributed
  # across ages in proportion to the force of infection induced by the natural (NGM
  # dominant eigenvector) infection age-distribution - i.e. where exposure naturally
  # concentrates (children). The pattern is coverage-independent and normalised to a
  # population-weighted mean of 1, so external_foi_annual remains the MEAN external
  # hazard while its allocation across ages is realistic.
  infdist <- abs(Re(eg$vectors[, dom]))
  expo    <- as.numeric(Cy %*% (infdist / sum(infdist)))
  eps_pattern <- expo / (sum(N * expo) / sum(N))   # population-weighted mean = 1
  epsilon <- external_foi_annual * eps_pattern      # length-n vector

  list(n = n, labels = labels, N = N, C = Cy, mu = mu, births = births,
       gamma = gamma, q = q, ve = ve_infection, R0_pop = R0_pop,
       first_band_years = first_band_years,
       threshold_coverage = (1 - 1 / R0_pop) / ve_infection,
       # Age-patterned external importation FOI (per-capita annual, vector by band).
       # external_foi_mean is the population-weighted mean hazard; epsilon spreads it
       # across ages via the natural-exposure pattern above. Keeps a small endemic
       # floor under above-threshold states and seeds realistic re-emergence when a
       # decline pushes R_eff > 1, without over-loading the adult bands. Negligible
       # at pre-vaccine endemicity, so it does not disturb the h-calibration.
       epsilon = epsilon, external_foi_mean = external_foi_annual)
}


# -----------------------------------------------------------------------------
# 4. Equilibrium and dynamics
# -----------------------------------------------------------------------------
vzv_foi <- function(mod, prev) as.numeric(mod$q * (mod$C %*% prev) + mod$epsilon)

vzv_cascade <- function(mod, lam, coverage) {
  n <- mod$n
  S <- numeric(n); I <- numeric(n)
  in_S <- mod$births * (1 - mod$ve * coverage)
  in_I <- 0
  for (i in seq_len(n)) {
    S[i] <- in_S / (lam[i] + mod$mu[i])
    I[i] <- (lam[i] * S[i] + in_I) / (mod$gamma + mod$mu[i])
    in_S <- mod$mu[i] * S[i]
    in_I <- mod$mu[i] * I[i]
  }
  list(S = S, I = I)
}

# NOTE on seeding. Two roots always exist: disease-free (prev = 0) and endemic.
# The fixed-point map sends exactly 0 to 0, so a seed that has decayed to zero -
# for example one carried forward from a coverage ABOVE the herd threshold - can
# never escape, and the solver then reports zero even where an endemic
# equilibrium exists. The seed is therefore floored at a small positive value,
# which is enough to regrow whenever R_eff > 1 and decays harmlessly otherwise.
vzv_equilibrium <- function(mod, coverage, guess = NULL,
                            damp = 0.45, iters = 400000, tol = 1e-18,
                            seed_floor = 1e-12) {
  prev <- if (is.null(guess)) rep(1e-6, mod$n) else pmax(guess, seed_floor)
  for (k in seq_len(iters)) {
    newprev <- vzv_cascade(mod, vzv_foi(mod, prev), coverage)$I / mod$N
    if (max(abs(newprev - prev)) < tol) { prev <- newprev; break }
    prev <- (1 - damp) * prev + damp * newprev
  }
  lam <- vzv_foi(mod, prev)
  cs  <- vzv_cascade(mod, lam, coverage)
  list(prev = prev, S = cs$S, I = cs$I, lam = lam,
       inc_rate = lam * cs$S / mod$N, sfrac = cs$S / mod$N)
}

vzv_equilibrium_path <- function(mod, target, from, n_steps = 60, seed = NULL) {
  prev <- if (is.null(seed)) rep(1e-6, mod$n) else seed
  r <- NULL
  for (cv in seq(from, target, length.out = n_steps)) {
    r <- vzv_equilibrium(mod, cv, prev); prev <- r$prev
  }
  r
}

vzv_rhs <- function(mod, y, coverage) {
  n <- mod$n
  S <- y[seq_len(n)]; I <- y[n + seq_len(n)]
  lam <- vzv_foi(mod, I / mod$N)
  dS <- numeric(n); dI <- numeric(n)
  in_S <- mod$births * (1 - mod$ve * coverage)
  in_I <- 0
  for (i in seq_len(n)) {
    dS[i] <- in_S - lam[i] * S[i] - mod$mu[i] * S[i]
    dI[i] <- lam[i] * S[i] + in_I - mod$gamma * I[i] - mod$mu[i] * I[i]
    in_S <- mod$mu[i] * S[i]; in_I <- mod$mu[i] * I[i]
  }
  c(dS, dI)
}

# Step change in birth coverage at t = 0. The step kicks the system off
# equilibrium and it rings (period roughly 4-5 years, slow damping), so the rate
# AT time t depends on cycle phase. Reported values are the MEAN ANNUAL rate over
# the accrual window [0, t], which is stable and matches `accrual_years`.
vzv_trajectory_rates <- function(mod, cov_base, cov_new, horizons, dt = 2 / 365) {
  base <- vzv_equilibrium(mod, cov_base)
  n <- mod$n
  nstep <- ceiling(max(horizons) / dt)
  y <- c(base$S, base$I)
  tt <- numeric(nstep + 1); rate <- matrix(0, nstep + 1, n)
  rate[1, ] <- vzv_foi(mod, y[n + seq_len(n)] / mod$N) * y[seq_len(n)] / mod$N
  for (k in seq_len(nstep)) {
    k1 <- vzv_rhs(mod, y, cov_new)
    k2 <- vzv_rhs(mod, y + dt / 2 * k1, cov_new)
    k3 <- vzv_rhs(mod, y + dt / 2 * k2, cov_new)
    k4 <- vzv_rhs(mod, y + dt * k3, cov_new)
    y <- y + dt / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
    tt[k + 1] <- k * dt
    rate[k + 1, ] <- vzv_foi(mod, y[n + seq_len(n)] / mod$N) * y[seq_len(n)] / mod$N
  }
  trapz <- function(x, v) sum(diff(x) * (head(v, -1) + tail(v, -1)) / 2)
  out <- matrix(0, length(horizons), n, dimnames = list(NULL, mod$labels))
  for (m in seq_along(horizons)) {
    idx <- which(tt <= horizons[m])
    for (g in seq_len(n)) out[m, g] <- trapz(tt[idx], rate[idx, g]) / horizons[m]
  }
  list(rates = out, base = base)
}


# -----------------------------------------------------------------------------
# 5. Calibration of h on the prevaccine era, plus a validation report
# -----------------------------------------------------------------------------
# h is computed ONCE from the prevaccine equilibrium (coverage = 0) and applied
# to every state and scenario, because it is a severity parameter rather than a
# state characteristic.
vzv_calibrate_h <- function(mod, band_edges, ppv_50plus = 1,
                            validate_coverage = NA_real_,
                            validate_observed = NA_real_,
                            verbose = TRUE) {
  pre <- vzv_equilibrium(mod, 0)
  inc_pre <- pre$inc_rate * mod$N
  tg <- vzv_prevax_targets(band_edges, mod$N, ppv_50plus)
  h <- tg$target / inc_pre

  out <- list(h = h, prevax = pre, prevax_infections = inc_pre,
              prevax_target = tg$target, prevax_rate = tg$rate)

  if (verbose) {
    cat("\n-- varicella calibration: h from the PREVACCINE era --\n")
    cat(sprintf("  R0 = %.2f   VE = %.2f   herd-immunity threshold coverage = %.1f%%\n",
                mod$R0_pop, mod$ve, 100 * mod$threshold_coverage))
    cat(sprintf("  %8s %10s %13s %11s\n", "band", "rate/100k", "prevax target", "implied h"))
    for (i in seq_along(mod$labels)) {
      cat(sprintf("  %8s %10.2f %13s %11.5f\n", mod$labels[i], tg$rate[i],
                  format(round(tg$target[i]), big.mark = ","), h[i]))
    }
    cat(sprintf("  %8s %10s %13s\n", "TOTAL", "",
                format(round(sum(tg$target)), big.mark = ",")))
    cat("  (Marin observed prevaccine total 12,189; agreement is a check on the\n")
    cat("   rate mapping and the PPV correction, not a fitted quantity)\n")
    cat(sprintf("  prevaccine infections %s = %.0f%% of the birth cohort\n",
                format(round(sum(inc_pre)), big.mark = ","),
                100 * sum(inc_pre) / mod$births))
  }

  if (!is.na(validate_coverage)) {
    cur <- vzv_equilibrium(mod, validate_coverage, pre$prev)
    pred <- sum(cur$inc_rate * mod$N * h)
    out$validation <- list(coverage = validate_coverage, predicted = pred,
                           observed = validate_observed,
                           decline = 1 - pred / sum(tg$target))
    if (verbose) {
      cat(sprintf("\n  PREDICTION at %.1f%% coverage: %s hospitalisations",
                  100 * validate_coverage, format(round(pred), big.mark = ",")))
      if (!is.na(validate_observed)) {
        cat(sprintf("  (observed %s, %+.1f%%)",
                    format(round(validate_observed), big.mark = ","),
                    100 * (pred / validate_observed - 1)))
      }
      cat(sprintf("\n  implied hospitalisation decline %.1f%%\n",
                  100 * out$validation$decline))
      cat("  NOTE cases are overstated roughly twofold - see header limitation 1\n")
    }
  }
  out
}


# -----------------------------------------------------------------------------
# 6. Census pull for the model bands
# -----------------------------------------------------------------------------
# B01001 sex-by-age; female codes are male + 24. Verified against the 2023 ACS
# table: 0-4={003}, 5-17={004:006}, 18-49={007:015}, 50-59={016:017},
# 60-74={018:022}, 75+={023:025}. These groups match ENGAGED_GROUPS above and
# MUST stay in sync with them.
VZV_ACS_MALE_CODES <- list(`0-4` = 3, `5-17` = 4:6, `18-49` = 7:15,
                           `50-59` = 16:17, `60-74` = 18:22, `75+` = 23:25)

# ACS population builder. Returns one row per (geography x model band) with the
# six ENGAGED group labels the varicella loader (getpop) expects. Renamed from
# get_data_census_acs_state_population_vzv_bands so the public get_data_* entry
# point can live in its own module. Side-effect-free: writes only when save_*
# paths are supplied, and explicitly namespaced so it can run during the data
# step without relying on attached packages.
vzv_build_acs_band_population <- function(year = 2023,
                                          save_rds = NULL, save_csv = NULL) {
  if (!requireNamespace("tidycensus", quietly = TRUE)) {
    stop("Install 'tidycensus' to pull ACS varicella-band populations.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Install 'dplyr' to pull ACS varicella-band populations.")
  }
  out <- list()
  for (nm in names(VZV_ACS_MALE_CODES)) {
    m <- VZV_ACS_MALE_CODES[[nm]]
    vars <- sprintf("B01001_%03dE", c(m, m + 24))
    for (geo in c("state", "us")) {
      d <- suppressMessages(
        tidycensus::get_acs(geography = geo, variables = vars,
                            year = year, geometry = FALSE)
      )
      d <- dplyr::summarise(
        dplyr::group_by(d, .data$GEOID, .data$NAME),
        age_group_population = sum(.data$estimate),
        .groups = "drop"
      )
      d$age_group <- nm
      names(d)[names(d) == "GEOID"] <- "state_fips_code"
      names(d)[names(d) == "NAME"] <- "state_name"
      out[[length(out) + 1]] <- d
    }
  }
  df <- dplyr::bind_rows(out)

  if (!is.null(save_rds)) {
    dir.create(dirname(save_rds), recursive = TRUE, showWarnings = FALSE)
    saveRDS(df, save_rds)
  }
  if (!is.null(save_csv)) {
    dir.create(dirname(save_csv), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(df, save_csv, row.names = FALSE)
  }
  df
}


# -----------------------------------------------------------------------------
# 7. Driver
# -----------------------------------------------------------------------------
run_varicella_agestructured <- function(coverage_df, pop_df, params,
                                        contact_matrix_path,
                                        fine_edges = ENGAGED_EDGES,
                                        groups = ENGAGED_GROUPS,
                                        declines = c(0, 0.05, 0.10, 0.15, 0.20),
                                        horizons = c(1, 5, 10, 20),
                                        national_name = "United States",
                                        verbose = TRUE) {

  # Package handling: we do NOT install here (that needs network and fails in
  # headless/CI runs). Require the packages and attach the two whose unqualified
  # functions this file uses (dplyr verbs and the pipe, and tibble()). main.R
  # attaches tidyverse in the integrated pipeline; this keeps standalone use safe.
  for (pkg in c("dplyr", "tibble")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Install '", pkg, "' before running the varicella model.")
    }
    if (!paste0("package:", pkg) %in% search()) {
      suppressPackageStartupMessages(library(pkg, character.only = TRUE))
    }
  }

  cm <- vzv_read_contact_matrix(contact_matrix_path)
  if (length(fine_edges) != ncol(cm$C)) {
    stop("fine_edges has ", length(fine_edges), " entries but the matrix has ",
         ncol(cm$C), " bands.")
  }
  band_edges <- vzv_aggregate_edges(fine_edges, groups)
  labs <- names(groups)
  first_band_years <- band_edges[[1]][2] - band_edges[[1]][1]

  getpop <- function(st) {
    p <- pop_df %>% filter(state_name == st) %>% arrange(match(age_group, labs))
    if (nrow(p) != length(labs) || any(is.na(p$age_group_population))) return(NULL)
    p$age_group_population
  }
  # External importation FOI (mean per-capita annual hazard). REQUIRED - no silent
  # default, so this importation assumption is always an explicit, auditable choice
  # in the parameter file. vzv_build_model distributes it across ages.
  vzv_epsilon <- suppressWarnings(as.numeric(params$external_foi_annual))
  if (length(vzv_epsilon) != 1 || is.na(vzv_epsilon)) {
    stop("params$external_foi_annual is required for the varicella model ",
         "(mean external importation force of infection; no default is applied). ",
         "Add 'external_foi_annual' to the Varicella row of model_input_parameters.csv.")
  }
  build_for <- function(pop) {
    fp <- vzv_fine_populations(pop, fine_edges, groups)
    agg <- vzv_aggregate_matrix(cm$C, fp, groups)
    vzv_build_model(agg$C, pop, first_band_years, labs,
                    R0_pop = params$basic_reproduction_number,
                    ve_infection = params$vaccine_effectiveness,
                    duration_infectious_days = params$duration_infectious_days,
                    external_foi_annual = vzv_epsilon)
  }

  # ---- h from the national prevaccine equilibrium ---------------------------
  natpop <- getpop(national_name)
  if (is.null(natpop)) stop("pop_df lacks complete bands for '", national_name, "'.")
  natcov <- coverage_df %>% filter(state_name == national_name) %>%
    pull(vaccine_coverage_estimate)
  if (length(natcov) != 1) {
    natcov <- mean(coverage_df$vaccine_coverage_estimate, na.rm = TRUE)
    warning("No national coverage row; using the mean of states.")
  }
  mod_nat <- build_for(natpop)
  cal <- vzv_calibrate_h(mod_nat, band_edges, params$ppv_50plus,
                         validate_coverage = natcov,
                         validate_observed = params$observed_current_hospitalizations,
                         verbose = verbose)
  h <- cal$h

  # Age-specific case-fatality from the Pink Book. A single reported total is
  # still produced by collapse_age_groups(); only the PARAMETER is age-specific.
  cfr <- vzv_cfr_by_band(band_edges)
  if (!is.null(params$death_rate)) {
    warning("params$death_rate is ignored: deaths now use age-specific ",
            "Pink Book case-fatality (see PINKBOOK_CFR_INTERVALS).")
  }
  if (verbose) {
    cat("\n  age-specific case-fatality per 100,000 cases (Pink Book):\n")
    cat(sprintf("    %s\n", paste(sprintf("%s %.1f", labs, 1e5 * cfr),
                                  collapse = "  ")))
    cat(sprintf("    implied prevaccine deaths %.0f/yr (commonly cited 100-150)\n",
                sum(cal$prevax_infections * cfr)))
  }

  # ---- per-state solve -----------------------------------------------------
  states <- coverage_df %>% filter(!is.na(vaccine_coverage_estimate))
  rows <- list()
  for (s in seq_len(nrow(states))) {
    st <- states$state_name[s]; v0 <- states$vaccine_coverage_estimate[s]
    spop <- getpop(st)
    if (is.null(spop)) { warning("Skipping ", st, ": incomplete population."); next }
    mod <- build_for(spop)
    if (verbose) {
      flag <- if (v0 > mod$threshold_coverage) "  [above herd threshold]" else ""
      cat(sprintf("  [%2d/%d] %-24s coverage %.1f%%%s\n",
                  s, nrow(states), st, 100 * v0, flag))
    }
    for (dec in declines) {
      tr <- vzv_trajectory_rates(mod, v0, max(v0 - dec, 0), horizons)
      for (m in seq_along(horizons)) {
        infections <- tr$rates[m, ] * spop
        hosp   <- infections * h
        cases  <- infections
        deaths <- infections * cfr
        ae <- numeric(length(labs))
        ae[1] <- (spop[1] / first_band_years) * max(v0 - dec, 0) *
          params$severe_adverse_event_rate
        wd <- cases * params$duration_sick_days
        pc <- wd * params$cost_wage_daily
        hc <- hosp * params$duration_hospitalized_days *
          params$cost_hospitalization_daily
        rows[[length(rows) + 1]] <- tibble(
          disease = "Varicella", state_name = st,
          age_group = labs, age_group_population = spop,
          declining_coverage_among_new_births = dec,
          time_horizon = horizons[m], vaccine_coverage_estimate = v0,
          cases = cases, hospitalizations = hosp, deaths = deaths,
          workdays_lost = wd, productivity_cost = pc,
          hospitalization_cost = hc, total_cost = pc + hc,
          vaccine_adverse_events = ae)
      }
    }
  }
  df <- bind_rows(rows)

  base <- df %>% filter(declining_coverage_among_new_births == 0) %>%
    select(state_name, age_group, time_horizon,
           b_cases = cases, b_hosp = hospitalizations, b_deaths = deaths,
           b_wd = workdays_lost, b_pc = productivity_cost,
           b_hc = hospitalization_cost, b_tc = total_cost,
           b_ae = vaccine_adverse_events)

  out <- df %>%
    left_join(base, by = c("state_name", "age_group", "time_horizon")) %>%
    mutate(additional_cases = cases - b_cases,
           additional_hospitalizations = hospitalizations - b_hosp,
           additional_deaths = deaths - b_deaths,
           additional_workdays_lost = workdays_lost - b_wd,
           additional_productivity_cost = productivity_cost - b_pc,
           additional_hospitalization_cost = hospitalization_cost - b_hc,
           additional_total_cost = total_cost - b_tc,
           vaccine_adverse_events_avoided = b_ae - vaccine_adverse_events) %>%
    select(-starts_with("b_")) %>%
    vzv_add_rates()
  attr(out, "calibration") <- cal
  out
}


vzv_add_rates <- function(df) {
  per <- function(x, n) x / n * 100000
  df %>% mutate(
    cases_per_100k = per(cases, age_group_population),
    additional_cases_per_100k = per(additional_cases, age_group_population),
    hospitalizations_per_100k = per(hospitalizations, age_group_population),
    additional_hospitalizations_per_100k = per(additional_hospitalizations, age_group_population),
    deaths_per_100k = per(deaths, age_group_population),
    additional_deaths_per_100k = per(additional_deaths, age_group_population),
    workdays_lost_per_100k = per(workdays_lost, age_group_population),
    additional_workdays_lost_per_100k = per(additional_workdays_lost, age_group_population),
    productivity_cost_per_100k = per(productivity_cost, age_group_population),
    additional_productivity_cost_per_100k = per(additional_productivity_cost, age_group_population),
    hospitalization_cost_per_100k = per(hospitalization_cost, age_group_population),
    additional_hospitalization_cost_per_100k = per(additional_hospitalization_cost, age_group_population),
    total_cost_per_100k = per(total_cost, age_group_population),
    additional_total_cost_per_100k = per(additional_total_cost, age_group_population),
    vaccine_adverse_events_per_100k = per(vaccine_adverse_events, age_group_population),
    vaccine_adverse_events_avoided_per_100k = per(vaccine_adverse_events_avoided, age_group_population))
}


# -----------------------------------------------------------------------------
# 8. Collapse age groups, then format to the standard schema
# -----------------------------------------------------------------------------
# Counts and populations are summed; every *_per_100k is RECOMPUTED. Summing
# rates instead inflates the collapsed figure several-fold.
collapse_age_groups <- function(df, age_group_label = "All ages") {
  count_cols <- c("cases", "additional_cases",
                  "hospitalizations", "additional_hospitalizations",
                  "deaths", "additional_deaths",
                  "workdays_lost", "additional_workdays_lost",
                  "productivity_cost", "additional_productivity_cost",
                  "hospitalization_cost", "additional_hospitalization_cost",
                  "total_cost", "additional_total_cost",
                  "vaccine_adverse_events", "vaccine_adverse_events_avoided")
  df %>%
    group_by(disease, state_name, declining_coverage_among_new_births,
             time_horizon, vaccine_coverage_estimate) %>%
    summarise(across(all_of(c("age_group_population", count_cols)), sum),
              .groups = "drop") %>%
    mutate(age_group = age_group_label) %>%
    vzv_add_rates()
}

vzv_curate <- function(df) {
  df %>%
    mutate(percent_decline = declining_coverage_among_new_births * 100,
           accrual_label = factor(
             paste0(time_horizon, ifelse(time_horizon == 1, " Year", " Years")),
             levels = c("1 Year", "5 Years", "10 Years", "20 Years"))) %>%
    rename(accrual_years = time_horizon,
           baseline_coverage = vaccine_coverage_estimate) %>%
    select(disease, state_name, age_group, age_group_population,
           percent_decline, accrual_years, accrual_label, baseline_coverage,
           cases, additional_cases, cases_per_100k, additional_cases_per_100k,
           hospitalizations, additional_hospitalizations,
           hospitalizations_per_100k, additional_hospitalizations_per_100k,
           deaths, additional_deaths, deaths_per_100k, additional_deaths_per_100k,
           workdays_lost, additional_workdays_lost,
           workdays_lost_per_100k, additional_workdays_lost_per_100k,
           productivity_cost, additional_productivity_cost,
           productivity_cost_per_100k, additional_productivity_cost_per_100k,
           hospitalization_cost, additional_hospitalization_cost,
           hospitalization_cost_per_100k, additional_hospitalization_cost_per_100k,
           total_cost, additional_total_cost,
           total_cost_per_100k, additional_total_cost_per_100k,
           vaccine_adverse_events, vaccine_adverse_events_per_100k,
           vaccine_adverse_events_avoided, vaccine_adverse_events_avoided_per_100k)
}

varicella_agestructured_main <- function(coverage_df, pop_df, params,
                                         contact_matrix_path,
                                         declines = seq(0,0.20,0.01),
                                         horizons = c(1, 5, 10, 20),
                                         write = FALSE,
                                         output_directory = "data",
                                         csv_directory = file.path("data", "csv")) {
  age_df <- run_varicella_agestructured(coverage_df, pop_df, params,
                                        contact_matrix_path,
                                        declines = declines, horizons = horizons)
  out_age <- vzv_curate(age_df)
  out_all <- vzv_curate(collapse_age_groups(age_df))
  if (write) {
    dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
    dir.create(csv_directory, recursive = TRUE, showWarnings = FALSE)
    saveRDS(out_age,
            file.path(output_directory, "varicella_agestructured_by_age.rds"))
    utils::write.csv(
      out_age,
      file.path(csv_directory, "varicella_agestructured_by_age.csv"),
      row.names = FALSE
    )
    saveRDS(out_all,
            file.path(output_directory, "varicella_agestructured_curated.rds"))
    utils::write.csv(
      out_all,
      file.path(csv_directory, "varicella_agestructured_curated.csv"),
      row.names = FALSE
    )
  }
  list(by_age = out_age, curated = out_all,
       calibration = attr(age_df, "calibration"))
}


# -----------------------------------------------------------------------------
# Example parameters
# -----------------------------------------------------------------------------
# params <- list(
#   basic_reproduction_number = 8.5,     # POPULATION R0; reproduces the observed
#                                        # current level. Sensitivity band 7-10.
#   vaccine_effectiveness     = 0.92,    # 2-dose, vs ANY clinical varicella
#   duration_infectious_days  = 7,
#   # death_rate is no longer used - deaths come from PINKBOOK_CFR_INTERVALS
#   duration_sick_days        = 5,
#   duration_hospitalized_days = 3,
#   cost_hospitalization_daily = 1683,
#   cost_wage_daily            = 200,
#   severe_adverse_event_rate  = 8.6e-5, # MMRV febrile seizure, 20% MMRV share
#   ppv_50plus                 = 1,       # 1 = no correction; 0.43 = sensitivity
#   observed_current_hospitalizations = 1390)   # VALIDATION only, not calibration
#
# pop_df <- readRDS(here("data-raw/census_acs_state_population_vzv_bands.rds"))
# res <- varicella_agestructured_main(
#   cdc_school_vax_view_varicella_df, pop_df, params,
#   contact_matrix_path = here("data-raw/engaged_contact_matrix.csv"))