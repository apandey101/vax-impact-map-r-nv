# Compute incidence at endemic equilibrium by scenario for the diseases of interest
# --------------------------------------------------------------------------

calculate_structural_vaccine_coverage <- function(df) {
  
  # Install & load required libraries
  # --------------------------------------------------------------------------
  packages <- c("tidyverse","here")
  install.packages(setdiff(packages, rownames(installed.packages())))
  invisible(lapply(packages, library, character.only = TRUE))
  
  # Set file location relative to current project
  # --------------------------------------------------------------------------
  suppressMessages(here::i_am("R/calculate_structural_vaccine_coverage.R"))
  print("-C. calculate_structural_vaccine_coverage.R")
  
  ## Calculate vaccine coverage among birth cohorts impacted by declining vaccine coverage
  df$coverage_with_decline_applied <- pmax(df$vaccine_coverage_estimate - df$declining_coverage_among_new_births,0)
  
  ## Calculate structural vaccine coverage
  # --------------------------------------------------------------------------
  
  # Consider when the time horizon is larger than the size of the size of the age group...
  
  # The entire age group will reflect the declining coverage from the current baseline vaccine coverage estimate.
  # Otherwise, some birth cohorts will be impacted by the declining coverage (declining coverage component) while some will not (baseline coverage component)
  # Also, coverage cannot drop below 0 so use pmax to ensure coverage is not negative
  df$structural_vaccine_coverage <- ifelse(df$time_horizon > df$age_group_length,
                                           df$coverage_with_decline_applied,
                                           (((df$age_group_length - df$time_horizon) * df$vaccine_coverage_estimate) + # baseline coverage component
                                              ((df$time_horizon) * df$coverage_with_decline_applied)) / # declined coverage component
                                             df$age_group_length)
  
  ## Calculate effective structural vaccine coverage by multiplying structural vaccine coverage by vaccine effectiveness
  # --------------------------------------------------------------------------
  df$effective_structural_vaccine_coverage <- df$structural_vaccine_coverage * df$vaccine_effectiveness

  ## RSV (static_direct): two-product direct protection with a FIXED maternal layer.
  # Infants are protected by EITHER the maternal RSV vaccine OR infant monoclonal
  # antibody / nirsevimab, treated as mutually exclusive per CDC guidance. Maternal
  # coverage and effectiveness are national fixed values, HARDWIRED here (not read
  # from the parameter file): 41.6% cumulative maternal coverage (CDC RSVVaxView,
  # applied uniformly to all states as a simplification) and 70% maternal VE against
  # infant RSV hospitalization. Effective protection is additive:
  #
  #     v = m*VE_m + c*VE_mAb
  #
  # where c is the phased/declined mAb coverage (structural_vaccine_coverage) and
  # VE_mAb is the vaccine_effectiveness column. Only the mAb term responds to the
  # decline scenarios; maternal is a fixed floor. The mAb distinct fraction is
  # capped at (1 - m) so m + c <= 1, and v is clamped to [0, 1].
  # --------------------------------------------------------------------------
  RSV_MATERNAL_COVERAGE <- 0.416   # CDC RSVVaxView cumulative, national, fixed
  RSV_MATERNAL_VE       <- 0.70    # maternal VE vs infant RSV hospitalization

  if (any(df$model_type == "static_direct", na.rm = TRUE)) {
    is_static <- df$model_type == "static_direct" & !is.na(df$model_type)
    m     <- RSV_MATERNAL_COVERAGE
    ve_m  <- RSV_MATERNAL_VE
    c_mab <- pmin(df$structural_vaccine_coverage, max(1 - m, 0))  # enforce m + c <= 1
    v_rsv <- pmin(pmax(m * ve_m + c_mab * df$vaccine_effectiveness, 0), 1)
    df$effective_structural_vaccine_coverage <- ifelse(
      is_static, v_rsv, df$effective_structural_vaccine_coverage
    )
  }

  return(df)
  
}