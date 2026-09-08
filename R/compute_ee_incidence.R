# Function to compute incidence at endemic equilibrium 
# --------------------------------------------------------------------------

compute_ee_incidence <- function(df) {
  
  # Install & load required libraries
  # --------------------------------------------------------------------------
  packages <- c("tidyverse","here")
  install.packages(setdiff(packages, rownames(installed.packages())))
  invisible(lapply(packages, library, character.only = TRUE))
  
  # Set file location relative to current project
  # --------------------------------------------------------------------------
  suppressMessages(here::i_am("R/compute_ee_incidence.R"))
  print("-D. compute_ee_incidence.R")
  
  ## Apply a naive assumption that the population of all birth cohorts, past and future, comprising the age group band of interest is the same
  # ex. If the age band of interest for rotavirus is age 0-4 years and the population is 100, then assume 0-1y, 1-2y, 2-3y, 3-4y, 4-5y all has population of 20 an that that pattern will continue for future birth cohorts
  # --------------------------------------------------------------------------
  population_turnover_rate_annual = 1 / df$age_group_length
  
  ## Calculate recovery rate based on number of days infectious
  # --------------------------------------------------------------------------
  recovery_rate_annual = 365/df$duration_infectious_days
  
  ## Get annual waning rate
  # --------------------------------------------------------------------------
  waning_rate_annual = df$waning_rate_annual
  
  ## RELATIVE INCIDENCE
  # Three model types are supported. Only the relative term differs; the prefactor
  # applied below is coverage-independent and cancels during calibration.
  #
  #   SIR / SIRS     : transmission at endemic equilibrium, with optional importation.
  #   static_direct  : NO transmission. Used for RSV/nirsevimab, where a seasonal
  #                    monoclonal antibody (and maternal vaccine) confer direct
  #                    protection to the recipient only (no herd effect, no
  #                    threshold). Relative incidence is just the unprotected
  #                    fraction, 1 - effective_structural_vaccine_coverage.
  #                    basic_reproduction_number and importation_delta are NOT used
  #                    and may be blank/NA.
  # --------------------------------------------------------------------------
  
  # -- transmission branch (SIR / SIRS) --
  ee_incidence_core = 1 - 1 / (df$basic_reproduction_number * (1 - df$effective_structural_vaccine_coverage))
  
  # Importation smoothing: an external/out-of-band force of infection keeps incidence
  # strictly positive below the herd threshold and smooths the kink. With
  # importation_delta = 0 this reduces exactly to pmax(core, 0), so diseases without
  # an importation term are numerically unchanged.
  #
  # importation_delta is optional. Hib and Varicella are handled by the
  # age-structured producers and are not in this data frame; the remaining
  # transmission diseases carry importation_delta = 0. If the column has been
  # removed from the parameter file, default to 0 so this branch is unaffected.
  importation_delta = if ("importation_delta" %in% names(df)) {
    ifelse(is.na(df$importation_delta), 0, df$importation_delta)
  } else {
    0
  }
  ee_incidence_rel_transmission = 0.5 * (ee_incidence_core + sqrt(ee_incidence_core^2 + 4 * importation_delta))
  
  # -- static direct-protection branch (RSV / nirsevimab + maternal vaccine) --
  ee_incidence_rel_static = pmax(0, 1 - df$effective_structural_vaccine_coverage)
  
  # -- select branch --
  ee_incidence_rel = ifelse(
                       df$model_type=='static_direct',
                       ee_incidence_rel_static,
                       ee_incidence_rel_transmission
                     )
  
  ## Compute incidence at endemic equilibrium
  # SIRS uses the waning-adjusted prefactor. SIR and static_direct use the population
  # turnover rate (for static_direct, age_group_length = 1, so this is simply 1).
  # --------------------------------------------------------------------------
  sirs_prefactor = ((recovery_rate_annual + population_turnover_rate_annual) * (population_turnover_rate_annual + waning_rate_annual)) / (recovery_rate_annual + population_turnover_rate_annual + waning_rate_annual)
  
  df$endemic_equilibrium_incidence_rate_annual <- ifelse(
                                                    df$model_type=='SIRS',
                                                    sirs_prefactor * ee_incidence_rel,
                                                    population_turnover_rate_annual * ee_incidence_rel
                                                  )
  
  return(df)
  
}