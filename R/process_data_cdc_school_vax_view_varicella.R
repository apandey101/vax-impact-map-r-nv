# Create function process_data_cdc_school_vax_view_varicella for processing cdc_school_vax_view.rds
# --------------------------------------------------------------------------
process_data_cdc_school_vax_view_varicella <- function() {
  
  # Install & load required libraries
  # --------------------------------------------------------------------------
  packages <- c("here","tidyverse")
  install.packages(setdiff(packages, rownames(installed.packages())))
  invisible(lapply(packages, library, character.only = TRUE))
  
  # Set file location relative to current project
  # --------------------------------------------------------------------------
  suppressMessages(here::i_am("R/process_data_cdc_school_vax_view_varicella.R"))
  print("----ii. process_data_cdc_school_vax_view_varicella.R")
  
  # Create function process_data_cdc_school_vax_view_varicella by reading cdc_school_vax_view.rds in data-raw
  # --------------------------------------------------------------------------
  
  # Read cdc_school_vax_view.rds from the project `data-raw` folder
  read_path_rds <- here("data-raw/cdc_school_vax_view.rds")
  df <- readRDS(read_path_rds)
  
  # Filter the data for varicella, up-to-date (UTD) dose among kindergartners.
  # UTD is used (rather than 1 dose or 2 dose) because the required number of
  # doses varies by state; UTD reflects each state's own requirement.
  #Change on 8/17: UTD is no longer reported for varicella, so changing the code based on availability and priority
  df_processed <-  df %>%
  filter(
    Vaccine.Exemption == "Varicella",
    Dose %in% c(
      "UTD (unknown disease history)",
      "1 Dose (or disease history)",
      "2 Doses (or disease history)"
    ),
    Geography.Type %in% c("States", "National"),
    School.Year == "2025-26",
    !Geography %in% c(
      "NY-City of New York",
      "TX-City of Houston",
      "U.S. Median",
      "Puerto Rico"
    )
  ) %>%
  mutate(
    vaccine_coverage_estimate = readr::parse_number(
      as.character(Estimate....),
      na = c("", "NA", "NReq", "NR", "NP", "NULL")
    ),
    dose_priority = case_when(
      Dose == "UTD (unknown disease history)" ~ 3L,
      str_starts(Dose, "2 Doses") ~ 2L,
      str_starts(Dose, "1 Dose") ~ 1L,
      TRUE ~ 0L
    )
  ) %>%
  filter(!is.na(vaccine_coverage_estimate)) %>%
  group_by(Vaccine.Exemption, Geography, School.Year) %>%
  slice_max(
    order_by = dose_priority,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  transmute(
    vaccine = Vaccine.Exemption,
    state_name = Geography,
    vaccine_coverage_estimate
  )

if (nrow(df_processed) == 0L) {
  stop(
    "No usable 2025-26 varicella coverage records were found. ",
    "Check the Dose and School.Year values in the raw SchoolVaxView data."
  )
}
  
  # Write data as a rds called cdc_school_vax_view_varicella.rds to the project `data-raw` folder
  write_path_rds <- here("data-raw/cdc_school_vax_view_varicella.rds")
  saveRDS(df_processed, file = write_path_rds)
  
  # Message specifying where data was written
  # print(paste0("Saved state data to ",write_path_rds))
  
  # Write data as a csv called cdc_school_vax_view_varicella.csv to the project `data-raw` folder
  write_path_csv <- here("data-raw/csv/cdc_school_vax_view_varicella.csv")
  write.csv(df_processed, file = write_path_csv)
  
  # Message specifying where data was written
  # print(paste0("Saved state data to ",write_path_csv))
  
}
