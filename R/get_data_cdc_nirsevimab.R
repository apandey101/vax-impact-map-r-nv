# Create function get_data_cdc_nirsevimab for retrieving RSV monoclonal antibody
# (nirsevimab / clesrovimab) coverage among infants <8 months from CDC RSVVaxView
# --------------------------------------------------------------------------
get_data_cdc_nirsevimab <- function() {
  
  # Install & load required libraries
  # --------------------------------------------------------------------------
  packages <- c("here")
  install.packages(setdiff(packages, rownames(installed.packages())))
  invisible(lapply(packages, library, character.only = TRUE))
  
  # Set file location relative to current project
  # --------------------------------------------------------------------------
  suppressMessages(here::i_am("R/get_data_cdc_nirsevimab.R"))
  print("---c. get_data_cdc_nirsevimab.R")
  
  # Read the CSV from the data.cdc.gov Socrata API.
  #
  # Dataset: "Monthly Cumulative Number and Percent of Children <8 Months Who Received
  #           1+ Monoclonal Antibody Doses by Jurisdiction, United States"  (vhcj-3k53)
  # See: https://data.cdc.gov/Vaccinations/Monthly-Cumulative-Number-and-Percent-of-Children-/vhcj-3k53
  #
  # NOTE: this dataset covers EITHER nirsevimab or clesrovimab (the 2025-26 addition).
  # The nirsevimab-only equivalent is 4bdk-kyzv, if a product-specific series is wanted.
  #
  # IMPORTANT CAVEATS (documented by CDC on the source dashboard):
  #  * NO NATIONAL ROW. "National estimates are not presented since not all U.S.
  #    jurisdictions are currently reporting their IIS data to CDC." The national
  #    anchor row must be constructed in the processor from sum(numerator)/sum(denominator).
  #  * New York City and Philadelphia report SEPARATELY from their states; NY state data
  #    exclude NYC and PA state data exclude Philadelphia County. The processor re-aggregates.
  #  * Chicago is a funded jurisdiction but is ALREADY reported within Illinois.
  #  * IIS coverage may UNDERESTIMATE true coverage due to reporting incompleteness,
  #    and does NOT account for maternal RSV vaccination (an alternative product).
  # --------------------------------------------------------------------------
  
  df <- read.csv('https://data.cdc.gov/api/views/vhcj-3k53/rows.csv?accessType=DOWNLOAD&api_foundry=true')
  
  # Write data as a rds called cdc_nirsevimab.rds to the project `data-raw` folder
  write_path_rds <- here("data-raw/cdc_nirsevimab.rds")
  saveRDS(df, file = write_path_rds)
  
  # Write data as a csv called cdc_nirsevimab.csv to the project `data-raw` folder
  write_path_csv <- here("data-raw/csv/cdc_nirsevimab.csv")
  write.csv(df, file = write_path_csv)
  
}
