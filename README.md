# vax-impact-map-r-nv

This repository contains the vaccine-impact model pipeline used for current outputs.

## Which entry point should I run?

- **Maintained pipeline:** `R/main.R` (recommended)
- **Legacy interface:** `app.R` (Shiny app is still runnable, but it is **not maintained** to reflect recent model updates)

## Run the maintained model pipeline

1. Open the project in R/RStudio from the repository root.
2. Ensure you have internet access for package installs and data pulls.
3. Configure a Census API key for `tidycensus` before data refresh steps (see `tidycensus::census_api_key`).
4. Run:

```r
source("R/main.R")
```

`main.R` runs this end-to-end sequence:
1. `get_and_process_data()`  
2. `run_model()`  
3. `curate_model_output()`  
4. `combine_model_output()`

Main output files:
- `data/csv/vax_impact_map_model_output_curated.csv`
- `data/vax_impact_map_model_output_curated.rds`

## Optional: run the legacy Shiny app

The app remains available for exploration, but it may not match the latest maintained model logic.

```r
shiny::runApp("app.R")
```

## R scripts map (model-focused)

| R file(s) | Purpose |
|---|---|
| `R/main.R` | Top-level orchestrator for the maintained workflow. |
| `R/get_and_process_data.R` | Runs data retrieval and processing wrappers. |
| `R/get_data.R` | Pulls CDC/Census/model-parameter/geography inputs. |
| `R/get_data_cdc.R` | Wrapper for CDC input pulls by disease/source. |
| `R/get_data_cdc_coverage_timeseries.R` | Pulls CDC baseline coverage trend data. |
| `R/get_data_cdc_school_exemptions_timeseries.R` | Pulls CDC school exemption trend data. |
| `R/get_data_cdc_nirsevimab.R` | Pulls CDC RSV/nirsevimab-related data. |
| `R/get_data_census.R` | Wrapper for Census input pulls. |
| `R/get_data_census_acs_states.R` | Pulls ACS state-level demographic context. |
| `R/get_data_census_acs_state_population_0_4_years.R`, `R/get_data_census_acs_state_population_0_14_years.R`, `R/get_data_census_acs_state_population_0_19_years.R`, `R/get_data_census_acs_state_population.R` | Pull age-banded and total population inputs used by equilibrium models. |
| `R/get_data_census_acs_state_population_hib_bands.R`, `R/get_data_census_acs_state_population_vzv_bands.R` | Pull ACS bands required by age-structured Hib/Varicella models. |
| `R/get_data_model_input_parameters.R` | Loads model parameter inputs. |
| `R/get_data_tigris_states.R` | Pulls state shapefiles/geometries for map outputs. |
| `R/process_data.R` | Wrapper for all input processing steps. |
| `R/process_data_cdc.R` | Wrapper for CDC cleaning/transforms. |
| `R/process_data_cdc_child_vax_view.R` | Cleans CDC ChildVaxView coverage inputs. |
| `R/process_data_cdc_child_vax_view_hib.R`, `R/process_data_cdc_child_vax_view_pcv.R`, `R/process_data_cdc_child_vax_view_rotavirus.R` | Disease-specific ChildVaxView processing modules. |
| `R/process_data_cdc_school_vax_view.R` | Cleans CDC SchoolVaxView inputs. |
| `R/process_data_cdc_school_vax_view_dtap.R`, `R/process_data_cdc_school_vax_view_varicella.R` | DTaP/Varicella-specific SchoolVaxView processing modules. |
| `R/process_data_cdc_nirsevimab.R` | Processes nirsevimab-related CDC inputs. |
| `R/run_model.R` | Core equilibrium-model pipeline and output writer. |
| `R/read_data.R` | Reads prepared input datasets into the model pipeline. |
| `R/validate_model_input.R` | Validates compiled model input consistency/quality. |
| `R/compile_model_input_data.R` | Joins processed inputs into model-ready table(s). |
| `R/calculate_structural_vaccine_coverage.R` | Calculates structural and effective coverage values. |
| `R/impute_rsv_mab_coverage.R` | Fills RSV monoclonal antibody coverage assumptions. |
| `R/compute_ee_incidence.R` | Computes equilibrium-endemic incidence by scenario. |
| `R/calculate_infections.R` | Converts incidence to infection counts. |
| `R/calculate_disease_burden.R` | Estimates cases, hospitalizations, deaths, and rates. |
| `R/calibrate.R` | Wrapper for disease-specific calibration passes. |
| `R/calibrate_pertussis.R`, `R/calibrate_pneumo.R`, `R/calibrate_rota.R`, `R/calibrate_rsv.R` | Disease-level calibration logic. |
| `R/calibrate_pertussis_cases.R`, `R/calibrate_pertussis_hospitalizations.R`, `R/calibrate_pertussis_deaths.R`, `R/calibrate_pneumo_cases.R`, `R/calibrate_pneumo_hospitalizations.R`, `R/calibrate_pneumo_deaths.R`, `R/calibrate_rota_cases.R`, `R/calibrate_rota_hospitalizations.R`, `R/calibrate_rota_deaths.R`, `R/calibrate_rsv_cases.R`, `R/calibrate_rsv_hospitalizations.R`, `R/calibrate_rsv_deaths.R` | Outcome-specific calibration helpers. |
| `R/calculate_economic_impact.R` | Computes direct + productivity cost outcomes. |
| `R/calculate_vaccine_adverse_events.R` | Estimates severe vaccine adverse event outcomes. |
| `R/calculate_additional_disease_burden.R` | Computes incremental burden vs current baseline coverage. |
| `R/curate_model_output.R` | Standardizes/renames final output columns for downstream use. |
| `R/build_agestructured_disease_output.R` | Builds canonical Hib/Varicella outputs from age-structured models. |
| `R/run_hib_agestructured.R`, `R/run_varicella_agestructured.R` | Disease transmission models for Hib and Varicella. |
| `R/combine_model_output.R` | Merges equilibrium outputs with age-structured Hib/Varicella outputs. |