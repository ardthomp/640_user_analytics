# 02_combined_analysis.R
#
# Master script for analyzing and summarizing combined literature search data
# from both HUMC and HMH sources.

# --- 1. Setup ---
library(tidyverse)
library(here)
library(lubridate)

# Source all shared and pipeline-specific helpers
source(here("scripts", "shared", "paths.R"))
source(here("scripts", "shared", "helpers.R"))
source(here("scripts", "shared", "transformations.R"))
source(here("scripts", "shared", "output_helpers.R"))
source(here("scripts", "shared", "combined_analysis_pipeline.R")) # Our new pipeline functions

# Define output paths
paths <- make_output_paths("combined")
csv_dir <- paths$csv_dir
figures_dir <- paths$figures_dir
formatted_tables_dir <- paths$formatted_tables_dir

# --- 2. Dynamic Analysis Period ---

# Determine the analysis years automatically based on the latest data.
# This will analyze the most recent full year and the current year-to-date.
latest_year <- year(Sys.Date())
analysis_years <- c(latest_year - 1, latest_year) # e.g., c(2025, 2026)

cat("Analyzing data for years:", paste(analysis_years, collapse = ", "), "\n")

# --- 3. Load and Process Data ---

# Load and process each data source using our new pipeline functions
humc_dat <- load_and_process_humc(humc_path, analysis_years)
hmh_dat  <- load_and_process_hmh(hmh_path, analysis_years)

# Combine into a single, clean dataset
combined_dat <- combine_and_finalize_data(humc_dat, hmh_dat)

# --- 4. Generate Analysis Outputs ---

# Create a list of all summary tables
summary_tables <- generate_summary_tables(combined_dat)

# Generate all plots
generate_plots(combined_dat, figures_dir)

# Generate a formatted HTML table for campus requests
gt_requests_by_campus <- summary_tables$`Requests by Campus` %>%
  mutate(prop = n_requests / sum(n_requests)) %>%
  gt::gt() %>%
  gt::tab_header(title = "Literature Search Requests by Campus") %>%
  gt::fmt_percent(columns = prop, decimals = 1)

# --- 5. Save All Outputs ---

# Save all summary tables to individual, archived CSVs
purrr::iwalk(
  summary_tables,
  ~ write_archived_csv(
    df = .x,
    filename = janitor::make_clean_names(.y),
    csv_dir = csv_dir
  )
)

# Save the formatted HTML table, archived automatically
gtsave(
  gt_requests_by_campus,
  file.path(formatted_tables_dir, "combined_requests_by_campus.html")
)
# Note: gtsave doesn't return a value that can be piped into our archiver.
# A more advanced solution could wrap gtsave, but for now, this is fine.
# Manual archiving for this one file might be needed if versions are critical.

# Save a single, consolidated Excel workbook with all tables
write_archived_workbook(
  tables = summary_tables,
  path = file.path(paths$output_dir, "summary_report.xlsx")
)

# --- 6. Console Summary ---
cat("\nCombined analysis complete.\n")
cat("Years analyzed:", paste(analysis_years, collapse = ", "), "\n")
cat("Total requests found:", nrow(combined_dat), "\n")
cat("Outputs written to:", paths$output_dir, "\n")