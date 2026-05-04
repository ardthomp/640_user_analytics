# 02_generate_combined_report.R
#
# Loads the pre-processed combined dataset and generates the final
# summary report, tables, and figures for the analysis years.

# --- 1. Setup ---
library(tidyverse)
library(here)
library(lubridate)

# Source all shared and pipeline-specific helpers
source(here("scripts", "shared", "paths.R"))
source(here("scripts", "shared", "helpers.R"))
source(here("scripts", "shared", "output_helpers.R"))
source(here("scripts", "shared", "combined_analysis_pipeline.R")) # Our new functions

# Define output paths
paths <- make_output_paths("combined")

# --- 2. Load Pre-Processed Data ---

# Load the final data object created by the '01_build' script.
# This completely replaces all the old data loading and cleaning steps.
analysis_data <- readRDS(here("data", "processed", "combined_analysis_data.rds"))
combined_dat <- analysis_data$all_topics
tidy_lemmas <- analysis_data$all_lemmas

# Determine analysis years dynamically from the data
analysis_years <- unique(combined_dat$year)

cat("--- Report Generation Started ---\n")
cat("Loaded pre-processed data with", nrow(combined_dat), "records.\n")
cat("Analyzing data for years:", paste(analysis_years, collapse = ", "), "\n")

# --- 3. Generate Analysis Outputs ---

# Create a list of all summary tables using our new helper functions
summary_tables <- generate_summary_tables(combined_dat)
purpose_tables <- generate_purpose_tables(combined_dat)

# Generate lemma tables
lemma_tables <- list(
  "Top Lemmas" = tidy_lemmas %>% count(lemma, sort = TRUE, name = "n") %>% head(500),
  "All Lemmas" = tidy_lemmas %>% distinct(lemma) %>% arrange(lemma)
)

# Combine all tables into one list for export
all_tables_to_export <- c(summary_tables, purpose_tables, lemma_tables)
all_tables_to_export <- all_tables_to_export[map_lgl(all_tables_to_export, ~ is.data.frame(.x) && nrow(.x) > 0)]

# Generate and save all plots
generate_and_save_plots(combined_dat, paths$figures_dir)

# --- 4. Save All Outputs ---

# Save all summary tables to individual, archived CSVs
purrr::iwalk(
  all_tables_to_export,
  ~ write_archived_csv(
    df = .x,
    filename = janitor::make_clean_names(.y),
    csv_dir = paths$csv_dir
  )
)

# Save a single, consolidated Excel workbook with all tables
write_archived_workbook(
  tables = all_tables_to_export,
  path = file.path(paths$output_dir, "summary_report.xlsx")
)

# --- 5. Console Summary ---
cat("--- Report Generation Complete ---\n")
cat("Summary workbook, CSVs, and figures written to:", paths$output_dir, "\n")