# run_all_analyses.R
#
# Master runner for the user analytics project.
#
# Purpose:
#   Run the main HUMC, HMH, and combined analysis scripts in the correct order.
#
# How to use:
#   source("run_all_analyses.R")
#
# Notes:
#   - Data and outputs are intentionally ignored by Git.
#   - Individual scripts control whether CSVs are exported.
#   - By default, this runner rebuilds the main analysis outputs but does not
#     run optional long-term legacy trend figures unless run_humc_legacy_trends
#     is set to TRUE.

# Setup --------------------------------------------------------------------

library(here)

# Run switches --------------------------------------------------------------

run_humc_build <- TRUE
run_humc_analysis_object <- TRUE
run_humc_tables <- TRUE
run_humc_figures <- TRUE
run_humc_legacy_trends <- FALSE

run_hmh_literature_search <- TRUE
run_hmh_article_requests <- TRUE

run_combined_build <- TRUE
run_combined_report <- TRUE

# Helper -------------------------------------------------------------------

run_script <- function(path) {
  full_path <- here::here(path)

  if (!file.exists(full_path)) {
    stop("Could not find script: ", full_path)
  }

  cat("\n=================================================================\n")
  cat("Running:", path, "\n")
  cat("=================================================================\n\n")

  source(full_path, local = new.env(parent = globalenv()))

  cat("\nFinished:", path, "\n")
}

# HUMC pipeline -------------------------------------------------------------

if (run_humc_build) {
  run_script("scripts/humc/00_build_humc_master_csv.R")
}

if (run_humc_analysis_object) {
  run_script("scripts/humc/01_build_analysis_object.R")
}

if (run_humc_tables) {
  run_script("scripts/humc/02_generate_tables.R")
}

if (run_humc_figures) {
  run_script("scripts/humc/03_generate_figures.R")
}

if (run_humc_legacy_trends) {
  run_script("scripts/humc/04_trends_legacy.R")
}

# HMH pipeline --------------------------------------------------------------

if (run_hmh_literature_search) {
  run_script("scripts/hmh/run_hmh_literature_search_analysis.R")
}

if (run_hmh_article_requests) {
  run_script("scripts/hmh/run_hmh_article_request_analysis.R")
}

# Combined pipeline ---------------------------------------------------------

if (run_combined_build) {
  run_script("scripts/combined/01_build_combined_dataset.R")
}

if (run_combined_report) {
  run_script("scripts/combined/02_generate_combined_report.R")
}

# Done ---------------------------------------------------------------------

cat("\n=================================================================\n")
cat("All selected analyses complete.\n")
cat("=================================================================\n")
