# run_all_analyses.R
#
# Master runner for the refactored hospital library user analytics project.
#
# Purpose:
#   Run the HUMC historical pipeline and the HMH network pipeline in a clearer
#   order. HUMC historical text analysis stays separate from the HMH network
#   text analysis, but both use the same shared reference files.
#
# How to use:
#   source("run_all_analyses.R")
#
# Record current state of this project's library:
#   renv::snapshot()

libs <- c(
  "tidyverse",
  "here",
  "janitor",
  "lubridate",
  "readxl",
  "openxlsx",
  "tidytext",
  "textstem",
  "stringi",
  "gt",
  "viridis",
  "scales"
)

missing <- libs[!sapply(libs, requireNamespace, quietly = TRUE)]

if (length(missing) > 0) {
  stop(
    "Missing packages: ",
    paste(missing, collapse = ", "),
    "\nRun renv::restore()"
  )
}

invisible(lapply(libs, library, character.only = TRUE))

# Run switches --------------------------------------------------------------

run_humc_build <- TRUE
run_humc_analysis_object <- TRUE
run_humc_tables <- TRUE
run_humc_figures <- TRUE
run_humc_historical_text <- TRUE
run_humc_longitudinal_time_series <- FALSE

run_hmh_network_build <- TRUE
run_hmh_literature_searches <- TRUE
run_hmh_article_requests <- TRUE
run_hmh_overall_requests <- TRUE
run_hmh_text_topics <- TRUE

run_optional_models <- FALSE

# Helper -------------------------------------------------------------------

run_script <- function(path) {
  full_path <- here::here(path)

  if (!file.exists(full_path)) {
    stop("Could not find script: ", full_path)
  }

  cat("\n=================================================================\n")
  cat("Running:", path, "\n")
  cat("=================================================================\n\n")

  source(full_path, local = globalenv())

  cat("\nFinished:", path, "\n")
}

# HUMC historical pipeline --------------------------------------------------

if (run_humc_build) {
  run_script("scripts/humc/00_build_humc_master_csv.R")
}

if (run_humc_analysis_object) {
  run_script("scripts/humc/01_build_humc_analysis_object.R")
}

if (run_humc_tables) {
  run_script("scripts/humc/02_generate_humc_tables.R")
}

if (run_humc_figures) {
  run_script("scripts/humc/03_generate_humc_figures.R")
}

if (run_humc_historical_text) {
  run_script("scripts/humc/04_analyze_humc_historical_text_topics.R")
}

# HMH network pipeline ------------------------------------------------------

if (run_hmh_network_build) {
  run_script("scripts/hmh/00_build_hmh_network_dataset.R")
}

if (run_hmh_literature_searches) {
  run_script("scripts/hmh/01_analyze_hmh_literature_searches.R")
}

if (run_hmh_article_requests) {
  run_script("scripts/hmh/02_analyze_hmh_article_requests.R")
}

if (run_hmh_overall_requests) {
  run_script("scripts/hmh/03_analyze_hmh_overall_requests.R")
}

if (run_hmh_text_topics) {
  run_script("scripts/hmh/04_analyze_hmh_text_topics.R")
}

# Optional models -----------------------------------------------------------

if (run_optional_models) {
  run_script("scripts/optional_models/humc_01_citation_count_prediction_model.R")
  run_script("scripts/optional_models/humc_02_longitudinal_time_series.R")
  run_script("scripts/optional_models/hmh_01_time_series_analysis.R")
  run_script("scripts/optional_models/hmh_02_effort_level_by_requestor_model.R")
}

cat("\n=================================================================\n")
cat("All selected analyses complete.\n")
cat("=================================================================\n")
