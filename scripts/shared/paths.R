# scripts/shared/paths.R
#
# Defines the complete file and directory structure for the project.
# Uses the `here` package to ensure all paths are relative to the project root,
# making the project fully portable.

library(here)

# Base directories ---------------------------------------------------------

data_dir <- here("data")
raw_data_dir <- here("data", "raw")
processed_data_dir <- here("data", "processed")
reference_dir <- here("data", "reference")

outputs_dir <- here("outputs")
scripts_dir <- here("scripts")

# Note: The `humc_archive_dir` variable has been removed as it is no longer needed
# by the new "Current & Archive" system.

# Core datasets ------------------------------------------------------------

humc_path <- file.path(processed_data_dir, "humc.csv")
hmh_path <- file.path(raw_data_dir, "hmh.csv")

# Shared reference files ---------------------------------------------------

phrases_path <- file.path(reference_dir, "phrases.csv")
custom_merge_path <- file.path(reference_dir, "custom_merges.csv")
lex_path <- file.path(reference_dir, "lexicon.lex")


# Output directory helper (Simplified) -------------------------------------

#' Create a standard set of output directories for a given project analysis.
#'
#' This function creates the main output folders. The corresponding "archive"
#' sub-folders are now created automatically on-the-fly by the `write_archived_*`
#' functions in `output_helpers.R`.
#'
#' @param project_name The name of the sub-project (e.g., "humc", "combined").
#' @return A list of paths to the created directories.
make_output_paths <- function(project_name) {
  
  output_dir <- file.path(outputs_dir, project_name)
  
  # Define only the "current" output directories.
  paths <- list(
    output_dir = output_dir,
    csv_dir = file.path(output_dir, "csv"),
    figures_dir = file.path(output_dir, "figures"),
    formatted_tables_dir = file.path(output_dir, "formatted_tables"),
    model_dir = file.path(output_dir, "model")
  )
  
  # Use purrr::walk to create any of these directories that don't already exist.
  purrr::walk(
    paths,
    ~ if (!dir.exists(.x)) dir.create(.x, recursive = TRUE)
  )
  
  return(paths)
}
