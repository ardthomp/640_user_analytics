# scripts/shared/paths.R

library(here)

# Base directories
data_dir <- here("data")
raw_data_dir <- here("data", "raw")
processed_data_dir <- here("data", "processed")
reference_dir <- here("data", "reference")
outputs_dir <- here("outputs")
scripts_dir <- here("scripts")

# Core datasets
humc_path <- file.path(processed_data_dir, "humc.csv")
hmh_path <- file.path(raw_data_dir, "hmh.csv")

# Shared reference files
phrases_path <- here("data", "reference", "phrases.csv")
custom_merges_path <- here("data", "reference", "custom_merges.csv")
lex_path <- here("data", "reference", "lexicon.lex")
categories_path <- here("data", "reference", "categories_long.xlsx")

# Output directory helper (Simplified)
make_output_paths <- function(project_name) {
  output_dir <- file.path(outputs_dir, project_name)
  paths <- list(
    output_dir = output_dir,
    csv_dir = file.path(output_dir, "csv"),
    figures_dir = file.path(output_dir, "figures"),
    formatted_tables_dir = file.path(output_dir, "formatted_tables"),
    model_dir = file.path(output_dir, "model")
  )
  purrr::walk(paths, ~ if (!dir.exists(.x)) dir.create(.x, recursive = TRUE))
  return(paths)
}