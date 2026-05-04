# 01_build_combined_dataset.R
#
# This script builds the primary analytical datasets by loading raw data from
# both HUMC and HMH sources, performing text processing and lemmatization,
# and saving the final, clean data objects for downstream analysis.

# --- 1. Setup ---
library(tidyverse)
library(here)

# Source all shared and pipeline-specific helpers
source(here("scripts", "shared", "paths.R"))
source(here("scripts", "shared", "helpers.R"))
source(here("scripts", "shared", "transformations.R"))
source(here("scripts", "shared", "text_helpers.R"))
source(here("scripts", "shared", "reference_data_loaders.R"))
source(here("scripts", "shared", "output_helpers.R"))
source(here("scripts", "shared", "text_processing_pipeline.R")) # Our new functions

# Define output paths
paths <- make_output_paths("text_analysis")
csv_dir <- paths$csv_dir

# --- 2. Load Data ---

# Load reference data using our dedicated loaders
phrases_tbl <- read_phrases(phrases_path)
custom_map  <- read_custom_merges(custom_merge_path)
lex_map     <- read_lex(lex_path)

# Load and prepare source data using our new pipeline functions
humc_topics <- load_and_prep_humc(humc_path)
hmh_topics  <- load_and_prep_hmh(hmh_path)

# --- 3. Process and Analyze ---

# Combine sources into a single dataset
all_research_topics_full <- bind_rows(humc_topics, hmh_topics) %>%
  mutate(
    global_request_id = row_number(),
    research_topic = str_squish(research_topic),
    year = year(submitted_date),
    year_month = floor_date(submitted_date, "month")
  ) %>%
  filter(!is.na(research_topic), research_topic != "")

# Generate the core lemma dataset
tidy_lemmas_all <- lemmatize_topics(all_research_topics_full, phrases_tbl, lex_map, custom_map)

# Generate candidates for future phrase/lemma mapping
phrase_lemma_candidates <- generate_ngram_candidates(all_research_topics_full, phrases_tbl, custom_map)

# --- 4. Save Intermediate Outputs & Final Dataset ---

# Save intermediate CSVs for review (archiving is now automatic)
write_archived_csv(all_research_topics_full, "all_research_topics_full", csv_dir)
write_archived_csv(tidy_lemmas_all, "all_lemma_records_full", csv_dir)
write_archived_csv(phrase_lemma_candidates, "phrase_lemma_candidates", csv_dir)

# Save the final, clean data objects for the next script to use.
# This is the primary output of this script.
saveRDS(
  object = list(
    "all_topics" = all_research_topics_full,
    "all_lemmas" = tidy_lemmas_all
  ),
  file = here("data", "processed", "combined_analysis_data.rds")
)

# --- 5. Console Summary ---

cat("\n--- Build Complete ---\n")
cat("Total research topic records:", nrow(all_research_topics_full), "\n")
cat("Unique lemmas found:", n_distinct(tidy_lemmas_all$lemma), "\n")
cat("Final analysis dataset saved to: data/processed/combined_analysis_data.rds\n")