# 01_build_combined_dataset.R
#
# This script builds the primary analytical datasets by loading raw data,
# performing text processing, and saving both the final, clean data objects
# and all intermediate text analysis summary tables.

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
source(here("scripts", "shared", "text_processing_pipeline.R"))

# Define output paths
paths <- make_output_paths("text_analysis")
csv_dir <- paths$csv_dir

# --- 2. Load Data ---

# Load reference data. Your phrases and custom merges are used here.
phrases_tbl <- read_phrases(phrases_path)
custom_map  <- read_custom_merges(custom_merge_path)
lex_map     <- read_lex(lex_path)

# Load and prepare source data
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

# Generate the core lemma dataset. This uses your phrases and custom merges.
tidy_lemmas_all <- lemmatize_topics(all_research_topics_full, phrases_tbl, lex_map, custom_map)

# Generate candidates for future phrase/lemma mapping
phrase_lemma_candidates <- generate_ngram_candidates(all_research_topics_full, phrases_tbl, custom_map)

# --- 4. Generate and Save All Text Analysis Outputs ---
# This section is now restored to produce all your desired CSVs.

# Save the detailed, record-level files
write_archived_csv(all_research_topics_full, "all_research_topics_full", csv_dir)
write_archived_csv(tidy_lemmas_all, "all_lemma_records_full", csv_dir)
write_archived_csv(phrase_lemma_candidates, "phrase_lemma_candidates", csv_dir)

# Create and save summary tables based on the processed data
write_archived_csv(
  df = count(all_research_topics_full, research_topic, sort = TRUE, name = "n_records"),
  filename = "all_research_topics_counts",
  csv_dir = csv_dir
)
write_archived_csv(
  df = tidy_lemmas_all %>% distinct(lemma) %>% arrange(lemma),
  filename = "all_lemmas",
  csv_dir = csv_dir
)
write_archived_csv(
  df = tidy_lemmas_all %>% count(lemma, sort = TRUE, name = "n_mentions") %>% slice_head(n = 500),
  filename = "top_500_lemmas",
  csv_dir = csv_dir
)
write_archived_csv(
  df = count(tidy_lemmas_all, source_file_type, lemma, sort = TRUE, name = "n_mentions"),
  filename = "lemma_counts_by_source",
  csv_dir = csv_dir
)
write_archived_csv(
  df = count(tidy_lemmas_all, campus_affiliation, lemma, sort = TRUE, name = "n_mentions"),
  filename = "lemma_counts_by_campus",
  csv_dir = csv_dir
)

# --- 5. Save Final Dataset for Downstream Analysis ---

# This is the primary output for the next script in the pipeline.
saveRDS(
  object = list(
    "all_topics" = all_research_topics_full,
    "all_lemmas" = tidy_lemmas_all
  ),
  file = here("data", "processed", "combined_analysis_data.rds")
)

# --- 6. Console Summary ---
cat("\n--- Build Complete ---\n")
cat("Total research topic records:", nrow(all_research_topics_full), "\n")
cat("Unique lemmas found:", n_distinct(tidy_lemmas_all$lemma), "\n")
cat("All text analysis CSVs have been saved to:", csv_dir, "\n")
cat("Final analysis dataset saved to: data/processed/combined_analysis_data.rds\n")