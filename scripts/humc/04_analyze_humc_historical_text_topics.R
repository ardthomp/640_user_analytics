# scripts/humc/04_analyze_humc_historical_text_topics.R
#
# HUMC historical text/topic analysis.
#
# Purpose:
#   Export clean research topics, lemma tables, and phrase candidates for the
#   HUMC legacy historical corpus. This keeps the HUMC historical text corpus
#   separate from the HMH network text workflow while using the same shared
#   reference files.
#
# Run after:
#   source("scripts/humc/01_build_humc_analysis_object.R")

library(tidyverse)
library(janitor)
library(here)
library(lubridate)
library(tidytext)
library(openxlsx)

source(here("scripts", "shared", "paths.R"))
source(here("scripts", "shared", "helpers.R"))
source(here("scripts", "shared", "text_helpers.R"))
source(here("scripts", "shared", "output_helpers.R"))
source(here("scripts", "shared", "reference_data_loaders.R"))
source(here("scripts", "shared", "plotting_helpers.R"))

paths <- make_output_paths(file.path("humc", "historical_text"))
humc_paths <- make_output_paths("humc")
out_path <- file.path(humc_paths$output_dir, "out.rds")

export_csvs <- TRUE
export_workbook <- TRUE

if (!file.exists(out_path)) {
  stop("Cannot find outputs/humc/out.rds. Run scripts/humc/01_build_humc_analysis_object.R first.")
}

clear_output_folder(paths$csv_dir, "\\.csv$")
clear_output_folder(paths$figures_dir, "\\.(png|jpg|jpeg|pdf)$")
clear_output_folder(paths$output_dir, "^humc_historical_text_report\\.xlsx$")

# Load the reusable HUMC historical analysis object created upstream.
out <- readRDS(out_path)

# Clean research topic inventory -------------------------------------------

all_research_topics_clean <- out$humc_request_data %>%
  transmute(
    global_request_id = request_id,
    request_id,
    source_file_type = "humc",
    source_label = "HUMC legacy form",
    submitted_date = as.Date(date),
    year = lubridate::year(submitted_date),
    year_month = as.Date(lubridate::floor_date(submitted_date, unit = "month")),
    submitter_type,
    research_topic = stringr::str_squish(as.character(topic)),
    research_topic_clean = Topic
  ) %>%
  filter(!is.na(research_topic), research_topic != "") %>%
  arrange(submitted_date, request_id)

# Lemma tables --------------------------------------------------------------

# Reuse the lemma-level records from the HUMC analysis object and add
# source labels for downstream comparison.
tidy_lemmas_humc_historical <- out$lemma_records %>%
  mutate(
    global_request_id = request_id,
    source_file_type = "humc",
    source_label = "HUMC legacy form"
  )

unique_lemmas_humc_historical <- tidy_lemmas_humc_historical %>%
  distinct(lemma) %>%
  arrange(lemma)

top_500_lemmas_humc_historical <- tidy_lemmas_humc_historical %>%
  count(lemma, sort = TRUE, name = "n_mentions") %>%
  mutate(prop_mentions = n_mentions / sum(n_mentions)) %>%
  slice_head(n = 500)

lemma_counts_by_year <- tidy_lemmas_humc_historical %>%
  mutate(year = lubridate::year(as.Date(date))) %>%
  count(year, lemma, sort = TRUE, name = "n_mentions")

lemma_counts_by_submitter <- tidy_lemmas_humc_historical %>%
  count(submitter_type, lemma, sort = TRUE, name = "n_mentions")

# TF-IDF highlights terms that are especially distinctive within each
# submitter group, not simply the most frequent terms overall.
lemma_tfidf_by_submitter <- tidy_lemmas_humc_historical %>%
  filter(!is.na(submitter_type), submitter_type != "Unknown") %>%
  count(submitter_type, lemma, name = "n") %>%
  bind_tf_idf(term = lemma, document = submitter_type, n = n) %>%
  arrange(desc(tf_idf))

# Phrase / n-gram candidates ------------------------------------------------
# These are generated in the HUMC analysis object using PMI and exclude terms
# already represented in phrases.csv.

phrase_lemma_candidates <- out$phrase_results$phrase_candidates %>%
  arrange(desc(score), desc(n))

phrases_to_add <- out$phrase_results$phrases_to_add

bigram_candidates <- out$bigram_candidates
trigram_candidates <- out$trigram_candidates

# Export tables -------------------------------------------------------------

humc_text_tables <- list(
  "All Research Topics Clean" = all_research_topics_clean,
  "All Lemma Records Full" = tidy_lemmas_humc_historical,
  "Top 500 Lemmas" = top_500_lemmas_humc_historical,
  "Unique Lemmas" = unique_lemmas_humc_historical,
  "Lemma Counts by Year" = lemma_counts_by_year,
  "Lemma Counts by Submitter" = lemma_counts_by_submitter,
  "Lemma TF-IDF by Submitter" = lemma_tfidf_by_submitter,
  "Phrase Lemma Candidates" = phrase_lemma_candidates,
  "Phrases to Add" = phrases_to_add,
  "Bigram Candidates" = bigram_candidates,
  "Trigram Candidates" = trigram_candidates
)

if (export_csvs) {
  purrr::iwalk(humc_text_tables, ~ write_pretty_csv(.x, janitor::make_clean_names(.y), paths$csv_dir))
}

if (export_workbook) {
  write_pretty_workbook(humc_text_tables, file.path(paths$output_dir, "humc_historical_text_report.xlsx"))
}

# Figure -------------------------------------------------------------------

# Simple overview figure showing the most frequent lemmas in the HUMC
# historical corpus.
p_top_lemmas <- top_500_lemmas_humc_historical %>%
  slice_head(n = 25) %>%
  ggplot(aes(x = reorder(lemma, n_mentions), y = n_mentions)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top Research Topic Lemmas, HUMC Historical Corpus",
    subtitle = "Legacy literature search request topics",
    x = NULL,
    y = "Number of Mentions"
  ) +
  theme_project()

ggsave(file.path(paths$figures_dir, "humc_historical_top_lemmas.png"), p_top_lemmas, width = 9, height = 7, dpi = 300)

# Save RDS -----------------------------------------------------------------

humc_historical_text_data <- list(
  all_research_topics_clean = all_research_topics_clean,
  tidy_lemmas_humc_historical = tidy_lemmas_humc_historical,
  unique_lemmas_humc_historical = unique_lemmas_humc_historical,
  top_500_lemmas_humc_historical = top_500_lemmas_humc_historical,
  lemma_counts_by_year = lemma_counts_by_year,
  lemma_counts_by_submitter = lemma_counts_by_submitter,
  lemma_tfidf_by_submitter = lemma_tfidf_by_submitter,
  phrase_lemma_candidates = phrase_lemma_candidates,
  phrases_to_add = phrases_to_add,
  bigram_candidates = bigram_candidates,
  trigram_candidates = trigram_candidates
)

saveRDS(humc_historical_text_data, here("data", "processed", "humc_historical_text_data.rds"))

cat("\n--- HUMC Historical Text Analysis Complete ---\n")
cat("Research topic records:", nrow(all_research_topics_clean), "\n")
cat("Lemma records:", nrow(tidy_lemmas_humc_historical), "\n")
cat("Unique lemmas:", n_distinct(tidy_lemmas_humc_historical$lemma), "\n")
cat("Phrase/lemma candidates:", nrow(phrase_lemma_candidates), "\n")
cat("Outputs written to:", paths$output_dir, "\n")
