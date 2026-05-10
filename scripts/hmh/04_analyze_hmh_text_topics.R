# scripts/hmh/04_analyze_hmh_text_topics.R
#
# HMH network text/topic analysis.
#
# Purpose:
#   Use the harmonized HMH network dataset to export clean research topics,
#   lemma tables, TF-IDF tables, and bigram/trigram phrase candidates. This
#   script uses the same shared reference files as the HUMC historical text
#   pipeline: phrases.csv, custom_merges.csv, categories_long.xlsx, and lexicon.lex.
#
# Run after:
#   source("scripts/hmh/00_build_hmh_network_dataset.R")

library(tidyverse)
library(janitor)
library(here)
library(lubridate)
library(tidytext)
library(textstem)
library(stringi)
library(openxlsx)

source(here("scripts", "shared", "paths.R"))
source(here("scripts", "shared", "helpers.R"))
source(here("scripts", "shared", "text_helpers.R"))
source(here("scripts", "shared", "output_helpers.R"))
source(here("scripts", "shared", "reference_data_loaders.R"))
source(here("scripts", "shared", "plotting_helpers.R"))

paths <- make_output_paths(file.path("hmh", "text_topics"))
network_rds_path <- here("data", "processed", "hmh_network_analysis_data.rds")

export_csvs <- TRUE
export_workbook <- TRUE

if (!file.exists(network_rds_path)) {
  stop("Could not find data/processed/hmh_network_analysis_data.rds. Run scripts/hmh/00_build_hmh_network_dataset.R first.")
}

clear_output_folder(paths$csv_dir, "\\.csv$")
clear_output_folder(paths$figures_dir, "\\.(png|jpg|jpeg|pdf)$")
clear_output_folder(paths$output_dir, "^hmh_text_topic_report\\.xlsx$")

analysis_data <- readRDS(network_rds_path)
network_dat <- analysis_data$hmh_network_dat
phrases_tbl <- read_phrases(phrases_path)
custom_map <- read_custom_merges(custom_merges_path)
lex_map <- read_lex(lex_path)

# Topic-level data ----------------------------------------------------------

all_research_topics_full <- network_dat %>%
  transmute(
    global_request_id,
    request_id,
    source_file_type,
    source_label,
    original_id,
    submitted_date,
    year,
    year_month,
    campus_affiliation_clean,
    requestor_category,
    research_topic = str_squish(research_topic)
  ) %>%
  filter(!is.na(research_topic), research_topic != "")

topics_normalized <- all_research_topics_full %>%
  mutate(
    research_topic_clean = clean_text(research_topic, preset = "normalize"),
    research_topic_clean = collapse_phrases(research_topic_clean, phrases_tbl),
    research_topic_clean = na_if(research_topic_clean, "")
  ) %>%
  filter(!is.na(research_topic_clean))

research_topics_only <- all_research_topics_full %>%
  dplyr::select(research_topic) %>%
  arrange(research_topic)

# Lemmatization -------------------------------------------------------------

tidy_lemmas_all <- topics_normalized %>%
  dplyr::select(
    global_request_id,
    request_id,
    source_file_type,
    source_label,
    original_id,
    submitted_date,
    year,
    year_month,
    campus_affiliation_clean,
    requestor_category,
    research_topic,
    research_topic_clean
  ) %>%
  tidytext::unnest_tokens(word, research_topic_clean) %>%
  mutate(
    word = str_replace_all(word, "[’`]", "'"),
    word = str_replace_all(word, "\\.", ""),
    word = str_replace_all(word, "[^a-z0-9'_-]", "")
  ) %>%
  filter(word != "", str_detect(word, "[a-z0-9]")) %>%
  left_join(lex_map, by = c("word" = "token")) %>%
  mutate(
    lemma0 = coalesce(lemma_from_lex, word),
    lemma1 = if_else(is.na(lemma_from_lex), textstem::lemmatize_words(word), lemma0)
  ) %>%
  left_join(custom_map, by = c("lemma1" = "token")) %>%
  mutate(lemma = coalesce(lemma_custom, lemma1)) %>%
  anti_join(tidytext::stop_words, by = c("lemma" = "word")) %>%
  filter(!str_detect(lemma, "^\\d+$"))

all_lemmas <- tidy_lemmas_all %>%
  distinct(lemma) %>%
  arrange(lemma)

top_500_lemmas <- tidy_lemmas_all %>%
  count(lemma, sort = TRUE, name = "n_mentions") %>%
  mutate(prop_mentions = n_mentions / sum(n_mentions)) %>%
  slice_head(n = 500)

lemma_counts_combined <- tidy_lemmas_all %>%
  count(lemma, sort = TRUE, name = "n_mentions") %>%
  mutate(prop_mentions = n_mentions / sum(n_mentions))

lemma_counts_by_source <- tidy_lemmas_all %>%
  count(source_file_type, lemma, sort = TRUE, name = "n_mentions")

lemma_counts_by_campus <- tidy_lemmas_all %>%
  count(campus_affiliation_clean, lemma, sort = TRUE, name = "n_mentions")

lemma_counts_by_requestor <- tidy_lemmas_all %>%
  count(requestor_category, lemma, sort = TRUE, name = "n_mentions")

lemma_tfidf_by_source <- tidy_lemmas_all %>%
  count(source_file_type, lemma, name = "n") %>%
  bind_tf_idf(term = lemma, document = source_file_type, n = n) %>%
  arrange(desc(tf_idf))

lemma_tfidf_by_requestor <- tidy_lemmas_all %>%
  filter(!is.na(requestor_category), requestor_category != "Unknown/Not specified") %>%
  count(requestor_category, lemma, name = "n") %>%
  bind_tf_idf(term = lemma, document = requestor_category, n = n) %>%
  arrange(desc(tf_idf))

# Phrase / n-gram candidates ------------------------------------------------

phrases_exclude <- phrases_tbl %>%
  transmute(
    phrase_raw = str_to_lower(str_squish(phrase)),
    phrase_collapsed = str_replace_all(phrase_raw, " ", "_")
  ) %>%
  pivot_longer(cols = everything(), values_to = "term") %>%
  pull(term) %>%
  unique()

custom_exclude <- custom_map %>%
  transmute(term = str_to_lower(coalesce(lemma_custom, token))) %>%
  pull(term) %>%
  unique()

exclude_terms <- unique(c(phrases_exclude, custom_exclude))

candidate_bigrams <- topics_normalized %>%
  dplyr::select(global_request_id, research_topic_clean) %>%
  tidytext::unnest_tokens(ngram, research_topic_clean, token = "ngrams", n = 2) %>%
  filter(!is.na(ngram)) %>%
  separate(ngram, into = c("word1", "word2"), sep = " ", remove = FALSE) %>%
  filter(
    !is.na(word1), !is.na(word2),
    !word1 %in% tidytext::stop_words$word,
    !word2 %in% tidytext::stop_words$word
  ) %>%
  count(ngram, sort = TRUE, name = "n") %>%
  mutate(type = "bigram")

candidate_trigrams <- topics_normalized %>%
  dplyr::select(global_request_id, research_topic_clean) %>%
  tidytext::unnest_tokens(ngram, research_topic_clean, token = "ngrams", n = 3) %>%
  filter(!is.na(ngram)) %>%
  separate(ngram, into = c("word1", "word2", "word3"), sep = " ", remove = FALSE) %>%
  filter(
    !is.na(word1), !is.na(word2), !is.na(word3),
    !word1 %in% tidytext::stop_words$word,
    !word2 %in% tidytext::stop_words$word,
    !word3 %in% tidytext::stop_words$word
  ) %>%
  count(ngram, sort = TRUE, name = "n") %>%
  mutate(type = "trigram")

phrase_lemma_candidates <- bind_rows(candidate_bigrams, candidate_trigrams) %>%
  mutate(ngram = str_to_lower(ngram)) %>%
  filter(!ngram %in% exclude_terms) %>%
  arrange(desc(n), type, ngram)

# Export tables -------------------------------------------------------------

topic_lemma_tables <- list(
 "Research Topics Only" = research_topics_only,
  "All Research Topics Full" = all_research_topics_full,
  "Topics Normalized" = topics_normalized,
  "All Lemma Records Full" = tidy_lemmas_all,
  "Top 500 Lemmas" = top_500_lemmas,
  "All Lemmas" = all_lemmas,
  "Lemma Counts Combined" = lemma_counts_combined,
  "Lemma Counts by Source" = lemma_counts_by_source,
  "Lemma Counts by Campus" = lemma_counts_by_campus,
  "Lemma Counts by Requestor" = lemma_counts_by_requestor,
  "Lemma TF-IDF by Source" = lemma_tfidf_by_source,
  "Lemma TF-IDF by Requestor" = lemma_tfidf_by_requestor,
  "Phrase Lemma Candidates" = phrase_lemma_candidates
)

if (export_csvs) {
  purrr::iwalk(topic_lemma_tables, ~ write_pretty_csv(.x, janitor::make_clean_names(.y), paths$csv_dir))
}

if (export_workbook) {
  write_pretty_workbook(topic_lemma_tables, file.path(paths$output_dir, "hmh_text_topic_report.xlsx"))
}

# Figure -------------------------------------------------------------------

p_top_lemmas <- top_500_lemmas %>%
  slice_head(n = 25) %>%
  ggplot(aes(x = reorder(lemma, n_mentions), y = n_mentions)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top Research Topic Lemmas, HMH Network 2025–Present",
    subtitle = "Harmonized literature search topic text",
    x = NULL,
    y = "Number of Mentions"
  ) +
  theme_project()

ggsave(file.path(paths$figures_dir, "hmh_network_top_lemmas.png"), p_top_lemmas, width = 9, height = 7, dpi = 300)

# Save RDS -----------------------------------------------------------------

hmh_text_topic_data <- list(
  research_topics_only = research_topics_only,
  all_research_topics_full = all_research_topics_full,
  topics_normalized = topics_normalized,
  tidy_lemmas_all = tidy_lemmas_all,
  all_lemmas = all_lemmas,
  top_500_lemmas = top_500_lemmas,
  lemma_counts_combined = lemma_counts_combined,
  lemma_counts_by_source = lemma_counts_by_source,
  lemma_counts_by_campus = lemma_counts_by_campus,
  lemma_counts_by_requestor = lemma_counts_by_requestor,
  lemma_tfidf_by_source = lemma_tfidf_by_source,
  lemma_tfidf_by_requestor = lemma_tfidf_by_requestor,
  phrase_lemma_candidates = phrase_lemma_candidates
)

saveRDS(hmh_text_topic_data, here("data", "processed", "hmh_text_topic_data.rds"))

cat("\n--- HMH Text Topic Analysis Complete ---\n")
cat("Research topic records:", nrow(all_research_topics_full), "\n")
cat("Lemma records:", nrow(tidy_lemmas_all), "\n")
cat("Unique lemmas:", n_distinct(tidy_lemmas_all$lemma), "\n")
cat("Phrase/lemma candidates:", nrow(phrase_lemma_candidates), "\n")
cat("Outputs written to:", paths$output_dir, "\n")
