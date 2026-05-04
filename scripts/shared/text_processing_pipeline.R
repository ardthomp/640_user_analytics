# scripts/shared/text_processing_pipeline.R
#
# Contains the core data loading and text processing functions for the
# combined text analysis pipeline.

library(tidyverse)
library(tidytext)
library(textstem)

# --- Data Loading and Preparation ---
load_and_prep_humc <- function(path) {
  read_csv(path, show_col_types = FALSE) %>%
    clean_names() %>%
    transmute(
      source_file_type = "humc",
      source_label = "HUMC legacy form",
      original_id = request_id,
      submitted_date = as.Date(date),
      campus_affiliation = standardize_campus_name(campus_affiliation),
      research_topic = clean_blank(topic)
    )
}

load_and_prep_hmh <- function(path) {
  read_csv(path, show_col_types = FALSE) %>%
    clean_names() %>%
    filter(str_to_lower(select_question_request_type) == "literature search") %>%
    transmute(
      source_file_type = "hmh",
      source_label = "HMH shared form",
      original_id = row_number(),
      submitted_date = as.Date(parse_timestamp(timestamp)),
      campus_affiliation = standardize_campus_name(campus_affiliation),
      research_topic = clean_blank(research_topic)
    )
}

# --- Lemmatization ---

lemmatize_topics <- function(topics_df, phrases_tbl, lex_map, custom_map) {
  
  topics_normalized <- topics_df %>%
    mutate(
      research_topic_clean = clean_text(research_topic, preset = "normalize"),
      research_topic_clean = collapse_phrases(research_topic_clean, phrases_tbl),
      research_topic_clean = na_if(research_topic_clean, "")
    ) %>%
    filter(!is.na(research_topic_clean))
  
  possible_cols <- c(
    "request_id", "global_request_id", "source_file_type", "source_label", 
    "original_id", "submitted_date", "campus_affiliation", "research_topic", 
    "research_topic_clean", "requestor_category", "time_spent"
  )
  
  tidy_lemmas <- topics_normalized %>%
    # THE FIX: Explicitly use dplyr::select to avoid masking by MASS::select
    dplyr::select(any_of(possible_cols)) %>%
    unnest_tokens(word, research_topic_clean) %>%
    mutate(word = str_replace_all(word, "[^a-z0-9'_-]", "")) %>%
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
  
  return(tidy_lemmas)
}

# --- Candidate Phrase Generation ---
generate_ngram_candidates <- function(topics_df, phrases_tbl, custom_map) {
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
  
  phrase_candidate_tokens <- topics_df %>%
    mutate(
      research_topic_clean = clean_text(research_topic, preset = "normalize"),
      research_topic_clean = collapse_phrases(research_topic_clean, phrases_tbl)
    ) %>%
    dplyr::select(any_of(c("global_request_id", "request_id")), research_topic_clean)
  
  candidate_bigrams <- phrase_candidate_tokens %>%
    unnest_tokens(ngram, research_topic_clean, token = "ngrams", n = 2) %>%
    filter(!is.na(ngram)) %>%
    separate(ngram, into = c("word1", "word2"), sep = " ", remove = FALSE) %>%
    filter(!is.na(word1), !is.na(word2), !word1 %in% stop_words$word, !word2 %in% stop_words$word) %>%
    count(ngram, sort = TRUE, name = "n") %>%
    mutate(type = "bigram")
  
  candidate_trigrams <- phrase_candidate_tokens %>%
    unnest_tokens(ngram, research_topic_clean, token = "ngrams", n = 3) %>%
    filter(!is.na(ngram)) %>%
    separate(ngram, into = c("word1", "word2", "word3"), sep = " ", remove = FALSE) %>%
    filter(!is.na(word1), !is.na(word2), !is.na(word3), !word1 %in% stop_words$word, !word2 %in% stop_words$word, !word3 %in% stop_words$word) %>%
    count(ngram, sort = TRUE, name = "n") %>%
    mutate(type = "trigram")
  
  bind_rows(candidate_bigrams, candidate_trigrams) %>%
    mutate(ngram = str_to_lower(ngram)) %>%
    filter(!ngram %in% exclude_terms) %>%
    arrange(desc(n), type, ngram)
}