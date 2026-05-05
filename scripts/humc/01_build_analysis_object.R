# Build HUMC analysis object -----------------------------------------------
#
# Purpose:
#   Read data/processed/humc.csv and build the reusable analysis object.
#   This is where text normalization, phrase collapsing, lemmatization,
#   category joining, purpose reshaping, citation variables, and n-gram
#   candidates are created.
#
# Output:
#   outputs/humc/out.rds

# Setup libraries and sources -----------------------------------------------

library(here)
library(tidyverse)
library(tidytext)
library(textstem)
library(readxl)
library(janitor)
library(stringr)
library(stringi)

source(here("scripts", "shared", "paths.R"))
source(here("scripts", "shared", "helpers.R"))
source(here("scripts", "shared", "text_helpers.R"))
source(here("scripts", "shared", "output_helpers.R"))
source(here("scripts", "shared", "reference_data_loaders.R"))

project_name <- "humc"
paths <- make_output_paths(project_name)
output_dir <- paths$output_dir
project_reference_dir <- file.path(reference_dir, project_name)
if (!dir.exists(project_reference_dir)) dir.create(project_reference_dir, recursive = TRUE)

raw_path <- humc_path
categories_path <- file.path(project_reference_dir, "categories_long.xlsx")
coding_path <- file.path(project_reference_dir, "lemma_coding.csv")
out_path <- file.path(output_dir, "out.rds")

read_categories <- function(path) {
  if (!file.exists(path)) {
    message("categories_long.xlsx not found. Continuing without categories.")
    return(tibble(lemma = character(), category = character()))
  }
  readxl::read_excel(path) %>%
    dplyr::select(lemma, category) %>%
    mutate(
      lemma = stringi::stri_enc_toutf8(as.character(lemma), validate = TRUE),
      category = stringi::stri_enc_toutf8(as.character(category), validate = TRUE),
      lemma = str_replace_all(lemma, "\uFFFD", ""),
      category = str_replace_all(category, "\uFFFD", ""),
      lemma = str_to_lower(str_trim(lemma)),
      category = str_trim(category)
    ) %>%
    filter(!is.na(lemma), lemma != "", !is.na(category), category != "") %>%
    distinct(lemma, .keep_all = TRUE)
}

read_lemma_coding <- function(path = coding_path) {
  if (!file.exists(path)) {
    starter <- tibble(lemma = character(), category = character(), specialty_1 = character(), specialty_2 = character())
    write_csv(starter, path)
    message("Created empty lemma_coding.csv. Add coding and rerun.")
  }
  read_csv(
    path,
    show_col_types = FALSE,
    col_types = cols(lemma = col_character(), category = col_character(), specialty_1 = col_character(), specialty_2 = col_character()),
    locale = locale(encoding = "latin1")
  ) %>%
    mutate(
      across(everything(), ~ stringi::stri_enc_toutf8(as.character(.x), validate = TRUE)),
      across(everything(), ~ str_replace_all(.x, "\uFFFD", "")),
      lemma = str_to_lower(str_trim(lemma)),
      category = na_if(str_trim(category), ""),
      specialty_1 = na_if(str_trim(specialty_1), ""),
      specialty_2 = na_if(str_trim(specialty_2), "")
    ) %>%
    filter(!is.na(lemma), lemma != "") %>%
    distinct(lemma, .keep_all = TRUE)
}

get_top_lemmas <- function(data_norm, top_n = 300) {
  data_norm %>% count(lemma, sort = TRUE) %>% slice_head(n = top_n)
}

standardize_submitter_type <- function(attending, med_ed, nurse, other_provider, committee) {
  case_when(
    nurse == 1 ~ "Nurse",
    other_provider == 1 ~ "OtherProvider",
    attending == 1 ~ "Attending",
    med_ed == 1 ~ "MedEd",
    committee == 1 ~ "Committee",
    TRUE ~ "Unknown"
  )
}

make_purpose_long <- function(my_data2) {
  purpose_cols <- c("continuing_education", "patient_care", "lecture", "ebp", "research", "grant", "publication", "irb_app", "admin", "policy", "patient_info")
  my_data2 %>%
    dplyr::select(request_id, all_of(purpose_cols)) %>%
    pivot_longer(cols = all_of(purpose_cols), names_to = "purpose", values_to = "flag") %>%
    filter(flag == 1) %>%
    mutate(
      purpose = case_when(
        purpose == "continuing_education" ~ "ContinuingEducation",
        purpose == "patient_care" ~ "PatientCare",
        purpose == "irb_app" ~ "IRBApp",
        purpose == "patient_info" ~ "PatientInfo",
        purpose == "ebp" ~ "EBP",
        TRUE ~ str_to_title(purpose)
      )
    ) %>%
    dplyr::select(request_id, purpose)
}

generate_ngram_candidates <- function(df, n_words = 2, min_n = 3) {
  df %>%
    transmute(Topic = clean_text(as.character(Topic), preset = "topic")) %>%
    filter(!is.na(Topic), Topic != "") %>%
    tidytext::unnest_tokens(output = ngram, input = Topic, token = "ngrams", n = n_words) %>%
    separate(ngram, into = paste0("w", seq_len(n_words)), sep = " ", remove = FALSE, fill = "right") %>%
    filter(if_all(starts_with("w"), ~ !is.na(.x) & .x != "" & !.x %in% tidytext::stop_words$word)) %>%
    count(ngram, sort = TRUE) %>%
    filter(n >= min_n)
}

generate_phrase_candidates_pmi <- function(out, min_n = 3) {
  existing_phrases <- read_phrases(phrases_path) %>%
    mutate(phrase_clean = str_replace_all(phrase, " ", "_")) %>%
    pull(phrase_clean)

  approved_parts <- tibble(phrase = existing_phrases) %>%
    mutate(words = str_split(phrase, "_")) %>%
    mutate(
      bigram_parts = map(words, \(x) {
        if (length(x) < 2) return(character(0))
        map_chr(seq_len(length(x) - 1), \(i) paste(x[i:(i + 1)], collapse = "_"))
      }),
      trigram_parts = map(words, \(x) {
        if (length(x) < 3) return(character(0))
        map_chr(seq_len(length(x) - 2), \(i) paste(x[i:(i + 2)], collapse = "_"))
      })
    ) %>%
    transmute(all_parts = map2(phrase, map2(bigram_parts, trigram_parts, c), \(p, parts) c(p, parts))) %>%
    pull(all_parts) %>%
    unlist() %>%
    unique()

  topic_clean <- out$my_data2 %>%
    transmute(Topic = clean_text(as.character(Topic), preset = "topic"))
  words <- topic_clean %>%
    tidytext::unnest_tokens(word, Topic) %>%
    filter(word != "", str_detect(word, "[a-z]"), !word %in% tidytext::stop_words$word)
  word_counts <- words %>% count(word, name = "word_n")
  total_words <- sum(word_counts$word_n)

  bigram_counts <- topic_clean %>%
    tidytext::unnest_tokens(bigram, Topic, token = "ngrams", n = 2) %>%
    separate(bigram, into = c("w1", "w2"), sep = " ", remove = FALSE, fill = "right") %>%
    filter(!is.na(w1), !is.na(w2), w1 != "", w2 != "", !w1 %in% tidytext::stop_words$word, !w2 %in% tidytext::stop_words$word) %>%
    count(w1, w2, bigram, name = "n")
  total_bigrams <- sum(bigram_counts$n)

  bigram_pmi <- bigram_counts %>%
    left_join(word_counts, by = c("w1" = "word")) %>% rename(w1_n = word_n) %>%
    left_join(word_counts, by = c("w2" = "word")) %>% rename(w2_n = word_n) %>%
    mutate(
      p_bigram = n / total_bigrams,
      p_w1 = w1_n / total_words,
      p_w2 = w2_n / total_words,
      score = log2(p_bigram / (p_w1 * p_w2)),
      phrase = str_replace_all(bigram, " ", "_"),
      type = "bigram"
    ) %>%
    dplyr::select(w1, w2, phrase, n, score, type)

  trigram_counts <- topic_clean %>%
    tidytext::unnest_tokens(trigram, Topic, token = "ngrams", n = 3) %>%
    separate(trigram, into = c("w1", "w2", "w3"), sep = " ", remove = FALSE, fill = "right") %>%
    filter(!is.na(w1), !is.na(w2), !is.na(w3), w1 != "", w2 != "", w3 != "", !w1 %in% tidytext::stop_words$word, !w2 %in% tidytext::stop_words$word, !w3 %in% tidytext::stop_words$word) %>%
    count(w1, w2, w3, trigram, name = "n")

  trigram_pmi <- trigram_counts %>%
    left_join(bigram_pmi %>% dplyr::select(w1, w2, score), by = c("w1", "w2")) %>% rename(pmi12 = score) %>%
    left_join(bigram_pmi %>% dplyr::select(w1, w2, score) %>% rename(w2 = w1, w3 = w2), by = c("w2", "w3")) %>% rename(pmi23 = score) %>%
    mutate(score = (pmi12 + pmi23) / 2, phrase = str_replace_all(trigram, " ", "_"), type = "trigram") %>%
    dplyr::select(phrase, n, score, type)

  phrase_candidates <- bind_rows(bigram_pmi %>% dplyr::select(phrase, n, score, type), trigram_pmi) %>%
    filter(n >= min_n, !phrase %in% approved_parts) %>%
    arrange(desc(score), desc(n))
  phrases_to_add <- phrase_candidates %>% mutate(phrase = str_replace_all(phrase, "_", " ")) %>% dplyr::select(phrase)
  list(phrase_candidates = phrase_candidates, phrases_to_add = phrases_to_add)
}

build_outputs <- function(my_data, lex_path, phrases_path, custom_merge_path, categories_path) {
  phrases_tbl <- read_phrases(phrases_path)
  lex_map <- read_lex(lex_path)
  custom_map <- read_custom_merges(custom_merge_path)
  categories_map <- read_categories(categories_path)
  purpose_cols <- c("continuing_education", "patient_care", "lecture", "ebp", "research", "grant", "publication", "irb_app", "admin", "policy", "patient_info")
  needed_flags <- c("attending", "med_ed", "nurse", "other_provider", "committee", purpose_cols)
  missing_flags <- setdiff(needed_flags, names(my_data))
  if (length(missing_flags) > 0) stop("Missing expected HUMC columns: ", paste(missing_flags, collapse = ", "), ". Re-run 00_build_humc_master_csv.R first.")

  my_data2 <- my_data %>%
    mutate(request_id = row_number()) %>%
    mutate(
      across(c(attending, med_ed, nurse, other_provider, committee), flag_to_binary),
      across(all_of(purpose_cols), flag_to_binary),
      submitter_type = standardize_submitter_type(attending, med_ed, nurse, other_provider, committee),
      Topic = clean_text(topic, preset = "normalize"),
      Topic = collapse_phrases(Topic, phrases_tbl),
      citation_count = suppressWarnings(as.numeric(citation_count))
    )

  purpose_long <- make_purpose_long(my_data2)
  tidy_clean <- my_data2 %>%
    tidytext::unnest_tokens(word, Topic) %>%
    mutate(word = str_replace_all(word, "[’`]", "'"), word = str_replace_all(word, "\\.", ""), word = str_replace_all(word, "[^a-z'_-]", "")) %>%
    filter(word != "", word != "pre", str_detect(word, "[a-z]"))

  data_norm <- tidy_clean %>%
    left_join(lex_map, by = c("word" = "token")) %>%
    mutate(lemma0 = coalesce(lemma_from_lex, word), lemma1 = if_else(is.na(lemma_from_lex), textstem::lemmatize_words(word), lemma0)) %>%
    left_join(custom_map, by = c("lemma1" = "token")) %>%
    mutate(lemma = coalesce(lemma_custom, lemma1)) %>%
    anti_join(tidytext::stop_words, by = c("lemma" = "word")) %>%
    left_join(categories_map, by = "lemma")

  category_by_purpose_request <- data_norm %>%
    filter(!is.na(category)) %>%
    distinct(request_id, category) %>%
    left_join(purpose_long, by = "request_id", relationship = "many-to-many") %>%
    filter(!is.na(purpose)) %>%
    count(purpose, category, sort = TRUE)

  category_submitter_purpose_req <- data_norm %>%
    filter(!is.na(category)) %>%
    distinct(request_id, submitter_type, category) %>%
    left_join(purpose_long, by = "request_id", relationship = "many-to-many") %>%
    filter(!is.na(purpose)) %>%
    count(submitter_type, purpose, category, sort = TRUE)

  out <- list(
    my_data2 = my_data2,
    purpose_long = purpose_long,
    phrases_tbl = phrases_tbl,
    lex_map = lex_map,
    custom_map = custom_map,
    categories_map = categories_map,
    data_norm = data_norm,
    lemma_counts = data_norm %>% count(lemma, sort = TRUE),
    top_500_lemmas = get_top_lemmas(data_norm, top_n = 500),
    lemma_counts_by_person = data_norm %>% count(submitter_type, lemma, sort = TRUE),
    category_counts = data_norm %>% filter(!is.na(category)) %>% count(category, sort = TRUE),
    category_by_person = data_norm %>% filter(!is.na(category)) %>% distinct(request_id, submitter_type, category) %>% count(submitter_type, category, sort = TRUE),
    category_by_purpose_request = category_by_purpose_request,
    category_submitter_purpose_req = category_submitter_purpose_req,
    bigram_candidates = generate_ngram_candidates(my_data2, n_words = 2, min_n = 3),
    trigram_candidates = generate_ngram_candidates(my_data2, n_words = 3, min_n = 3)
  )
  out$phrase_results <- generate_phrase_candidates_pmi(out, min_n = 3)
  out
}

if (!file.exists(raw_path)) stop("Cannot find HUMC processed file: ", raw_path, "\nRun 00_build_humc_master_csv.R first.")
my_data <- read_csv(raw_path, show_col_types = FALSE)
out <- build_outputs(my_data, lex_path, phrases_path, custom_merge_path, categories_path)
saveRDS(out, out_path)
message("Saved HUMC analysis object to: ", out_path)
message("Requests in analysis object: ", nrow(out$my_data2))
message("Unique lemmas: ", nrow(out$lemma_counts))
message("Categorized categories: ", nrow(out$category_counts))
