# I used AI to help write this code

# Setup ------------------------------------------------------------

library(here)
library(tidyverse)
library(tidytext)
library(textstem)
library(readxl)
library(janitor)
library(stringr)
library(stringi)

# Shared helpers ---------------------------------------------------

source(here::here("scripts", "shared", "paths.R"))
source(here::here("scripts", "shared", "helpers.R"))
source(here::here("scripts", "shared", "text_helpers.R"))
source(here::here("scripts", "shared", "output_helpers.R"))

# Project settings -------------------------------------------------

project_name <- "humc"

# Output directories -----------------------------------------------

paths <- make_output_paths(project_name)

output_dir <- paths$output_dir
csv_dir <- paths$csv_dir
old_csv_dir <- paths$old_csv_dir

figures_dir <- paths$figures_dir
old_figures_dir <- paths$old_figures_dir

formatted_tables_dir <- paths$formatted_tables_dir
old_formatted_tables_dir <- paths$old_formatted_tables_dir

old_summaries_dir <- paths$old_summaries_dir

model_dir <- paths$model_dir
old_model_dir <- paths$old_model_dir

# HUMC-specific paths ----------------------------------------------

project_reference_dir <- file.path(reference_dir, project_name)

if (!dir.exists(project_reference_dir)) {
  dir.create(project_reference_dir, recursive = TRUE)
}

raw_path <- humc_path

categories_path <- file.path(project_reference_dir, "categories_long.xlsx")
coding_path <- file.path(project_reference_dir, "lemma_coding.csv")

out_path <- file.path(output_dir, "out.rds")

# Pipeline runner --------------------------------------------------

run_full_analysis <- function() {
  source(here::here("scripts", project_name, "02_run_pipeline.R"))
  source(here::here("scripts", project_name, "03_analysis_tables.R"))
  source(here::here("scripts", project_name, "04_figures.R"))
}

# HUMC reference file readers --------------------------------------

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
    filter(
      !is.na(lemma), lemma != "",
      !is.na(category), category != ""
    ) %>%
    distinct(lemma, .keep_all = TRUE)
}

read_lemma_coding <- function(path = coding_path) {
  if (!file.exists(path)) {
    starter <- tibble(
      lemma = character(),
      category = character(),
      specialty_1 = character(),
      specialty_2 = character()
    )
    readr::write_csv(starter, path)
    message("Created empty lemma_coding.csv. Add coding and rerun.")
  }
  
  readr::read_csv(
    path,
    show_col_types = FALSE,
    col_types = cols(
      lemma = col_character(),
      category = col_character(),
      specialty_1 = col_character(),
      specialty_2 = col_character()
    ),
    locale = readr::locale(encoding = "latin1")
  ) %>%
    mutate(
      across(
        everything(),
        ~ stringi::stri_enc_toutf8(as.character(.x), validate = TRUE)
      ),
      across(
        everything(),
        ~ str_replace_all(.x, "\uFFFD", "")
      ),
      lemma = str_to_lower(str_trim(lemma)),
      category = na_if(str_trim(category), ""),
      specialty_1 = na_if(str_trim(specialty_1), ""),
      specialty_2 = na_if(str_trim(specialty_2), "")
    ) %>%
    filter(!is.na(lemma), lemma != "") %>%
    distinct(lemma, .keep_all = TRUE)
}

# Lemma/coding helpers ---------------------------------------------

get_top_lemmas <- function(data_norm, top_n = 300) {
  data_norm %>%
    count(lemma, sort = TRUE) %>%
    head(top_n)
}

refresh_top_lemmas <- function(out, top_n = 300) {
  get_top_lemmas(out$data_norm, top_n = top_n)
}

regenerate_lemmas_to_categorize <- function(data_norm, categories_map, top_n = 300) {
  data_norm %>%
    count(lemma, sort = TRUE) %>%
    anti_join(categories_map, by = "lemma") %>%
    slice_head(n = top_n)
}

refresh_lemmas_to_categorize <- function(out, top_n = 300) {
  regenerate_lemmas_to_categorize(
    data_norm = out$data_norm,
    categories_map = out$categories_map,
    top_n = top_n
  )
}

refresh_coding_sheet <- function(out, top_n = 500, coding_path = coding_path) {
  current_lemmas <- out$data_norm %>%
    filter(!is.na(lemma), lemma != "") %>%
    count(lemma, sort = TRUE) %>%
    slice_head(n = top_n)
  
  coding_master <- read_lemma_coding(coding_path)
  
  coding_sheet <- current_lemmas %>%
    left_join(coding_master, by = "lemma") %>%
    mutate(
      needs_category = is.na(category),
      needs_specialty = is.na(specialty_1)
    ) %>%
    arrange(desc(n), lemma)
  
  uncoded_only <- coding_sheet %>%
    filter(is.na(category))
  
  readr::write_csv(
    coding_sheet,
    file.path(output_dir, "lemma_coding_sheet_refreshed.csv")
  )
  
  readr::write_csv(
    uncoded_only,
    file.path(output_dir, "lemma_coding_uncoded_only.csv")
  )
  
  list(
    coding_master = coding_master,
    coding_sheet = coding_sheet,
    uncoded_only = uncoded_only
  )
}

update_master_coding <- function(
    coding_path = coding_path,
    reviewed_path = file.path(output_dir, "lemma_coding_sheet_refreshed.csv")
) {
  existing <- read_lemma_coding(coding_path)
  
  reviewed <- readr::read_csv(
    reviewed_path,
    show_col_types = FALSE,
    col_types = cols(
      lemma = col_character(),
      n = col_double(),
      category = col_character(),
      specialty_1 = col_character(),
      specialty_2 = col_character(),
      needs_category = col_logical(),
      needs_specialty = col_logical()
    ),
    locale = readr::locale(encoding = "latin1")
  ) %>%
    mutate(
      across(
        where(is.character),
        ~ stringi::stri_enc_toutf8(.x, validate = TRUE)
      ),
      across(
        where(is.character),
        ~ str_replace_all(.x, "\uFFFD", "")
      )
    ) %>%
    transmute(
      lemma = str_to_lower(str_trim(lemma)),
      category = na_if(str_trim(category), ""),
      specialty_1 = na_if(str_trim(specialty_1), ""),
      specialty_2 = na_if(str_trim(specialty_2), "")
    ) %>%
    filter(!is.na(lemma), lemma != "") %>%
    distinct(lemma, .keep_all = TRUE)
  
  updated <- bind_rows(existing, reviewed) %>%
    arrange(lemma) %>%
    group_by(lemma) %>%
    slice_tail(n = 1) %>%
    ungroup()
  
  readr::write_csv(updated, coding_path)
  
  updated
}

# N-gram helpers ---------------------------------------------------

generate_ngram_candidates <- function(df, n_words = 2, min_n = 3) {
  df %>%
    transmute(
      Topic = clean_topic_text(as.character(Topic))
    ) %>%
    filter(!is.na(Topic), Topic != "") %>%
    tidytext::unnest_tokens(
      output = ngram,
      input = Topic,
      token = "ngrams",
      n = n_words
    ) %>%
    tidyr::separate(
      ngram,
      into = paste0("w", seq_len(n_words)),
      sep = " ",
      remove = FALSE,
      fill = "right"
    ) %>%
    filter(
      if_all(
        starts_with("w"),
        ~ !is.na(.x) & .x != "" & !.x %in% tidytext::stop_words$word
      )
    ) %>%
    count(ngram, sort = TRUE) %>%
    filter(n >= min_n)
}

refresh_phrase_candidates <- function(out, min_n = 3) {
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
    transmute(
      all_parts = map2(
        phrase,
        map2(bigram_parts, trigram_parts, c),
        \(p, parts) c(p, parts)
      )
    ) %>%
    pull(all_parts) %>%
    unlist() %>%
    unique()
  
  topic_clean <- out$my_data2 %>%
    transmute(Topic = clean_topic_text(as.character(Topic)))
  
  words <- topic_clean %>%
    tidytext::unnest_tokens(word, Topic) %>%
    filter(
      word != "",
      str_detect(word, "[a-z]"),
      !word %in% tidytext::stop_words$word
    )
  
  word_counts <- words %>%
    count(word, name = "word_n")
  
  total_words <- sum(word_counts$word_n)
  
  bigram_counts <- topic_clean %>%
    tidytext::unnest_tokens(bigram, Topic, token = "ngrams", n = 2) %>%
    tidyr::separate(
      bigram,
      into = c("w1", "w2"),
      sep = " ",
      remove = FALSE,
      fill = "right"
    ) %>%
    filter(
      !is.na(w1), !is.na(w2),
      w1 != "", w2 != "",
      !w1 %in% tidytext::stop_words$word,
      !w2 %in% tidytext::stop_words$word
    ) %>%
    count(w1, w2, bigram, name = "n")
  
  total_bigrams <- sum(bigram_counts$n)
  
  bigram_pmi <- bigram_counts %>%
    left_join(word_counts, by = c("w1" = "word")) %>%
    rename(w1_n = word_n) %>%
    left_join(word_counts, by = c("w2" = "word")) %>%
    rename(w2_n = word_n) %>%
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
    tidyr::separate(
      trigram,
      into = c("w1", "w2", "w3"),
      sep = " ",
      remove = FALSE,
      fill = "right"
    ) %>%
    filter(
      !is.na(w1), !is.na(w2), !is.na(w3),
      w1 != "", w2 != "", w3 != "",
      !w1 %in% tidytext::stop_words$word,
      !w2 %in% tidytext::stop_words$word,
      !w3 %in% tidytext::stop_words$word
    ) %>%
    count(w1, w2, w3, trigram, name = "n")
  
  trigram_pmi <- trigram_counts %>%
    left_join(
      bigram_pmi %>% dplyr::select(w1, w2, score),
      by = c("w1", "w2")
    ) %>%
    rename(pmi12 = score) %>%
    left_join(
      bigram_pmi %>%
        dplyr::select(w1, w2, score) %>%
        rename(w2 = w1, w3 = w2),
      by = c("w2", "w3")
    ) %>%
    rename(pmi23 = score) %>%
    mutate(
      score = (pmi12 + pmi23) / 2,
      phrase = str_replace_all(trigram, " ", "_"),
      type = "trigram"
    ) %>%
    dplyr::select(phrase, n, score, type)
  
  phrase_candidates <- bind_rows(
    bigram_pmi %>% dplyr::select(phrase, n, score, type),
    trigram_pmi
  ) %>%
    filter(
      n >= min_n,
      !phrase %in% approved_parts
    ) %>%
    arrange(desc(score), desc(n))
  
  phrases_to_add <- phrase_candidates %>%
    mutate(phrase = str_replace_all(phrase, "_", " ")) %>%
    dplyr::select(phrase)
  
  list(
    phrase_candidates = phrase_candidates,
    phrases_to_add = phrases_to_add
  )
}

# Main pipeline ----------------------------------------------------

build_outputs <- function(my_data,
                          lex_path,
                          phrases_path,
                          custom_merge_path,
                          categories_path) {
  
  phrases_tbl <- read_phrases(phrases_path)
  lex_map <- read_lex(lex_path)
  custom_map <- read_custom_merges(custom_merge_path)
  categories_map <- read_categories(categories_path)
  
  purpose_cols <- c(
    "continuing_education", "patient_care", "lecture", "ebp", "research",
    "grant", "publication", "irb_app", "admin", "policy", "patient_info"
  )
  
  my_data2 <- my_data %>%
    mutate(request_id = row_number()) %>%
    mutate(
      across(c(attending, med_ed, nurse, other_provider, committee), flag_to_binary),
      across(all_of(purpose_cols), flag_to_binary),
      submitter_type = case_when(
        nurse == 1 ~ "Nurse",
        other_provider == 1 ~ "OtherProvider",
        attending == 1 ~ "Attending",
        med_ed == 1 ~ "MedEd",
        committee == 1 ~ "Committee",
        TRUE ~ "Unknown"
      ),
      Topic = clean_topic_for_normalization(topic),
      Topic = collapse_phrases(Topic, phrases_tbl),
      citation_count = suppressWarnings(as.numeric(citation_count))
    )
  
  purpose_long <- my_data2 %>%
    dplyr::select(request_id, all_of(purpose_cols)) %>%
    pivot_longer(
      cols = all_of(purpose_cols),
      names_to = "purpose",
      values_to = "flag"
    ) %>%
    filter(flag == 1) %>%
    mutate(
      purpose = case_when(
        purpose == "continuing_education" ~ "ContinuingEducation",
        purpose == "patient_care" ~ "PatientCare",
        purpose == "irb_app" ~ "IRBApp",
        purpose == "patient_info" ~ "PatientInfo",
        TRUE ~ str_to_title(purpose)
      )
    ) %>%
    dplyr::select(request_id, purpose)
  
  tidy_clean <- my_data2 %>%
    tidytext::unnest_tokens(word, Topic) %>%
    mutate(
      word = str_replace_all(word, "[’`]", "'"),
      word = str_replace_all(word, "\\.", ""),
      word = str_replace_all(word, "[^a-z'_-]", "")
    ) %>%
    filter(
      word != "",
      word != "pre",
      str_detect(word, "[a-z]")
    )
  
  data_norm <- tidy_clean %>%
    left_join(lex_map, by = c("word" = "token")) %>%
    mutate(
      lemma0 = coalesce(lemma_from_lex, word),
      lemma1 = if_else(
        is.na(lemma_from_lex),
        textstem::lemmatize_words(word),
        lemma0
      )
    ) %>%
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
  
  list(
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
    category_counts = data_norm %>%
      filter(!is.na(category)) %>%
      count(category, sort = TRUE),
    category_by_person = data_norm %>%
      filter(!is.na(category)) %>%
      distinct(request_id, submitter_type, category) %>%
      count(submitter_type, category, sort = TRUE),
    category_by_purpose_request = category_by_purpose_request,
    category_submitter_purpose_req = category_submitter_purpose_req,
    bigram_candidates = generate_ngram_candidates(my_data2, n_words = 2, min_n = 3),
    trigram_candidates = generate_ngram_candidates(my_data2, n_words = 3, min_n = 3)
  )
}

refresh_out <- function() {
  my_data <- readr::read_csv(raw_path, show_col_types = FALSE)
  
  build_outputs(
    my_data = my_data,
    lex_path = lex_path,
    phrases_path = phrases_path,
    custom_merge_path = custom_merge_path,
    categories_path = categories_path
  )
}

# Analysis refresh helpers ----------------------------------------

refresh_category_purpose <- function(out) {
  category_by_purpose <- out$data_norm %>%
    filter(!is.na(category)) %>%
    distinct(request_id, category) %>%
    left_join(out$purpose_long, by = "request_id", relationship = "many-to-many") %>%
    count(purpose, category, sort = TRUE)
  
  category_by_person_purpose <- out$data_norm %>%
    filter(!is.na(category)) %>%
    distinct(request_id, submitter_type, category) %>%
    left_join(out$purpose_long, by = "request_id", relationship = "many-to-many") %>%
    count(submitter_type, purpose, category, sort = TRUE)
  
  list(
    category_by_purpose = category_by_purpose,
    category_by_person_purpose = category_by_person_purpose
  )
}

refresh_citation_summaries <- function(out) {
  
  purpose_cols <- c(
    "continuing_education", "patient_care", "lecture", "ebp", "research",
    "grant", "publication", "irb_app", "admin", "policy", "patient_info"
  )
  
  request_category_citations <- out$data_norm %>%
    filter(!is.na(category)) %>%
    distinct(request_id, category, citation_count)
  
  citation_by_category <- request_category_citations %>%
    group_by(category) %>%
    summarize(
      n_requests = n(),
      mean_citations = mean(citation_count, na.rm = TRUE),
      median_citations = median(citation_count, na.rm = TRUE),
      .groups = "drop"
    )
  
  citation_by_purpose <- out$my_data2 %>%
    dplyr::select(request_id, citation_count, all_of(purpose_cols)) %>%
    pivot_longer(
      cols = all_of(purpose_cols),
      names_to = "purpose",
      values_to = "flag"
    ) %>%
    filter(flag == 1) %>%
    group_by(purpose) %>%
    summarize(
      n_requests = n(),
      mean_citations = mean(citation_count, na.rm = TRUE),
      median_citations = median(citation_count, na.rm = TRUE),
      .groups = "drop"
    )
  
  list(
    citation_by_category = citation_by_category,
    citation_by_purpose = citation_by_purpose
  )
}

refresh_category_chi <- function(out) {
  data_norm_chi <- out$data_norm %>%
    mutate(
      submitter_type_chi = case_when(
        submitter_type == "Committee" ~ "OtherProvider",
        TRUE ~ submitter_type
      )
    )
  
  submitter_counts <- out$data_norm %>%
    distinct(request_id, submitter_type) %>%
    count(submitter_type)
  
  chi_table <- data_norm_chi %>%
    filter(!is.na(category)) %>%
    distinct(request_id, submitter_type_chi, category) %>%
    count(submitter_type_chi, category) %>%
    pivot_wider(
      names_from = category,
      values_from = n,
      values_fill = 0
    ) %>%
    tibble::column_to_rownames("submitter_type_chi") %>%
    as.matrix()
  
  chi_result <- chisq.test(chi_table)
  
  chi_residuals <- as.data.frame(chi_result$stdres) %>%
    rownames_to_column("submitter_type") %>%
    pivot_longer(
      cols = -submitter_type,
      names_to = "category",
      values_to = "std_residual"
    )
  
  n <- sum(chi_table)
  r <- nrow(chi_table)
  c <- ncol(chi_table)
  cramers_v <- sqrt(chi_result$statistic / (n * min(r - 1, c - 1)))
  
  list(
    submitter_counts = submitter_counts,
    chi_table = chi_table,
    chi_result = chi_result,
    chi_residuals = chi_residuals,
    cramers_v = cramers_v
  )
}

refresh_purpose_analysis <- function(out) {
  my_data2_purpose <- out$my_data2 %>%
    mutate(
      submitter_type = if_else(
        submitter_type == "Committee",
        "OtherProvider",
        submitter_type
      )
    )
  
  purpose_counts <- out$purpose_long %>%
    count(purpose, sort = TRUE)
  
  purpose_by_person <- out$purpose_long %>%
    left_join(
      my_data2_purpose %>% dplyr::select(request_id, submitter_type),
      by = "request_id"
    ) %>%
    count(submitter_type, purpose, sort = TRUE)
  
  purpose_table <- purpose_by_person %>%
    pivot_wider(
      names_from = purpose,
      values_from = n,
      values_fill = 0
    ) %>%
    tibble::column_to_rownames("submitter_type") %>%
    as.matrix()
  
  purpose_chi <- chisq.test(purpose_table)
  
  purpose_residuals <- as.data.frame(purpose_chi$stdres) %>%
    rownames_to_column("submitter_type") %>%
    pivot_longer(
      cols = -submitter_type,
      names_to = "purpose",
      values_to = "std_residual"
    )
  
  n <- sum(purpose_table)
  r <- nrow(purpose_table)
  c <- ncol(purpose_table)
  cramers_v_purpose <- sqrt(purpose_chi$statistic / (n * min(r - 1, c - 1)))
  
  list(
    purpose_counts = purpose_counts,
    purpose_by_person = purpose_by_person,
    purpose_table = purpose_table,
    purpose_chi = purpose_chi,
    purpose_residuals = purpose_residuals,
    cramers_v_purpose = cramers_v_purpose
  )
}