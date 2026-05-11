# scripts/hmh/00_build_hmh_network_dataset.R
#
# Build the HMH network analysis dataset.
#
# Purpose:
#   1. Read HUMC legacy-form data and HMH shared-form data.
#   2. Standardize dates, campus names, requestor categories, purposes, and workload fields.
#   3. Build the request-level network dataset used by the HMH analysis scripts.
#   4. Build the text/lemma inventory using phrases.csv, custom_merges.csv, and lexicon.lex.
#   5. Build phrase/lemma candidates while excluding terms already in phrases.csv/custom_merges.csv.
#   6. Save one RDS object that the HMH network analysis scripts can load quickly.
#
# Run:
#   source("scripts/hmh/00_build_hmh_network_dataset.R")

# Setup --------------------------------------------------------------------

library(tidyverse)
library(janitor)
library(here)
library(lubridate)
library(tidytext)
library(textstem)
library(stringi)

source(here("scripts", "shared", "paths.R"))
source(here("scripts", "shared", "helpers.R"))
source(here("scripts", "shared", "text_helpers.R"))
source(here("scripts", "shared", "output_helpers.R"))
source(here("scripts", "shared", "reference_data_loaders.R"))

# Settings -----------------------------------------------------------------
# This workflow intentionally uses 2025–2026 only because it builds the
# HMH network dataset, not the full HUMC historical dataset.

analysis_years <- c(2025, 2026)

text_inventory_paths <- make_output_paths(file.path("hmh", "text_topics"))

hmh_network_rds_path <- here("data", "processed", "hmh_network_analysis_data.rds")

if (!dir.exists(dirname(hmh_network_rds_path))) {
  dir.create(dirname(hmh_network_rds_path), recursive = TRUE)
}

# Export settings ----------------------------------------------------------

export_text_inventory_csvs <- TRUE

if (export_text_inventory_csvs) {
  clear_output_folder(text_inventory_paths$csv_dir, "\\.csv$")
}

# Compatibility helpers ----------------------------------------------------

if (!exists("clean_blank")) {
  clean_blank <- function(x) {
    x <- stringr::str_squish(as.character(x))
    x[x %in% c("", "NA", "N/A", "NULL", "null", "n/a")] <- NA_character_
    x
  }
}

if (!exists("parse_timestamp")) {
  parse_timestamp <- function(x) {
    dplyr::coalesce(
      suppressWarnings(lubridate::mdy_hms(x)),
      suppressWarnings(lubridate::mdy_hm(x)),
      suppressWarnings(lubridate::ymd_hms(x)),
      suppressWarnings(lubridate::ymd_hm(x)),
      suppressWarnings(lubridate::mdy(x)),
      suppressWarnings(lubridate::ymd(x))
    )
  }
}

pull_col <- function(data, col_name, default = NA_character_) {
  if (col_name %in% names(data)) {
    data[[col_name]]
  } else {
    rep(default, nrow(data))
  }
}

standardize_campus_name <- function(x) {
  x_clean <- str_squish(as.character(x))
  x_lower <- str_to_lower(x_clean)
  
  case_when(
    is.na(x_clean) | x_clean == "" | x_lower %in% c("na", "n/a", "unknown") ~ "Unknown/Not specified",
    str_detect(x_lower, "hackensack") | x_lower == "humc" ~ "Hackensack University Medical Center",
    str_detect(x_lower, "\\bjfk\\b") ~ "JFK University Medical Center",
    str_detect(x_lower, "palisades") ~ "Palisades Medical Center",
    str_detect(x_lower, "carrier") ~ "Carrier Clinic",
    str_detect(x_lower, "network") ~ "Network",
    TRUE ~ x_clean
  )
}

standardize_requestor_name <- function(x) {
  x_original <- clean_blank(x)
  x_lower <- str_to_lower(x_original)
  
  case_when(
    is.na(x_original) ~ "Unknown/Not specified",
    x_lower %in% c("unknown", "na", "n/a") ~ "Unknown/Not specified",
    str_detect(x_lower, "med ed|medical education|resident|fellow") ~ "Resident/Fellow",
    str_detect(
      x_lower,
      "nurse practitioner|nurse practitioner/ pa|nurse practitioner/pa|\\bnp\\b|\\bapn\\b|physician assistant|\\bpa\\b"
    ) ~ "Nurse Practitioner/PA",
    str_detect(x_lower, "attending|physician|doctor|md|do") ~ "Physician",
    str_detect(x_lower, "nurse|\\brn\\b") ~ "Nursing",
    str_detect(x_lower, "social work|therapy|pharmacy|diet|rehab|pt|ot|slp|allied") ~ "Allied Health Professional",
    str_detect(x_lower, "patient|family|consumer") ~ "Consumer",
    str_detect(x_lower, "committee") ~ "Committee",
    TRUE ~ x_original
  )
}

make_campus_affiliation <- function(campus_affiliation, humc, carrier, jfk, palisades, network) {
  case_when(
    !is.na(campus_affiliation) & campus_affiliation != "" ~ campus_affiliation,
    carrier == 1 ~ "Carrier",
    jfk == 1 ~ "JFK",
    palisades == 1 ~ "Palisades",
    network == 1 ~ "Network",
    humc == 1 ~ "HUMC",
    TRUE ~ NA_character_
  )
}

make_purpose_string <- function(
    continuing_education,
    patient_care,
    lecture,
    ebp,
    research,
    grant,
    publication,
    irb_app,
    admin,
    policy,
    patient_info
) {
  purposes <- c(
    if_else(continuing_education == 1, "Continuing Education", NA_character_),
    if_else(patient_care == 1, "Patient Care", NA_character_),
    if_else(lecture == 1, "Lecture / Presentation", NA_character_),
    if_else(ebp == 1, "Evidence-based Practice", NA_character_),
    if_else(research == 1, "Research", NA_character_),
    if_else(grant == 1, "Grant", NA_character_),
    if_else(publication == 1, "Publication", NA_character_),
    if_else(irb_app == 1, "IRB App", NA_character_),
    if_else(admin == 1, "Admin", NA_character_),
    if_else(policy == 1, "Policy", NA_character_),
    if_else(patient_info == 1, "Patient Info", NA_character_)
  )
  
  purposes <- purposes[!is.na(purposes)]
  
  if (length(purposes) == 0) {
    return(NA_character_)
  }
  
  paste(purposes, collapse = ", ")
}

make_requestor_string <- function(
    attending,
    med_ed,
    nurse,
    other_provider,
    committee,
    consumer_health
) {
  requestors <- c(
    if_else(attending == 1, "Physician", NA_character_),
    if_else(med_ed == 1, "Resident/Fellow", NA_character_),
    if_else(nurse == 1, "Nursing", NA_character_),
    if_else(other_provider == 1, "Other Provider", NA_character_),
    if_else(committee == 1, "Committee", NA_character_),
    if_else(consumer_health == 1, "Consumer", NA_character_)
  )
  
  requestors <- requestors[!is.na(requestors)]
  
  if (length(requestors) == 0) {
    return(NA_character_)
  }
  
  paste(requestors, collapse = ", ")
}

categorize_time_spent <- function(x) {
  x_clean <- str_to_lower(clean_blank(x))
  
  case_when(
    is.na(x_clean) ~ NA_character_,
    str_detect(x_clean, "1\\s*-\\s*2|1–2") ~ "Low time",
    str_detect(x_clean, "2\\s*-\\s*5|2–5") ~ "Medium time",
    str_detect(x_clean, "more than 5|5\\+|over 5") ~ "High time",
    TRUE ~ NA_character_
  )
}

parse_purpose_categories <- function(purpose) {
  purpose_cleaned <- str_to_lower(str_squish(purpose))
  
  case_when(
    purpose_cleaned == "patient care" ~ "Patient Care",
    purpose_cleaned == "research" ~ "Research",
    purpose_cleaned == "lecture / presentation" ~ "Lecture / Presentation",
    purpose_cleaned == "lecture/presentation" ~ "Lecture / Presentation",
    purpose_cleaned == "publication" ~ "Publication",
    purpose_cleaned == "evidence-based practice" ~ "Evidence-Based Practice",
    purpose_cleaned == "evidence based practice" ~ "Evidence-Based Practice",
    purpose_cleaned == "continuing education" ~ "Continuing Education",
    purpose_cleaned == "grant" ~ "Grant",
    purpose_cleaned == "irb app" ~ "IRB App",
    purpose_cleaned == "admin" ~ "Admin",
    purpose_cleaned == "policy" ~ "Policy",
    purpose_cleaned == "patient info" ~ "Patient Info",
    TRUE ~ "Other"
  )
}

# Load reference files ------------------------------------------------------

phrases_tbl <- read_phrases(phrases_path)
custom_map <- read_custom_merges(custom_merges_path)
lex_map <- read_lex(lex_path)

# Import HUMC legacy data ---------------------------------------------------
# HUMC rows are included only for the transition period so they can be
# compared with newer HMH shared-form records.

humc_raw <- readr::read_csv(humc_path, show_col_types = FALSE) %>%
  janitor::clean_names()

humc_dat <- humc_raw %>%
  mutate(
    source_file_type = "humc",
    source_label = "HUMC legacy form",
    original_id = pull_col(humc_raw, "request_id"),
    
    humc = flag_to_binary(pull_col(humc_raw, "humc")),
    carrier = flag_to_binary(pull_col(humc_raw, "carrier")),
    jfk = flag_to_binary(pull_col(humc_raw, "jfk")),
    palisades = flag_to_binary(pull_col(humc_raw, "palisades")),
    network = flag_to_binary(pull_col(humc_raw, "network")),
    
    submitted_date = as.Date(pull_col(humc_raw, "date")),
    submitted_at = as.POSIXct(submitted_date),
    year = year(submitted_date),
    month = month(submitted_date, label = TRUE, abbr = TRUE),
    month_num = month(submitted_date),
    year_month = as.Date(floor_date(submitted_at, "month")),
    week = floor_date(submitted_date, unit = "week", week_start = 1),
    weekday = wday(submitted_date, label = TRUE, abbr = FALSE),
    hour = NA_integer_,
    
    campus_affiliation_raw = clean_blank(pull_col(humc_raw, "campus_affiliation")),
    campus_affiliation_clean = make_campus_affiliation(
      campus_affiliation = campus_affiliation_raw,
      humc = humc,
      carrier = carrier,
      jfk = jfk,
      palisades = palisades,
      network = network
    ),
    campus_affiliation_detail = clean_blank(pull_col(humc_raw, "campus_affiliation_detail")),
    
    requestor_category_raw = purrr::pmap_chr(
      list(
        flag_to_binary(pull_col(humc_raw, "attending")),
        flag_to_binary(pull_col(humc_raw, "med_ed")),
        flag_to_binary(pull_col(humc_raw, "nurse")),
        flag_to_binary(pull_col(humc_raw, "other_provider")),
        flag_to_binary(pull_col(humc_raw, "committee")),
        flag_to_binary(pull_col(humc_raw, "consumer_health"))
      ),
      ~ make_requestor_string(..1, ..2, ..3, ..4, ..5, ..6)
    ),
    requestor_category = standardize_requestor_name(requestor_category_raw),
    
    request_received = NA_character_,
    research_topic = clean_blank(pull_col(humc_raw, "topic")),
    time_spent = NA_character_,
    effort_level = NA_character_,
    
    purpose = purrr::pmap_chr(
      list(
        flag_to_binary(pull_col(humc_raw, "continuing_education")),
        flag_to_binary(pull_col(humc_raw, "patient_care")),
        flag_to_binary(pull_col(humc_raw, "lecture")),
        flag_to_binary(pull_col(humc_raw, "ebp")),
        flag_to_binary(pull_col(humc_raw, "research")),
        flag_to_binary(pull_col(humc_raw, "grant")),
        flag_to_binary(pull_col(humc_raw, "publication")),
        flag_to_binary(pull_col(humc_raw, "irb_app")),
        flag_to_binary(pull_col(humc_raw, "admin")),
        flag_to_binary(pull_col(humc_raw, "policy")),
        flag_to_binary(pull_col(humc_raw, "patient_info"))
      ),
      ~ make_purpose_string(..1, ..2, ..3, ..4, ..5, ..6, ..7, ..8, ..9, ..10, ..11)
    ),
    
    n_searches = NA_real_,
    citation_count = suppressWarnings(as.numeric(pull_col(humc_raw, "citation_count")))
  ) %>%
  filter(year %in% analysis_years) %>%
  transmute(
    source_file_type,
    source_label,
    original_id,
    submitted_at,
    submitted_date,
    year,
    month,
    month_num,
    year_month,
    week,
    weekday,
    hour,
    campus_affiliation_raw,
    campus_affiliation_clean,
    campus_affiliation_detail,
    humc,
    carrier,
    jfk,
    palisades,
    network,
    requestor_category_raw,
    requestor_category,
    request_received,
    research_topic,
    time_spent,
    effort_level,
    purpose,
    n_searches,
    citation_count
  )

# Import HMH shared-form literature search data -----------------------------

hmh_raw <- readr::read_csv(hmh_raw_csv_path, show_col_types = FALSE) %>%
  janitor::clean_names()

hmh_dat <- hmh_raw %>%
  mutate(
    source_file_type = "hmh",
    source_label = "HMH shared form",
    original_id = row_number(),
    
    campus_text = str_to_lower(clean_blank(pull_col(hmh_raw, "campus_affiliation"))),
    humc = if_else(str_detect(campus_text, "hackensack|humc"), 1L, 0L),
    carrier = if_else(str_detect(campus_text, "carrier"), 1L, 0L),
    jfk = if_else(str_detect(campus_text, "jfk"), 1L, 0L),
    palisades = if_else(str_detect(campus_text, "palisades"), 1L, 0L),
    network = if_else(str_detect(campus_text, "network"), 1L, 0L),
    
    submitted_at = parse_timestamp(pull_col(hmh_raw, "timestamp")),
    submitted_date = as.Date(submitted_at),
    year = year(submitted_at),
    month = month(submitted_at, label = TRUE, abbr = TRUE),
    month_num = month(submitted_at),
    year_month = as.Date(floor_date(submitted_at, "month")),
    week = floor_date(submitted_at, unit = "week", week_start = 1),
    weekday = wday(submitted_at, label = TRUE, abbr = FALSE),
    hour = hour(submitted_at),
    
    requestor_category_raw = clean_blank(
      pull_col(hmh_raw, "who_requested_this_information")
    ),
    requestor_category = standardize_requestor_name(requestor_category_raw),
    
    campus_affiliation_raw = clean_blank(pull_col(hmh_raw, "campus_affiliation")),
    campus_affiliation_clean = make_campus_affiliation(
      campus_affiliation = campus_affiliation_raw,
      humc = humc,
      carrier = carrier,
      jfk = jfk,
      palisades = palisades,
      network = network
    ),
    campus_affiliation_detail = campus_affiliation_clean,
    
    requestor_category = standardize_requestor_name(
      clean_blank(pull_col(hmh_raw, "who_requested_this_information"))
    ),
    request_received = clean_blank(pull_col(hmh_raw, "how_was_the_question_request_received")),
    research_topic = clean_blank(pull_col(hmh_raw, "research_topic")),
    time_spent = clean_blank(pull_col(hmh_raw, "time_spent_on_searches")),
    effort_level = categorize_time_spent(time_spent),
    purpose = clean_blank(pull_col(hmh_raw, "purpose_of_request")),
    n_searches = suppressWarnings(as.numeric(pull_col(hmh_raw, "number_of_literature_searches"))),
    citation_count = NA_real_,
    select_question_request_type = clean_blank(pull_col(hmh_raw, "select_question_request_type"))
  ) %>%
  filter(
    select_question_request_type == "Literature Search",
    year %in% analysis_years
  ) %>%
  transmute(
    source_file_type,
    source_label,
    original_id,
    submitted_at,
    submitted_date,
    year,
    month,
    month_num,
    year_month,
    week,
    weekday,
    hour,
    campus_affiliation_raw,
    campus_affiliation_clean,
    campus_affiliation_detail,
    humc,
    carrier,
    jfk,
    palisades,
    network,
    requestor_category_raw,
    requestor_category,
    request_received,
    research_topic,
    time_spent,
    effort_level,
    purpose,
    n_searches,
    citation_count
  )

# Combine request-level data -----------------------------------------------

hmh_network_dat <- bind_rows(humc_dat, hmh_dat) %>%
  mutate(
    request_id = row_number(),
    global_request_id = request_id,
    campus_affiliation_clean = standardize_campus_name(campus_affiliation_clean),
    campus_affiliation_raw = standardize_campus_name(campus_affiliation_raw),
    requestor_category = standardize_requestor_name(requestor_category),

    requestor_category = case_when(
      year == 2026 & requestor_category == "Physical Therapist" ~ "Allied Health Professional",
      TRUE ~ requestor_category
    ),
    plot_group = case_when(
      source_file_type == "humc" ~ "HUMC legacy form",
      source_file_type == "hmh" & !is.na(campus_affiliation_clean) ~ campus_affiliation_clean,
      source_file_type == "hmh" ~ "HMH shared form",
      TRUE ~ "Other"
    )
  ) %>%
  relocate(request_id, global_request_id)

# Keep a separate shared-form subset for analyses that require fields
# available only in the newer HMH form.

hmh_shared_form_dat <- hmh_network_dat %>%
  filter(source_file_type == "hmh")

# Purpose-level data --------------------------------------------------------

hmh_tidy_purposes <- hmh_network_dat %>%
  filter(!is.na(purpose), purpose != "") %>%
  separate_rows(purpose, sep = ",") %>%
  mutate(
    purpose = str_squish(purpose),
    purpose_category = parse_purpose_categories(purpose),
    purpose_other_detail = if_else(
      purpose_category == "Other",
      purpose,
      NA_character_
    )
  ) %>%
  dplyr::select(
    request_id,
    global_request_id,
    source_file_type,
    source_label,
    submitted_date,
    year,
    year_month,
    campus_affiliation_raw,
    campus_affiliation_clean,
    campus_affiliation_detail,
    humc,
    carrier,
    jfk,
    palisades,
    network,
    plot_group,
    requestor_category,
    time_spent,
    effort_level,
    purpose_category,
    purpose_other_detail
  )

hmh_shared_form_purposes <- hmh_tidy_purposes %>%
  filter(source_file_type == "hmh")

# Topic-level data ----------------------------------------------------------

all_research_topics_full <- hmh_network_dat %>%
  transmute(
    global_request_id,
    request_id,
    source_file_type,
    source_label,
    original_id,
    submitted_date,
    year,
    year_month,
    campus_affiliation = campus_affiliation_clean,
    campus_affiliation_clean,
    requestor_category,
    research_topic = str_squish(research_topic)
  ) %>%
  filter(!is.na(research_topic), research_topic != "")

# Text normalization and lemmatization -------------------------------------

topics_normalized <- all_research_topics_full %>%
  mutate(
    research_topic_clean = clean_text(research_topic, preset = "normalize"),
    research_topic_clean = collapse_phrases(research_topic_clean, phrases_tbl),
    research_topic_clean = na_if(research_topic_clean, "")
  ) %>%
  filter(!is.na(research_topic_clean))

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
    campus_affiliation,
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
    lemma1 = if_else(
      is.na(lemma_from_lex),
      textstem::lemmatize_words(word),
      lemma0
    )
  ) %>%
  left_join(custom_map, by = c("lemma1" = "token")) %>%
  mutate(lemma = coalesce(lemma_custom, lemma1)) %>%
  anti_join(tidytext::stop_words, by = c("lemma" = "word")) %>%
  filter(!str_detect(lemma, "^\\d+$"))

# Summary text tables -------------------------------------------------------

all_lemmas <- tidy_lemmas_all %>%
  distinct(lemma) %>%
  arrange(lemma)

top_500_lemmas <- tidy_lemmas_all %>%
  count(lemma, sort = TRUE, name = "n_mentions") %>%
  mutate(prop_mentions = n_mentions / sum(n_mentions)) %>%
  slice_head(n = 500)

lemma_counts_by_source <- tidy_lemmas_all %>%
  count(source_file_type, lemma, sort = TRUE, name = "n_mentions")

lemma_counts_by_campus <- tidy_lemmas_all %>%
  count(campus_affiliation, lemma, sort = TRUE, name = "n_mentions")

lemma_counts_by_requestor <- tidy_lemmas_all %>%
  count(requestor_category, lemma, sort = TRUE, name = "n_mentions")

# Phrase/lemma candidates --------------------------------------------------
# Candidate phrases are generated for manual review only. Approved terms
# should be added to phrases.csv or custom_merges.csv and then the pipeline
# should be rerun.

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

phrase_candidate_tokens <- topics_normalized %>%
  dplyr::select(global_request_id, research_topic_clean)

candidate_bigrams <- phrase_candidate_tokens %>%
  tidytext::unnest_tokens(ngram, research_topic_clean, token = "ngrams", n = 2) %>%
  filter(!is.na(ngram)) %>%
  separate(ngram, into = c("word1", "word2"), sep = " ", remove = FALSE) %>%
  filter(
    !is.na(word1),
    !is.na(word2),
    !word1 %in% tidytext::stop_words$word,
    !word2 %in% tidytext::stop_words$word
  ) %>%
  count(ngram, sort = TRUE, name = "n") %>%
  mutate(type = "bigram")

candidate_trigrams <- phrase_candidate_tokens %>%
  tidytext::unnest_tokens(ngram, research_topic_clean, token = "ngrams", n = 3) %>%
  filter(!is.na(ngram)) %>%
  separate(ngram, into = c("word1", "word2", "word3"), sep = " ", remove = FALSE) %>%
  filter(
    !is.na(word1),
    !is.na(word2),
    !is.na(word3),
    !word1 %in% tidytext::stop_words$word,
    !word2 %in% tidytext::stop_words$word,
    !word3 %in% tidytext::stop_words$word
  ) %>%
  count(ngram, sort = TRUE, name = "n") %>%
  mutate(type = "trigram")

phrase_candidates <- bind_rows(
  candidate_bigrams,
  candidate_trigrams
) %>%
  mutate(ngram = str_to_lower(ngram)) %>%
  filter(!ngram %in% exclude_terms) %>%
  arrange(desc(n), type, ngram)

# Optional text inventory CSV exports --------------------------------------

if (export_text_inventory_csvs) {
  write_pretty_csv(all_research_topics_full, "all_research_topics_full", text_inventory_paths$csv_dir)
  
  write_pretty_csv(
    all_research_topics_full %>%
      count(research_topic, sort = TRUE, name = "n_records"),
    "all_research_topics_counts",
    text_inventory_paths$csv_dir)
  
  
  write_pretty_csv(tidy_lemmas_all, "all_lemma_records_full", text_inventory_paths$csv_dir)
  write_pretty_csv(all_lemmas, "all_lemmas", text_inventory_paths$csv_dir)
  write_pretty_csv(top_500_lemmas, "top_500_lemmas", text_inventory_paths$csv_dir)
  write_pretty_csv(lemma_counts_by_source, "lemma_counts_by_source", text_inventory_paths$csv_dir)
  write_pretty_csv(lemma_counts_by_campus, "lemma_counts_by_campus", text_inventory_paths$csv_dir)
  write_pretty_csv(lemma_counts_by_requestor, "lemma_counts_by_requestor", text_inventory_paths$csv_dir)
  write_pretty_csv(phrase_candidates, "phrase_candidates", text_inventory_paths$csv_dir)
}

# Save final object for report script --------------------------------------

hmh_network_analysis_data <- list(
  analysis_years = analysis_years,
  hmh_network_dat = hmh_network_dat,
  hmh_shared_form_dat = hmh_shared_form_dat,
  hmh_tidy_purposes = hmh_tidy_purposes,
  hmh_shared_form_purposes = hmh_shared_form_purposes,
  all_research_topics_full = all_research_topics_full,
  topics_normalized = topics_normalized,
  tidy_lemmas_all = tidy_lemmas_all,
  all_lemmas = all_lemmas,
  top_500_lemmas = top_500_lemmas,
  lemma_counts_by_source = lemma_counts_by_source,
  lemma_counts_by_campus = lemma_counts_by_campus,
  lemma_counts_by_requestor = lemma_counts_by_requestor,
  phrase_candidates = phrase_candidates,
  reference = list(
    phrases_tbl = phrases_tbl,
    custom_map = custom_map,
    lex_map = lex_map
  )
)

saveRDS(
  object = hmh_network_analysis_data,
  file = hmh_network_rds_path
)

# Console summary ----------------------------------------------------------

cat("\n--- HMH Network Dataset Build Complete ---\n")
cat("Request-level rows:", nrow(hmh_network_dat), "\n")
cat("HUMC rows:", nrow(humc_dat), "\n")
cat("HMH rows:", nrow(hmh_dat), "\n")
cat("Shared-form rows:", nrow(hmh_shared_form_dat), "\n")
cat("Purpose rows:", nrow(hmh_tidy_purposes), "\n")
cat("Research topic records:", nrow(all_research_topics_full), "\n")
cat("Lemma records:", nrow(tidy_lemmas_all), "\n")
cat("Unique lemmas:", n_distinct(tidy_lemmas_all$lemma), "\n")
cat("Phrase/lemma candidates:", nrow(phrase_candidates), "\n")

if (export_text_inventory_csvs) {
  cat("Text inventory CSVs written to:", text_inventory_paths$csv_dir, "\n")
} else {
  cat("Text inventory CSV export skipped. Set export_text_inventory_csvs <- TRUE to write CSVs.\n")
}

cat("HMH network analysis object saved to:", hmh_network_rds_path, "\n")
