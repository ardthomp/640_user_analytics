# Line endings normalized for GitHub display
# scripts/hmh/run_hmh_literature_search_analysis.R
#
# HMH literature search request analysis.
#
# This script overwrites the latest outputs instead of archiving every run.
#
# Outputs:
#   outputs/hmh/literature_searches/csv/
#   outputs/hmh/literature_searches/figures/
#   outputs/hmh/literature_searches/formatted_tables/
#   outputs/hmh/literature_searches/hmh_lit_search_summary.xlsx

# Setup --------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(tidytext)
library(janitor)
library(scales)
library(textstem)
library(purrr)
library(stringi)
library(gt)
library(openxlsx)
library(viridis)
library(MASS)
library(grid)
library(here)

source(here("scripts", "shared", "paths.R"))
source(here("scripts", "shared", "helpers.R"))
source(here("scripts", "shared", "output_helpers.R"))
source(here("scripts", "shared", "text_helpers.R"))
source(here("scripts", "shared", "reference_data_loaders.R"))
source(here("scripts", "shared", "plotting_helpers.R"))

# Settings and paths -------------------------------------------------------

analysis_year <- 2026

weekday_levels <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
time_level_levels <- c("Low time", "Medium time", "High time")

raw_path <- hmh_path
paths <- make_output_paths(file.path("hmh", "literature_searches"))

# Export settings -----------------------------------------------------------

export_csvs <- FALSE
export_workbook <- TRUE

# Clear outputs -------------------------------------------------------------

if (export_csvs) {
  clear_output_folder(paths$csv_dir, "\\.(csv|rds)$")
}

clear_output_folder(paths$figures_dir, "\\.(png|jpg|jpeg|pdf)$")
clear_output_folder(paths$formatted_tables_dir, "\\.html$")
clear_output_folder(paths$output_dir, "^hmh_lit_search_summary\\.xlsx$")

summary_filepath <- file.path(paths$output_dir, "hmh_lit_search_summary.xlsx")

# Local helpers ------------------------------------------------------------

safe_max_numeric <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  max(x, na.rm = TRUE)
}

collapse_requestor_group <- function(x) {
  x_original <- stringr::str_trim(as.character(x))
  x_clean <- stringr::str_to_lower(x_original)

  dplyr::case_when(
    is.na(x_original) | x_original == "" ~ NA_character_,
    stringr::str_detect(
      x_clean,
      "nurse practitioner|\\bnp\\b|\\bapn\\b|physician assistant|\\bpa\\b"
    ) ~ "Nurse Practitioner/PA",
    TRUE ~ x_original
  )
}

if (!exists("parse_timestamp")) {
  parse_timestamp <- function(x) {
    dplyr::coalesce(
      suppressWarnings(lubridate::mdy_hms(x)),
      suppressWarnings(lubridate::mdy_hm(x)),
      suppressWarnings(lubridate::ymd_hms(x)),
      suppressWarnings(lubridate::ymd_hm(x))
    )
  }
}

if (!exists("clean_blank")) {
  clean_blank <- function(x) {
    x <- stringr::str_squish(as.character(x))
    x[x %in% c("", "NA", "N/A", "NULL", "null", "n/a")] <- NA_character_
    x
  }
}

# Load and process data ----------------------------------------------------

raw <- readr::read_csv(raw_path, show_col_types = FALSE) %>%
  janitor::clean_names()

dat <- raw %>%
  mutate(submitted_at_raw = parse_timestamp(timestamp)) %>%
  filter(
    select_question_request_type == "Literature Search",
    lubridate::year(submitted_at_raw) == analysis_year
  ) %>%
  mutate(request_id = row_number()) %>%
  mutate(
    submitted_at = submitted_at_raw,
    submitted_date = as.Date(submitted_at),
    year = lubridate::year(submitted_at),
    month = lubridate::month(submitted_at, label = TRUE, abbr = TRUE),
    month_num = lubridate::month(submitted_at),
    year_month = lubridate::floor_date(submitted_at, "month"),
    week = lubridate::floor_date(submitted_at, unit = "week", week_start = 1),
    weekday = lubridate::wday(submitted_at, label = TRUE, abbr = FALSE) %>%
      factor(levels = weekday_levels),
    hour = lubridate::hour(submitted_at),
    season = case_when(
      month_num %in% c(12, 1, 2) ~ "Winter",
      month_num %in% c(3, 4, 5) ~ "Spring",
      month_num %in% c(6, 7, 8) ~ "Summer",
      TRUE ~ "Fall"
    ),
    requestor_category = clean_blank(who_requested_this_information),
    requestor_group = collapse_requestor_group(requestor_category),
    request_received = clean_blank(how_was_the_question_request_received),
    campus_affiliation = clean_blank(campus_affiliation),
    research_topic = clean_blank(research_topic),
    time_spent = clean_blank(time_spent_on_searches),
    purpose = clean_blank(purpose_of_request),
    n_searches = suppressWarnings(as.numeric(number_of_literature_searches))
  ) %>%
  filter(!is.na(weekday)) %>%
  filter(requestor_group != "Consumer" | is.na(requestor_group))

# Demand / utilization tables ----------------------------------------------

requests_per_month <- dat %>%
  count(year_month, name = "n_requests")

requests_by_weekday <- dat %>%
  count(weekday, name = "n_requests")

requests_by_season <- dat %>%
  count(season, name = "n_requests")

requests_by_hour <- dat %>%
  count(hour, name = "n_requests") %>%
  arrange(hour)

requests_by_requestor <- dat %>%
  count(requestor_group, sort = TRUE, name = "n_requests") %>%
  mutate(prop = n_requests / sum(n_requests))

requests_by_received <- dat %>%
  count(request_received, sort = TRUE, name = "n_requests") %>%
  mutate(prop = n_requests / sum(n_requests))

requests_by_campus <- dat %>%
  count(campus_affiliation, sort = TRUE, name = "n_requests") %>%
  mutate(prop = n_requests / sum(n_requests))

# Purpose tables -----------------------------------------------------------

if (sum(!is.na(dat$purpose)) > 0) {
  tidy_purposes <- dat %>%
    filter(!is.na(purpose), purpose != "") %>%
    tidyr::separate_rows(purpose, sep = ",") %>%
    mutate(
      purpose_cleaned = purpose %>%
        str_trim() %>%
        str_to_lower(),
      purpose_category = case_when(
        purpose_cleaned == "patient care" ~ "Patient Care",
        purpose_cleaned == "research" ~ "Research",
        purpose_cleaned == "lecture / presentation" ~ "Lecture / Presentation",
        purpose_cleaned == "publication" ~ "Publication",
        purpose_cleaned == "evidence-based practice" ~ "Evidence-based Practice",
        TRUE ~ "Other"
      ),
      purpose_other_detail = if_else(
        purpose_category == "Other",
        str_trim(purpose),
        NA_character_
      )
    ) %>%
    dplyr::select(
      request_id,
      requestor_group,
      campus_affiliation,
      time_spent,
      purpose_category,
      purpose_other_detail
    )

  requests_by_purpose <- tidy_purposes %>%
    count(purpose_category, sort = TRUE, name = "n_selections") %>%
    mutate(prop = n_selections / sum(n_selections))

  requestor_by_purpose <- tidy_purposes %>%
    count(requestor_group, purpose_category, sort = TRUE, name = "n_selections") %>%
    group_by(requestor_group) %>%
    mutate(prop_within_requestor = n_selections / sum(n_selections)) %>%
    ungroup()

  campus_by_purpose <- tidy_purposes %>%
    count(campus_affiliation, purpose_category, sort = TRUE, name = "n_selections") %>%
    group_by(campus_affiliation) %>%
    mutate(prop_within_campus = n_selections / sum(n_selections)) %>%
    ungroup()

  other_purpose_details <- tidy_purposes %>%
    filter(purpose_category == "Other") %>%
    count(purpose_other_detail, sort = TRUE, name = "n")

  other_purpose_list <- tidy_purposes %>%
    filter(purpose_category == "Other") %>%
    left_join(
      dat %>% dplyr::select(request_id, research_topic),
      by = "request_id"
    ) %>%
    distinct(request_id, purpose_other_detail, requestor_group, research_topic)
} else {
  tidy_purposes <- tibble()
  requests_by_purpose <- tibble()
  requestor_by_purpose <- tibble()
  campus_by_purpose <- tibble()
  other_purpose_details <- tibble()
  other_purpose_list <- tibble()
}

# Workload / search tables -------------------------------------------------

searches_summary <- dat %>%
  summarize(
    n_requests = n(),
    mean_searches = mean(n_searches, na.rm = TRUE),
    median_searches = median(n_searches, na.rm = TRUE),
    max_searches = safe_max_numeric(n_searches)
  )

searches_by_requestor <- dat %>%
  group_by(requestor_group) %>%
  summarize(
    n_requests = n(),
    mean_searches = mean(n_searches, na.rm = TRUE),
    median_searches = median(n_searches, na.rm = TRUE),
    max_searches = safe_max_numeric(n_searches),
    .groups = "drop"
  )

searches_by_campus <- dat %>%
  group_by(campus_affiliation) %>%
  summarize(
    n_requests = n(),
    mean_searches = mean(n_searches, na.rm = TRUE),
    median_searches = median(n_searches, na.rm = TRUE),
    max_searches = safe_max_numeric(n_searches),
    .groups = "drop"
  )

searches_by_time_spent <- dat %>%
  group_by(time_spent) %>%
  summarize(
    n_requests = n(),
    mean_searches = mean(n_searches, na.rm = TRUE),
    median_searches = median(n_searches, na.rm = TRUE),
    max_searches = safe_max_numeric(n_searches),
    .groups = "drop"
  )

time_spent_counts <- dat %>%
  count(time_spent, sort = TRUE, name = "n_requests") %>%
  mutate(prop = n_requests / sum(n_requests))

time_spent_by_requestor <- dat %>%
  count(requestor_group, time_spent, sort = TRUE, name = "n_requests") %>%
  group_by(requestor_group) %>%
  mutate(prop_within_requestor = n_requests / sum(n_requests)) %>%
  ungroup()

time_spent_pct_by_requestor <- dat %>%
  filter(!is.na(requestor_group), !is.na(time_spent)) %>%
  count(requestor_group, time_spent, name = "n_requests") %>%
  group_by(requestor_group) %>%
  mutate(pct_within_requestor = n_requests / sum(n_requests)) %>%
  ungroup()

if (nrow(tidy_purposes) > 0) {
  time_spent_by_purpose <- tidy_purposes %>%
    filter(!is.na(time_spent)) %>%
    count(purpose_category, time_spent, sort = TRUE, name = "n_selections") %>%
    group_by(purpose_category) %>%
    mutate(prop_within_purpose = n_selections / sum(n_selections)) %>%
    ungroup()

  time_spent_pct_by_purpose <- tidy_purposes %>%
    filter(!is.na(purpose_category), !is.na(time_spent)) %>%
    count(purpose_category, time_spent, name = "n_selections") %>%
    group_by(purpose_category) %>%
    mutate(pct_within_purpose = n_selections / sum(n_selections)) %>%
    ungroup()
} else {
  time_spent_by_purpose <- tibble()
  time_spent_pct_by_purpose <- tibble()
}

# Text analysis ------------------------------------------------------------

phrases_tbl <- read_phrases(phrases_path)
custom_map <- read_custom_merges(custom_merge_path)
lex_map <- read_lex(lex_path)

dat_text <- dat %>%
  mutate(
    research_topic_clean = clean_topic_for_normalization(research_topic),
    research_topic_clean = collapse_phrases(research_topic_clean, phrases_tbl),
    research_topic_clean = na_if(research_topic_clean, "")
  )

tidy_lemmas <- dat_text %>%
  filter(!is.na(research_topic_clean)) %>%
  dplyr::select(
    request_id,
    research_topic_clean,
    requestor_group,
    campus_affiliation,
    time_spent
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

top_lemmas <- tidy_lemmas %>%
  count(lemma, sort = TRUE) %>%
  slice_head(n = 100)

top_bigrams_clean <- dat_text %>%
  filter(!is.na(research_topic_clean)) %>%
  dplyr::select(request_id, research_topic_clean) %>%
  tidytext::unnest_tokens(bigram, research_topic_clean, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram)) %>%
  separate(bigram, into = c("word1", "word2"), sep = " ", remove = FALSE) %>%
  filter(
    !is.na(word1),
    !is.na(word2),
    !word1 %in% tidytext::stop_words$word,
    !word2 %in% tidytext::stop_words$word
  ) %>%
  count(bigram, sort = TRUE) %>%
  slice_head(n = 100)

requestor_tfidf <- tidy_lemmas %>%
  filter(!is.na(requestor_group)) %>%
  count(requestor_group, lemma, sort = TRUE) %>%
  tidytext::bind_tf_idf(lemma, requestor_group, n) %>%
  arrange(desc(tf_idf))

top_requestor_lemmas <- requestor_tfidf %>%
  group_by(requestor_group) %>%
  slice_max(tf_idf, n = 15, with_ties = FALSE) %>%
  ungroup()

if (nrow(tidy_purposes) > 0) {
  lemmas_with_purpose <- tidy_lemmas %>%
    inner_join(
      dplyr::select(tidy_purposes, request_id, purpose_category),
      by = "request_id",
      relationship = "many-to-many"
    )

  purpose_tfidf <- lemmas_with_purpose %>%
    count(purpose_category, lemma, sort = TRUE) %>%
    tidytext::bind_tf_idf(lemma, purpose_category, n) %>%
    arrange(desc(tf_idf))

  top_purpose_lemmas <- purpose_tfidf %>%
    group_by(purpose_category) %>%
    slice_max(tf_idf, n = 15, with_ties = FALSE) %>%
    ungroup()
} else {
  lemmas_with_purpose <- tibble()
  purpose_tfidf <- tibble()
  top_purpose_lemmas <- tibble()
}

research_topics <- dat %>%
  filter(!is.na(research_topic), research_topic != "") %>%
  dplyr::select(request_id, research_topic) %>%
  distinct() %>%
  arrange(research_topic)

# Ordinal regression model -------------------------------------------------

model_data_ordinal <- dat %>%
  mutate(
    time_category = case_when(
      time_spent == "1-2 hours" ~ "Low time",
      time_spent == "2-5 hours" ~ "Medium time",
      time_spent == "More than 5 hours" ~ "High time",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(time_category), !is.na(requestor_group), !is.na(weekday)) %>%
  mutate(
    time_category = factor(time_category, levels = time_level_levels, ordered = TRUE),
    requestor_group = factor(requestor_group)
  ) %>%
  dplyr::select(request_id, time_category, requestor_group, weekday)

polr_results_df <- tibble()

if (
  nrow(model_data_ordinal) > 50 &&
    dplyr::n_distinct(model_data_ordinal$time_category) > 1
) {
  polr_time <- MASS::polr(
    time_category ~ requestor_group + weekday,
    data = model_data_ordinal,
    Hess = TRUE
  )

  polr_summary <- summary(polr_time)

  p_values <- pnorm(
    abs(polr_summary$coefficients[, "t value"]),
    lower.tail = FALSE
  ) * 2

  polr_results <- cbind(
    polr_summary$coefficients,
    "p value" = p_values,
    "odds_ratio" = exp(polr_summary$coefficients[, "Value"])
  )

  polr_results_df <- as.data.frame(polr_results) %>%
    tibble::rownames_to_column("term")
}

# Tables to export ---------------------------------------------------------

tables_to_export <- list(
  "Requests by Month" = requests_per_month,
  "Requests by Weekday" = requests_by_weekday,
  "Requests by Season" = requests_by_season,
  "Requests by Hour" = requests_by_hour,
  "Requests by Requestor" = requests_by_requestor,
  "Requests by Received" = requests_by_received,
  "Requests by Campus" = requests_by_campus,
  "Requests by Purpose" = requests_by_purpose,
  "Requestor by Purpose" = requestor_by_purpose,
  "Campus by Purpose" = campus_by_purpose,
  "Other Purpose Details" = other_purpose_details,
  "Other Purpose List" = other_purpose_list,
  "Research Topics" = research_topics,
  "Searches Summary" = searches_summary,
  "Searches by Requestor" = searches_by_requestor,
  "Searches by Campus" = searches_by_campus,
  "Searches by Time Spent" = searches_by_time_spent,
  "Time Spent Counts" = time_spent_counts,
  "Time Spent by Requestor" = time_spent_by_requestor,
  "Time Spent by Purpose" = time_spent_by_purpose,
  "Time Pct by Requestor" = time_spent_pct_by_requestor,
  "Time Pct by Purpose" = time_spent_pct_by_purpose,
  "Top 100 Lemmas" = top_lemmas,
  "Top Clean Bigrams" = top_bigrams_clean,
  "Top Requestor Lemmas" = top_requestor_lemmas,
  "Top Purpose Lemmas" = top_purpose_lemmas,
  "Ordinal Model Results" = polr_results_df
)

tables_to_export_clean <- tables_to_export[
  purrr::map_lgl(tables_to_export, ~ is.data.frame(.x) && ncol(.x) > 0)
]

# Figures ------------------------------------------------------------------

p1 <- ggplot(requests_per_month, aes(x = year_month, y = n_requests)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "Literature Search Requests by Month",
    x = "Month",
    y = "Number of Requests"
  ) +
  theme_minimal()

p2 <- ggplot(
  requests_by_requestor,
  aes(x = reorder(requestor_group, n_requests), y = n_requests)
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Requests by Requestor Category",
    x = "Requestor Category",
    y = "Number of Requests"
  ) +
  theme_minimal()

ggsave(file.path(paths$figures_dir, "requests_per_month.png"), p1, width = 8, height = 5, dpi = 300)
ggsave(file.path(paths$figures_dir, "requests_by_requestor.png"), p2, width = 8, height = 6, dpi = 300)

if (nrow(requests_by_purpose) > 0) {
  p3 <- ggplot(
    requests_by_purpose,
    aes(x = reorder(purpose_category, n_selections), y = n_selections)
  ) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Requests by Purpose",
      x = "Purpose Category",
      y = "Number of Selections"
    ) +
    theme_minimal()

  ggsave(file.path(paths$figures_dir, "requests_by_purpose.png"), p3, width = 8, height = 6, dpi = 300)
}

if (nrow(requestor_by_purpose) > 0) {
  p_purpose_stacked <- requestor_by_purpose %>%
    ggplot(aes(x = requestor_group, y = prop_within_requestor, fill = purpose_category)) +
    geom_col(position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_viridis_d(option = "D") +
    coord_flip() +
    labs(
      title = "Proportional Purpose of Literature Searches by Requestor",
      x = "Requestor Category",
      y = "Proportion of Requests",
      fill = "Purpose of Request"
    ) +
    theme_light() +
    theme(legend.position = "bottom")

  ggsave(
    file.path(paths$figures_dir, "requestor_by_purpose_stacked.png"),
    p_purpose_stacked,
    width = 10,
    height = 8,
    dpi = 300
  )
}

if (nrow(top_requestor_lemmas) > 0) {
  p_top_lemmas_faceted <- top_requestor_lemmas %>%
    mutate(lemma = tidytext::reorder_within(lemma, tf_idf, requestor_group)) %>%
    ggplot(aes(x = lemma, y = tf_idf, fill = requestor_group)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ requestor_group, scales = "free_y", ncol = 3) +
    tidytext::scale_x_reordered() +
    coord_flip() +
    labs(
      title = "Most Distinctive Terms by Requestor Category (TF-IDF)",
      x = "Lemma",
      y = "TF-IDF Score"
    ) +
    theme_light() +
    theme(panel.spacing = unit(1.5, "lines"))

  ggsave(
    file.path(paths$figures_dir, "top_lemmas_by_requestor_faceted.png"),
    p_top_lemmas_faceted,
    width = 14,
    height = 12,
    dpi = 300
  )
}

requests_by_day_hour <- dat %>%
  count(weekday, hour, name = "n_requests")

if (nrow(requests_by_day_hour) > 0) {
  p_heatmap <- requests_by_day_hour %>%
    ggplot(aes(x = hour, y = weekday, fill = n_requests)) +
    geom_tile(color = "white") +
    scale_fill_viridis_c(option = "A", name = "Number of\nRequests") +
    scale_y_discrete(limits = rev) +
    scale_x_continuous(breaks = seq(0, 23, by = 2)) +
    labs(
      title = "Heatmap of Literature Search Requests by Hour and Day",
      x = "Hour of Day (24-hour format)",
      y = "Day of Week"
    ) +
    theme_minimal() +
    theme(panel.grid = element_blank())

  ggsave(
    file.path(paths$figures_dir, "requests_heatmap_by_hour.png"),
    p_heatmap,
    width = 10,
    height = 6,
    dpi = 300
  )
}

# Formatted table ----------------------------------------------------------

gt_requests_by_requestor <- requests_by_requestor %>%
  gt::gt() %>%
  gt::tab_header(title = "Literature Search Requests by Requestor") %>%
  gt::fmt_percent(columns = prop, decimals = 1)

gt::gtsave(
  gt_requests_by_requestor,
  file.path(paths$formatted_tables_dir, "requests_by_requestor.html")
)

# Save all outputs ---------------------------------------------------------

if (export_csvs) {
  clear_output_folder(paths$csv_dir, "\\.(csv|rds)$")
  
  purrr::iwalk(
    tables_to_export_clean,
    ~ write_pretty_csv(
      df = .x,
      filename = janitor::make_clean_names(.y),
      csv_dir = paths$csv_dir
    )
  )
}

if (export_workbook) {
  write_pretty_workbook(
    tables = tables_to_export_clean,
    path = summary_filepath
  )
}