# I used AI to help generate this code
# Hospital library literature search request analysis ----------------------

# 0) Load libraries --------------------------------------------------------

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

# 0A) Source shared helper files ------------------------------------------

source(here("scripts", "shared", "paths.R"))
source(here("scripts", "shared", "output_helpers.R"))
source(here("scripts", "shared", "text_helpers.R"))

# 0B) Analysis settings ----------------------------------------------------

analysis_year <- 2026
weekday_levels <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
time_level_levels <- c("Low time", "Medium time", "High time")

# 1) File paths ------------------------------------------------------------

file_path <- hmh_path

paths <- make_output_paths(file.path("hmh", "literature_searches"))

output_dir <- paths$output_dir
csv_dir <- paths$csv_dir
old_csv_dir <- paths$old_csv_dir

figures_dir <- paths$figures_dir
old_figures_dir <- paths$old_figures_dir

formatted_tables_dir <- paths$formatted_tables_dir
old_formatted_tables_dir <- paths$old_formatted_tables_dir

old_summaries_dir <- paths$old_summaries_dir

# 2) Timestamp and archiving -----------------------------------------------

run_timestamp <- format(Sys.time(), "%Y_%m_%d-%H-%M-%S")

archive_existing_files(
  source_dir = csv_dir,
  pattern = "\\.csv$",
  archive_parent_dir = old_csv_dir,
  timestamp = run_timestamp
)

archive_existing_files(
  source_dir = formatted_tables_dir,
  pattern = "\\.html$",
  archive_parent_dir = old_formatted_tables_dir,
  timestamp = run_timestamp
)

archive_existing_files(
  source_dir = figures_dir,
  pattern = "\\.(png|jpg|jpeg|pdf)$",
  archive_parent_dir = old_figures_dir,
  timestamp = run_timestamp
)

archive_existing_files(
  source_dir = output_dir,
  pattern = "^summary_report_.*\\.xlsx$",
  archive_parent_dir = old_summaries_dir,
  timestamp = run_timestamp
)

summary_filename <- paste0("summary_report_", run_timestamp, ".xlsx")
summary_filepath <- file.path(output_dir, summary_filename)

# 3) Import data -----------------------------------------------------------

raw <- readr::read_csv(file_path, show_col_types = FALSE) %>%
  janitor::clean_names()

# 4) Set column names ------------------------------------------------------

request_type_col <- "select_question_request_type"
timestamp_col    <- "timestamp"
requestor_col    <- "who_requested_this_information"
received_col     <- "how_was_the_question_request_received"
num_searches_col <- "number_of_literature_searches"
campus_col       <- "campus_affiliation"
topic_col        <- "research_topic"
time_spent_col   <- "time_spent_on_searches"
purpose_col      <- "purpose_of_request"

# 5) Helper functions ------------------------------------------------------

safe_max_numeric <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  max(x, na.rm = TRUE)
}

# This keeps the original requestor categories,
# except it combines Nurse Practitioner into Nurse Practitioner/PA.
collapse_requestor_group <- function(x) {
  x_original <- str_trim(as.character(x))
  x_clean <- str_to_lower(x_original)
  
  case_when(
    is.na(x_original) | x_original == "" ~ NA_character_,
    
    str_detect(
      x_clean,
      "nurse practitioner|\\bnp\\b|\\bapn\\b|physician assistant|\\bpa\\b"
    ) ~ "Nurse Practitioner/PA",
    
    TRUE ~ x_original
  )
}

parse_timestamp <- function(x) {
  coalesce(
    suppressWarnings(mdy_hms(x)),
    suppressWarnings(mdy_hm(x)),
    suppressWarnings(ymd_hms(x)),
    suppressWarnings(ymd_hm(x))
  )
}

# 6) Filter to literature search requests from analysis year ---------------

dat <- raw %>%
  mutate(
    submitted_at_raw = parse_timestamp(.data[[timestamp_col]])
  ) %>%
  filter(
    .data[[request_type_col]] == "Literature Search",
    year(submitted_at_raw) == analysis_year
  ) %>%
  mutate(request_id = row_number())

# 7) Clean variables -------------------------------------------------------

dat <- dat %>%
  mutate(
    submitted_at = submitted_at_raw,
    submitted_date = as.Date(submitted_at),
    year = year(submitted_at),
    month = month(submitted_at, label = TRUE, abbr = TRUE),
    month_num = month(submitted_at),
    year_month = floor_date(submitted_at, "month"),
    week = floor_date(submitted_at, unit = "week", week_start = 1),
    weekday = wday(submitted_at, label = TRUE, abbr = FALSE) %>%
      factor(levels = weekday_levels),
    hour = hour(submitted_at),
    season = case_when(
      month_num %in% c(12, 1, 2) ~ "Winter",
      month_num %in% c(3, 4, 5) ~ "Spring",
      month_num %in% c(6, 7, 8) ~ "Summer",
      month_num %in% c(9, 10, 11) ~ "Fall",
      TRUE ~ NA_character_
    ),
    requestor_category = na_if(str_trim(as.character(.data[[requestor_col]])), ""),
    requestor_group = collapse_requestor_group(requestor_category),
    request_received = na_if(str_trim(as.character(.data[[received_col]])), ""),
    campus_affiliation = na_if(str_trim(as.character(.data[[campus_col]])), ""),
    research_topic = na_if(str_trim(as.character(.data[[topic_col]])), ""),
    time_spent = na_if(str_trim(as.character(.data[[time_spent_col]])), ""),
    purpose = na_if(str_trim(as.character(.data[[purpose_col]])), ""),
    n_searches = suppressWarnings(as.numeric(.data[[num_searches_col]]))
  ) %>%
  filter(!is.na(weekday))

# 8) Demand / utilization --------------------------------------------------

requests_per_month <- dat %>%
  count(year_month, name = "n_requests")

requests_by_weekday <- dat %>%
  count(weekday, name = "n_requests")

requests_by_season <- dat %>%
  count(season, name = "n_requests")

requests_by_hour <- dat %>%
  count(hour, name = "n_requests")

# 9) Requestor / service patterns -----------------------------------------

requests_by_requestor <- dat %>%
  count(requestor_group, sort = TRUE, name = "n_requests") %>%
  mutate(prop = n_requests / sum(n_requests))

requests_by_received <- dat %>%
  count(request_received, sort = TRUE, name = "n_requests") %>%
  mutate(prop = n_requests / sum(n_requests))

requests_by_campus <- dat %>%
  count(campus_affiliation, sort = TRUE, name = "n_requests") %>%
  mutate(prop = n_requests / sum(n_requests))

if (sum(!is.na(dat$purpose)) > 0) {
  
  tidy_purposes <- dat %>%
    filter(!is.na(purpose), purpose != "") %>%
    separate_rows(purpose, sep = ",") %>%
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

# 10) Workload / time ------------------------------------------------------

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

# 11) Normalized text analysis --------------------------------------------

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
  unnest_tokens(word, research_topic_clean) %>%
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
  anti_join(stop_words, by = c("lemma" = "word")) %>%
  filter(!str_detect(lemma, "^\\d+$"))

top_lemmas <- tidy_lemmas %>%
  count(lemma, sort = TRUE) %>%
  slice_head(n = 100)

top_bigrams_clean <- dat_text %>%
  filter(!is.na(research_topic_clean)) %>%
  dplyr::select(request_id, research_topic_clean) %>%
  unnest_tokens(bigram, research_topic_clean, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram)) %>%
  separate(bigram, into = c("word1", "word2"), sep = " ", remove = FALSE) %>%
  filter(
    !is.na(word1), !is.na(word2),
    !word1 %in% stop_words$word,
    !word2 %in% stop_words$word
  ) %>%
  count(bigram, sort = TRUE) %>%
  slice_head(n = 100)

requestor_tfidf <- tidy_lemmas %>%
  filter(!is.na(requestor_group)) %>%
  count(requestor_group, lemma, sort = TRUE) %>%
  bind_tf_idf(lemma, requestor_group, n) %>%
  arrange(desc(tf_idf))

top_requestor_lemmas <- requestor_tfidf %>%
  group_by(requestor_group) %>%
  slice_max(tf_idf, n = 15, with_ties = FALSE) %>%
  ungroup()

if (nrow(tidy_purposes) > 0) {
  lemmas_with_purpose <- tidy_lemmas %>%
    inner_join(
      dplyr::select(tidy_purposes, request_id, purpose_category),
      by = "request_id"
    )
  
  purpose_tfidf <- lemmas_with_purpose %>%
    count(purpose_category, lemma, sort = TRUE) %>%
    bind_tf_idf(lemma, purpose_category, n) %>%
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

# 12) Optional manual topic coding ----------------------------------------

coding_file <- here("data", "reference", "hmh", "lemma_coding.csv")

if (file.exists(coding_file)) {
  coding <- readr::read_csv(coding_file, show_col_types = FALSE) %>%
    janitor::clean_names() %>%
    mutate(
      lemma = str_to_lower(str_trim(lemma)),
      topic_category = str_trim(topic_category)
    ) %>%
    distinct(lemma, .keep_all = TRUE)
  
  tidy_coded <- tidy_lemmas %>%
    left_join(coding, by = "lemma")
  
  topic_counts <- tidy_coded %>%
    filter(!is.na(topic_category)) %>%
    distinct(request_id, topic_category) %>%
    count(topic_category, sort = TRUE)
  
  topic_by_requestor <- tidy_coded %>%
    filter(!is.na(topic_category)) %>%
    distinct(request_id, requestor_group, topic_category) %>%
    count(requestor_group, topic_category, sort = TRUE)
  
  if (nrow(tidy_purposes) > 0) {
    topic_by_purpose <- tidy_coded %>%
      filter(!is.na(topic_category)) %>%
      inner_join(
        dplyr::select(tidy_purposes, request_id, purpose_category),
        by = "request_id"
      ) %>%
      distinct(request_id, purpose_category, topic_category) %>%
      count(purpose_category, topic_category, sort = TRUE)
  } else {
    topic_by_purpose <- tibble()
  }
  
  topic_time_spent <- tidy_coded %>%
    filter(!is.na(topic_category)) %>%
    distinct(request_id, topic_category, time_spent) %>%
    count(topic_category, time_spent, sort = TRUE)
  
} else {
  coding <- tibble()
  tidy_coded <- tibble()
  topic_counts <- tibble()
  topic_by_requestor <- tibble()
  topic_by_purpose <- tibble()
  topic_time_spent <- tibble()
}

# 13) Model-ready time categories for text linkage ------------------------

time_category_lookup <- dat %>%
  mutate(
    time_category = case_when(
      time_spent == "1-2 hours" ~ "Low time",
      time_spent == "2-5 hours" ~ "Medium time",
      time_spent == "More than 5 hours" ~ "High time",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(time_category)) %>%
  mutate(
    time_category = factor(
      time_category,
      levels = time_level_levels,
      ordered = TRUE
    )
  ) %>%
  dplyr::select(request_id, time_category)

lemmas_by_time <- tidy_lemmas %>%
  inner_join(time_category_lookup, by = "request_id") %>%
  count(time_category, lemma, sort = TRUE)

lemmas_time_tfidf <- lemmas_by_time %>%
  bind_tf_idf(lemma, time_category, n) %>%
  arrange(desc(tf_idf))

top_time_lemmas <- lemmas_time_tfidf %>%
  group_by(time_category) %>%
  slice_max(tf_idf, n = 15, with_ties = FALSE) %>%
  ungroup()

top_high_time_terms <- lemmas_by_time %>%
  filter(time_category == "High time") %>%
  slice_max(n, n = 30, with_ties = FALSE)

# 14) Tables to export -----------------------------------------------------

research_topics <- dat %>%
  filter(!is.na(research_topic), research_topic != "") %>%
  dplyr::select(request_id, research_topic) %>%
  distinct() %>%
  arrange(research_topic)

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
  "Top Time Lemmas" = top_time_lemmas,
  "Top High Time Terms" = top_high_time_terms
)

tables_to_export_clean <- tables_to_export[
  purrr::map_lgl(tables_to_export, ~ is.data.frame(.x) && ncol(.x) > 0)
]

# 15) Export CSVs ----------------------------------------------------------

purrr::iwalk(
  tables_to_export_clean,
  ~ write_pretty_csv(
    df = .x,
    filename = janitor::make_clean_names(.y),
    csv_dir = csv_dir
  )
)

# 16) Formatted HTML table -------------------------------------------------

gt_requests_by_requestor <- requests_by_requestor %>%
  gt() %>%
  tab_header(title = "Literature Search Requests by Requestor") %>%
  fmt_percent(columns = prop, decimals = 1)

gtsave(
  gt_requests_by_requestor,
  file.path(formatted_tables_dir, "requests_by_requestor.html")
)

# 17) Figures --------------------------------------------------------------

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

ggsave(file.path(figures_dir, "requests_per_month.png"), p1, width = 8, height = 5, dpi = 300)

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

ggsave(file.path(figures_dir, "requests_by_requestor.png"), p2, width = 8, height = 6, dpi = 300)

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
  
  ggsave(file.path(figures_dir, "requests_by_purpose.png"), p3, width = 8, height = 6, dpi = 300)
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
    file.path(figures_dir, "requestor_by_purpose_stacked.png"),
    p_purpose_stacked,
    width = 10,
    height = 8,
    dpi = 300
  )
}

if (nrow(top_requestor_lemmas) > 0) {
  p_top_lemmas_faceted <- top_requestor_lemmas %>%
    mutate(lemma = reorder_within(lemma, tf_idf, requestor_group)) %>%
    ggplot(aes(x = lemma, y = tf_idf, fill = requestor_group)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ requestor_group, scales = "free_y", ncol = 3) +
    scale_x_reordered() +
    coord_flip() +
    labs(
      title = "Most Distinctive Terms by Requestor Category (TF-IDF)",
      x = "Lemma",
      y = "TF-IDF Score"
    ) +
    theme_light() +
    theme(panel.spacing = unit(1.5, "lines"))
  
  ggsave(
    file.path(figures_dir, "top_lemmas_by_requestor_faceted.png"),
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
    file.path(figures_dir, "requests_heatmap_by_hour.png"),
    p_heatmap,
    width = 10,
    height = 6,
    dpi = 300
  )
}

# 18) Ordinal logistic regression -----------------------------------------

dat_with_week <- dat %>%
  group_by(week) %>%
  mutate(weekly_requests = n()) %>%
  ungroup()

model_data_ordinal <- dat_with_week %>%
  filter(
    !is.na(time_spent),
    !is.na(requestor_group),
    !is.na(weekday)
  ) %>%
  mutate(
    time_category = case_when(
      time_spent == "1-2 hours" ~ "Low time",
      time_spent == "2-5 hours" ~ "Medium time",
      time_spent == "More than 5 hours" ~ "High time",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(time_category)) %>%
  mutate(
    time_category = factor(
      time_category,
      levels = time_level_levels,
      ordered = TRUE
    ),
    requestor_group = factor(requestor_group)
  ) %>%
  dplyr::select(
    request_id,
    time_category,
    requestor_group,
    weekday,
    weekly_requests
  )

cat("Distribution of Request Time Categories:\n")
print(count(model_data_ordinal, time_category))

polr_results_df <- tibble()

if (
  nrow(model_data_ordinal) > 0 &&
  dplyr::n_distinct(model_data_ordinal$time_category) == 3
) {
  
  polr_time <- MASS::polr(
    time_category ~ requestor_group + weekday + weekly_requests,
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
  
  write_pretty_csv(
    polr_results_df,
    "polr_time_results",
    csv_dir
  )
  
  tables_to_export_clean[["Ordinal Model Results"]] <- polr_results_df
  
  cat("\n\n--- Ordinal Model Summary: Predicting Time Category ---\n")
  print(polr_summary)
  
  cat("\n\n--- Coefficients with P-Values and Odds Ratios ---\n")
  print(polr_results)
  
} else {
  cat("\n\n--- Not enough complete data to build an ordinal regression model. ---\n")
}

# 19) Write Excel workbook last -------------------------------------------

tables_to_export_clean <- tables_to_export_clean[
  purrr::map_lgl(tables_to_export_clean, ~ is.data.frame(.x) && ncol(.x) > 0)
]

write_pretty_workbook(
  tables = tables_to_export_clean,
  path = summary_filepath
)

cat("\nNewest summary workbook written to:\n", summary_filepath, "\n")
cat("CSV files written to:\n", csv_dir, "\n")
cat("Figures written to:\n", figures_dir, "\n")