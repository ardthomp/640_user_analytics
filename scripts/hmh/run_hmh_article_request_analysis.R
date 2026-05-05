# scripts/hmh/run_hmh_article_request_analysis.R
#
# HMH article/chapter request analysis.
#
# This script overwrites the latest outputs instead of archiving every run.
#
# Outputs:
#   outputs/hmh/article_requests/csv/
#   outputs/hmh/article_requests/figures/
#   outputs/hmh/article_requests/formatted_tables/
#   outputs/hmh/article_requests/hmh_article_request_summary.xlsx

# Setup --------------------------------------------------------------------

library(tidyverse)
library(janitor)
library(here)
library(lubridate)
library(scales)
library(forcats)
library(gt)

source(here("scripts", "shared", "paths.R"))
source(here("scripts", "shared", "helpers.R"))
source(here("scripts", "shared", "output_helpers.R"))
source(here("scripts", "shared", "plotting_helpers.R"))

# Settings and paths -------------------------------------------------------

analysis_year <- 2026

raw_path <- hmh_path
paths <- make_output_paths(file.path("hmh", "article_requests"))

# Clear latest outputs instead of archiving old outputs.
clear_output_folder(paths$csv_dir, "\\.(csv|rds)$")
clear_output_folder(paths$figures_dir, "\\.(png|jpg|jpeg|pdf)$")
clear_output_folder(paths$formatted_tables_dir, "\\.html$")
clear_output_folder(paths$output_dir, "^hmh_article_request_summary\\.xlsx$")

summary_filepath <- file.path(paths$output_dir, "hmh_article_request_summary.xlsx")

# Local helpers ------------------------------------------------------------

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
      suppressWarnings(lubridate::ymd_hm(x))
    )
  }
}

collapse_requestor_role <- function(x) {
  x_clean <- str_to_lower(str_squish(as.character(x)))

  case_when(
    str_detect(x_clean, "nurse practitioner|\\bnp\\b|\\bpa\\b|physician assistant") ~ "Nurse Practitioner/ PA",
    TRUE ~ clean_blank(x)
  )
}

classify_article_source <- function(x) {
  x <- str_to_lower(as.character(x))
  x <- str_squish(x)

  case_when(
    is.na(x) | x == "" ~ NA_character_,
    str_detect(x, "pubmed central|pmc") ~ "PubMed Central",
    str_detect(x, "google scholar|scholar") ~ "Google Scholar",
    str_detect(x, "publisher|publisher's|publisher website|journal website|website") ~ "Publisher Website",
    str_detect(x, "pubmed") ~ "PubMed",
    str_detect(x, "open access|\\boa\\b") ~ "Other",
    TRUE ~ "Other"
  )
}

clean_num_na <- function(x) {
  x <- clean_blank(as.character(x))
  readr::parse_number(x)
}

# Load and process data ----------------------------------------------------

hmh_raw <- readr::read_csv(
  raw_path,
  show_col_types = FALSE,
  na = c("", "NA", "N/A", "NULL", "null")
) %>%
  janitor::clean_names()

hmh_data <- hmh_raw %>%
  mutate(
    request_id = row_number(),
    timestamp = parse_timestamp(timestamp),
    request_year = lubridate::year(timestamp),
    request_month = as.Date(lubridate::floor_date(timestamp, unit = "month")),
    requestor_role = collapse_requestor_role(who_requested_this_information),
    request_received = clean_blank(how_was_the_question_request_received),
    campus_affiliation = clean_blank(campus_affiliation),
    select_question_request_type = clean_blank(select_question_request_type),
    article_chapter_other_source_text =
      clean_blank(number_of_articles_chapters_retrieved_from_other_include_source),
    article_chapter_other_source_category =
      classify_article_source(article_chapter_other_source_text),
    n_article_chapter_other =
      clean_num_na(number_of_articles_chapters_retrieved_from_other_include_source),
    n_article_chapter_subscribed =
      clean_num_na(number_of_articles_chapters_retrieved_from_subscribed_content),
    n_article_chapter_docline =
      clean_num_na(number_of_articles_chapters_retrieved_from_docline),
    total_articles_chapters_retrieved = rowSums(
      cbind(
        coalesce(n_article_chapter_other, 0),
        coalesce(n_article_chapter_subscribed, 0),
        coalesce(n_article_chapter_docline, 0)
      )
    ),
    is_article_chapter_request = if_else(
      select_question_request_type == "Article/Chapter Request",
      TRUE,
      FALSE,
      missing = FALSE
    )
  )

article_chapter_data <- hmh_data %>%
  filter(
    !is.na(timestamp),
    request_year == analysis_year,
    is_article_chapter_request
  )

# Analysis tables ----------------------------------------------------------

article_chapter_source_long <- bind_rows(
  article_chapter_data %>%
    transmute(
      request_id,
      timestamp,
      requestor_role,
      request_received,
      campus_affiliation,
      source_type = article_chapter_other_source_category,
      n_items = n_article_chapter_other
    ) %>%
    filter(!is.na(source_type)),

  article_chapter_data %>%
    transmute(
      request_id,
      timestamp,
      requestor_role,
      request_received,
      campus_affiliation,
      source_type = "Subscribed Content",
      n_items = n_article_chapter_subscribed
    ),

  article_chapter_data %>%
    transmute(
      request_id,
      timestamp,
      requestor_role,
      request_received,
      campus_affiliation,
      source_type = "Docline",
      n_items = n_article_chapter_docline
    )
) %>%
  mutate(
    n_items = coalesce(n_items, 0),
    source_type = if_else(source_type == "Open Access", "Other", source_type)
  )

article_chapter_n <- tibble(
  analysis_year = analysis_year,
  n_article_chapter_requests = nrow(article_chapter_data)
)

article_chapter_by_requestor <- article_chapter_data %>%
  count(requestor_role, sort = TRUE, name = "n_requests") %>%
  mutate(prop = n_requests / sum(n_requests))

article_chapter_by_received <- article_chapter_data %>%
  count(request_received, sort = TRUE, name = "n_requests") %>%
  mutate(prop = n_requests / sum(n_requests))

article_chapter_by_campus <- article_chapter_data %>%
  count(campus_affiliation, sort = TRUE, name = "n_requests") %>%
  mutate(prop = n_requests / sum(n_requests))

article_chapter_by_month <- article_chapter_data %>%
  count(request_month, sort = FALSE, name = "n_requests")

article_chapter_by_source <- article_chapter_source_long %>%
  group_by(source_type) %>%
  summarise(
    total_items = sum(n_items, na.rm = TRUE),
    mean_items = mean(n_items, na.rm = TRUE),
    median_items = median(n_items, na.rm = TRUE),
    requests_with_items = sum(n_items > 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_items))

other_source_summary <- article_chapter_data %>%
  filter(!is.na(article_chapter_other_source_category)) %>%
  count(article_chapter_other_source_category, sort = TRUE, name = "n_requests") %>%
  mutate(prop = n_requests / sum(n_requests))

other_source_detail <- article_chapter_data %>%
  filter(!is.na(article_chapter_other_source_text)) %>%
  transmute(
    request_id,
    timestamp,
    requestor_role,
    request_received,
    campus_affiliation,
    article_chapter_other_source_text,
    article_chapter_other_source_category,
    n_article_chapter_other
  ) %>%
  arrange(article_chapter_other_source_category, timestamp)

request_level_export <- article_chapter_data %>%
  transmute(
    request_id,
    timestamp,
    requestor_role,
    request_received,
    campus_affiliation,
    n_article_chapter_other,
    article_chapter_other_source_text,
    article_chapter_other_source_category,
    n_article_chapter_subscribed,
    n_article_chapter_docline,
    total_articles_chapters_retrieved
  ) %>%
  arrange(timestamp)

article_chapter_source_by_requestor <- article_chapter_source_long %>%
  filter(!is.na(requestor_role)) %>%
  group_by(requestor_role, source_type) %>%
  summarise(
    total_items = sum(n_items, na.rm = TRUE),
    requests_with_items = sum(n_items > 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(requestor_role) %>%
  mutate(
    requestor_total_items = sum(total_items, na.rm = TRUE),
    prop_items_within_requestor = total_items / requestor_total_items
  ) %>%
  ungroup()

article_chapter_source_by_campus <- article_chapter_source_long %>%
  filter(!is.na(campus_affiliation)) %>%
  group_by(campus_affiliation, source_type) %>%
  summarise(
    total_items = sum(n_items, na.rm = TRUE),
    requests_with_items = sum(n_items > 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(campus_affiliation) %>%
  mutate(
    campus_total_items = sum(total_items, na.rm = TRUE),
    prop_items_within_campus = total_items / campus_total_items
  ) %>%
  ungroup()

tables_to_export <- list(
  "Request Count" = article_chapter_n,
  "By Requestor" = article_chapter_by_requestor,
  "By Received" = article_chapter_by_received,
  "By Campus" = article_chapter_by_campus,
  "By Month" = article_chapter_by_month,
  "By Source" = article_chapter_by_source,
  "Source by Requestor Expanded" = article_chapter_source_by_requestor,
  "Source by Campus Expanded" = article_chapter_source_by_campus,
  "Other Source Summary" = other_source_summary,
  "Other Source Detail" = other_source_detail,
  "Request Level Export" = request_level_export
)

tables_to_export_clean <- tables_to_export[
  purrr::map_lgl(tables_to_export, ~ is.data.frame(.x) && ncol(.x) > 0)
]

# Figures ------------------------------------------------------------------

plot_requestor <- article_chapter_by_requestor %>%
  filter(!is.na(requestor_role)) %>%
  mutate(requestor_role = forcats::fct_reorder(requestor_role, n_requests))

plot_received <- article_chapter_by_received %>%
  filter(!is.na(request_received)) %>%
  mutate(request_received = forcats::fct_reorder(request_received, n_requests))

plot_campus <- article_chapter_by_campus %>%
  filter(!is.na(campus_affiliation)) %>%
  mutate(campus_affiliation = forcats::fct_reorder(campus_affiliation, n_requests))

plot_source_totals <- article_chapter_by_source %>%
  mutate(source_type = forcats::fct_reorder(source_type, total_items))

plot_source_by_requestor <- article_chapter_source_by_requestor %>%
  group_by(requestor_role) %>%
  mutate(requestor_total = sum(total_items, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(requestor_role = forcats::fct_reorder(requestor_role, requestor_total))

plot_total_per_request <- article_chapter_data %>%
  transmute(total_articles_chapters_retrieved)

p_article_by_requestor <- ggplot(plot_requestor, aes(x = requestor_role, y = n_requests)) +
  geom_col() +
  coord_flip() +
  labs(
    title = paste0("Article/Chapter Requests by Requestor Role (", analysis_year, ")"),
    x = NULL,
    y = "Number of Requests"
  ) +
  theme_minimal(base_size = 13)

p_article_by_received <- ggplot(plot_received, aes(x = request_received, y = n_requests)) +
  geom_col() +
  coord_flip() +
  labs(
    title = paste0("Article/Chapter Requests by How Request Was Received (", analysis_year, ")"),
    x = NULL,
    y = "Number of Requests"
  ) +
  theme_minimal(base_size = 13)

p_article_by_campus <- ggplot(plot_campus, aes(x = campus_affiliation, y = n_requests)) +
  geom_col() +
  coord_flip() +
  labs(
    title = paste0("Article/Chapter Requests by Campus Affiliation (", analysis_year, ")"),
    x = NULL,
    y = "Number of Requests"
  ) +
  theme_minimal(base_size = 13)

p_article_by_month <- ggplot(article_chapter_by_month, aes(x = request_month, y = n_requests)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_date(
    date_labels = "%b",
    date_breaks = "1 month",
    limits = as.Date(c(paste0(analysis_year, "-01-01"), paste0(analysis_year, "-12-31")))
  ) +
  labs(
    title = paste0("Article/Chapter Requests Over Time (", analysis_year, ")"),
    x = NULL,
    y = "Number of Requests"
  ) +
  theme_minimal(base_size = 13)

p_article_source_totals <- ggplot(plot_source_totals, aes(x = source_type, y = total_items)) +
  geom_col() +
  coord_flip() +
  labs(
    title = paste0("Articles/Chapters Retrieved by Source (", analysis_year, ")"),
    x = NULL,
    y = "Total Items Retrieved"
  ) +
  theme_minimal(base_size = 13)

p_article_source_by_requestor <- ggplot(
  plot_source_by_requestor,
  aes(x = requestor_role, y = total_items, fill = source_type)
) +
  geom_col(position = "fill", color = "white", linewidth = 0.2) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = paste0("Retrieval Source Mix by Requestor Role (", analysis_year, ")"),
    x = NULL,
    y = "Proportion of Retrieved Items",
    fill = "Source"
  ) +
  theme_minimal(base_size = 13)

p_article_total_per_request <- ggplot(
  plot_total_per_request,
  aes(x = total_articles_chapters_retrieved)
) +
  geom_histogram(binwidth = 1, boundary = 0) +
  labs(
    title = paste0("Articles/Chapters Retrieved per Request (", analysis_year, ")"),
    x = "Total Items Retrieved",
    y = "Number of Requests"
  ) +
  theme_minimal(base_size = 13)

ggsave(file.path(paths$figures_dir, "article_requests_by_requestor.png"), p_article_by_requestor, width = 8, height = 5, dpi = 300)
ggsave(file.path(paths$figures_dir, "article_requests_by_received.png"), p_article_by_received, width = 8, height = 5, dpi = 300)
ggsave(file.path(paths$figures_dir, "article_requests_by_campus.png"), p_article_by_campus, width = 8, height = 5, dpi = 300)
ggsave(file.path(paths$figures_dir, "article_requests_by_month.png"), p_article_by_month, width = 9, height = 5, dpi = 300)
ggsave(file.path(paths$figures_dir, "article_request_source_totals.png"), p_article_source_totals, width = 8, height = 5, dpi = 300)
ggsave(file.path(paths$figures_dir, "article_request_source_mix_by_requestor.png"), p_article_source_by_requestor, width = 10, height = 6, dpi = 300)
ggsave(file.path(paths$figures_dir, "article_request_total_per_request_histogram.png"), p_article_total_per_request, width = 8, height = 5, dpi = 300)

# Formatted table ----------------------------------------------------------

gt_article_requestor <- article_chapter_by_requestor %>%
  gt::gt() %>%
  gt::tab_header(title = paste0("Article/Chapter Requests by Requestor Role (", analysis_year, ")")) %>%
  gt::fmt_percent(columns = prop, decimals = 1)

gt::gtsave(
  gt_article_requestor,
  file.path(paths$formatted_tables_dir, "article_chapter_by_requestor.html")
)

# Save all outputs ---------------------------------------------------------

purrr::iwalk(
  tables_to_export_clean,
  ~ write_pretty_csv(
    df = .x,
    filename = janitor::make_clean_names(.y),
    csv_dir = paths$csv_dir
  )
)

write_pretty_workbook(
  tables = tables_to_export_clean,
  path = summary_filepath
)

cat("\n--- HMH Article Request Analysis Complete ---\n")
cat("Rows analyzed:", nrow(article_chapter_data), "\n")
cat("Summary workbook:", summary_filepath, "\n")
cat("CSV files:", paths$csv_dir, "\n")
cat("Figures:", paths$figures_dir, "\n")
