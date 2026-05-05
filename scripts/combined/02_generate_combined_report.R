# scripts/combined/02_generate_combined_report.R
#
# Generate the combined HUMC + HMH report.
#
# Purpose:
#   1. Load the RDS object created by 01_build_combined_dataset.R.
#   2. Recreate the utilization, purpose, workload, citation, and lemma tables.
#   3. Export figures, selected HTML tables, one Excel workbook, and optional CSVs.
#
# Run after:
#   source("scripts/combined/01_build_combined_dataset.R")
#
# Then run:
#   source("scripts/combined/02_generate_combined_report.R")

# Setup --------------------------------------------------------------------

library(tidyverse)
library(janitor)
library(here)
library(lubridate)
library(scales)
library(forcats)
library(gt)
library(viridis)
library(tidytext)

source(here("scripts", "shared", "paths.R"))
source(here("scripts", "shared", "helpers.R"))
source(here("scripts", "shared", "output_helpers.R"))

if (file.exists(here("scripts", "shared", "plotting_helpers.R"))) {
  source(here("scripts", "shared", "plotting_helpers.R"))
}

if (!exists("theme_project")) {
  theme_project <- function() {
    theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold"),
        legend.position = "right"
      )
  }
}

# Paths --------------------------------------------------------------------

paths <- make_output_paths("combined")

output_dir <- paths$output_dir
csv_dir <- paths$csv_dir
figures_dir <- paths$figures_dir
formatted_tables_dir <- paths$formatted_tables_dir

combined_rds_path <- here("data", "processed", "combined_analysis_data.rds")

# Export settings -----------------------------------------------------------

export_csvs <- FALSE
export_workbook <- TRUE

# Check input ---------------------------------------------------------------

if (!file.exists(combined_rds_path)) {
  stop(
    "Could not find data/processed/combined_analysis_data.rds. ",
    "Run scripts/combined/01_build_combined_dataset.R first."
  )
}

# Clear outputs -------------------------------------------------------------

if (export_csvs) {
  clear_output_folder(csv_dir, "\\.(csv|rds)$")
}

clear_output_folder(figures_dir, "\\.(png|jpg|jpeg|pdf)$")
clear_output_folder(formatted_tables_dir, "\\.html$")
clear_output_folder(output_dir, "^summary_report_.*\\.xlsx$|^combined_summary_report\\.xlsx$")

# Fixed output path (no timestamps) -----------------------------------------

summary_filepath <- file.path(output_dir, "combined_summary_report.xlsx")

# Load built data -----------------------------------------------------------

analysis_data <- readRDS(combined_rds_path)

combined_dat <- analysis_data$combined_dat
tidy_purposes <- analysis_data$tidy_purposes
tidy_lemmas_all <- analysis_data$tidy_lemmas_all
phrase_lemma_candidates <- analysis_data$phrase_lemma_candidates
all_research_topics_full <- analysis_data$all_research_topics_full

# Data checks ---------------------------------------------------------------

campus_check <- combined_dat %>%
  count(
    source_file_type,
    source_label,
    campus_affiliation_raw,
    campus_affiliation_clean,
    campus_affiliation_detail,
    humc,
    carrier,
    jfk,
    palisades,
    network,
    name = "n"
  ) %>%
  arrange(source_file_type, desc(n))

requestor_check <- combined_dat %>%
  count(source_file_type, requestor_category, name = "n") %>%
  arrange(source_file_type, desc(n))

# Demand / utilization tables ---------------------------------------------

requests_over_time <- combined_dat %>%
  count(year_month, plot_group, name = "n_requests")

requests_by_source <- combined_dat %>%
  count(source_label, name = "n_requests") %>%
  mutate(prop = n_requests / sum(n_requests)) %>%
  arrange(desc(n_requests))

requests_by_campus <- combined_dat %>%
  count(campus_affiliation_clean, name = "n_requests") %>%
  mutate(prop = n_requests / sum(n_requests)) %>%
  arrange(desc(n_requests))

requests_by_requestor <- combined_dat %>%
  count(requestor_category, name = "n_requests") %>%
  mutate(prop = n_requests / sum(n_requests)) %>%
  arrange(desc(n_requests))

requests_by_month_total <- combined_dat %>%
  count(year_month, name = "n_requests")

requests_by_year <- combined_dat %>%
  count(year, name = "n_requests") %>%
  mutate(prop = n_requests / sum(n_requests))

requests_by_weekday <- combined_dat %>%
  count(weekday, name = "n_requests") %>%
  arrange(weekday)

requests_by_hour <- combined_dat %>%
  count(hour, name = "n_requests") %>%
  arrange(hour)

time_spent_counts <- combined_dat %>%
  count(time_spent, name = "n_requests") %>%
  mutate(prop = n_requests / sum(n_requests)) %>%
  arrange(desc(n_requests))

searches_by_year <- combined_dat %>%
  group_by(year) %>%
  summarize(
    n_requests = n(),
    mean_n_searches = mean(n_searches, na.rm = TRUE),
    median_n_searches = median(n_searches, na.rm = TRUE),
    max_n_searches = safe_max_numeric(n_searches),
    mean_citation_count = mean(citation_count, na.rm = TRUE),
    median_citation_count = median(citation_count, na.rm = TRUE),
    max_citation_count = safe_max_numeric(citation_count),
    .groups = "drop"
  )

searches_by_campus <- combined_dat %>%
  group_by(campus_affiliation_clean) %>%
  summarize(
    n_requests = n(),
    mean_searches = mean(n_searches, na.rm = TRUE),
    median_searches = median(n_searches, na.rm = TRUE),
    max_searches = safe_max_numeric(n_searches),
    .groups = "drop"
  ) %>%
  arrange(desc(n_requests))

# Purpose tables -----------------------------------------------------------

if (is.data.frame(tidy_purposes) && nrow(tidy_purposes) > 0) {
  requests_by_purpose <- tidy_purposes %>%
    count(purpose_category, name = "n_selections") %>%
    mutate(prop = n_selections / sum(n_selections)) %>%
    arrange(desc(n_selections))

  purpose_by_campus <- tidy_purposes %>%
    count(campus_affiliation_clean, purpose_category, name = "n_selections") %>%
    group_by(campus_affiliation_clean) %>%
    mutate(prop_within_campus = n_selections / sum(n_selections)) %>%
    ungroup() %>%
    arrange(campus_affiliation_clean, desc(n_selections))

  purpose_by_requestor <- tidy_purposes %>%
    count(requestor_category, purpose_category, name = "n_selections") %>%
    group_by(requestor_category) %>%
    mutate(prop_within_requestor = n_selections / sum(n_selections)) %>%
    ungroup() %>%
    arrange(requestor_category, desc(n_selections))

  other_purpose_details <- tidy_purposes %>%
    filter(purpose_category == "Other") %>%
    count(purpose_other_detail, name = "n") %>%
    arrange(desc(n))
} else {
  requests_by_purpose <- tibble()
  purpose_by_campus <- tibble()
  purpose_by_requestor <- tibble()
  other_purpose_details <- tibble()
}

# Lemma tables --------------------------------------------------------------

top_lemmas <- tidy_lemmas_all %>%
  count(lemma, sort = TRUE, name = "n") %>%
  mutate(prop = n / sum(n))

top_500_lemmas <- top_lemmas %>%
  slice_head(n = 500)

all_lemmas <- tidy_lemmas_all %>%
  distinct(lemma) %>%
  arrange(lemma)

lemma_counts <- tidy_lemmas_all %>%
  count(lemma, sort = TRUE, name = "n") %>%
  mutate(prop = n / sum(n))

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

if (nrow(tidy_purposes) > 0) {
  lemma_by_purpose <- tidy_lemmas_all %>%
    inner_join(
      tidy_purposes %>%
        distinct(global_request_id, purpose_category),
      by = "global_request_id",
      relationship = "many-to-many"
    ) %>%
    count(purpose_category, lemma, sort = TRUE, name = "n_mentions")

  lemma_tfidf_by_purpose <- lemma_by_purpose %>%
    rename(n = n_mentions) %>%
    bind_tf_idf(term = lemma, document = purpose_category, n = n) %>%
    arrange(desc(tf_idf))
} else {
  lemma_by_purpose <- tibble()
  lemma_tfidf_by_purpose <- tibble()
}

# Matched-month comparison table ------------------------------------------

latest_month_2026 <- combined_dat %>%
  filter(year == 2026) %>%
  summarize(max_month = max(month_num, na.rm = TRUE)) %>%
  pull(max_month)

if (is.infinite(latest_month_2026) || is.na(latest_month_2026)) {
  latest_month_2026 <- max(combined_dat$month_num, na.rm = TRUE)
}

requests_per_month_matched <- combined_dat %>%
  filter(
    year %in% c(2025, 2026),
    month_num <= latest_month_2026
  ) %>%
  count(year, month_num, month, name = "n_requests") %>%
  mutate(
    year = factor(year),
    month_label = factor(
      month.abb[month_num],
      levels = month.abb[1:latest_month_2026]
    )
  )

requests_per_month_full <- combined_dat %>%
  mutate(year_month = as.Date(year_month)) %>%
  count(year_month, name = "n_requests")

requests_by_month_source <- combined_dat %>%
  count(year_month, source_file_type, name = "n_requests")

# Tables to export ---------------------------------------------------------

tables_to_export <- list(
  "Campus Check" = campus_check,
  "Requestor Check" = requestor_check,

  "Requests Over Time" = requests_over_time,
  "Requests by Source" = requests_by_source,
  "Requests by Campus" = requests_by_campus,
  "Requests by Requestor" = requests_by_requestor,
  "Requests by Month Total" = requests_by_month_total,
  "Requests by Year" = requests_by_year,
  "Requests by Weekday" = requests_by_weekday,
  "Requests by Hour" = requests_by_hour,
  "Time Spent Counts" = time_spent_counts,

  "Searches by Year" = searches_by_year,
  "Searches by Campus" = searches_by_campus,

  "Requests by Purpose" = requests_by_purpose,
  "Purpose by Campus" = purpose_by_campus,
  "Purpose by Requestor" = purpose_by_requestor,
  "Other Purpose Details" = other_purpose_details,

  "All Research Topics Full" = all_research_topics_full,
  "Top Lemmas" = top_lemmas,
  "Top 500 Lemmas" = top_500_lemmas,
  "All Lemmas" = all_lemmas,
  "Lemma Counts Combined" = lemma_counts,
  "Lemma Counts by Source" = lemma_counts_by_source,
  "Lemma Counts by Campus" = lemma_counts_by_campus,
  "Lemma Counts by Requestor" = lemma_counts_by_requestor,
  "Lemma TF-IDF by Source" = lemma_tfidf_by_source,
  "Lemma TF-IDF by Requestor" = lemma_tfidf_by_requestor,
  "Lemma by Purpose" = lemma_by_purpose,
  "Lemma TF-IDF by Purpose" = lemma_tfidf_by_purpose,
  "Phrase Lemma Candidates" = phrase_lemma_candidates,

  "Matched Month Requests" = requests_per_month_matched,
  "Monthly Requests Full" = requests_per_month_full,
  "Monthly Requests by Source" = requests_by_month_source
)

tables_to_export_clean <- tables_to_export[
  purrr::map_lgl(tables_to_export, ~ is.data.frame(.x) && ncol(.x) > 0 && nrow(.x) > 0)
]

# Optional CSV exports ------------------------------------------------------

if (export_csvs) {
  purrr::iwalk(
    tables_to_export_clean,
    ~ write_pretty_csv(
      df = .x,
      filename = janitor::make_clean_names(.y),
      csv_dir = csv_dir
    )
  )
}

# Figures ------------------------------------------------------------------

p_over_time <- requests_over_time %>%
  ggplot(aes(x = year_month, y = n_requests, color = plot_group, group = plot_group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "Literature Search Requests Over Time, 2025 - Present",
    subtitle = "In 2025, HUMC used a separate legacy form while other campuses used the HMH shared form; beginning January 2026, HUMC also moved onto the HMH shared form.",
    x = "Month",
    y = "Number of Requests",
    color = "Campus / Form Source"
  ) +
  theme_project()

ggsave(
  file.path(figures_dir, "combined_requests_over_time.png"),
  p_over_time,
  width = 12,
  height = 7,
  dpi = 300
)

p_by_campus <- requests_by_campus %>%
  filter(
    !is.na(campus_affiliation_clean),
    campus_affiliation_clean != "Unknown/Not specified"
  ) %>%
  ggplot(aes(x = reorder(campus_affiliation_clean, n_requests), y = n_requests)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Literature Search Requests by Campus, 2025 - Present",
    x = "Campus",
    y = "Number of Requests"
  ) +
  theme_project()

ggsave(
  file.path(figures_dir, "combined_requests_by_campus.png"),
  p_by_campus,
  width = 9,
  height = 6,
  dpi = 300
)

p_by_requestor <- requests_by_requestor %>%
  filter(!is.na(requestor_category)) %>%
  ggplot(aes(x = reorder(requestor_category, n_requests), y = n_requests)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Literature Search Requests by Requestor Category, January 2025 - Present",
    x = "Requestor Category",
    y = "Number of Requests"
  ) +
  theme_project()

ggsave(
  file.path(figures_dir, "combined_requests_by_requestor.png"),
  p_by_requestor,
  width = 9,
  height = 7,
  dpi = 300
)

if (nrow(requests_by_purpose) > 0) {
  p_by_purpose <- requests_by_purpose %>%
    ggplot(aes(x = reorder(purpose_category, n_selections), y = n_selections)) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Literature Search Requests by Purpose, 2025 - Present",
      subtitle = "IRB App, Patient Info, Policy, and Admin categories originate from HUMC legacy form (2025 only)",
      x = "Purpose",
      y = "Number of Selections"
    ) +
    theme_project()

  ggsave(
    file.path(figures_dir, "combined_requests_by_purpose.png"),
    p_by_purpose,
    width = 9,
    height = 6,
    dpi = 300
  )
}

p_matched <- ggplot(
  requests_per_month_matched,
  aes(x = month_label, y = n_requests, color = year, group = year)
) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "Monthly Literature Search Requests, Matched Months",
    subtitle = paste0("Comparison of Jan–", month.abb[latest_month_2026], " 2025 and 2026"),
    x = "Month",
    y = "Number of Requests",
    color = "Year"
  ) +
  theme_project()

ggsave(
  file.path(figures_dir, "requests_per_month_matched.png"),
  p_matched,
  width = 8,
  height = 5,
  dpi = 300
)

p_full <- ggplot(
  requests_per_month_full,
  aes(x = year_month, y = n_requests)
) +
  geom_line() +
  geom_point() +
  geom_vline(
    xintercept = as.Date("2026-01-01"),
    linetype = "dashed"
  ) +
  annotate(
    "text",
    x = as.Date("2026-01-01"),
    y = max(requests_per_month_full$n_requests, na.rm = TRUE),
    label = "Unified form begins",
    vjust = -0.5,
    hjust = 0,
    size = 3
  ) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "Monthly Literature Search Requests, Full Trend",
    subtitle = "Dashed line indicates transition to unified request form in January 2026",
    x = "Month",
    y = "Number of Requests"
  ) +
  theme_project()

ggsave(
  file.path(figures_dir, "requests_per_month_full.png"),
  p_full,
  width = 8,
  height = 5,
  dpi = 300
)

p_source <- ggplot(
  requests_by_month_source,
  aes(x = year_month, y = n_requests, color = source_file_type)
) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "Requests by Source System",
    subtitle = "Legacy HUMC vs unified HMH form",
    x = "Month",
    y = "Number of Requests",
    color = "Source"
  ) +
  theme_project()

ggsave(
  file.path(figures_dir, "requests_by_source.png"),
  p_source,
  width = 8,
  height = 5,
  dpi = 300
)

p_heatmap <- combined_dat %>%
  filter(!is.na(hour), !is.na(weekday)) %>%
  count(weekday, hour, name = "n_requests") %>%
  ggplot(aes(x = hour, y = fct_rev(weekday), fill = n_requests)) +
  geom_tile(color = "grey90", linewidth = 0.3) +
  scale_fill_viridis_c(
    option = "viridis",
    direction = 1,
    begin = 0.05,
    end = 0.80,
    name = "Number of\nRequests"
  ) +
  scale_x_continuous(breaks = seq(0, 23, by = 2), expand = c(0, 0)) +
  labs(
    title = "Request Density by Day and Hour",
    x = "Hour of Day",
    y = NULL
  ) +
  theme_project()

ggsave(
  file.path(figures_dir, "combined_requests_heatmap.png"),
  p_heatmap,
  width = 10,
  height = 6,
  dpi = 300
)

p_top_lemmas <- top_lemmas %>%
  slice_head(n = 25) %>%
  ggplot(aes(x = reorder(lemma, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top Lemmas in Literature Search Topics",
    x = "Lemma",
    y = "Number of Mentions"
  ) +
  theme_project()

ggsave(
  file.path(figures_dir, "combined_top_lemmas.png"),
  p_top_lemmas,
  width = 9,
  height = 7,
  dpi = 300
)

top_lemmas_by_requestor <- tidy_lemmas_all %>%
  filter(!is.na(requestor_category), requestor_category != "Unknown/Not specified") %>%
  count(requestor_category, lemma, sort = TRUE) %>%
  group_by(requestor_category) %>%
  slice_max(n, n = 10, with_ties = FALSE) %>%
  ungroup()

if (nrow(top_lemmas_by_requestor) > 0) {
  p_faceted_lemmas <- top_lemmas_by_requestor %>%
    ggplot(aes(x = reorder_within(lemma, n, requestor_category), y = n, fill = requestor_category)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    facet_wrap(~ requestor_category, scales = "free_y") +
    scale_x_reordered() +
    labs(
      title = "Top 10 Lemmas by Requestor Category",
      x = NULL,
      y = "Number of Mentions"
    ) +
    theme_project()

  ggsave(
    file.path(figures_dir, "combined_top_lemmas_by_requestor.png"),
    p_faceted_lemmas,
    width = 14,
    height = 9,
    dpi = 300
  )
}

# Formatted HTML tables -----------------------------------------------------

gt_requests_by_campus <- requests_by_campus %>%
  gt() %>%
  tab_header(title = "Literature Search Requests by Campus") %>%
  fmt_percent(columns = prop, decimals = 1)

gtsave(
  gt_requests_by_campus,
  file.path(formatted_tables_dir, "combined_requests_by_campus.html")
)

if (nrow(requests_by_purpose) > 0) {
  gt_requests_by_purpose <- requests_by_purpose %>%
    gt() %>%
    tab_header(title = "Literature Search Requests by Purpose") %>%
    fmt_percent(columns = prop, decimals = 1)

  gtsave(
    gt_requests_by_purpose,
    file.path(formatted_tables_dir, "combined_requests_by_purpose.html")
  )
}

# Save outputs --------------------------------------------------------------

if (export_csvs) {
  clear_output_folder(csv_dir, "\\.(csv|rds)$")
  
  purrr::iwalk(
    tables_to_export_clean,
    ~ write_pretty_csv(
      df = .x,
      filename = janitor::make_clean_names(.y),
      csv_dir = csv_dir
    )
  )
}

if (export_workbook) {
  write_pretty_workbook(
    tables = tables_to_export_clean,
    path = summary_filepath
  )
}

# Console summary ----------------------------------------------------------

cat("\n--- Combined Report Complete ---\n")

if (export_workbook) {
  cat("Summary workbook written to:\n", summary_filepath, "\n")
}

if (export_csvs && length(tables_to_export_clean) > 0) {
  message("Writing CSV exports...")
  
  purrr::iwalk(
    tables_to_export_clean,
    ~ write_pretty_csv(
      df = .x,
      filename = janitor::make_clean_names(.y),
      csv_dir = csv_dir
    )
  )
  
} else {
  message("CSV export skipped (either disabled or no valid tables).")
}

cat("Figures written to:\n", figures_dir, "\n")
cat("Formatted HTML tables written to:\n", formatted_tables_dir, "\n")