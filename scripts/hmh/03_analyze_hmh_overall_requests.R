# scripts/hmh/03_analyze_hmh_overall_requests.R
#
# HMH network overall request analysis.
#
# Purpose:
#   Load data/processed/hmh_network_analysis_data.rds and generate overall
#   network utilization tables and figures for harmonized 2025+ literature
#   search request data.
#
# Run after:
#   source("scripts/hmh/00_build_hmh_network_dataset.R")

library(tidyverse)
library(janitor)
library(here)
library(lubridate)
library(scales)
library(forcats)
library(gt)
library(viridis)
library(openxlsx)

source(here("scripts", "shared", "paths.R"))
source(here("scripts", "shared", "helpers.R"))
source(here("scripts", "shared", "output_helpers.R"))
source(here("scripts", "shared", "plotting_helpers.R"))

paths <- make_output_paths(file.path("hmh", "overall_requests"))
hmh_network_rds_path <- here(
  "data",
  "processed",
  "hmh_network_analysis_data.rds"
)

export_csvs <- FALSE
export_workbook <- TRUE

if (!file.exists(hmh_network_rds_path)) {
  stop("Could not find data/processed/hmh_network_analysis_data.rds. Run scripts/hmh/00_build_hmh_network_dataset.R first.")
}

if (export_csvs) clear_output_folder(paths$csv_dir, "\\.(csv|rds)$")
clear_output_folder(paths$figures_dir, "\\.(png|jpg|jpeg|pdf)$")
clear_output_folder(paths$formatted_tables_dir, "\\.html$")
clear_output_folder(paths$output_dir, "^hmh_overall_request_summary\\.xlsx$")

analysis_data <- readRDS(hmh_network_rds_path)
network_dat <- analysis_data$hmh_network_dat

overall_summary_path <- file.path(paths$output_dir, "hmh_overall_request_summary.xlsx")

# Data checks ---------------------------------------------------------------

campus_check <- network_dat %>%
  count(source_file_type, source_label, campus_affiliation_raw,
        campus_affiliation_clean, campus_affiliation_detail,
        humc, carrier, jfk, palisades, network, name = "n") %>%
  arrange(source_file_type, desc(n))

requestor_check <- network_dat %>%
  count(source_file_type, requestor_category, name = "n") %>%
  arrange(source_file_type, desc(n))

# Overall utilization tables ------------------------------------------------

requests_over_time <- network_dat %>%
  count(year_month, plot_group, name = "n_requests")

requests_by_source <- network_dat %>%
  count(source_label, name = "n_requests") %>%
  mutate(prop = n_requests / sum(n_requests)) %>%
  arrange(desc(n_requests))

requests_by_campus <- network_dat %>%
  count(campus_affiliation_clean, name = "n_requests") %>%
  mutate(prop = n_requests / sum(n_requests)) %>%
  arrange(desc(n_requests))

requests_by_requestor <- network_dat %>%
  mutate(
    requestor_category = case_when(
      requestor_category == "Allied Health" ~ "Allied Health Professional",
      requestor_category == "Nursing" ~ "Nurse",
      TRUE ~ requestor_category
    )
  ) %>%
  count(requestor_category, name = "n_requests") %>%
  mutate(prop = n_requests / sum(n_requests)) %>%
  arrange(desc(n_requests))

requests_by_month_total <- network_dat %>%
  count(year_month, name = "n_requests")

requests_by_year <- network_dat %>%
  count(year, name = "n_requests") %>%
  mutate(prop = n_requests / sum(n_requests))

requests_by_weekday <- network_dat %>%
  count(weekday, name = "n_requests") %>%
  arrange(weekday)

requests_by_hour <- network_dat %>%
  filter(!is.na(hour)) %>%
  count(hour, name = "n_requests") %>%
  arrange(hour)

time_spent_counts <- network_dat %>%
  count(time_spent, name = "n_requests") %>%
  mutate(prop = n_requests / sum(n_requests)) %>%
  arrange(desc(n_requests))

searches_by_year <- network_dat %>%
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

# Matched-month comparison --------------------------------------------------

penultimate_month_2026 <- network_dat %>%
  filter(year == 2026) %>%
  distinct(month_num) %>%
  arrange(desc(month_num)) %>%
  slice(2) %>%
  pull(month_num)

if (length(penultimate_month_2026) == 0 || is.na(penultimate_month_2026)) {
  penultimate_month_2026 <- network_dat %>%
    distinct(month_num) %>%
    arrange(desc(month_num)) %>%
    slice(2) %>%
    pull(month_num)
}

requests_per_month_matched <- network_dat %>%
  filter(year %in% c(2025, 2026), month_num <= penultimate_month_2026) %>%
  count(year, month_num, month, name = "n_requests") %>%
  mutate(
    year = factor(year),
    month_label = factor(month.abb[month_num], levels = month.abb[1:penultimate_month_2026])
  )

requests_by_month_source <- network_dat %>%
  count(year_month, source_file_type, name = "n_requests")

# Export tables -------------------------------------------------------------

summary_tables <- list(
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
  "Matched Month Requests" = requests_per_month_matched,
  "Monthly Requests by Source" = requests_by_month_source
)

summary_tables_clean <- summary_tables[
  purrr::map_lgl(summary_tables, ~ is.data.frame(.x) && nrow(.x) > 0 && ncol(.x) > 0)
]

if (export_workbook) {
  write_pretty_workbook(summary_tables_clean, overall_summary_path)
}

if (export_csvs) {
  purrr::iwalk(summary_tables_clean, ~ write_pretty_csv(.x, janitor::make_clean_names(.y), paths$csv_dir))
}

# Figures ------------------------------------------------------------------

p_over_time <- requests_over_time %>%
  ggplot(aes(x = year_month, y = n_requests, color = plot_group, group = plot_group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "HMH Network Literature Search Requests Over Time, 2025–Present",
    subtitle = "2025 includes HUMC legacy-form records plus other HMH shared-form records; 2026+ reflects the unified shared form.",
    x = "Month",
    y = "Number of Requests",
    color = "Campus / Form Source"
  ) +
  theme_project()

ggsave(file.path(paths$figures_dir, "hmh_network_requests_over_time.png"), p_over_time, width = 12, height = 7, dpi = 300)

p_by_campus <- requests_by_campus %>%
  filter(!is.na(campus_affiliation_clean), campus_affiliation_clean != "Unknown/Not specified") %>%
  ggplot(aes(x = reorder(campus_affiliation_clean, n_requests), y = n_requests)) +
  geom_col() +
  coord_flip() +
  labs(title = "HMH Network Literature Search Requests by Campus, 2025–Present", x = "Campus", y = "Number of Requests") +
  theme_project()

ggsave(file.path(paths$figures_dir, "hmh_network_requests_by_campus.png"), p_by_campus, width = 9, height = 6, dpi = 300)

p_by_requestor <- requests_by_requestor %>%
  filter(!is.na(requestor_category)) %>%
  ggplot(aes(x = reorder(requestor_category, n_requests), y = n_requests)) +
  geom_col() +
  coord_flip() +
  labs(title = "HMH Network Literature Search Requests by Requestor Category, 2025–Present", x = "Requestor Category", y = "Number of Requests") +
  theme_project()

ggsave(
  file.path(paths$figures_dir, "hmh_network_requests_by_requestor.png"),
  p_by_requestor,
  width = 11,
  height = 7,
  dpi = 300
)

if (nrow(requests_per_month_matched) > 0) {
  p_matched <- ggplot(
    requests_per_month_matched,
    aes(
      x = month_label,
      y = n_requests,
      color = year,
      group = year
    )
  ) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2.5) +
    scale_color_manual(
      values = c(
        "2025" = "#2166AC",
        "2026" = "#B2182B"
      )
    ) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(
      title = "HMH Monthly Literature Search Requests, Matched Months",
      subtitle = paste0("Comparison of Jan–", month.abb[penultimate_month_2026], " 2025 and 2026"),
      x = "Month",
      y = "Number of Requests",
      color = "Year"
    ) +
    theme_project()
  ggsave(file.path(paths$figures_dir, "hmh_network_requests_per_month_matched.png"), p_matched, width = 8, height = 5, dpi = 300)
}

cat("\n--- HMH Overall Request Analysis Complete ---\n")
if (export_workbook) cat("Workbook written to:", overall_summary_path, "\n")
cat("Figures written to:", paths$figures_dir, "\n")
