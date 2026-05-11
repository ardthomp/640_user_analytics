# scripts/hmh/01_analyze_hmh_literature_searches.R
#
# This script summarizes 2026 HMH shared-form literature search requests.
# It uses the standardized request fields to describe demand, requestor groups,
# campus patterns, request purposes, and workload indicators.
#
# Text analysis and optional workload modeling are handled in separate scripts.
#
# Input:
#   data/raw/hmh.csv
#
# Outputs:
#   outputs/hmh/literature_searches/

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

# This script analyzes 2026 HMH literature search requests collected through
# the standardized shared request form..
analysis_year <- 2026

weekday_levels <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
time_level_levels <- c("Low time", "Medium time", "High time")

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

hmh_network_objects <- readRDS(
  here("data", "processed", "hmh_network_analysis_data.rds")
)

dat <- hmh_network_objects$hmh_shared_form_dat %>%
  filter(year == analysis_year) %>%
  mutate(
    request_id = row_number(),
    
    weekday = factor(
      weekday,
      levels = weekday_levels
    ),
    
    season = case_when(
      month_num %in% c(12, 1, 2) ~ "Winter",
      month_num %in% c(3, 4, 5) ~ "Spring",
      month_num %in% c(6, 7, 8) ~ "Summer",
      TRUE ~ "Fall"
    ),
    
    requestor_group = collapse_requestor_group(requestor_category)
  ) %>%
  filter(!is.na(weekday)) %>%
  filter(requestor_group != "Consumer" | is.na(requestor_group))

# Penultimate month setup --------------------------------------------------
# Use the penultimate observed month so incomplete current-month data
# does not distort monthly trend figures.

latest_month_start <- dat %>%
  summarize(latest_month = max(year_month, na.rm = TRUE)) %>%
  pull(latest_month)

penultimate_month_start <- latest_month_start %m-% months(1)

month_breaks <- seq(
  from = as.Date(paste0(analysis_year, "-01-01")),
  to = penultimate_month_start,
  by = "1 month"
)

# Demand / utilization tables ----------------------------------------------

requests_per_month <- dat %>%
  filter(
    year_month >= as.Date(paste0(analysis_year, "-01-01")),
    year_month <= penultimate_month_start
  ) %>%
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
  count(campus_affiliation_clean, sort = TRUE, name = "n_requests") %>%
  mutate(prop = n_requests / sum(n_requests))

# Purpose tables -----------------------------------------------------------
# Purpose is a multi-select field, so one request may contribute to
# more than one purpose category.

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
      campus_affiliation_clean,
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
    count(campus_affiliation_clean, purpose_category, sort = TRUE, name = "n_selections") %>%
    group_by(campus_affiliation_clean) %>%
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
# Workload summaries use the structured fields from the shared form:
# reported time spent and number of literature searches.

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
  group_by(campus_affiliation_clean) %>%
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
  "Searches Summary" = searches_summary,
  "Searches by Requestor" = searches_by_requestor,
  "Searches by Campus" = searches_by_campus,
  "Searches by Time Spent" = searches_by_time_spent,
  "Time Spent Counts" = time_spent_counts,
  "Time Spent by Requestor" = time_spent_by_requestor,
  "Time Spent by Purpose" = time_spent_by_purpose,
  "Time Pct by Requestor" = time_spent_pct_by_requestor,
  "Time Pct by Purpose" = time_spent_pct_by_purpose
)

tables_to_export_clean <- tables_to_export[
  purrr::map_lgl(tables_to_export, ~ is.data.frame(.x) && ncol(.x) > 0)
]

# Figures ------------------------------------------------------------------

p1 <- ggplot(requests_per_month, aes(x = year_month, y = n_requests)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_date(
    breaks = month_breaks,
    date_labels = "%b",
    limits = range(month_breaks)
  ) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = paste0(
      "Literature Search Requests by Month (Jan–",
      month.abb[lubridate::month(penultimate_month_start)],
      " ",
      analysis_year,
      ")"
    ),
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
      title = "Purpose Selections for Literature Search Requests",
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

if (export_csvs && length(tables_to_export_clean) > 0) {
  message("Writing CSV exports...")
  
  purrr::iwalk(
    tables_to_export_clean,
    ~ write_pretty_csv(
      df = .x,
      filename = janitor::make_clean_names(.y),
      csv_dir = paths$csv_dir
    )
  )
  
} else {
  message("CSV export skipped (either disabled or no valid tables).")
}

if (export_workbook) {
  write_pretty_workbook(
    tables = tables_to_export_clean,
    path = summary_filepath
  )
}