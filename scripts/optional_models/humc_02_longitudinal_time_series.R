# Optional analysis: HUMC longitudinal time series --------------------------
# This script does not run automatically in the main project pipeline.
#
# Purpose:
#   1. Analyze HUMC literature search request volume from 2013-2026.
#   2. Look for seasonal patterns by month and year.
#   3. Compare requestor groups over time.
#   4. Explore research topic/lemma trends over time.
#
# Outputs:
#   outputs/optional_models/humc_02_longitudinal_time_series/
#     figures/
#     tables/

# Setup --------------------------------------------------------------------

library(tidyverse)
library(janitor)
library(here)
library(lubridate)
library(tidytext)
library(textstem)
library(stringi)
library(zoo)
library(scales)
library(gt)

source(here("scripts", "shared", "paths.R"))
source(here("scripts", "shared", "helpers.R"))
source(here("scripts", "shared", "text_helpers.R"))
source(here("scripts", "shared", "output_helpers.R"))
source(here("scripts", "shared", "reference_data_loaders.R"))

# Output folders ------------------------------------------------------------

analysis_output_dir <- here(
  "outputs",
  "optional_models",
  "humc_02_longitudinal_time_series"
)

figures_dir <- file.path(analysis_output_dir, "figures")
tables_dir  <- file.path(analysis_output_dir, "tables")

dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

clear_output_folder(
  analysis_output_dir,
  "\\.(csv|rds|xlsx|html|png|jpg|jpeg)$"
)

# Load reference files ------------------------------------------------------

phrases_tbl <- read_phrases(phrases_path)
custom_map  <- read_custom_merges(custom_merges_path)
lex_map     <- read_lex(lex_path)

# Helper functions ----------------------------------------------------------
# These mirror the conventions used in the combined build script so this
# optional script stays aligned with the rest of the project.

if (!exists("clean_blank")) {
  clean_blank <- function(x) {
    x <- stringr::str_squish(as.character(x))
    x[x %in% c("", "NA", "N/A", "NULL", "null", "n/a")] <- NA_character_
    x
  }
}

pull_col <- function(data, col_name, default = NA_character_) {
  if (col_name %in% names(data)) {
    data[[col_name]]
  } else {
    rep(default, nrow(data))
  }
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
    str_detect(x_lower, "social work|therapy|pharmacy|diet|rehab|pt|ot|slp|allied|other provider") ~ "Allied Health",
    str_detect(x_lower, "patient|family|consumer") ~ "Consumer",
    str_detect(x_lower, "committee") ~ "Committee",
    TRUE ~ x_original
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

# Load HUMC legacy data -----------------------------------------------------
# This uses humc_path from scripts/shared/paths.R.

humc_raw <- readr::read_csv(humc_path, show_col_types = FALSE) %>%
  janitor::clean_names()

# Build HUMC request-level time series data --------------------------------
# This intentionally keeps all available HUMC years rather than using the
# combined script's analysis_years setting.

humc_ts <- humc_raw %>%
  mutate(
    source_file_type = "humc",
    source_label = "HUMC legacy form",
    original_id = pull_col(humc_raw, "request_id"),
    
    submitted_date = as.Date(pull_col(humc_raw, "date")),
    submitted_at = as.POSIXct(submitted_date),
    year = year(submitted_date),
    month_num = month(submitted_date),
    month = month(submitted_date, label = TRUE, abbr = TRUE),
    year_month = floor_date(submitted_date, unit = "month"),
    week = floor_date(submitted_date, unit = "week", week_start = 1),
    weekday = wday(submitted_date, label = TRUE, abbr = TRUE, week_start = 1),
    
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
    
    research_topic = clean_blank(pull_col(humc_raw, "topic")),
    citation_count = suppressWarnings(as.numeric(pull_col(humc_raw, "citation_count")))
  ) %>%
  filter(
    !is.na(submitted_date),
    year >= 2013,
    year <= 2026
  ) %>%
  mutate(
    request_id = row_number(),
    global_request_id = request_id,
    
    covid_period = case_when(
      year < 2020 ~ "Pre-COVID",
      year %in% c(2020, 2021) ~ "COVID-era",
      year >= 2022 ~ "Post-2021",
      TRUE ~ NA_character_
    ),
    
    covid_period = factor(
      covid_period,
      levels = c("Pre-COVID", "COVID-era", "Post-2021")
    )
  ) %>%
  dplyr::select(
    request_id,
    global_request_id,
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
    covid_period,
    requestor_category_raw,
    requestor_category,
    purpose,
    research_topic,
    citation_count
  )

write_pretty_csv(
  humc_ts,
  "humc_longitudinal_time_series_ready_data",
  tables_dir
)

# Monthly request volume ----------------------------------------------------

monthly_requests <- humc_ts %>%
  count(year_month, name = "n_requests") %>%
  complete(
    year_month = seq.Date(
      min(year_month),
      max(year_month),
      by = "month"
    ),
    fill = list(n_requests = 0)
  ) %>%
  arrange(year_month) %>%
  mutate(
    year = year(year_month),
    month_num = month(year_month),
    month = month(year_month, label = TRUE, abbr = TRUE),
    rolling_3_month = zoo::rollmean(n_requests, k = 3, fill = NA, align = "right"),
    rolling_12_month = zoo::rollmean(n_requests, k = 12, fill = NA, align = "right")
  )

write_pretty_csv(
  monthly_requests,
  "humc_monthly_request_volume",
  tables_dir
)

# Figure 1: monthly request volume -----------------------------------------

p_monthly_volume <- ggplot(
  monthly_requests,
  aes(x = year_month, y = n_requests)
) +
  geom_line(alpha = 0.35, linewidth = 0.7) +
  geom_point(alpha = 0.5, size = 1.3) +
  geom_line(aes(y = rolling_3_month), linewidth = 1) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "HUMC Literature Search Requests Over Time",
    subtitle = "Monthly request counts, with 3-month rolling average",
    x = NULL,
    y = "Number of requests"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  file.path(figures_dir, "humc_monthly_request_volume.png"),
  p_monthly_volume,
  width = 12,
  height = 6,
  dpi = 300
)

# Figure 2: long-term smoothed trend ---------------------------------------

p_long_trend <- ggplot(
  monthly_requests,
  aes(x = year_month, y = n_requests)
) +
  geom_line(alpha = 0.25) +
  geom_line(aes(y = rolling_12_month), linewidth = 1.2) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Long-Term Trend in HUMC Literature Search Requests",
    subtitle = "Monthly requests with 12-month rolling average",
    x = NULL,
    y = "Number of requests"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  file.path(figures_dir, "humc_long_term_monthly_trend.png"),
  p_long_trend,
  width = 12,
  height = 6,
  dpi = 300
)

# Calendar month seasonality ------------------------------------------------

monthly_seasonality <- humc_ts %>%
  count(year, month_num, month, name = "n_requests") %>%
  group_by(month_num, month) %>%
  summarize(
    mean_requests = mean(n_requests, na.rm = TRUE),
    median_requests = median(n_requests, na.rm = TRUE),
    sd_requests = sd(n_requests, na.rm = TRUE),
    min_requests = min(n_requests, na.rm = TRUE),
    max_requests = max(n_requests, na.rm = TRUE),
    n_years_observed = n(),
    .groups = "drop"
  ) %>%
  arrange(month_num)

write_pretty_csv(
  monthly_seasonality,
  "humc_calendar_month_seasonality_summary",
  tables_dir
)

p_calendar_month <- ggplot(
  monthly_seasonality,
  aes(x = month, y = mean_requests, group = 1)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Average HUMC Requests by Calendar Month",
    subtitle = "Mean monthly request volume across observed years",
    x = NULL,
    y = "Average requests"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  file.path(figures_dir, "humc_average_requests_by_calendar_month.png"),
  p_calendar_month,
  width = 9,
  height = 5,
  dpi = 300
)

# Month-year heatmap --------------------------------------------------------

month_year_heatmap <- humc_ts %>%
  count(year, month, month_num, name = "n_requests") %>%
  arrange(year, month_num)

write_pretty_csv(
  month_year_heatmap,
  "humc_month_by_year_request_heatmap_data",
  tables_dir
)

p_month_year_heatmap <- ggplot(
  month_year_heatmap,
  aes(x = month, y = factor(year), fill = n_requests)
) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = n_requests), size = 3, fontface = "bold") +
  scale_fill_gradient(low = "#e8f0f4", high = "#24787f") +
  labs(
    title = "HUMC Request Volume by Month and Year",
    subtitle = "Darker cells indicate busier months",
    x = NULL,
    y = NULL,
    fill = "Requests"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 15),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold")
  )

ggsave(
  file.path(figures_dir, "humc_request_volume_month_year_heatmap.png"),
  p_month_year_heatmap,
  width = 10,
  height = 8,
  dpi = 300
)

# COVID-period summary ------------------------------------------------------
# This is not a causal analysis. It is a descriptive comparison of broad eras.

covid_summary <- humc_ts %>%
  count(covid_period, name = "n_requests") %>%
  mutate(prop_requests = n_requests / sum(n_requests))

write_pretty_csv(
  covid_summary,
  "humc_requests_by_covid_period",
  tables_dir
)

p_covid_period <- ggplot(
  covid_summary,
  aes(x = covid_period, y = n_requests)
) +
  geom_col(fill = "#24787f", width = 0.7) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "HUMC Requests by Broad Time Period",
    subtitle = "Descriptive comparison of pre-COVID, COVID-era, and post-2021 periods",
    x = NULL,
    y = "Number of requests"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  file.path(figures_dir, "humc_requests_by_covid_period.png"),
  p_covid_period,
  width = 8,
  height = 5,
  dpi = 300
)

# Requestor group trends ----------------------------------------------------

requestor_monthly <- humc_ts %>%
  filter(!is.na(requestor_category)) %>%
  count(year_month, requestor_category, name = "n_requests") %>%
  group_by(requestor_category) %>%
  arrange(year_month, .by_group = TRUE) %>%
  mutate(
    rolling_3_month = zoo::rollmean(n_requests, k = 3, fill = NA, align = "right")
  ) %>%
  ungroup()

write_pretty_csv(
  requestor_monthly,
  "humc_monthly_requests_by_requestor_group",
  tables_dir
)

top_requestor_groups <- humc_ts %>%
  filter(requestor_category != "Consumer") %>%
  count(requestor_category, sort = TRUE) %>%
  filter(!is.na(requestor_category)) %>%
  slice_head(n = 6) %>%
  pull(requestor_category)

requestor_monthly_top <- requestor_monthly %>%
  filter(requestor_category %in% top_requestor_groups) %>%
  filter(requestor_category != "Consumer") %>%
  mutate(
    requestor_plot = case_when(
      requestor_category == "Physician" ~ "Attending",
      requestor_category == "Resident/Fellow" ~ "Resident",
      requestor_category == "Nursing" ~ "Nurse",
      requestor_category == "OtherProvider" ~ "Allied Health Provider",
      TRUE ~ requestor_category
    )
  )

requestor_monthly_plot <- requestor_monthly %>%
  filter(
    !requestor_category %in% c(
      "Committee",
      "Unknown/Not specified",
      "Consumer"
    )
  ) %>%
  mutate(
    requestor_plot = case_when(
      requestor_category == "Physician" ~ "Attending",
      requestor_category == "Resident/Fellow" ~ "Resident",
      requestor_category == "Nursing" ~ "Nurse",
      requestor_category == "OtherProvider" ~ "Allied Health Provider",
      TRUE ~ requestor_category
    )
  )

p_requestor_trends <- ggplot(
  requestor_monthly_plot,
  aes(x = year_month, y = n_requests)
) +
  geom_line(alpha = 0.35) +
  geom_line(aes(y = rolling_3_month), linewidth = 0.9) +
  facet_wrap(~ requestor_plot, scales = "free_y") +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  labs(
    title = "HUMC Monthly Request Trends by Requestor Group, 2013–2025",
    subtitle = "Heavier line shows 3-month rolling average",
    x = NULL,
    y = "Requests"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

ggsave(
  file.path(figures_dir, "humc_monthly_request_trends_by_requestor_group.png"),
  p_requestor_trends,
  width = 12,
  height = 8,
  dpi = 300
)

# Requestor group seasonality ----------------------------------------------

requestor_seasonality <- humc_ts %>%
  filter(requestor_category %in% top_requestor_groups) %>%
  count(requestor_category, year, month_num, month, name = "n_requests") %>%
  group_by(requestor_category, month_num, month) %>%
  summarize(
    mean_requests = mean(n_requests, na.rm = TRUE),
    .groups = "drop"
  )

write_pretty_csv(
  requestor_seasonality,
  "humc_requestor_calendar_month_seasonality",
  tables_dir
)

p_requestor_seasonality <- ggplot(
  requestor_seasonality,
  aes(x = month, y = requestor_category, fill = mean_requests)
) +
  geom_tile(color = "white", linewidth = 1) +
  scale_fill_gradient(low = "#e8f0f4", high = "#24787f") +
  labs(
    title = "Average HUMC Monthly Demand by Requestor Group",
    subtitle = "Top requestor groups; darker cells indicate higher average request volume",
    x = NULL,
    y = NULL,
    fill = "Avg. requests"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 15),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold")
  )

ggsave(
  file.path(figures_dir, "humc_requestor_group_seasonality_heatmap.png"),
  p_requestor_seasonality,
  width = 11,
  height = 6,
  dpi = 300
)

# Ljung-Box test------------------------------------------------------

ljung_box <- Box.test(
  monthly_requests$n_requests,
  lag = 12,
  type = "Ljung-Box"
)

ljung_box_summary <- tibble(
  test = "Ljung-Box",
  statistic = unname(ljung_box$statistic),
  df = unname(ljung_box$parameter),
  p_value = ljung_box$p.value
)

write_pretty_csv(
  ljung_box_summary,
  "humc_ljung_box_test",
  tables_dir
)

ljung_box_gt <- ljung_box_summary %>%
  mutate(
    statistic = round(statistic, 2),
    p_value = case_when(
      p_value < 0.001 ~ "<0.001",
      TRUE ~ as.character(round(p_value, 3))
    )
  ) %>%
  gt() %>%
  tab_header(
    title = "Ljung-Box Test for Autocorrelation",
    subtitle = "HUMC monthly request volume"
  )

gtsave(
  ljung_box_gt,
  file.path(
    analysis_output_dir,
    "humc_ljung_box_test.html"
  )
)

# STL decomposition ---------------------------------------------------------
# STL separates a monthly time series into trend, seasonal, and remainder
# components. This is useful for seeing whether the series has a stable
# repeating seasonal pattern after removing long-term trend.

if (nrow(monthly_requests) >= 24) {
  
  request_ts <- ts(
    monthly_requests$n_requests,
    frequency = 12,
    start = c(
      year(min(monthly_requests$year_month)),
      month(min(monthly_requests$year_month))
    )
  )
  
  request_stl <- stl(request_ts, s.window = "periodic")
  
  stl_components <- tibble(
    year_month = monthly_requests$year_month,
    observed = as.numeric(request_stl$time.series[, "remainder"] +
                            request_stl$time.series[, "seasonal"] +
                            request_stl$time.series[, "trend"]),
    seasonal = as.numeric(request_stl$time.series[, "seasonal"]),
    trend = as.numeric(request_stl$time.series[, "trend"]),
    remainder = as.numeric(request_stl$time.series[, "remainder"])
  )
  
  write_pretty_csv(
    stl_components,
    "humc_monthly_request_stl_components",
    tables_dir
  )
  
  p_stl_trend <- ggplot(
    stl_components,
    aes(x = year_month)
  ) +
    geom_line(aes(y = observed), alpha = 0.3) +
    geom_line(aes(y = trend), linewidth = 1.1) +
    scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y"
    ) +
    scale_y_continuous(
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
      title = "STL Trend Component for HUMC Monthly Requests",
      subtitle = "Trend line overlaid on observed monthly request volume",
      x = NULL,
      y = "Requests"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 15),
      axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
      axis.text.y = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  ggsave(
    file.path(figures_dir, "humc_monthly_request_stl_trend.png"),
    p_stl_trend,
    width = 12,
    height = 6,
    dpi = 300
  )
  
  p_stl_seasonal <- ggplot(
    stl_components,
    aes(x = year_month, y = seasonal)
  ) +
    geom_line(linewidth = 0.9) +
    scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y"
    ) +
    labs(
      title = "STL Seasonal Component for HUMC Monthly Requests",
      subtitle = "Repeating seasonal pattern after removing long-term trend",
      x = NULL,
      y = "Seasonal component"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 15),
      axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
      axis.text.y = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  ggsave(
    file.path(figures_dir, "humc_monthly_request_stl_seasonal_component.png"),
    p_stl_seasonal,
    width = 12,
    height = 6,
    dpi = 300
  )
}

# Topic and lemma preparation ----------------------------------------------
# This repeats the same normalization logic used in the combined script but
# only for HUMC 2013-2026.

humc_topics <- humc_ts %>%
  transmute(
    global_request_id,
    request_id,
    source_file_type,
    source_label,
    original_id,
    submitted_date,
    year,
    year_month,
    month_num,
    month,
    requestor_category,
    research_topic = str_squish(research_topic)
  ) %>%
  filter(!is.na(research_topic), research_topic != "")

topics_normalized <- humc_topics %>%
  mutate(
    research_topic_clean = clean_text(research_topic, preset = "normalize"),
    research_topic_clean = collapse_phrases(research_topic_clean, phrases_tbl),
    research_topic_clean = na_if(research_topic_clean, "")
  ) %>%
  filter(!is.na(research_topic_clean))

tidy_lemmas_humc <- topics_normalized %>%
  dplyr::select(
    global_request_id,
    request_id,
    submitted_date,
    year,
    year_month,
    month_num,
    month,
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
  mutate(
    lemma = coalesce(lemma_custom, lemma1)
  ) %>%
  anti_join(tidytext::stop_words, by = c("lemma" = "word")) %>%
  filter(!str_detect(lemma, "^\\d+$"))

write_pretty_csv(
  tidy_lemmas_humc,
  "humc_tidy_lemmas_2013_2026",
  tables_dir
)

# Lemma seasonality ---------------------------------------------------------

top_lemmas <- tidy_lemmas_humc %>%
  count(lemma, sort = TRUE, name = "n_mentions") %>%
  slice_head(n = 20) %>%
  pull(lemma)

lemma_month_summary <- tidy_lemmas_humc %>%
  filter(lemma %in% top_lemmas) %>%
  count(lemma, year, month_num, month, name = "n_mentions") %>%
  group_by(lemma, month_num, month) %>%
  summarize(
    mean_mentions = mean(n_mentions, na.rm = TRUE),
    .groups = "drop"
  )

write_pretty_csv(
  lemma_month_summary,
  "humc_top_lemma_calendar_month_seasonality",
  tables_dir
)

p_lemma_seasonality <- ggplot(
  lemma_month_summary,
  aes(x = month, y = reorder(lemma, mean_mentions), fill = mean_mentions)
) +
  geom_tile(color = "white", linewidth = 0.8) +
  scale_fill_gradient(low = "#e8f0f4", high = "#24787f") +
  labs(
    title = "Seasonality of Common HUMC Research Topic Lemmas",
    subtitle = "Top 20 lemmas; darker cells indicate higher average monthly mentions",
    x = NULL,
    y = NULL,
    fill = "Avg. mentions"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 15),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold")
  )

ggsave(
  file.path(figures_dir, "humc_top_lemma_month_seasonality_heatmap.png"),
  p_lemma_seasonality,
  width = 11,
  height = 8,
  dpi = 300
)

# Lemma trends over time ----------------------------------------------------

top_lemmas_for_trends <- tidy_lemmas_humc %>%
  count(lemma, sort = TRUE, name = "n_mentions") %>%
  slice_head(n = 12) %>%
  pull(lemma)

lemma_monthly_trends <- tidy_lemmas_humc %>%
  filter(lemma %in% top_lemmas_for_trends) %>%
  count(year_month, lemma, name = "n_mentions") %>%
  group_by(lemma) %>%
  arrange(year_month, .by_group = TRUE) %>%
  mutate(
    rolling_3_month_mentions = zoo::rollmean(
      n_mentions,
      k = 3,
      fill = NA,
      align = "right"
    )
  ) %>%
  ungroup()

write_pretty_csv(
  lemma_monthly_trends,
  "humc_top_lemma_monthly_trends",
  tables_dir
)

p_lemma_trends <- ggplot(
  lemma_monthly_trends,
  aes(x = year_month, y = n_mentions)
) +
  geom_line(alpha = 0.35) +
  geom_line(aes(y = rolling_3_month_mentions), linewidth = 0.9) +
  facet_wrap(~ lemma, scales = "free_y") +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  labs(
    title = "Monthly Trends for Common HUMC Research Topic Lemmas, 2013-2025",
    subtitle = "Thin lines show monthly counts; heavier lines show 3-month rolling averages",
    x = NULL,
    y = "Mentions"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

ggsave(
  file.path(figures_dir, "humc_top_lemma_monthly_trends.png"),
  p_lemma_trends,
  width = 12,
  height = 8,
  dpi = 300
)

# Export summary workbook ---------------------------------------------------

summary_tables <- list(
  "Monthly Requests" = monthly_requests,
  "Monthly Seasonality" = monthly_seasonality,
  "Month-Year Heatmap Data" = month_year_heatmap,
  "COVID Period Summary" = covid_summary,
  "Requestor Monthly Trends" = requestor_monthly,
  "Requestor Seasonality" = requestor_seasonality,
  "Lemma Month Summary" = lemma_month_summary,
  "Lemma Monthly Trends" = lemma_monthly_trends
)

summary_workbook_path <- file.path(
  analysis_output_dir,
  "humc_longitudinal_time_series_summary.xlsx"
)

write_pretty_workbook(
  tables = summary_tables,
  path = summary_workbook_path
)

# Save key objects ----------------------------------------------------------

humc_longitudinal_objects <- list(
  humc_ts = humc_ts,
  monthly_requests = monthly_requests,
  monthly_seasonality = monthly_seasonality,
  requestor_monthly = requestor_monthly,
  requestor_seasonality = requestor_seasonality,
  humc_topics = humc_topics,
  tidy_lemmas_humc = tidy_lemmas_humc,
  lemma_month_summary = lemma_month_summary,
  lemma_monthly_trends = lemma_monthly_trends
)

saveRDS(
  humc_longitudinal_objects,
  file.path(analysis_output_dir, "humc_longitudinal_time_series_objects.rds")
)

# Console summary -----------------------------------------------------------

cat("\n--- HUMC Longitudinal Time Series Complete ---\n")
cat("Rows:", nrow(humc_ts), "\n")
cat("Date range:", as.character(min(humc_ts$submitted_date)), "to", as.character(max(humc_ts$submitted_date)), "\n")
cat("Years:", paste(sort(unique(humc_ts$year)), collapse = ", "), "\n")
cat("Unique requestor groups:", n_distinct(humc_ts$requestor_category), "\n")
cat("Research topic records:", nrow(humc_topics), "\n")
cat("Lemma records:", nrow(tidy_lemmas_humc), "\n")
cat("Figures saved to:", figures_dir, "\n")
cat("Tables saved to:", tables_dir, "\n")