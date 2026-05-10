# Optional analysis: HMH network time series and seasonality patterns --------
# This script is exploratory and does not run automatically in the main pipeline.
# Goal:
#   1. Look for seasonal patterns in request volume
#   2. See whether requestor groups vary by time of year
#   3. See whether certain lemmas become more/less common by month
#
# Output:
#   outputs/optional_models/hmh_01_time_series_analysis/
#     figures/
#     tables/

library(tidyverse)
library(here)
library(janitor)
library(lubridate)
library(scales)
library(zoo)

source(here("scripts", "shared", "paths.R"))
source(here("scripts", "shared", "output_helpers.R"))

# Output folders ------------------------------------------------------------

analysis_output_dir <- here(
  "outputs",
  "optional_models",
  "hmh_01_time_series_analysis"
)

figures_dir <- file.path(analysis_output_dir, "figures")
tables_dir  <- file.path(analysis_output_dir, "tables")

dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

clear_output_folder(
  analysis_output_dir,
  "\\.(csv|rds|xlsx|html|png|jpg|jpeg)$"
)

# Load HMH network data --------------------------------------------------------
# This script reads the harmonized HMH network dataset created by
# scripts/hmh/00_build_hmh_network_dataset.R.

hmh_network_rds_path <- here(
  "data",
  "processed",
  "hmh_network_analysis_data.rds"
)

if (!file.exists(hmh_network_rds_path)) {
  stop("Could not find data/processed/hmh_network_analysis_data.rds. Run scripts/hmh/00_build_hmh_network_dataset.R first.")
}

hmh_network_objects <- readRDS(hmh_network_rds_path)

# Pull the harmonized HMH network dataset.

if ("hmh_network_dat" %in% names(hmh_network_objects)) {
  hmh_network_dat <- hmh_network_objects$hmh_network_dat
} else {
  stop("The HMH network RDS does not contain an object named hmh_network_dat.")
}

# # Pull lemma-level data from the harmonized HMH network analysis object.
# If not, the script will skip lemma seasonality and still run the volume and
# requestor-group sections.

if ("tidy_lemmas_all" %in% names(hmh_network_objects)) {
  tidy_lemmas_all <- hmh_network_objects$tidy_lemmas_all
} else {
  tidy_lemmas_all <- NULL
  message("No tidy_lemmas_all object found. Lemma seasonality section will be skipped.")
}

# Prepare base time variables ----------------------------------------------
# submitted_date is the anchor date for time series analysis.
# year_month is the month-level time index.
# month is used for seasonality by calendar month.

hmh_network_ts <- hmh_network_dat %>%
  clean_names() %>%
  mutate(
    submitted_date = as.Date(submitted_date),
    year_month = floor_date(submitted_date, unit = "month"),
    year = year(submitted_date),
    month_num = month(submitted_date),
    month = month(submitted_date, label = TRUE, abbr = TRUE),
    weekday = wday(submitted_date, label = TRUE, abbr = TRUE, week_start = 1)
  ) %>%
  filter(!is.na(submitted_date))

# Standardize requestor groups ---------------------------------------------
# This is intentionally similar to the requestor grouping you have been using
# elsewhere, so the optional analysis stays aligned with the main project.

hmh_network_ts <- hmh_network_ts %>%
  mutate(
    requestor_text = str_to_lower(coalesce(requestor_category, "")),
    
    requestor_group = case_when(
      str_detect(requestor_text, "physical therapist|pt|occupational therapist|ot|speech|slp|therapy|therapist|allied") ~ "Allied Health Professional",
      str_detect(requestor_text, "nurse practitioner|np|physician assistant|pa") ~ "Nurse Practitioner/PA",
      str_detect(requestor_text, "physician|attending") ~ "Physician",
      str_detect(requestor_text, "nurse|rn|nursing") ~ "Nurse",
      str_detect(requestor_text, "resident/fellow|resident") ~ "Resident",
      str_detect(requestor_text, "fellow") ~ "Fellow",
      str_detect(requestor_text, "student") ~ "Student",
      str_detect(requestor_text, "corporate") ~ "Corporate",
      str_detect(requestor_text, "hospital") ~ "Hospital",
      str_detect(requestor_text, "pharmacist|pharmacy") ~ "Pharmacist",
      str_detect(requestor_text, "faculty") ~ "Faculty",
      TRUE ~ "Other"
    ),
    
    requestor_group = factor(
      requestor_group,
      levels = c(
        "Physician",
        "Nurse",
        "Allied Health Professional",
        "Student",
        "Resident",
        "Nurse Practitioner/PA",
        "Corporate",
        "Hospital",
        "Other",
        "Fellow",
        "Pharmacist",
        "Faculty"
      )
    )
  )

# Save the cleaned time-series-ready dataset --------------------------------

write_pretty_csv(
  hmh_network_ts,
  "hmh_network_time_series_ready_data",
  tables_dir
)

# Monthly request totals ---------------------------------------------------

monthly_requests <- hmh_network_ts %>%
  filter(!is.na(year_month)) %>%
  count(year_month)

monthly_requests_summary <- monthly_requests %>%
  mutate(
    month_num = lubridate::month(year_month),
    month_label = factor(
      month.abb[month_num],
      levels = month.abb
    )
  ) %>%
  group_by(month_num, month_label) %>%
  summarise(
    mean_requests = mean(n),
    sd_requests = sd(n),
    .groups = "drop"
  ) %>%
  arrange(month_num)

# Monthly request volume ----------------------------------------------------
# This is the main demand-over-time table.

monthly_requests <- hmh_network_ts %>%
  count(year_month, name = "n_requests") %>%
  arrange(year_month) %>%
  mutate(
    rolling_3_month = zoo::rollmean(n_requests, k = 3, fill = NA, align = "right"),
    rolling_12_month = zoo::rollmean(n_requests, k = 12, fill = NA, align = "right")
  )

write_pretty_csv(
  monthly_requests,
  "monthly_request_volume",
  tables_dir
)

# Figure 1: monthly request volume -----------------------------------------

p_monthly_requests <- ggplot(
  monthly_requests_summary,
  aes(x = month_label, y = mean_requests)
) +
  geom_col(fill = "#24787f") +
  geom_errorbar(
    aes(
      ymin = mean_requests - sd_requests,
      ymax = mean_requests + sd_requests
    ),
    width = 0.2
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Average Literature Search Requests by Calendar Month",
    subtitle = "HMH network literature search requests, 2025–2026",
    x = NULL,
    y = "Average requests per month"
  ) +
  theme_minimal(base_size = 13)

ggsave(
  file.path(figures_dir, "monthly_request_volume.png"),
  p_monthly_requests,
  width = 11,
  height = 6,
  dpi = 300
)

# Seasonal month summary ----------------------------------------------------
# This collapses across years to ask:
# Are some calendar months typically busier than others?

monthly_seasonality <- hmh_network_ts %>%
  count(year, month_num, month, name = "n_requests") %>%
  group_by(month_num, month) %>%
  summarize(
    mean_requests = mean(n_requests, na.rm = TRUE),
    median_requests = median(n_requests, na.rm = TRUE),
    min_requests = min(n_requests, na.rm = TRUE),
    max_requests = max(n_requests, na.rm = TRUE),
    n_years_observed = n(),
    .groups = "drop"
  ) %>%
  arrange(month_num)

write_pretty_csv(
  monthly_seasonality,
  "calendar_month_seasonality_summary",
  tables_dir
)

# Figure 2: calendar month pattern -----------------------------------------

p_month_seasonality <- ggplot(
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
    title = "Average Request Volume by Calendar Month",
    subtitle = "Mean monthly requests across all observed years",
    x = NULL,
    y = "Average number of requests"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  file.path(figures_dir, "average_requests_by_calendar_month.png"),
  p_month_seasonality,
  width = 9,
  height = 5,
  dpi = 300
)

# Month-by-year heatmap -----------------------------------------------------
# This is useful because it shows whether "seasonality" is consistent or whether
# specific years are driving the pattern.

month_year_heatmap <- hmh_network_ts %>%
  count(year, month, month_num, name = "n_requests") %>%
  arrange(year, month_num)

write_pretty_csv(
  month_year_heatmap,
  "month_by_year_request_heatmap_data",
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
    title = "Request Volume by Month and Year",
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
  file.path(figures_dir, "request_volume_month_year_heatmap.png"),
  p_month_year_heatmap,
  width = 10,
  height = 7,
  dpi = 300
)

# Requestor group by month --------------------------------------------------
# This asks whether certain requestor groups tend to use the library more at
# specific points in the year.

requestor_monthly <- hmh_network_ts %>%
  filter(!is.na(requestor_group)) %>%
  count(year_month, requestor_group, name = "n_requests") %>%
  group_by(year_month) %>%
  mutate(
    total_month_requests = sum(n_requests),
    prop_requests = n_requests / total_month_requests
  ) %>%
  ungroup()

write_pretty_csv(
  requestor_monthly,
  "monthly_requests_by_requestor_group",
  tables_dir
)

# Keep the most common requestor groups for readable figures.

top_requestor_groups <- hmh_network_ts %>%
  count(requestor_group, sort = TRUE) %>%
  filter(!is.na(requestor_group)) %>%
  slice_head(n = 8) %>%
  pull(requestor_group)

requestor_monthly_top <- requestor_monthly %>%
  filter(requestor_group %in% top_requestor_groups)

# Figure 4: requestor trends over time --------------------------------------

p_requestor_trends <- ggplot(
  requestor_monthly_top,
  aes(x = year_month, y = n_requests)
) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ requestor_group, scales = "free_y") +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  labs(
    title = "Monthly Request Volume by Requestor Group",
    subtitle = "Top requestor groups only; panels use independent y-axes",
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
  file.path(figures_dir, "monthly_request_volume_by_requestor_group.png"),
  p_requestor_trends,
  width = 12,
  height = 8,
  dpi = 300
)

# Figure 5: requestor group seasonal pattern --------------------------------

requestor_seasonality <- hmh_network_ts %>%
  filter(requestor_group %in% top_requestor_groups) %>%
  count(requestor_group, year, month_num, month, name = "n_requests") %>%
  group_by(requestor_group, month_num, month) %>%
  summarize(
    mean_requests = mean(n_requests, na.rm = TRUE),
    .groups = "drop"
  )

write_pretty_csv(
  requestor_seasonality,
  "requestor_group_calendar_month_seasonality",
  tables_dir
)

p_requestor_seasonality <- ggplot(
  requestor_seasonality,
  aes(x = month, y = requestor_group, fill = mean_requests)
) +
  geom_tile(color = "white", linewidth = 1) +
  scale_fill_gradient(low = "#e8f0f4", high = "#24787f") +
  labs(
    title = "Average Monthly Demand by Requestor Group",
    subtitle = "Mean request volume by calendar month across observed years",
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
  file.path(figures_dir, "requestor_group_seasonality_heatmap.png"),
  p_requestor_seasonality,
  width = 11,
  height = 6,
  dpi = 300
)

# Weekday by month operational heatmap --------------------------------------
# This is less about long-term trend and more about scheduling:
# Are requests concentrated on certain weekday/month combinations?

weekday_month <- hmh_network_ts %>%
  filter(!is.na(weekday), !is.na(month)) %>%
  count(month_num, month, weekday, name = "n_requests") %>%
  group_by(month_num, month) %>%
  mutate(prop_month_requests = n_requests / sum(n_requests)) %>%
  ungroup()

write_pretty_csv(
  weekday_month,
  "weekday_by_month_request_pattern",
  tables_dir
)

p_weekday_month <- ggplot(
  weekday_month,
  aes(x = weekday, y = month, fill = n_requests)
) +
  geom_tile(color = "white", linewidth = 1) +
  scale_fill_gradient(low = "#e8f0f4", high = "#24787f") +
  labs(
    title = "Requests by Weekday and Month",
    subtitle = "HMH network data; darker cells indicate higher request volume",
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
  file.path(figures_dir, "weekday_by_month_request_heatmap.png"),
  p_weekday_month,
  width = 8,
  height = 7,
  dpi = 300
)

# Optional simple time-series decomposition ---------------------------------
# STL decomposition works best when there are enough complete monthly values.
# This section fills missing months with zero so that the series is regular.

monthly_complete <- monthly_requests %>%
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
    month_num = month(year_month)
  )

if (nrow(monthly_complete) >= 24) {
  
  request_ts <- ts(
    monthly_complete$n_requests,
    frequency = 12,
    start = c(
      year(min(monthly_complete$year_month)),
      month(min(monthly_complete$year_month))
    )
  )
  
  request_stl <- stl(request_ts, s.window = "periodic")
  
  stl_components <- tibble(
    year_month = monthly_complete$year_month,
    observed = as.numeric(request_stl$time.series[, "remainder"] +
                            request_stl$time.series[, "seasonal"] +
                            request_stl$time.series[, "trend"]),
    seasonal = as.numeric(request_stl$time.series[, "seasonal"]),
    trend = as.numeric(request_stl$time.series[, "trend"]),
    remainder = as.numeric(request_stl$time.series[, "remainder"])
  )
  
  write_pretty_csv(
    stl_components,
    "monthly_request_stl_components",
    tables_dir
  )
  
  p_stl_trend <- ggplot(
    stl_components,
    aes(x = year_month)
  ) +
    geom_line(aes(y = observed), alpha = 0.35) +
    geom_line(aes(y = trend), linewidth = 1.1) +
    scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y"
    ) +
    labs(
      title = "Long-Term Trend in Monthly Request Volume",
      subtitle = "STL trend component overlaid on observed monthly request counts",
      x = NULL,
      y = "Requests"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 15),
      axis.text.x = element_text(face = "bold"),
      axis.text.y = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  ggsave(
    file.path(figures_dir, "monthly_request_stl_trend.png"),
    p_stl_trend,
    width = 11,
    height = 6,
    dpi = 300
  )
}

# Lemma seasonality ---------------------------------------------------------
# This section uses tidy_lemmas_all if available. It asks:
#   Which research topics/terms are more common in specific months?
#
# Important: this is not sentiment analysis. It is topic timing.

if (!is.null(tidy_lemmas_all)) {
  
  lemmas_ts <- tidy_lemmas_all %>%
    clean_names() %>%
    mutate(
      submitted_date = as.Date(submitted_date),
      year_month = floor_date(submitted_date, unit = "month"),
      year = year(submitted_date),
      month_num = month(submitted_date),
      month = month(submitted_date, label = TRUE, abbr = TRUE)
    ) %>%
    filter(
      !is.na(submitted_date),
      !is.na(lemma),
      lemma != ""
    )
  
  # Pick top lemmas overall so the heatmap is readable.
  
  top_lemmas <- lemmas_ts %>%
    count(lemma, sort = TRUE, name = "n_mentions") %>%
    slice_head(n = 20) %>%
    pull(lemma)
  
  lemma_month_summary <- lemmas_ts %>%
    filter(lemma %in% top_lemmas) %>%
    count(lemma, year, month_num, month, name = "n_mentions") %>%
    group_by(lemma, month_num, month) %>%
    summarize(
      mean_mentions = mean(n_mentions, na.rm = TRUE),
      .groups = "drop"
    )
  
  write_pretty_csv(
    lemma_month_summary,
    "top_lemma_calendar_month_seasonality",
    tables_dir
  )
  
  p_lemma_seasonality <- ggplot(
    lemma_month_summary,
    aes(x = month, y = reorder(lemma, mean_mentions), fill = mean_mentions)
  ) +
    geom_tile(color = "white", linewidth = 0.8) +
    scale_fill_gradient(low = "#e8f0f4", high = "#24787f") +
    labs(
      title = "Seasonality of Common Research Topic Lemmas",
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
    file.path(figures_dir, "top_lemma_month_seasonality_heatmap.png"),
    p_lemma_seasonality,
    width = 11,
    height = 8,
    dpi = 300
  )
  
  # Lemma trends over time ---------------------------------------------------
  # This faceted plot is useful for seeing whether a term is:
  #   - consistently common
  #   - increasing
  #   - decreasing
  #   - episodic/spiky
  
  top_lemmas_for_trends <- lemmas_ts %>%
    count(lemma, sort = TRUE, name = "n_mentions") %>%
    slice_head(n = 12) %>%
    pull(lemma)
  
  lemma_monthly_trends <- lemmas_ts %>%
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
    "top_lemma_monthly_trends",
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
      title = "Monthly Trends for Common Research Topic Lemmas",
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
    file.path(figures_dir, "top_lemma_monthly_trends.png"),
    p_lemma_trends,
    width = 12,
    height = 8,
    dpi = 300
  )
}

# Final messages ------------------------------------------------------------

message("HMH network time series analysis complete.")
message("Figures saved to: ", figures_dir)
message("Tables saved to: ", tables_dir)