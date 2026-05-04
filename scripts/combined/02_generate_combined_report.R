# 02_generate_combined_report.R
#
# Loads the pre-processed combined dataset and generates a final, comprehensive
# summary report, including detailed tables, all plots, and formatted outputs.

# --- 1. Setup ---
library(tidyverse)
library(here)
library(lubridate)
library(scales)
library(gt)
library(tidytext)

# Source all shared helpers
source(here("scripts", "shared", "paths.R"))
source(here("scripts", "shared", "helpers.R"))
source(here("scripts", "shared", "output_helpers.R"))
source(here("scripts", "shared", "combined_analysis_pipeline.R"))
source(here("scripts", "shared", "plotting_helpers.R"))

# Define output paths
paths <- make_output_paths("combined")

# Define output paths
paths <- make_output_paths("combined")

# --- 2. Load Pre-Processed Data ---
analysis_data <- readRDS(here("data", "processed", "combined_analysis_data.rds"))
combined_dat <- analysis_data$all_topics
tidy_lemmas <- analysis_data$all_lemmas

analysis_years <- unique(combined_dat$year)
cat("--- Report Generation Started ---\n")
cat("Loaded pre-processed data with", nrow(combined_dat), "records.\n")
cat("Analyzing data for years:", paste(analysis_years, collapse = ", "), "\n")

# --- 3. Generate All Analysis Tables ---
summary_tables <- generate_summary_tables(combined_dat)
purpose_tables <- generate_purpose_tables(combined_dat) # This now generates all purpose tables
time_tables <- list("Requests Over Time" = combined_dat %>% count(year_month, source_label, name = "n_requests"))
lemma_tables <- list(
  "Top 500 Lemmas" = tidy_lemmas %>% count(lemma, sort = TRUE, name = "n") %>% head(500),
  "All Lemmas" = tidy_lemmas %>% distinct(lemma) %>% arrange(lemma),
  "Lemma Counts Combined" = tidy_lemmas %>% count(lemma, sort = TRUE, name = "n")
)
all_tables_to_export <- c(summary_tables, purpose_tables, time_tables, lemma_tables)
all_tables_to_export <- all_tables_to_export[map_lgl(all_tables_to_export, ~ is.data.frame(.x) && nrow(.x) > 0)]

# --- 4. Generate and Save All Plots ---
generate_and_save_plots(combined_dat, paths$figures_dir)

# Specific time-based plots
current_year <- max(analysis_years)
previous_year <- current_year - 1
latest_month_in_current_year <- combined_dat %>% filter(year == current_year) %>% summarize(max_month = max(month_num, na.rm = TRUE)) %>% pull(max_month)
requests_per_month_matched <- combined_dat %>%
  filter(year %in% c(previous_year, current_year), month_num <= latest_month_in_current_year) %>%
  count(year, month_num, month, name = "n_requests") %>%
  mutate(year = factor(year), month_label = factor(month.abb[month_num], levels = month.abb[1:latest_month_in_current_year]))
p_matched <- ggplot(requests_per_month_matched, aes(x = month_label, y = n_requests, color = year, group = year)) +
  geom_line(linewidth = 1.1) + geom_point(size = 2.5) + scale_y_continuous(limits = c(0, NA)) +
  labs(title = "Monthly Requests, Matched Months Comparison", subtitle = paste0("Comparison of Jan–", month.abb[latest_month_in_current_year], " for ", previous_year, " and ", current_year), x = "Month", y = "Number of Requests", color = "Year") +
  theme_project()
ggsave(file.path(paths$figures_dir, "requests_per_month_matched.png"), p_matched, width = 8, height = 5, dpi = 300)

p_full <- summary_tables$`Requests by Month Total` %>%
  ggplot(aes(x = year_month, y = n_requests)) +
  geom_line() + geom_point() +
  geom_vline(xintercept = as.Date("2026-01-01"), linetype = "dashed", color = "gray40") +
  annotate("text", x = as.Date("2026-01-01"), y = Inf, label = "Unified Form Begins", vjust = 1.5, hjust = -0.1, size = 3, color = "gray40") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + scale_y_continuous(limits = c(0, NA)) +
  labs(title = "Monthly Literature Search Requests, Full Trend", x = "Month", y = "Number of Requests") +
  theme_project()
ggsave(file.path(paths$figures_dir, "requests_per_month_full.png"), p_full, width = 9, height = 5, dpi = 300)

# Faceted lemma bar chart
top_lemmas_by_requestor <- tidy_lemmas %>%
  filter(!is.na(requestor_category), requestor_category != "Unknown/Not specified") %>%
  count(requestor_category, lemma, sort = TRUE) %>%
  group_by(requestor_category) %>%
  slice_max(n, n = 10, with_ties = FALSE) %>%
  ungroup()

p_faceted_lemmas <- top_lemmas_by_requestor %>%
  ggplot(aes(x = reorder_within(lemma, n, requestor_category), y = n, fill = requestor_category)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ requestor_category, scales = "free_y") +
  scale_x_reordered() +
  labs(title = "Top 10 Most Frequent Lemmas by Requestor Category", x = NULL, y = "Number of Mentions") +
  theme_project() +
  theme(strip.text = element_text(face = "bold"))
ggsave(file.path(paths$figures_dir, "top_lemmas_by_requestor_faceted.png"), p_faceted_lemmas, width = 14, height = 9, dpi = 300)

# Plot for Requests by Purpose
if ("Requests by Purpose" %in% names(all_tables_to_export)) {
  p_by_purpose <- all_tables_to_export$`Requests by Purpose` %>%
    ggplot(aes(x = reorder(purpose_category, n_selections), y = n_selections)) +
    geom_col(fill = "#009E73") +
    coord_flip() +
    labs(title = "Total Selections by Request Purpose", x = NULL, y = "Number of Selections") +
    theme_project()
  ggsave(file.path(paths$figures_dir, "combined_requests_by_purpose.png"), p_by_purpose, width = 9, height = 6, dpi = 300)
}

# (Restored) Timestamp Heatmap
requests_by_hour_weekday <- combined_dat %>%
  filter(!is.na(hour), !is.na(weekday)) %>%
  count(weekday, hour, name = "n_requests")

p_heatmap <- requests_by_hour_weekday %>%
  ggplot(aes(x = hour, y = fct_rev(weekday), fill = n_requests)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_viridis_c(option = "plasma", name = "Number of\nRequests") +
  scale_x_continuous(breaks = seq(0, 23, by = 2), expand = c(0, 0)) +
  labs(title = "Request Density by Day and Hour", x = "Hour of Day (24-hour format)", y = NULL) +
  theme_project() +
  theme(legend.position = "right")
ggsave(file.path(paths$figures_dir, "requests_heatmap_by_hour.png"), p_heatmap, width = 10, height = 6, dpi = 300)

cat("All figures saved to:", paths$figures_dir, "\n")

# --- 5. Save All Data Outputs ---
gt_requests_by_campus <- summary_tables$`Requests by Campus` %>%
  mutate(prop = n_requests / sum(n_requests)) %>%
  gt() %>%
  tab_header(title = "Literature Search Requests by Campus") %>%
  fmt_percent(columns = prop, decimals = 1)
gtsave(gt_requests_by_campus, file.path(paths$formatted_tables_dir, "combined_requests_by_campus.html"))
cat("Formatted HTML table saved to:", paths$formatted_tables_dir, "\n")

purrr::iwalk(
  all_tables_to_export,
  ~ write_archived_csv(df = .x, filename = janitor::make_clean_names(.y), csv_dir = paths$csv_dir)
)
write_archived_workbook(
  tables = all_tables_to_export,
  path = file.path(paths$output_dir, "summary_report.xlsx")
)

# --- 6. Console Summary ---
cat("--- Report Generation Complete ---\n")
cat("Summary workbook, all CSVs, plots, and HTML tables written to:", paths$output_dir, "\n")