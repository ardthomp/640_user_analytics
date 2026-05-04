# scripts/combined/02_generate_combined_report.R
#
# Loads the pre-processed combined dataset and generates the final
# summary report, tables, and figures for the analysis years.

# --- 1. Setup ---
library(tidyverse)
library(here)
library(lubridate)
library(scales)
library(gt)

# Source all shared helpers
source(here("scripts", "shared", "paths.R"))
source(here("scripts", "shared", "helpers.R"))
source(here("scripts", "shared", "output_helpers.R"))
source(here("scripts", "shared", "combined_analysis_pipeline.R"))
source(here("scripts", "shared", "plotting_helpers.R")) # Add this line

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

# --- 3. Generate Analysis Tables ---
summary_tables <- generate_summary_tables(combined_dat)
purpose_tables <- generate_purpose_tables(combined_dat)
lemma_tables <- list(
  "Top Lemmas" = tidy_lemmas %>% count(lemma, sort = TRUE, name = "n") %>% head(500),
  "All Lemmas" = tidy_lemmas %>% distinct(lemma) %>% arrange(lemma)
)
all_tables_to_export <- c(summary_tables, purpose_tables, lemma_tables)
all_tables_to_export <- all_tables_to_export[map_lgl(all_tables_to_export, ~ is.data.frame(.x) && nrow(.x) > 0)]

# --- 4. Generate and Save Plots ---
# This section is now restored and expanded

# Plot 1: Requests over time by source
p_over_time <- combined_dat %>%
  count(year_month, source_label, name = "n_requests") %>%
  ggplot(aes(x = year_month, y = n_requests, color = source_label, group = source_label)) +
  geom_line(linewidth = 1.1) + geom_point(size = 2) +
  geom_vline(xintercept = as.Date("2026-01-01"), linetype = "dashed", color = "gray40") +
  annotate("text", x = as.Date("2026-01-01"), y = Inf, label = "Unified Form Begins", vjust = 1.5, hjust = -0.1, size = 3, color = "gray40") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  scale_y_continuous(limits = c(0, NA), breaks = pretty_breaks()) +
  labs(title = "Literature Search Requests Over Time by Source", subtitle = "Dashed line indicates transition to unified request form", x = "Month", y = "Number of Requests", color = "Source") +
  theme_project()
ggsave(file.path(paths$figures_dir, "requests_over_time_by_source.png"), p_over_time, width = 12, height = 7, dpi = 300)

# Plot 2: Requests by Campus
p_by_campus <- summary_tables$`Requests by Campus` %>%
  filter(!is.na(campus_affiliation_clean), campus_affiliation_clean != "Unknown/Not specified") %>%
  ggplot(aes(x = reorder(campus_affiliation_clean, n_requests), y = n_requests)) +
  geom_col(fill = "#0072B2") + coord_flip() +
  labs(title = "Total Literature Search Requests by Campus", x = NULL, y = "Number of Requests") +
  theme_project()
ggsave(file.path(paths$figures_dir, "requests_by_campus.png"), p_by_campus, width = 9, height = 6, dpi = 300)

# Plot 3: Requests by Requestor
p_by_requestor <- summary_tables$`Requests by Requestor` %>%
  filter(!is.na(requestor_category), requestor_category != "Unknown/Not specified") %>%
  ggplot(aes(x = reorder(requestor_category, n_requests), y = n_requests)) +
  geom_col(fill = "#0072B2") + coord_flip() +
  labs(title = "Total Literature Search Requests by Requestor", x = NULL, y = "Number of Requests") +
  theme_project()
ggsave(file.path(paths$figures_dir, "requests_by_requestor.png"), p_by_requestor, width = 9, height = 7, dpi = 300)

# Plot 4: Requests by Purpose
if ("Requests by Purpose" %in% names(all_tables_to_export)) {
  p_by_purpose <- all_tables_to_export$`Requests by Purpose` %>%
    ggplot(aes(x = reorder(purpose_category, n_selections), y = n_selections)) +
    geom_col(fill = "#0072B2") + coord_flip() +
    labs(title = "Total Selections by Request Purpose", x = NULL, y = "Number of Selections") +
    theme_project()
  ggsave(file.path(paths$figures_dir, "requests_by_purpose.png"), p_by_purpose, width = 9, height = 6, dpi = 300)
}

cat("Figures saved to:", paths$figures_dir, "\n")

# --- 5. Save All Outputs ---
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
cat("Summary workbook, CSVs, and figures written to:", paths$output_dir, "\n")