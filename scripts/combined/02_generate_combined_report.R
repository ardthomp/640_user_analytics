# scripts/combined/02_generate_combined_report.R
# A full, detailed analysis of the combined dataset, faithfully
# restoring all original tables, plots, and analyses.

# --- 1. Setup ---
library(tidyverse)
library(here)
library(lubridate)
library(scales)
library(gt)
library(tidytext)

source(here("scripts", "shared", "paths.R"))
source(here("scripts", "shared", "helpers.R"))
source(here("scripts", "shared", "output_helpers.R"))
source(here("scripts", "shared", "combined_analysis_pipeline.R"))
source(here("scripts", "shared", "plotting_helpers.R"))

# --- 2. Settings and Paths ---
paths <- make_output_paths("combined")

# --- 3. Load Pre-Processed Data ---
analysis_data <- readRDS(here("data", "processed", "combined_analysis_data.rds"))
combined_dat <- analysis_data$all_topics
tidy_lemmas <- analysis_data$all_lemmas
analysis_years <- unique(combined_dat$year)

# --- 4. Generate All Analysis Tables ---
summary_tables <- generate_summary_tables(combined_dat)
purpose_tables <- generate_purpose_tables(combined_dat)
time_tables <- list("Requests Over Time" = combined_dat %>% count(year_month, source_label, name = "n_requests"))
lemma_tables <- list(
  "Top 500 Lemmas" = tidy_lemmas %>% count(lemma, sort = TRUE, name = "n") %>% head(500),
  "All Lemmas" = tidy_lemmas %>% distinct(lemma) %>% arrange(lemma),
  "Lemma Counts Combined" = tidy_lemmas %>% count(lemma, sort = TRUE, name = "n")
)
all_tables <- c(summary_tables, purpose_tables, time_tables, lemma_tables)
all_tables <- all_tables[map_lgl(all_tables, ~ is.data.frame(.x) && nrow(.x) > 0)]

# --- 5. Generate All Plots ---
p_over_time <- combined_dat %>% count(year_month, source_label, name = "n_requests") %>% ggplot(aes(x = year_month, y = n_requests, color = source_label, group = source_label)) + geom_line(linewidth = 1.1) + geom_point(size = 2) + geom_vline(xintercept = as.Date("2026-01-01"), linetype = "dashed", color = "gray40") + annotate("text", x = as.Date("2026-01-01"), y = Inf, label = "Unified Form Begins", vjust = 1.5, hjust = -0.1, size = 3, color = "gray40") + scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + scale_y_continuous(limits = c(0, NA), breaks = pretty_breaks()) + labs(title = "Requests Over Time by Source", x = "Month", y = "Number of Requests", color = "Source") + theme_project()
p_by_campus <- summary_tables$`Requests by Campus` %>% filter(!is.na(campus_affiliation_clean), campus_affiliation_clean != "Unknown/Not specified") %>% ggplot(aes(x = reorder(campus_affiliation_clean, n_requests), y = n_requests)) + geom_col(fill = "#0072B2") + coord_flip() + labs(title = "Total Requests by Campus", x = NULL, y = "Number of Requests") + theme_project()
p_by_requestor <- summary_tables$`Requests by Requestor` %>% filter(!is.na(requestor_category), requestor_category != "Unknown/Not specified") %>% ggplot(aes(x = reorder(requestor_category, n_requests), y = n_requests)) + geom_col(fill = "#0072B2") + coord_flip() + labs(title = "Total Requests by Requestor", x = NULL, y = "Number of Requests") + theme_project()
p_by_purpose <- purpose_tables$`Requests by Purpose` %>% ggplot(aes(x = reorder(purpose_category, n_selections), y = n_selections)) + geom_col(fill = "#009E73") + coord_flip() + labs(title = "Total Selections by Request Purpose", x = NULL, y = "Number of Selections") + theme_project()
p_heatmap <- combined_dat %>% filter(!is.na(hour), !is.na(weekday)) %>% count(weekday, hour, name = "n_requests") %>% ggplot(aes(x = hour, y = fct_rev(weekday), fill = n_requests)) + geom_tile(color = "white", size = 0.5) + scale_fill_viridis_c(option = "plasma", name = "Number of\nRequests") + scale_x_continuous(breaks = seq(0, 23, by = 2), expand = c(0, 0)) + labs(title = "Request Density by Day and Hour", x = "Hour of Day", y = NULL) + theme_project()
top_lemmas_by_requestor <- tidy_lemmas %>% filter(!is.na(requestor_category), requestor_category != "Unknown/Not specified") %>% count(requestor_category, lemma, sort = TRUE) %>% group_by(requestor_category) %>% slice_max(n, n = 10, with_ties = FALSE) %>% ungroup()
p_faceted_lemmas <- top_lemmas_by_requestor %>% ggplot(aes(x = reorder_within(lemma, n, requestor_category), y = n, fill = requestor_category)) + geom_col(show.legend = FALSE) + coord_flip() + facet_wrap(~ requestor_category, scales = "free_y") + scale_x_reordered() + labs(title = "Top 10 Lemmas by Requestor Category", x = NULL, y = "Number of Mentions") + theme_project()

ggsave(file.path(paths$figures_dir, "combined_requests_over_time.png"), p_over_time, width = 12, height = 7, dpi = 300)
ggsave(file.path(paths$figures_dir, "combined_requests_by_campus.png"), p_by_campus, width = 9, height = 6, dpi = 300)
ggsave(file.path(paths$figures_dir, "combined_requests_by_requestor.png"), p_by_requestor, width = 9, height = 7, dpi = 300)
ggsave(file.path(paths$figures_dir, "combined_requests_by_purpose.png"), p_by_purpose, width = 9, height = 6, dpi = 300)
ggsave(file.path(paths$figures_dir, "combined_requests_heatmap.png"), p_heatmap, width = 10, height = 6, dpi = 300)
ggsave(file.path(paths$figures_dir, "combined_top_lemmas_faceted.png"), p_faceted_lemmas, width = 14, height = 9, dpi = 300)

# --- 6. Save All Outputs ---
gt_requests_by_campus <- summary_tables$`Requests by Campus` %>% mutate(prop = n_requests / sum(n_requests)) %>% gt() %>% tab_header(title = "Literature Search Requests by Campus") %>% fmt_percent(columns = prop, decimals = 1)
gtsave(gt_requests_by_campus, file.path(paths$formatted_tables_dir, "combined_requests_by_campus.html"))

purrr::iwalk(all_tables, ~ write_archived_csv(df = .x, filename = janitor::make_clean_names(.y), csv_dir = paths$csv_dir))
write_archived_workbook(tables = all_tables, path = file.path(paths$output_dir, "summary_report.xlsx"))

cat("--- Combined Report Generation Complete ---\n")