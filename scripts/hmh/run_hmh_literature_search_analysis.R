# scripts/hmh/run_hmh_literature_search_analysis.R
#
# Standalone analysis of HMH Literature Search requests. This script loads
# the raw HMH data, filters for literature searches, and generates a full
# set of summary tables, plots, and a final Excel report.

# --- 1. Setup ---
library(tidyverse)
library(here)
library(scales)

# Source all shared helpers
source(here("scripts", "shared", "paths.R"))
source(here("scripts", "shared", "helpers.R"))
source(here("scripts", "shared", "transformations.R"))
source(here("scripts", "shared", "output_helpers.R"))
source(here("scripts", "shared", "plotting_helpers.R"))

# Define output paths
paths <- make_output_paths("hmh_literature_searches")

# --- 2. Load and Process Data ---
hmh_dat <- read_csv(hmh_path) %>%
  janitor::clean_names() %>%
  filter(select_question_request_type == "Literature Search") %>%
  mutate(
    submitted_at = parse_timestamp(timestamp),
    year = year(submitted_at),
    campus_affiliation_clean = standardize_campus_name(campus_affiliation),
    requestor_category = standardize_requestor_name(who_requested_this_information),
    purpose = clean_blank(purpose_of_request)
  )

# --- 3. Generate Analysis Tables ---
summary_tables <- list(
  "Requests by Year" = count(hmh_dat, year, name = "n_requests", sort = TRUE),
  "Requests by Campus" = count(hmh_dat, campus_affiliation_clean, name = "n_requests", sort = TRUE),
  "Requests by Requestor" = count(hmh_dat, requestor_category, name = "n_requests", sort = TRUE),
  "Requests by Purpose" = count(hmh_dat, purpose, name = "n_requests", sort = TRUE)
)

# --- 4. Generate and Save Plots ---
# Plot 1: Requests by Campus
p_by_campus <- summary_tables$`Requests by Campus` %>%
  filter(!is.na(campus_affiliation_clean), campus_affiliation_clean != "Unknown/Not specified") %>%
  ggplot(aes(x = reorder(campus_affiliation_clean, n_requests), y = n_requests)) +
  geom_col(fill = "#56B4E9") +
  coord_flip() +
  labs(title = "HMH Literature Searches by Campus", x = NULL, y = "Number of Requests") +
  theme_project()
ggsave(file.path(paths$figures_dir, "hmh_lit_search_by_campus.png"), p_by_campus, width = 9, height = 6, dpi = 300)

# Plot 2: Requests by Requestor
p_by_requestor <- summary_tables$`Requests by Requestor` %>%
  filter(!is.na(requestor_category), requestor_category != "Unknown/Not specified") %>%
  ggplot(aes(x = reorder(requestor_category, n_requests), y = n_requests)) +
  geom_col(fill = "#56B4E9") +
  coord_flip() +
  labs(title = "HMH Literature Searches by Requestor", x = NULL, y = "Number of Requests") +
  theme_project()
ggsave(file.path(paths$figures_dir, "hmh_lit_search_by_requestor.png"), p_by_requestor, width = 9, height = 7, dpi = 300)

cat("Figures saved to:", paths$figures_dir, "\n")

# --- 5. Save All Outputs ---
purrr::iwalk(
  summary_tables,
  ~ write_archived_csv(df = .x, filename = paste0("lit_search_", janitor::make_clean_names(.y)), csv_dir = paths$csv_dir)
)
write_archived_workbook(
  tables = summary_tables,
  path = file.path(paths$output_dir, "hmh_lit_search_summary.xlsx")
)

cat("--- HMH Literature Search Analysis Complete ---\n")
cat("Summary workbook and CSVs written to:", paths$output_dir, "\n")