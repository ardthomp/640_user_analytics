# scripts/humc/run_humc_analysis.R
#
# Master script for the standalone analysis of the processed HUMC dataset.

# --- 1. Setup ---
library(tidyverse)
library(here)
library(scales)

# Source all shared helpers
source(here("scripts", "shared", "paths.R"))
source(here("scripts", "shared", "helpers.R"))
source(here("scripts", "shared", "transformations.R"))
source(here("scripts", "shared", "output_helpers.R"))
source(here("scripts", "shared", "plotting_helpers.R")) # Add this line

# Define output paths
paths <- make_output_paths("humc")

# --- 2. Load and Process Data ---
humc_dat <- read_csv(humc_path)

humc_processed <- humc_dat %>%
  mutate(
    year = year(date),
    year_month = floor_date(date, "month"),
    weekday = wday(date, label = TRUE, abbr = FALSE),
    campus_affiliation_clean = make_campus_affiliation(campus_affiliation, humc, carrier, jfk, palisades, network),
    requestor_category = make_requestor_string(attending, med_ed, nurse, other_provider, committee, consumer_health),
    purpose = make_purpose_string(continuing_education, patient_care, lecture, ebp, research, grant, publication, irb_app, admin, policy, patient_info)
  ) %>%
  mutate(
    campus_affiliation_clean = standardize_campus_name(campus_affiliation_clean),
    requestor_category = standardize_requestor_name(requestor_category)
  )

# --- 3. Generate Analysis Tables ---
summary_tables <- list(
  "Requests by Year" = count(humc_processed, year, name = "n_requests", sort = TRUE),
  "Requests by Campus" = count(humc_processed, campus_affiliation_clean, name = "n_requests", sort = TRUE),
  "Requests by Requestor" = count(humc_processed, requestor_category, name = "n_requests", sort = TRUE)
)

purpose_tables <- if (sum(!is.na(humc_processed$purpose)) > 0) {
  tidy_purposes <- humc_processed %>%
    filter(!is.na(purpose), purpose != "") %>%
    separate_rows(purpose, sep = ", ") %>%
    mutate(purpose_category = str_trim(purpose))
  list("Requests by Purpose" = count(tidy_purposes, purpose_category, name = "n_selections", sort = TRUE))
} else { list() }

all_tables_to_export <- c(summary_tables, purpose_tables)

# --- 4. Generate and Save Plots ---
# This section is now restored

# Plot 1: Requests by Year
p_by_year <- summary_tables$`Requests by Year` %>%
  ggplot(aes(x = as.factor(year), y = n_requests)) +
  geom_col(fill = "#0072B2") +
  labs(title = "HUMC Literature Search Requests by Year", x = "Year", y = "Number of Requests") +
  theme_project()
ggsave(file.path(paths$figures_dir, "humc_requests_by_year.png"), p_by_year, width = 8, height = 5, dpi = 300)

# Plot 2: Requests by Campus
p_by_campus <- summary_tables$`Requests by Campus` %>%
  filter(!is.na(campus_affiliation_clean), campus_affiliation_clean != "Unknown/Not specified") %>%
  ggplot(aes(x = reorder(campus_affiliation_clean, n_requests), y = n_requests)) +
  geom_col(fill = "#0072B2") +
  coord_flip() +
  labs(title = "HUMC Requests by Campus", x = NULL, y = "Number of Requests") +
  theme_project()
ggsave(file.path(paths$figures_dir, "humc_requests_by_campus.png"), p_by_campus, width = 9, height = 6, dpi = 300)

# Plot 3: Requests by Requestor
p_by_requestor <- summary_tables$`Requests by Requestor` %>%
  filter(!is.na(requestor_category), requestor_category != "Unknown/Not specified") %>%
  ggplot(aes(x = reorder(requestor_category, n_requests), y = n_requests)) +
  geom_col(fill = "#0072B2") +
  coord_flip() +
  labs(title = "HUMC Requests by Requestor Category", x = NULL, y = "Number of Requests") +
  theme_project()
ggsave(file.path(paths$figures_dir, "humc_requests_by_requestor.png"), p_by_requestor, width = 9, height = 7, dpi = 300)

cat("Figures saved to:", paths$figures_dir, "\n")

# --- 5. Save All Outputs ---
purrr::iwalk(
  all_tables_to_export,
  ~ write_archived_csv(df = .x, filename = janitor::make_clean_names(.y), csv_dir = paths$csv_dir)
)
write_archived_workbook(
  tables = all_tables_to_export,
  path = file.path(paths$output_dir, "humc_summary_report.xlsx")
)

cat("--- HUMC Analysis Complete ---\n")
cat("Summary workbook, CSVs, and figures written to:", paths$output_dir, "\n")