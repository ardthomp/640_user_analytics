# scripts/shared/combined_analysis_pipeline.R
#
# Contains the core data loading, processing, and analysis functions for the
# combined HUMC/HMH analysis.

library(tidyverse)
library(janitor)
library(lubridate)
library(scales)
library(gt)

# --- Data Loading and Wrangling Functions ---

load_and_process_humc <- function(path, analysis_years) {
  read_csv(path, show_col_types = FALSE) %>%
    clean_names() %>%
    mutate(
      source_file_type = "humc",
      source_label = "HUMC legacy form",
      submitted_date = as.Date(date),
      year = year(submitted_date),
      campus_affiliation_clean = make_campus_affiliation(
        campus_affiliation = clean_blank(campus_affiliation),
        humc = flag_to_binary(humc), carrier = flag_to_binary(carrier),
        jfk = flag_to_binary(jfk), palisades = flag_to_binary(palisades),
        network = flag_to_binary(network)
      ),
      requestor_category = pmap_chr(
        list(attending, med_ed, nurse, other_provider, committee, consumer_health),
        ~ make_requestor_string(..1, ..2, ..3, ..4, ..5, ..6)
      ),
      purpose = pmap_chr(
        list(continuing_education, patient_care, lecture, ebp, research, grant, publication, irb_app, admin, policy, patient_info),
        ~ make_purpose_string(..1, ..2, ..3, ..4, ..5, ..6, ..7, ..8, ..9, ..10, ..11)
      ),
      research_topic = clean_blank(topic),
      n_searches = NA_real_,
      citation_count = suppressWarnings(as.numeric(citation_count))
    ) %>%
    filter(year %in% analysis_years)
}

load_and_process_hmh <- function(path, analysis_years) {
  read_csv(path, show_col_types = FALSE) %>%
    clean_names() %>%
    filter(select_question_request_type == "Literature Search") %>%
    mutate(
      source_file_type = "hmh",
      source_label = "HMH shared form",
      submitted_at = parse_timestamp(timestamp),
      submitted_date = as.Date(submitted_at),
      year = year(submitted_at),
      campus_affiliation_clean = standardize_campus_name(clean_blank(campus_affiliation)),
      requestor_category = standardize_requestor_name(clean_blank(who_requested_this_information)),
      purpose = clean_blank(purpose_of_request),
      research_topic = clean_blank(research_topic),
      n_searches = suppressWarnings(as.numeric(number_of_literature_searches)),
      citation_count = NA_real_
    ) %>%
    filter(year %in% analysis_years)
}

combine_and_finalize_data <- function(humc_df, hmh_df) {
  bind_rows(humc_df, hmh_df) %>%
    mutate(
      request_id = row_number(),
      year = year(submitted_date),
      month_num = month(submitted_date),
      month = month(submitted_date, label = TRUE, abbr = TRUE),
      year_month = floor_date(submitted_date, "month"),
      weekday = wday(submitted_date, label = TRUE, abbr = FALSE),
      hour = if_else(!is.na(submitted_at), hour(submitted_at), NA_integer_),
      campus_affiliation_clean = standardize_campus_name(campus_affiliation_clean),
      requestor_category = standardize_requestor_name(requestor_category)
    )
}

# --- Analysis and Table Generation ---

generate_summary_tables <- function(combined_df) {
  list(
    "Requests by Campus" = count(combined_df, campus_affiliation_clean, name = "n_requests", sort = TRUE),
    "Requests by Requestor" = count(combined_df, requestor_category, name = "n_requests", sort = TRUE),
    "Requests by Year" = count(combined_df, year, name = "n_requests", sort = TRUE),
    "Requests by Month Total" = count(combined_df, year_month, name = "n_requests"),
    "Requests by Weekday" = count(combined_df, weekday, name = "n_requests"),
    "Requests by Hour" = count(combined_df, hour, name = "n_requests"),
    "Searches by Year" = combined_df %>%
      group_by(year) %>%
      summarize(
        n_requests = n(),
        mean_n_searches = mean(n_searches, na.rm = TRUE),
        median_n_searches = median(n_searches, na.rm = TRUE),
        .groups = "drop"
      )
    # Add other summary tables here as needed
  )
}

# --- Plotting Functions ---

generate_plots <- function(combined_df, figures_dir) {
  
  # Plot 1: Requests over time
  p_over_time <- combined_df %>%
    count(year_month, source_label, name = "n_requests") %>%
    ggplot(aes(x = year_month, y = n_requests, color = source_label, group = source_label)) +
    geom_line(linewidth = 1.2) + geom_point(size = 2.5) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(title = "Literature Search Requests Over Time", x = "Month", y = "Number of Requests", color = "Source") +
    theme_minimal()
  
  ggsave(file.path(figures_dir, "combined_requests_over_time.png"), p_over_time, width = 12, height = 7, dpi = 300)
  
  # Plot 2: Requests by campus
  p_by_campus <- combined_df %>%
    count(campus_affiliation_clean, name = "n_requests") %>%
    filter(!is.na(campus_affiliation_clean)) %>%
    ggplot(aes(x = reorder(campus_affiliation_clean, n_requests), y = n_requests)) +
    geom_col() + coord_flip() +
    labs(title = "Literature Search Requests by Campus", x = "Campus", y = "Number of Requests") +
    theme_minimal()
  
  ggsave(file.path(figures_dir, "combined_requests_by_campus.png"), p_by_campus, width = 9, height = 6, dpi = 300)
  
  # Add other plotting code here, converting each plot into a small, self-contained block
}