# scripts/humc/02_generate_humc_tables.R
#
# Generate HUMC analysis tables.
#
# Purpose:
#   Load outputs/humc/out.rds and generate the table objects needed for:
#     1. the HUMC Excel summary workbook
#     2. the HUMC figure script
#     3. optional CSV exports, if export_csvs is set to TRUE
#
# Run after:
#   source("scripts/humc/01_build_humc_analysis_object.R")
#
# Then run:
#   source("scripts/humc/03_generate_humc_figures.R")

# Setup --------------------------------------------------------------------

library(here)
library(tidyverse)
library(janitor)
library(openxlsx)
library(gt)

source(here("scripts", "shared", "paths.R"))
source(here("scripts", "shared", "helpers.R"))
source(here("scripts", "shared", "text_helpers.R"))
source(here("scripts", "shared", "output_helpers.R"))

# Paths --------------------------------------------------------------------

project_name <- "humc"
paths <- make_output_paths(project_name)

output_dir <- paths$output_dir
csv_dir <- paths$csv_dir
formatted_tables_dir <- paths$formatted_tables_dir
out_path <- file.path(output_dir, "out.rds")

dir.create(formatted_tables_dir, recursive = TRUE, showWarnings = FALSE)

# Export settings ----------------------------------------------------------

export_csvs <- FALSE
export_workbook <- TRUE

# Check inputs --------------------------------------------------------------

if (!file.exists(out_path)) {
  stop("Cannot find ", out_path, ". Run 01_build_humc_analysis_object.R first.")
}

# Clear only the outputs this script will recreate -------------------------

if (export_csvs) {
  clear_output_folder(csv_dir, "\\.(csv|rds)$")
}

clear_output_folder(
  output_dir,
  "^summary_report_.*\\.xlsx$|^humc_summary_report\\.xlsx$"
)

summary_filepath <- file.path(output_dir, "humc_summary_report.xlsx")

# Load analysis object -----------------------------------------------------

out <- readRDS(out_path)

# Local analysis helpers ---------------------------------------------------

# Refresh summary tables from the saved HUMC analysis object instead of
# recalculating the full text-processing pipeline.
refresh_top_lemmas <- function(out, top_n = 300) {
  out$lemma_records %>%
    count(lemma, sort = TRUE) %>%
    slice_head(n = top_n)
}

refresh_lemmas_to_categorize <- function(out, top_n = 300) {
  out$lemma_records %>%
    count(lemma, sort = TRUE) %>%
    anti_join(out$categories_map, by = "lemma") %>%
    slice_head(n = top_n)
}

# Refresh summary tables from the saved HUMC analysis object instead of
# recalculating the full text-processing pipeline.
refresh_category_purpose <- function(out) {
  category_by_purpose <- out$lemma_records %>%
    filter(!is.na(category)) %>%
    distinct(request_id, category) %>%
    left_join(out$purpose_long, by = "request_id", relationship = "many-to-many") %>%
    filter(!is.na(purpose)) %>%
    count(purpose, category, sort = TRUE)
  
  category_by_person_purpose <- out$lemma_records %>%
    filter(!is.na(category)) %>%
    distinct(request_id, submitter_type, category) %>%
    left_join(out$purpose_long, by = "request_id", relationship = "many-to-many") %>%
    filter(!is.na(purpose)) %>%
    count(submitter_type, purpose, category, sort = TRUE)
  
  list(
    category_by_purpose = category_by_purpose,
    category_by_person_purpose = category_by_person_purpose
  )
}

# Summarize citation counts by topic category and request purpose.
refresh_citation_summaries <- function(out) {
  purpose_cols <- c(
    "continuing_education",
    "patient_care",
    "lecture",
    "ebp",
    "research",
    "grant",
    "publication",
    "irb_app",
    "admin",
    "policy",
    "patient_info"
  )
  
  request_category_citations <- out$lemma_records %>%
    filter(!is.na(category)) %>%
    distinct(request_id, category, citation_count)
  
  citation_by_category <- request_category_citations %>%
    group_by(category) %>%
    summarize(
      n_requests = n(),
      mean_citations = mean(citation_count, na.rm = TRUE),
      median_citations = median(citation_count, na.rm = TRUE),
      max_citations = suppressWarnings(max(citation_count, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(
      across(
        c(mean_citations, median_citations, max_citations),
        ~ if_else(is.infinite(.x), NA_real_, .x)
      )
    )
  
  citation_by_purpose <- out$humc_request_data %>%
    dplyr::select(request_id, citation_count, all_of(purpose_cols)) %>%
    pivot_longer(
      cols = all_of(purpose_cols),
      names_to = "purpose",
      values_to = "flag"
    ) %>%
    filter(flag == 1) %>%
    group_by(purpose) %>%
    summarize(
      n_requests = n(),
      mean_citations = mean(citation_count, na.rm = TRUE),
      median_citations = median(citation_count, na.rm = TRUE),
      max_citations = suppressWarnings(max(citation_count, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(
      purpose = case_when(
        purpose == "continuing_education" ~ "ContinuingEducation",
        purpose == "patient_care" ~ "PatientCare",
        purpose == "irb_app" ~ "IRBApp",
        purpose == "patient_info" ~ "PatientInfo",
        purpose == "ebp" ~ "EBP",
        TRUE ~ str_to_title(purpose)
      ),
      across(
        c(mean_citations, median_citations, max_citations),
        ~ if_else(is.infinite(.x), NA_real_, .x)
      )
    )
  
  list(
    citation_by_category = citation_by_category,
    citation_by_purpose = citation_by_purpose
  )
}

# Run chi-squared tests safely and return standardized residuals plus
# Cramer's V effect size.
safe_chisq_analysis <- function(table_matrix) {
  if (nrow(table_matrix) < 2 || ncol(table_matrix) < 2 || sum(table_matrix) == 0) {
    return(list(
      chi_result = NA,
      residuals = tibble(),
      cramers_v = NA_real_
    ))
  }
  
  chi_result <- suppressWarnings(chisq.test(table_matrix))
  
  residuals <- as.data.frame(chi_result$stdres) %>%
    rownames_to_column("row_name") %>%
    pivot_longer(
      cols = -row_name,
      names_to = "column_name",
      values_to = "std_residual"
    )
  
  n <- sum(table_matrix)
  r <- nrow(table_matrix)
  c <- ncol(table_matrix)
  
  cramers_v <- sqrt(
    as.numeric(chi_result$statistic) / (n * min(r - 1, c - 1))
  )
  
  list(
    chi_result = chi_result,
    residuals = residuals,
    cramers_v = cramers_v
  )
}

# Format chi-squared results for workbook export and HTML display.
format_chi_square_summary <- function(chi_result, cramers_v, analysis_name) {
  if (length(chi_result) == 1 && is.na(chi_result)) {
    return(tibble(
      analysis = analysis_name,
      chi_square = NA_real_,
      df = NA_real_,
      p_value = NA_character_,
      cramers_v = cramers_v
    ))
  }
  
  tibble(
    analysis = analysis_name,
    chi_square = as.numeric(chi_result$statistic),
    df = as.numeric(chi_result$parameter),
    p_value_raw = chi_result$p.value,
    cramers_v = cramers_v
  ) %>%
    mutate(
      chi_square = round(chi_square, 2),
      df = round(df, 0),
      p_value = case_when(
        p_value_raw < 0.001 ~ "<0.001",
        TRUE ~ as.character(round(p_value_raw, 3))
      ),
      cramers_v = round(cramers_v, 3)
    ) %>%
    dplyr::select(
      analysis,
      chi_square,
      df,
      p_value,
      cramers_v
    )
}

# Test whether categorized research topics vary by submitter type.
refresh_category_chi <- function(out) {
  lemma_records_chi <- out$lemma_records %>%
    mutate(
      submitter_type_chi = case_when(
        submitter_type == "Committee" ~ "OtherProvider",
        TRUE ~ submitter_type
      )
    )
  
  submitter_counts <- out$lemma_records %>%
    distinct(request_id, submitter_type) %>%
    count(submitter_type)
  
  chi_table <- lemma_records_chi %>%
    filter(!is.na(category)) %>%
    distinct(request_id, submitter_type_chi, category) %>%
    count(submitter_type_chi, category) %>%
    pivot_wider(
      names_from = category,
      values_from = n,
      values_fill = 0
    )
  
  if (nrow(chi_table) == 0) {
    return(list(
      submitter_counts = submitter_counts,
      chi_table = matrix(numeric(0), nrow = 0),
      chi_result = NA,
      chi_residuals = tibble(),
      cramers_v = NA_real_
    ))
  }
  
  chi_matrix <- chi_table %>%
    column_to_rownames("submitter_type_chi") %>%
    as.matrix()
  
  chi <- safe_chisq_analysis(chi_matrix)
  
  chi_residuals <- chi$residuals %>%
    rename(
      submitter_type = row_name,
      category = column_name
    )
  
  list(
    submitter_counts = submitter_counts,
    chi_table = chi_matrix,
    chi_result = chi$chi_result,
    chi_residuals = chi_residuals,
    cramers_v = chi$cramers_v
  )
}

# Test whether request purposes vary by submitter type.
refresh_purpose_analysis <- function(out) {
  humc_request_data_purpose <- out$humc_request_data %>%
    mutate(
      submitter_type = if_else(
        submitter_type == "Committee",
        "OtherProvider",
        submitter_type
      )
    )
  
  purpose_counts <- out$purpose_long %>%
    count(purpose, sort = TRUE)
  
  purpose_by_person <- out$purpose_long %>%
    left_join(
      humc_request_data_purpose %>% dplyr::select(request_id, submitter_type),
      by = "request_id"
    ) %>%
    count(submitter_type, purpose, sort = TRUE)
  
  purpose_table <- purpose_by_person %>%
    pivot_wider(
      names_from = purpose,
      values_from = n,
      values_fill = 0
    )
  
  if (nrow(purpose_table) == 0) {
    return(list(
      purpose_counts = purpose_counts,
      purpose_by_person = purpose_by_person,
      purpose_table = matrix(numeric(0), nrow = 0),
      purpose_chi = NA,
      purpose_residuals = tibble(),
      cramers_v_purpose = NA_real_
    ))
  }
  
  purpose_matrix <- purpose_table %>%
    column_to_rownames("submitter_type") %>%
    as.matrix()
  
  chi <- safe_chisq_analysis(purpose_matrix)
  
  purpose_residuals <- chi$residuals %>%
    rename(
      submitter_type = row_name,
      purpose = column_name
    )
  
  list(
    purpose_counts = purpose_counts,
    purpose_by_person = purpose_by_person,
    purpose_table = purpose_matrix,
    purpose_chi = chi$chi_result,
    purpose_residuals = purpose_residuals,
    cramers_v_purpose = chi$cramers_v
  )
}

refresh_phrase_candidates <- function(out, min_n = 3) {
  if (!is.null(out$phrase_results)) {
    return(out$phrase_results)
  }
  
  list(
    phrase_candidates = tibble(),
    phrases_to_add = tibble()
  )
}

# Refresh analysis objects -------------------------------------------------

lemmas_to_categorize <- refresh_lemmas_to_categorize(out, top_n = 300)
cat_purp <- refresh_category_purpose(out)
citation_results <- refresh_citation_summaries(out)
category_chi_results <- refresh_category_chi(out)
purpose_results <- refresh_purpose_analysis(out)
phrase_results <- refresh_phrase_candidates(out, min_n = 3)
top_lemmas <- refresh_top_lemmas(out, top_n = 300)

category_chi_summary <- format_chi_square_summary(
  chi_result = category_chi_results$chi_result,
  cramers_v = category_chi_results$cramers_v,
  analysis_name = "Request Category by Submitter Type"
)

purpose_chi_summary <- format_chi_square_summary(
  chi_result = purpose_results$purpose_chi,
  cramers_v = purpose_results$cramers_v_purpose,
  analysis_name = "Request Purpose by Submitter Type"
)

chi_square_summary <- bind_rows(
  category_chi_summary,
  purpose_chi_summary
)

# Save a small formatted HTML table for the final write-up.
# The write-up only uses the purpose-by-submitter test.
chi_square_summary_gt <- chi_square_summary %>%
  filter(analysis == "Request Purpose by Submitter Type") %>%
  gt() %>%
  tab_header(
    title = "Chi-Square Test Summary",
    subtitle = "HUMC request purpose by submitter type"
  ) %>%
  cols_label(
    analysis = "Analysis",
    chi_square = "Chi-square",
    df = "df",
    p_value = "p-value",
    cramers_v = "Cramer's V"
  )

gt::gtsave(
  chi_square_summary_gt,
  file.path(formatted_tables_dir, "chi_square_summary.html")
)

# Required RDS files for figure script -------------------------------------
#
# These stay on even when export_csvs = FALSE because 03_generate_figures.R
# needs them.

saveRDS(cat_purp, file.path(csv_dir, "cat_purp.rds"))
saveRDS(category_chi_results, file.path(csv_dir, "category_chi_results.rds"))
saveRDS(purpose_results, file.path(csv_dir, "purpose_results.rds"))

# Optional CSV/RDS exports -------------------------------------------------

if (export_csvs) {
  saveRDS(lemmas_to_categorize, file.path(csv_dir, "lemmas_to_categorize.rds"))
  saveRDS(citation_results, file.path(csv_dir, "citation_results.rds"))
  saveRDS(phrase_results, file.path(csv_dir, "phrase_results.rds"))
  saveRDS(top_lemmas, file.path(csv_dir, "top_lemmas.rds"))
}


# Create summary workbook --------------------------------------------------

tables_to_export <- list(
  "Top Lemmas" = top_lemmas,
  "Top 500 Lemmas" = out$top_500_lemmas,
  "Lemmas to Categorize" = lemmas_to_categorize,
  "Lemma Counts" = out$lemma_counts,
  "Lemma Counts by Submitter" = out$lemma_counts_by_person,
  "Category Counts" = out$category_counts,
  "Category by Submitter" = out$category_by_person,
  "Category by Purpose" = cat_purp$category_by_purpose,
  "Category by Submitter Purpose" = cat_purp$category_by_person_purpose,
  "Citation by Category" = citation_results$citation_by_category,
  "Citation by Purpose" = citation_results$citation_by_purpose,
  "Submitter Counts" = category_chi_results$submitter_counts,
  "Category Chi Residuals" = category_chi_results$chi_residuals,
  "Purpose Counts" = purpose_results$purpose_counts,
  "Purpose by Submitter" = purpose_results$purpose_by_person,
  "Purpose Chi Residuals" = purpose_results$purpose_residuals,
  "Phrase Candidates" = phrase_results$phrase_candidates,
  "Phrases to Add" = phrase_results$phrases_to_add,
  "Bigram Candidates" = out$bigram_candidates,
  "Chi Square Summary" = chi_square_summary,
  "Trigram Candidates" = out$trigram_candidates
)

tables_to_export_clean <- tables_to_export[
  purrr::map_lgl(tables_to_export, ~ is.data.frame(.x) && ncol(.x) > 0)
]

# Write summary workbook ---------------------------------------------------

if (export_workbook) {
  write_pretty_workbook(
    tables = tables_to_export_clean,
    path = summary_filepath
  )
}

# Print key stats ----------------------------------------------------------

message("\nCategory chi-square result:")
print(category_chi_results$chi_result)

message("\nCategory Cramer's V:")
print(category_chi_results$cramers_v)

message("\nPurpose chi-square result:")
print(purpose_results$purpose_chi)

message("\nPurpose Cramer's V:")
print(purpose_results$cramers_v_purpose)

message("\nRequired RDS files saved to: ", csv_dir)

if (export_csvs && length(tables_to_export_clean) > 0) {
  message("Writing CSV exports...")
  
  purrr::iwalk(
    tables_to_export_clean,
    ~ write_pretty_csv(
      df = .x,
      filename = janitor::make_clean_names(.y),
      csv_dir = csv_dir
    )
  )
  
} else {
  message("CSV export skipped (either disabled or no valid tables).")
}

if (export_workbook) {
  message("Summary report saved to: ", summary_filepath)
} else {
  message("Workbook export skipped. Set export_workbook <- TRUE to write workbook.")
}
