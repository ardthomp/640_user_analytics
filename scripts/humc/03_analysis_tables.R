# I used AI to help me write this code
# Analysis tables --------------------------------------------------

source(here::here("scripts", "humc", "01_setup_functions.R"))

out <- readRDS(out_path)

# Timestamp --------------------------------------------------------

run_timestamp <- format(Sys.time(), "%Y_%m_%d-%H-%M-%S")

# Archive old CSV/RDS files ---------------------------------------

archive_existing_files(
  source_dir = csv_dir,
  pattern = "\\.(csv|rds)$",
  archive_parent_dir = old_csv_dir,
  timestamp = run_timestamp
)

# Archive old summary reports -------------------------------------

archive_existing_files(
  source_dir = output_dir,
  pattern = "^summary_report_.*\\.xlsx$",
  archive_parent_dir = old_summaries_dir,
  timestamp = run_timestamp
)

# Summary report path ---------------------------------------------

summary_filename <- paste0("summary_report_", run_timestamp, ".xlsx")
summary_filepath <- file.path(output_dir, summary_filename)

# Refresh analysis objects ----------------------------------------

lemmas_to_categorize <- refresh_lemmas_to_categorize(out, top_n = 300)
cat_purp <- refresh_category_purpose(out)
citation_results <- refresh_citation_summaries(out)
category_chi_results <- refresh_category_chi(out)
purpose_results <- refresh_purpose_analysis(out)
phrase_results <- refresh_phrase_candidates(out, min_n = 3)
top_lemmas <- refresh_top_lemmas(out, top_n = 300)

# Export pretty CSV tables ----------------------------------------

write_pretty_csv(lemmas_to_categorize, "lemmas_to_categorize", csv_dir)
write_pretty_csv(top_lemmas, "top_lemmas", csv_dir)
write_pretty_csv(out$top_500_lemmas, "top_500_lemmas", csv_dir)
write_pretty_csv(out$lemma_counts, "lemma_counts", csv_dir)
write_pretty_csv(out$lemma_counts_by_person, "lemma_counts_by_submitter", csv_dir)
write_pretty_csv(out$category_counts, "category_counts", csv_dir)
write_pretty_csv(out$category_by_person, "category_by_submitter", csv_dir)
write_pretty_csv(cat_purp$category_by_purpose, "category_by_purpose", csv_dir)
write_pretty_csv(cat_purp$category_by_person_purpose, "category_by_submitter_purpose", csv_dir)
write_pretty_csv(citation_results$citation_by_category, "citation_by_category", csv_dir)
write_pretty_csv(citation_results$citation_by_purpose, "citation_by_purpose", csv_dir)
write_pretty_csv(category_chi_results$submitter_counts, "submitter_counts", csv_dir)
write_pretty_csv(category_chi_results$chi_residuals, "category_chi_residuals", csv_dir)
write_pretty_csv(purpose_results$purpose_counts, "purpose_counts", csv_dir)
write_pretty_csv(purpose_results$purpose_by_person, "purpose_by_submitter", csv_dir)
write_pretty_csv(purpose_results$purpose_residuals, "purpose_chi_residuals", csv_dir)
write_pretty_csv(phrase_results$phrase_candidates, "pmi_phrase_candidates", csv_dir)
write_pretty_csv(phrase_results$phrases_to_add, "phrases_to_add", csv_dir)
write_pretty_csv(out$bigram_candidates, "bigram_candidates", csv_dir)
write_pretty_csv(out$trigram_candidates, "trigram_candidates", csv_dir)

# Save RDS objects -------------------------------------------------

saveRDS(lemmas_to_categorize, file.path(csv_dir, "lemmas_to_categorize.rds"))
saveRDS(cat_purp, file.path(csv_dir, "cat_purp.rds"))
saveRDS(citation_results, file.path(csv_dir, "citation_results.rds"))
saveRDS(category_chi_results, file.path(csv_dir, "category_chi_results.rds"))
saveRDS(purpose_results, file.path(csv_dir, "purpose_results.rds"))
saveRDS(phrase_results, file.path(csv_dir, "phrase_results.rds"))
saveRDS(top_lemmas, file.path(csv_dir, "top_lemmas.rds"))

# Create summary workbook -----------------------------------------

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
  "Trigram Candidates" = out$trigram_candidates
)

tables_to_export_clean <- tables_to_export[
  purrr::map_lgl(tables_to_export, ~ is.data.frame(.x) && ncol(.x) > 0)
]

# Write summary workbook ------------------------------------------

write_pretty_workbook(
  tables = tables_to_export_clean,
  path = summary_filepath
)

# Print key stats --------------------------------------------------

category_chi_results$chi_result
category_chi_results$cramers_v

purpose_results$purpose_chi
purpose_results$cramers_v_purpose

message("CSV/RDS files saved to: ", csv_dir)
message("Summary report saved to: ", summary_filepath)