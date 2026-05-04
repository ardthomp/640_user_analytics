# Run full literature request analysis pipeline --------------------

library(here)

cat("\nStarting full analysis pipeline...\n")

cat("\n1. Running pipeline...\n")
source(here("scripts", "humc", "02_run_pipeline.R"))

cat("\n2. Generating analysis tables...\n")
source(here("scripts", "humc", "03_analysis_tables.R"))

cat("\n3. Generating figures...\n")
source(here("scripts", "humc", "04_figures.R"))

cat("\nPipeline complete.\n")
cat("Outputs saved to:\n")
cat(" - outputs/humc/out.rds\n")
cat(" - outputs/humc/csv\n")
cat(" - outputs/humc/figures\n")