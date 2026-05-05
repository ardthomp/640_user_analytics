# Hospital Library User Analytics

End-to-end analytics pipeline for hospital library services, combining structured usage data with text mining to evaluate demand, workload, and research trends.

---

## Overview

This project analyzes literature search and article request activity across a large hospital network, with a focus on its flagship campus and system-wide usage patterns.

The pipeline:

- Combines multi-year flagship legacy logs (2013–2025)
- Integrates network shared-form data (2025–present)
- Standardizes request metadata (date, campus, requestor, purpose)
- Processes research topics using text normalization and lemmatization
- Generates summary tables, figures, and a combined report

---

## Project Structure


scripts/
humc/
00_build_humc_master_csv.R
01_build_analysis_object.R
02_generate_tables.R
03_generate_figures.R

hmh/
run_hmh_literature_search_analysis.R
run_hmh_article_request_analysis.R

combined/
01_build_combined_dataset.R
02_generate_combined_report.R

shared/
helpers.R
text_helpers.R
output_helpers.R
reference_data_loaders.R
paths.R

data/
raw/ # original files (not tracked)
processed/ # cleaned datasets

outputs/
humc/
combined/


---

## How to Run

From the project root in R:

```r
setwd("path/to/thompson_user_analytics")
source("run_all_analyses.R")

This runs the full pipeline:

Build HUMC dataset
Create HUMC analysis object
Generate HUMC tables and figures
Run HMH analyses
Build combined dataset
Generate combined report
Key Outputs
HUMC report:
outputs/humc/humc_summary_report.xlsx
Combined report:
outputs/combined/combined_summary_report.xlsx
Figures:
outputs/*/figures/
Formatted tables (HTML):
outputs/*/formatted_tables/
Text Analysis Pipeline

Research topics are processed using:

Text cleaning (encoding fixes, punctuation removal)
Phrase collapsing (custom phrase dictionary)
Lemmatization:
BioLemmatizer lexicon
fallback: textstem
custom overrides (custom_merges.csv)
Stop word removal
Category mapping (categories_long.xlsx)

Outputs include:

Lemma counts
TF-IDF comparisons
Category summaries
Phrase/lemma candidates (for iterative refinement)
Important Notes
Data files are not tracked in Git (see .gitignore)
Legacy logs contain inconsistent formatting and missing values
Requestor categories are standardized but some values remain unmapped
“Unknown/Not specified” reflects missing or uncategorized entries
Recent Updates
Refactored helper functions and standardized pipelines
Simplified output system (removed archived CSV workflow)
Improved requestor classification and auditability
Rebuilt combined dataset and reporting workflow
Cleaned repository (removed OS files, normalized structure)
Future Improvements
Improve requestor classification coverage
Expand phrase dictionary for better topic grouping
Add longitudinal trend analysis across HUMC and HMH
Improve visualization styling for presentation use

## License

All rights reserved. This project is provided for viewing purposes only.
Please contact the author for permission to reuse any part of this work.
