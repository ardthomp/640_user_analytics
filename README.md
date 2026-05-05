# Hospital Library User Analytics

End-to-end analytics pipeline for hospital library services, combining structured usage data with text mining to evaluate demand, workload, and research trends.

---

## Overview

This project builds a full analytics pipeline to evaluate hospital library service utilization, combining structured request data with natural language processing of research topics.

The workflow integrates legacy and modern data sources to:

- Analyze request volume and trends over time  
- Compare usage across campuses and requestor groups  
- Examine research topics using text normalization and lemmatization  
- Generate reproducible reports for operational and strategic insights  

---

## Key Features

- Multi-source data integration (HUMC legacy logs + HMH shared forms)  
- End-to-end reproducible pipeline in R  
- Advanced text processing (cleaning, phrase collapsing, lemmatization)  
- NLP-based topic analysis (lemma counts, TF-IDF, category mapping)  
- Purpose and requestor classification with auditability  
- Automated reporting (Excel workbooks, figures, HTML tables)  

---

## Project Pipeline

1. Build HUMC master dataset from legacy logs (2013–2025)  
2. Normalize and analyze HUMC data  
3. Process HMH literature search and article request data (2025–present)  
4. Combine datasets into a unified structure  
5. Generate tables, figures, and summary reports  

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

HUMC report
outputs/humc/humc_summary_report.xlsx

Combined report
outputs/combined/combined_summary_report.xlsx

Figures
outputs/*/figures/

Formatted tables (HTML)
outputs/*/formatted_tables/

Text Analysis Pipeline

Research topics are processed using:

Text cleaning (encoding fixes, punctuation normalization)
Phrase collapsing using a custom phrase dictionary
Lemmatization:
BioLemmatizer lexicon
fallback: textstem
custom overrides (custom_merges.csv)
Stop word removal
Category mapping (categories_long.xlsx)

OOutputs include:

- Lemma frequency counts  
- TF-IDF comparisons across groups  
- Category-level summaries  
- Phrase/lemma candidates for iterative refinement

---

## Package environment

This project uses `renv` for package reproducibility. To restore the package environment, run:

```r
renv::restore()

The main package groups used in the analysis are:

Project workflow: here, renv
Data cleaning and wrangling: tidyverse, janitor, lubridate
Reading/writing files: readxl, openxlsx, readr
Text analysis: tidytext, textstem, koRpus, koRpus.lang.en, stringi
Tables and reporting: gt
Plotting: ggplot2, scales, viridis
Modeling/statistics: MASS

The exact package versions are recorded in renv.lock.

---

## Data Notes

This repository does not contain protected health information (PHI).  
All data are de-identified, aggregated, or derived for analytical purposes.

The structure reflects real-world hospital library workflows,  
but no patient-level data are included.

---

## Important Notes

- Data files are not tracked in Git (see `.gitignore`)  
- Legacy logs contain inconsistent formatting and missing values  
- Requestor categories are standardized, but some values remain unmapped  
- “Unknown/Not specified” reflects missing or uncategorized entries  

---

## Recent Updates

- Refactored helper functions and standardized pipelines  
- Simplified output system (removed archived CSV workflow)  
- Improved requestor classification and auditability  
- Rebuilt combined dataset and reporting workflow  
- Cleaned repository and normalized structure  

---

## Future Improvements

- Improve requestor classification coverage  
- Expand phrase dictionary for better topic grouping  
- Add longitudinal trend analysis across HUMC and HMH  
- Enhance visualization styling for presentation use  

---

## License

All rights reserved. This repository is provided for viewing and educational purposes only.

It contains original analytic workflows developed for hospital library data analysis.  
No reuse, distribution, or derivative use is permitted without prior written permission.

See the LICENSE file for full terms.
