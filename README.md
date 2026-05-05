# Hospital Library User Analytics

This project analyzes hospital library service requests across HUMC, HMH, and combined datasets. The goal is to understand request patterns, workload, and research topics using reproducible R pipelines.

## Project overview

The analysis focuses on:

- Literature search requests
- Article/chapter requests
- Research topic text analysis, including lemmatization and categorization
- Workload and usage trends across time, requestor groups, and purposes

Outputs include:

- Lemma frequency counts
- TF-IDF comparisons across groups
- Category-level summaries
- Phrase/lemma candidates for iterative refinement

## Project structure


data/ private raw and processed data (not tracked)
scripts/ analysis scripts
outputs/ generated figures and tables (not tracked)
renv/ project package environment
run_all_analyses.R
renv.lock
README.md


## Reproducing the analysis

1. Clone this repository  
2. Open `hmh_user_analytics.Rproj` in RStudio  
3. Restore the package environment:

```r
renv::restore()
Add required private data files to the data/ folder
Run:
source("run_all_analyses.R")
Data availability

The data/ and outputs/ folders are intentionally excluded from GitHub because they contain private and/or generated data.

Package environment

This project uses renv for reproducibility. The exact package versions are stored in renv.lock.

Main package groups
Workflow: here, renv
Data wrangling: tidyverse, janitor, lubridate
File I/O: readxl, openxlsx, readr
Text analysis: tidytext, textstem, koRpus, koRpus.lang.en, stringi
Tables: gt
Visualization: ggplot2, scales, viridis
Modeling: MASS
Notes
Scripts run sequentially via run_all_analyses.R
Outputs are regenerated each run
