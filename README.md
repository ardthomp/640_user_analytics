# User Analytics Project

This project analyzes hospital library user requests across HUMC (Hackensack University Medical Center), HMH, and combined datasets. The goal is to understand request patterns, workload, and research topics using reproducible data pipelines.

---

## Project overview

The analysis focuses on:

- Literature search requests
- Article/chapter requests
- Research topic text analysis (lemmatization and categorization)
- Workload and usage trends across time, requestor groups, and purposes

Outputs include:

- Lemma frequency counts  
- TF-IDF comparisons across groups  
- Category-level summaries  
- Phrase/lemma candidates for iterative refinement  

---

## Project structure


data/ # private raw and processed data (not tracked)
scripts/ # analysis scripts
outputs/ # generated figures and tables (not tracked)
renv/ # project package environment
run_all_analyses.R
renv.lock
README.md


---

## Reproducing the analysis

1. Clone this repository

2. Open the project in RStudio:
   

hmh_user_analytics.Rproj


3. Restore the package environment:

```r
renv::restore()
Add the required private data files to the data/ folder

Run the full pipeline:

source("run_all_analyses.R")
Data availability

The data/ and outputs/ folders are intentionally excluded from GitHub because they contain private and/or generated data.

To reproduce the analysis, you must provide your own input data in the data/ directory.

Package environment

This project uses renv to ensure reproducibility.

Run:

renv::restore()

to install all required packages from the lockfile.

Main package groups used
Workflow: here, renv
Data wrangling: tidyverse, janitor, lubridate
File I/O: readxl, openxlsx, readr
Text analysis: tidytext, textstem, koRpus, koRpus.lang.en, stringi
Tables/reporting: gt
Visualization: ggplot2, scales, viridis
Modeling: MASS

The exact package versions are recorded in renv.lock.

Notes
Scripts are designed to run in sequence via run_all_analyses.R
Outputs are regenerated each time the pipeline is run
Some scripts may take longer depending on data size and text processing steps
