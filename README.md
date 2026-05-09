# Hospital Library User Analytics

This project analyzes hospital library service requests across a large hospital network using reproducible R workflows. The goal is to understand request patterns, workload, research topics, and information needs across the hospital system through operational analytics, text analysis, and statistical modeling. 

---

## ⭐ Project Overview

The analysis focuses on:

- Literature search requests
- Article/chapter requests
- Research topic text analysis, including phrase normalization and lemmatization
- TF-IDF comparisons across requestor groups and purposes
- Workload and effort-level analysis
- Request trends across time
- Longitudinal and seasonal demand analysis
- Retrieval source analysis for article/chapter requests

Analytical methods include:

- Descriptive summaries
- TF-IDF analysis
- Chi-squared testing
- Ordinal logistic regression
- Time series analysis
- Biomedical text preprocessing and lemmatization

Outputs include:

- Lemma frequency counts
- TF-IDF comparisons across groups
- Category-level summaries
- Phrase/lemma candidates for iterative refinement
- Excel summary workbooks
- Presentation-ready figures
- Formatted tables for reporting and presentation use

---

## ⭐ Project Structure

```text
data/              ❗private raw and processed data (not tracked)
outputs/           ❗generated figures and tables (not tracked)

docs/
    data_dictionary.md
    
renv/              ❗project package environment

scripts/
    combined/
        01_build_combined_dataset.R
        02_generate_combined_report.R
    hmh/
        run_hmh_article_request_analysis.R
        run_hmh_literature_search_analysis.R
    humc/
        00_build_humc_master_csv.R
        01_build_analysis_object.R
        02_generate_tables.R
        03_generate_figures.R
    optional_models/
        combined_01_time_series_patterns.R
        hmh_01_effort_level_requestor_model.R
        humc_01_citation_count_prediction_model.R
        humc_02_longitudinal_time_series.R
    shared/
        helpers.R
        output_helpers.R
        paths.R
        plotting_helpers.R
        reference_data_loaders.R
        text_helpers.R

run_all_analyses.R   ❗master analysis script
README.md

```

---

## ⭐ Data Sources

❗Due to institutional restrictions, raw data files are not included in this repository.

### Legacy HUMC dataset (2013–2025)

The legacy HUMC dataset contains more than 5,800 literature search requests collected over multiple years. These records rely heavily on free-text fields and include inconsistent formatting, abbreviations, and terminology across time periods.


### HMH structured dataset (2025–present)

The newer HMH dataset includes standardized request forms with structured variables such as:

- Request type
- Requestor role
- Campus affiliation
- Time spent on requests
- Article/chapter retrieval source

As of 2026, the structured dataset includes both literature search requests and article/chapter requests conducted across the HMH network.


### ❗Important Notes
- Some legacy logs contain inconsistent formatting and missing values
- Requestor categories are standardized but raw fields may be incomplete
- “Unknown/Not specified” reflects missing or unmapped values

---

## ⭐ Methods

All analyses were conducted in R using modular script-based workflows.

The project uses separate scripts for:

- Data cleaning
- Text processing
- Statistical analysis
- Figure generation
- Export workflows

Shared helper scripts are used throughout the project to standardize:

- Paths and directory creation
- Plot styling
- Export formatting
- Text preprocessing
- Reference data loading


### Text analysis workflow

Research topic text was normalized using:

- Phrase preservation rules (`phrases.csv`)
- BioLemmatizer
- `tidytext`
- Custom merge mappings (`custom_merges.csv`)

The workflow:

- Cleans and normalizes free-text research topics
- Preserves meaningful biomedical phrases before tokenization
- Tokenizes and lemmatizes text
- Applies custom mappings to merge equivalent terms
- Rejoins processed text with structured metadata for downstream analysis

The scripts also generate updated n-gram and phrase candidate lists to support ongoing refinement of the phrase dictionary.


### Modeling and trend analysis

The project includes several analytical approaches beyond descriptive summaries, including:

- Ordinal logistic regression models examining effort level by requestor group
- Comparative workload analysis using time-spent categories
- TF-IDF comparisons across patron groups and request purposes
- Longitudinal time series analysis of request activity
- Seasonal trend analysis across years

---

## ⭐ Reproducing the analysis

1. Clone this repository
2. Open the project `.Rproj` file in RStudio
3. Restore the package environment:
```r
renv::restore()
```
4. Add required private datasets to the `data/` directory
5. Run:
```r
source("run_all_analyses.R")
```

❗Optional or exploratory models located in `scripts/optional_models/` are not run by the master pipeline and should be executed separately as needed.

---

## ⭐ Outputs

Scripts generate:

- CSV summary tables
- Excel workbooks
- HTML formatted tables
- PNG figures for reports and posters

Outputs are written automatically to structured subdirectories within `outputs/`.

❗ Generated outputs and private institutional data are excluded from version control using `.gitignore`.

--- 

## ⭐ Reproducibility and workflow

The project uses:

- `renv` for package management
- Git/GitHub for version control
- Modular helper scripts for reusable workflows
- Structured output directories for reproducibility

The workflow was developed iteratively using both standard R development practices and the hospital's AI-assisted support tools for coding, troubleshooting, and interpretation of analytical results.

---

## 😔 Limitations

Because the underlying datasets contain protected institutional information, the raw data and some reference files cannot be shared publicly. External users may therefore be unable to fully reproduce the analyses without access to the original data sources.

In addition, the project relies heavily on cleaning and standardizing free-text biomedical language, which requires ongoing maintenance of phrase dictionaries and custom mappings.

---

## 🔮 Future directions

- Integration of medical school library data
- Expanded workload modeling
- Additional longitudinal forecasting
- Improved automated categorization workflows
- Expand phrase dictionary for better topic grouping
- Improve visualization styling
- Publication of methods and findings in the health sciences library literature

✨ More broadly, the project demonstrates how hospital libraries can use operational data to better understand user needs, evaluate services, and support institutional decision-making. ✨


---

## ✅ License

©️ Andrea Thompson 2026

All rights reserved. This repository is provided for viewing and educational purposes only. It contains original analytic workflows developed for hospital library data analysis. No reuse, distribution, or derivative use is permitted without prior written permission.

❗ See the `LICENSE` file for full terms.
