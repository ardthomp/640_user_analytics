# Data Dictionary
================
2026-05-10

This project uses harmonized hospital library request data from a large hospital network (HMH) and its flagship campus (HUMC). The datasets include literature search requests, article/chapter requests, free-text research topics, and structured request metadata.

Because library data collection practices evolved over time, some variables are only available in newer HMH network records.

---

## Core request identifiers

| Variable | Description |
|---|---|
| `global_request_id` | Unique request identifier generated during harmonization |
| `request_id` | Original request identifier from source system |
| `source_label` | Source dataset label (e.g., HUMC, HMH) |
| `source_file_type` | Dataset origin/type used during workflow processing |

---

## Date and time variables

| Variable | Description |
|---|---|
| `submitted_at` | Parsed request completion timestamp |
| `submitted_date` | Request completion date |
| `year` | Calendar year derived from request completion date |
| `month` | Calendar month label |
| `month_num` | Numeric month |
| `year_month` | Month-level date used for time-series aggregation |
| `week` | Week-level aggregation variable |
| `weekday` | Day of week |
| `hour` | Hour of request completion |
| `season` | Derived seasonal grouping |

---

# Request metadata

| Variable | Description |
|---|---|
| `select_question_request_type` | Request category selected in request form |
| `request_type` | Harmonized request type |
| `requestor_category` | Harmonized requestor role/category |
| `who_requested_this_information` | Original requestor-role field |
| `campus_affiliation` | Original campus affiliation |
| `campus_affiliation_clean` | Harmonized campus affiliation |
| `how_was_the_question_request_received` | Method used to receive request |
| `purpose_of_request` | Free-text or multi-select purpose field |
| `time_spent_on_searches` | Reported librarian time category |
| `number_of_literature_searches` | Number of searches completed for request |

---

## Article/chapter request variables

| Variable | Description |
|---|---|
| `number_of_articles_chapters_retrieved_from_subscribed_content` | Requests fulfilled through subscribed resources |
| `number_of_articles_chapters_retrieved_from_docline` | Requests fulfilled through Docline/interlibrary loan |
| `number_of_articles_chapters_retrieved_from_other_include_source` | Requests fulfilled through other retrieval methods |
| `article_source_category` | Harmonized retrieval-source category |

---

## Research topic text variables

| Variable | Description |
|---|---|
| `research_topic` | Original free-text research topic |
| `research_topic_clean` | Normalized research topic text |
| `token` | Tokenized term |
| `lemma` | Lemmatized token |
| `category` | Assigned thematic category |
| `specialty` | Optional medical specialty assignment |

---

## Text-processing resources

| File | Description |
|---|---|
| `phrases.csv` | Multi-word biomedical phrases preserved before tokenization |
| `custom_merges.csv` | Manual mappings used to merge equivalent terms |
| `categories_long.xlsx` | Lemma-to-category mapping file |
| `lexicon.lex` | Biolemmatizer lexicon |

---

## Derived analytical datasets

| Object/File | Description |
|---|---|
| `hmh_network_dat` | Harmonized HMH network request-level dataset |
| `hmh_tidy_purposes` | Long-format request-purpose dataset |
| `tidy_lemmas_all` | Lemma-level analytical dataset |
| `all_research_topics_full` | Full cleaned research-topic dataset |
| `phrase_lemma_candidates` | Candidate phrases identified during text analysis |

---

## Output directories

| Directory | Description |
|---|---|
| `outputs/humc/` | HUMC workflow outputs |
| `outputs/hmh/` | HMH workflow outputs |
| `outputs/optional_models/` | Optional exploratory models and time-series analyses |
| `outputs/.../figures/` | Generated figures |
| `outputs/.../tables/` | Generated CSV tables |
| `outputs/.../formatted_tables/` | HTML/GT formatted tables |

---

## Notes

- Raw institutional data are not publicly included in this repository.
- Some variables are only available in newer HMH network request forms.
- Several variables were harmonized across datasets during preprocessing.
- Text-analysis outputs depend on evolving phrase dictionaries and custom merge mappings.
