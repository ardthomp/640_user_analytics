data_dictionary
================
2026-05-05

# Data Dictionary

## Dataset: `combined_dat`

This dataset combines HUMC legacy literature search data and HMH
shared-form literature search data for analysis of request volume,
campus patterns, requestor groups, purposes, workload, and research
topics.

| Variable | Description |
|----|----|
| `request_id` | Unique row number assigned after HUMC and HMH records are combined. |
| `global_request_id` | Same as `request_id`; used for joins with lemma and purpose tables. |
| `source_file_type` | Source system/type of the record, such as `humc` or `hmh`. |
| `source_label` | Human-readable source label, such as `HUMC legacy form` or `HMH shared form`. |
| `original_id` | Original request ID from the source dataset, when available. |
| `submitted_at` | Full date/time when the request was submitted. HUMC legacy records usually only have dates. |
| `submitted_date` | Date of the request. |
| `year` | Year of the request. |
| `month` | Month label of the request. |
| `month_num` | Numeric month of the request. |
| `year_month` | Month-level date used for trend summaries. |
| `week` | Week-level date used for weekly summaries. |
| `weekday` | Day of week of the request. |
| `hour` | Hour of request submission. Usually available for HMH shared-form records; unavailable for HUMC legacy records. |
| `campus_affiliation_raw` | Original or minimally cleaned campus value from the source data. |
| `campus_affiliation_clean` | Standardized campus affiliation used for reporting. |
| `campus_affiliation_detail` | More detailed campus flag information, where available. |
| `humc` | Binary indicator for HUMC affiliation. |
| `carrier` | Binary indicator for Carrier Clinic affiliation. |
| `jfk` | Binary indicator for JFK affiliation. |
| `palisades` | Binary indicator for Palisades affiliation. |
| `network` | Binary indicator for Network affiliation. |
| `requestor_category_raw` | Raw or source-derived requestor category before final standardization. |
| `requestor_category` | Standardized requestor category used for reporting. |
| `request_received` | Method by which the request was received, when available. |
| `research_topic` | Original research topic/request text after basic cleaning. |
| `time_spent` | Original time-spent category, when available. |
| `effort_level` | Simplified workload level derived from `time_spent`, such as low, medium, or high time. |
| `purpose` | Original or reconstructed purpose-of-request field. May contain multiple values. |
| `n_searches` | Number of literature searches associated with a request, when available. |
| `citation_count` | Number of citations/articles associated with the request, when available. |
| `plot_group` | Reporting group used in combined visualizations. |

------------------------------------------------------------------------

## Dataset: `tidy_purposes`

This dataset reshapes request purposes so that each request-purpose
pairing appears as its own row.

| Variable | Description |
|----|----|
| `request_id` | Request ID linking back to `combined_dat`. |
| `global_request_id` | Global request ID linking back to `combined_dat` and lemma tables. |
| `source_file_type` | Source system/type of the record. |
| `source_label` | Human-readable source label. |
| `submitted_date` | Date of the request. |
| `year` | Year of the request. |
| `year_month` | Month-level date. |
| `campus_affiliation_raw` | Raw or minimally cleaned campus value. |
| `campus_affiliation_clean` | Standardized campus value. |
| `campus_affiliation_detail` | Detailed campus flag information, where available. |
| `humc` | HUMC indicator. |
| `carrier` | Carrier Clinic indicator. |
| `jfk` | JFK indicator. |
| `palisades` | Palisades indicator. |
| `network` | Network indicator. |
| `plot_group` | Reporting group used for figures. |
| `requestor_category` | Standardized requestor category. |
| `time_spent` | Original time-spent category. |
| `effort_level` | Simplified workload category. |
| `purpose_category` | Standardized purpose category. |
| `purpose_other_detail` | Original purpose text when categorized as `Other`. |

------------------------------------------------------------------------

## Dataset: `tidy_lemmas_all`

This dataset contains one row per request-topic lemma after text
cleaning, phrase collapsing, lemmatization, stop-word removal, and
custom merging.

| Variable | Description |
|----|----|
| `global_request_id` | Global request ID linking back to `combined_dat`. |
| `request_id` | Request ID linking back to `combined_dat`. |
| `source_file_type` | Source system/type of the record. |
| `source_label` | Human-readable source label. |
| `original_id` | Original request ID from the source dataset, when available. |
| `submitted_date` | Date of the request. |
| `year` | Year of the request. |
| `year_month` | Month-level date. |
| `campus_affiliation` | Campus affiliation used in text summaries. |
| `campus_affiliation_clean` | Standardized campus affiliation. |
| `requestor_category` | Standardized requestor category. |
| `research_topic` | Original research topic text after basic cleaning. |
| `research_topic_clean` | Text-normalized version of the research topic used for tokenization. |
| `word` | Token extracted from the cleaned research topic. |
| `lemma_from_lex` | Lemma found from the BioLemmatizer lexicon, when available. |
| `lemma0` | First-stage lemma, using the lexicon result when available. |
| `lemma1` | Second-stage lemma, using `textstem` fallback when needed. |
| `lemma_custom` | Custom merged lemma from `custom_merges.csv`, when available. |
| `lemma` | Final lemma used for analysis. |

------------------------------------------------------------------------

## Dataset: `phrase_lemma_candidates`

This dataset identifies possible multi-word phrases that may be useful
additions to `phrases.csv`.

| Variable | Description                                    |
|----------|------------------------------------------------|
| `ngram`  | Candidate phrase or multi-word term.           |
| `n`      | Number of times the candidate appears.         |
| `type`   | Candidate type, usually `bigram` or `trigram`. |

------------------------------------------------------------------------

## Notes

- Data files are not tracked in Git.
- HUMC legacy logs and HMH shared-form data have different source
  structures, so some variables are only available for one source.
- `Unknown/Not specified` indicates missing, blank, or unmapped values.
- Raw fields such as `requestor_category_raw` are preserved for
  auditability.
