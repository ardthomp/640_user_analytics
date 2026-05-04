# scripts/shared/reference_data_loaders.R
#
# Contains specialized functions for loading and processing this project's
# specific reference data files (phrases, merges, lexicons).

library(readr)
library(dplyr)
library(stringr)
library(stringi)
library(tibble)

#' Read and clean the phrases.csv file.
#' Creates a template if the file does not exist.
read_phrases <- function(path) {
  if (!file.exists(path)) {
    readr::write_csv(tibble(phrase = character()), path)
    message("Created empty phrases.csv. Add phrases and rerun.")
  }
  
  readr::read_csv(
    path,
    show_col_types = FALSE,
    col_types = cols(phrase = col_character()),
    locale = readr::locale(encoding = "latin1")
  ) %>%
    mutate(
      phrase = stringi::stri_enc_toutf8(phrase, validate = TRUE),
      phrase = str_replace_all(phrase, "\uFFFD", ""),
      phrase = str_to_lower(str_trim(phrase))
    ) %>%
    filter(!is.na(phrase), phrase != "") %>%
    distinct()
}


#' Vectorized and highly efficient phrase collapsing.
#' Replaces phrases in text with an underscore-collapsed version.
collapse_phrases <- function(text_vec, phrases_tbl) {
  text_vec <- as.character(text_vec)
  
  if (nrow(phrases_tbl) == 0) return(text_vec)
  
  # Sort phrases to replace longer ones first (e.g., "rotator cuff repair"
  # before "rotator cuff"). This is critical.
  phrases_to_replace <- phrases_tbl %>%
    mutate(n_words = stringr::str_count(phrase, "\\s+") + 1) %>%
    arrange(desc(n_words), desc(nchar(phrase))) %>%
    pull(phrase)
  
  # Create a named vector for replacement:
  # name = original phrase, value = collapsed phrase
  # e.g., c("rotator cuff" = "rotator_cuff", "physical therapy" = "physical_therapy")
  replacement_vec <- setNames(
    str_replace_all(phrases_to_replace, " ", "_"), # new values
    phrases_to_replace # old values (as names)
  )
  
  # A single, vectorized str_replace_all call is MUCH faster than a for loop.
  stringr::str_replace_all(text_vec, replacement_vec)
}


#' Read and clean the custom_merges.csv file.
#' Creates a starter template if the file does not exist.
read_custom_merges <- function(path) {
  # (Your original code is excellent and remains unchanged)
  if (!file.exists(path)) {
    starter <- tibble(
      token = c("pt", "pts", "patient", "patients", "pediatric", "pediatrics"),
      lemma = c("patient", "patient", "patient", "patient", "pediatric", "pediatric")
    )
    readr::write_csv(starter, path)
    message("Created starter custom_merges.csv. Edit it and rerun.")
  }
  
  readr::read_csv(
    path,
    show_col_types = FALSE,
    col_types = cols(token = col_character(), lemma = col_character()),
    locale = readr::locale(encoding = "latin1")
  ) %>%
    mutate(
      token = stringi::stri_enc_toutf8(token, validate = TRUE),
      lemma = stringi::stri_enc_toutf8(lemma, validate = TRUE),
      token = str_replace_all(token, "\uFFFD", ""),
      lemma = str_replace_all(lemma, "\uFFFD", "")
    ) %>%
    transmute(
      token = str_to_lower(str_trim(token)),
      lemma_custom = str_to_lower(str_trim(lemma))
    ) %>%
    filter(
      !is.na(token), token != "",
      !is.na(lemma_custom), lemma_custom != ""
    ) %>%
    distinct(token, .keep_all = TRUE)
}


#' Read and parse the tab-delimited lexicon.lex file.
read_lex <- function(path) {
  # (Your original code is excellent and remains unchanged)
  lex_raw <- utils::read.delim(
    file = path, header = FALSE, sep = "\t", quote = "",
    comment.char = "", fill = TRUE, stringsAsFactors = FALSE,
    encoding = "UTF-8"
  )
  
  if (ncol(lex_raw) < 4) {
    stop("lexicon.lex did not parse into at least 4 tab-separated columns.")
  }
  
  lex_raw %>%
    transmute(
      token = stringi::stri_enc_toutf8(as.character(V1), validate = TRUE),
      lemma_from_lex = stringi::stri_enc_toutf8(as.character(V4), validate = TRUE)
    ) %>%
    mutate(
      token = str_replace_all(token, "\uFFFD", ""),
      lemma_from_lex = str_replace_all(lemma_from_lex, "\uFFFD", ""),
      token = str_to_lower(str_trim(token)),
      lemma_from_lex = str_to_lower(str_trim(lemma_from_lex))
    ) %>%
    filter(
      !is.na(token), token != "",
      !is.na(lemma_from_lex), lemma_from_lex != "",
      str_detect(token, "[a-z]")
    ) %>%
    distinct(token, .keep_all = TRUE)
}