# scripts/shared/text_helpers.R
#
# Contains generic, project-agnostic helper functions for cleaning text.

library(stringr)
library(stringi)

#' A robust function for cleaning character vectors.
#'
#' This function provides a consistent base cleaning (UTF-8 validation, lowercasing,
#' apostrophe normalization) and allows for custom final filtering and whitespace handling.
#'
#' @param text A character vector to clean.
#' @param preset The cleaning preset to use. One of "default", "topic", or "normalize".
#'   This controls the final regex and whitespace handling.
#' @return A cleaned character vector.
clean_text <- function(text, preset = "default") {
  
  # 1. Perform base cleaning steps common to all versions
  cleaned_text <- text %>%
    as.character() %>%
    stringi::stri_enc_toutf8(validate = TRUE) %>%
    stringr::str_replace_all("\uFFFD", "") %>% # Remove Unicode replacement char
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[’`]", "'") # Normalize apostrophes
  
  # 2. Apply preset-specific filtering and whitespace handling
  if (preset == "topic") {
    cleaned_text <- cleaned_text %>%
      stringr::str_replace_all("\\.", "") %>% # Specific rule for topics
      stringr::str_replace_all("[^a-z\\s']", " ") %>%
      stringr::str_squish()
  } else if (preset == "normalize") {
    cleaned_text <- cleaned_text %>%
      stringr::str_replace_all("[^a-z0-9\\s'_-]", " ") %>%
      stringr::str_replace_all("\\s+", " ") %>%
      stringr::str_trim()
  } else { # Default preset
    cleaned_text <- cleaned_text %>%
      stringr::str_replace_all("[^a-z0-9\\s']", " ") %>%
      stringr::str_replace_all("\\s+", " ") %>%
      stringr::str_trim()
  }
  
  return(cleaned_text)
}