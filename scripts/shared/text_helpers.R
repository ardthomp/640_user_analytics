# scripts/shared/text_helpers.R
#
# Contains generic, project-agnostic helper functions for cleaning text.

library(stringr)
library(stringi)

#' A robust function for cleaning character vectors with presets.

clean_text <- function(text, preset = "default") {
  cleaned_text <- text %>%
    as.character() %>%
    stringi::stri_enc_toutf8(validate = TRUE) %>%
    stringr::str_replace_all("\uFFFD", "") %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[’`]", "'")
  
  if (preset == "topic") {
    cleaned_text <- cleaned_text %>%
      stringr::str_replace_all("\\.", "") %>%
      stringr::str_replace_all("[^a-z\\s']", " ") %>%
      stringr::str_squish()
  } else if (preset == "normalize") {
    cleaned_text <- cleaned_text %>%
      stringr::str_replace_all("[^a-z0-9\\s'_-]", " ") %>%
      stringr::str_replace_all("\\s+", " ") %>%
      stringr::str_trim()
  } else { # Default
    cleaned_text <- cleaned_text %>%
      stringr::str_replace_all("[^a-z0-9\\s']", " ") %>%
      stringr::str_replace_all("\\s+", " ") %>%
      stringr::str_trim()
  }
  
  return(cleaned_text)
}