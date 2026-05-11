# scripts/shared/text_helpers.R
#
# Shared helper functions for text normalization and preprocessing used across
# HUMC and HMH text-analysis workflows.

library(stringr)
library(stringi)

# Clean and normalize text using reusable preprocessing presets.
#
# Presets:
#   default   - general lowercase/token cleanup
#   topic     - stricter cleaning for n-gram generation
#   normalize - preserve underscores/hyphens for phrase collapsing

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
  } else { # General-purpose cleaning
    cleaned_text <- cleaned_text %>%
      stringr::str_replace_all("[^a-z0-9\\s']", " ") %>%
      stringr::str_replace_all("\\s+", " ") %>%
      stringr::str_trim()
  }
  
  cleaned_text
}