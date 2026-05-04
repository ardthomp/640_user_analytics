# scripts/shared/helpers.R
#
# Contains generic, project-agnostic helper functions.

#' Convert various "true" values (e.g., "yes", "t", "1", "x") to a binary 0/1 integer.
flag_to_binary <- function(x) {
  x <- stringr::str_trim(stringr::str_to_lower(as.character(x)))
  dplyr::case_when(
    x %in% c("1", "`1", "true", "t", "yes", "y", "x", "checked") ~ 1L,
    TRUE ~ 0L
  )
}

#' Safely calculate the maximum of a numeric vector, returning NA if all values are NA.
safe_max_numeric <- function(x) {
  if (all(is.na(x))) {
    NA_real_
  } else {
    max(x, na.rm = TRUE)
  }
}

#' Safely parse multiple common date-time formats.
parse_timestamp <- function(x) {
  dplyr::coalesce(
    suppressWarnings(lubridate::mdy_hms(x)),
    suppressWarnings(lubridate::mdy_hm(x)),
    suppressWarnings(lubridate::ymd_hms(x)),
    suppressWarnings(lubridate::ymd_hm(x))
  )
}

#' Convert common blank or NA-like strings to a true NA_character_.
clean_blank <- function(x) {
  x_squished <- stringr::str_squish(as.character(x))
  x_squished[x_squished %in% c("", "NA", "N/A", "NULL", "null", "n/a")] <- NA_character_
  x_squished
}