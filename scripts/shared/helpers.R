# scripts/shared/helpers.R
#
# Contains generic, project-agnostic helper functions.

#' Convert various "true" values (e.g., "yes", "t", "1", "x") to a binary 0/1 integer.
#'
#' @param x A character vector.
#' @return An integer vector of 0s and 1s.
flag_to_binary <- function(x) {
  # Using case_when is robust, but a simple regex might be slightly faster
  # and clearer for this common task. Let's stick with your proven logic.
  x <- stringr::str_trim(stringr::str_to_lower(as.character(x)))
  
  dplyr::case_when(
    x %in% c("1", "`1", "true", "t", "yes", "y", "x", "checked") ~ 1L,
    TRUE ~ 0L
  )
}

#' Safely calculate the maximum of a numeric vector, returning NA if all
#' values are NA, instead of -Inf.
#'
#' @param x A numeric vector.
#' @return The maximum value or NA_real_.
safe_max_numeric <- function(x) {
  # This is a great utility function, perfectly placed in a helpers file.
  if (all(is.na(x))) {
    NA_real_
  } else {
    max(x, na.rm = TRUE)
  }
}

# Note: The `archive_existing_files` function has been removed.
# Its functionality is now handled by the `archive_and_write` engine
# in `output_helpers.R`.

#' Safely parse multiple common date-time formats.
#'
#' Tries several `lubridate` functions in order, returning the first that succeeds.
#'
#' @param x A character vector of timestamps.
#' @return A POSIXct vector.
parse_timestamp <- function(x) {
  dplyr::coalesce(
    suppressWarnings(lubridate::mdy_hms(x)),
    suppressWarnings(lubridate::mdy_hm(x)),
    suppressWarnings(lubridate::ymd_hms(x)),
    suppressWarnings(lubridate::ymd_hm(x))
  )
}

#' Convert common blank or NA-like strings to a true NA_character_.
#'
#' @param x A character vector.
#' @return A cleaned character vector with true NAs.
clean_blank <- function(x) {
  x_squished <- stringr::str_squish(as.character(x))
  x_squished[x_squished %in% c("", "NA", "N/A", "NULL", "null", "n/a")] <- NA_character_
  x_squished
}