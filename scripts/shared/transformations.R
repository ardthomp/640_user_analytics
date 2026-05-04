# scripts/shared/transformations.R
#
# Contains project-specific data transformation and business logic functions.

#' Standardize campus names from various sources into a consistent set.
standardize_campus_name <- function(x) {
  x_clean <- stringr::str_squish(as.character(x))
  x_lower <- stringr::str_to_lower(x_clean)
  
  dplyr::case_when(
    is.na(x_clean) | x_clean == "" | x_lower == "na" ~ "Unknown/Not specified",
    stringr::str_detect(x_lower, "hackensack") | x_lower == "humc" ~ "Hackensack University Medical Center",
    stringr::str_detect(x_lower, "\\bjfk\\b") ~ "JFK University Medical Center",
    stringr::str_detect(x_lower, "palisades") ~ "Palisades Medical Center",
    stringr::str_detect(x_lower, "carrier") ~ "Carrier Clinic",
    TRUE ~ x_clean
  )
}

#' Standardize requestor names, collapsing detailed roles into broader categories.
standardize_requestor_name <- function(x) {
  x_clean <- stringr::str_squish(as.character(x))
  x_lower <- stringr::str_to_lower(x_clean)
  
  dplyr::case_when(
    is.na(x_clean) | x_clean == "" | x_lower == "na" ~ "Unknown/Not specified",
    x_lower == "med ed" | stringr::str_detect(x_lower, "medical education") ~ "Resident",
    stringr::str_detect(x_lower, "nurse practitioner|nurse practitioner/ pa|nurse practitioner/pa|\\bnp\\b|\\bapn\\b|physician assistant|\\bpa\\b") ~ "Nurse Practitioner/PA",
    TRUE ~ x_clean
  )
}

#' Create a single campus affiliation string from multiple flag columns or a text field.
make_campus_affiliation <- function(campus_affiliation, humc, carrier, jfk, palisades, network) {
  dplyr::case_when(
    !is.na(campus_affiliation) & campus_affiliation != "" ~ campus_affiliation,
    carrier == 1 ~ "Carrier",
    jfk == 1 ~ "JFK",
    palisades == 1 ~ "Palisades",
    network == 1 ~ "Network",
    humc == 1 ~ "HUMC",
    TRUE ~ NA_character_
  )
}

#' Collapse multiple purpose flag columns into a single comma-separated string.
make_purpose_string <- function(continuing_education, patient_care, lecture, ebp, research, grant, publication, irb_app, admin, policy, patient_info) {
  purposes <- c(
    if_else(continuing_education == 1, "Continuing Education", NA_character_),
    if_else(patient_care == 1, "Patient Care", NA_character_),
    if_else(lecture == 1, "Lecture / Presentation", NA_character_),
    if_else(ebp == 1, "Evidence-based Practice", NA_character_),
    if_else(research == 1, "Research", NA_character_),
    if_else(grant == 1, "Grant", NA_character_),
    if_else(publication == 1, "Publication", NA_character_),
    if_else(irb_app == 1, "IRB App", NA_character_),
    if_else(admin == 1, "Admin", NA_character_),
    if_else(policy == 1, "Policy", NA_character_),
    if_else(patient_info == 1, "Patient Info", NA_character_)
  )
  purposes <- purposes[!is.na(purposes)]
  if (length(purposes) == 0) return(NA_character_)
  paste(purposes, collapse = ", ")
}

#' Collapse multiple requestor flag columns into a single comma-separated string.
make_requestor_string <- function(attending, med_ed, nurse, other_provider, committee, consumer_health) {
  requestors <- c(
    if_else(attending == 1, "Physician", NA_character_),
    if_else(med_ed == 1, "Med Ed", NA_character_),
    if_else(nurse == 1, "Nurse", NA_character_),
    if_else(other_provider == 1, "Other Provider", NA_character_),
    if_else(committee == 1, "Committee", NA_character_),
    if_else(consumer_health == 1, "Consumer", NA_character_)
  )
  requestors <- requestors[!is.na(requestors)]
  if (length(requestors) == 0) return(NA_character_)
  paste(requestors, collapse = ", ")
}