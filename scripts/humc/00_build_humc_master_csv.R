# Build comprehensive HUMC search log --------------------------------------
#
# Purpose:
#   Read all raw HUMC legacy Excel workbooks, standardize messy column names,
#   clean dates/flags/topics, run import checks, and save one master file:
#   data/processed/humc.csv
#
# Run first:
#   source("scripts/humc/00_build_humc_master_csv.R")

library(tidyverse)
library(readxl)
library(janitor)
library(here)
library(lubridate)
library(stringr)
library(purrr)

source(here("scripts", "shared", "paths.R"))
source(here("scripts", "shared", "helpers.R"))
source(here("scripts", "shared", "output_helpers.R"))

raw_humc_dir <- here("data", "raw", "humc")
processed_dir <- here("data", "processed")
archive_dir <- file.path(processed_dir, "old_humc")

old_master_files_dir <- file.path(archive_dir, "old_master_files")
old_year_mismatch_dir <- file.path(archive_dir, "old_year_mismatch")
old_requestor_flag_check_dir <- file.path(archive_dir, "old_requestor_flag_check")
old_purpose_flag_check_dir <- file.path(archive_dir, "old_purpose_flag_check")
old_campus_flag_check_dir <- file.path(archive_dir, "old_campus_flag_check")
old_missing_topics_dir <- file.path(archive_dir, "old_missing_topics")
old_missing_dates_dir <- file.path(archive_dir, "old_missing_dates")
old_import_check_by_file_dir <- file.path(archive_dir, "old_import_check_by_file")

dirs_to_make <- c(
  processed_dir,
  archive_dir,
  old_master_files_dir,
  old_year_mismatch_dir,
  old_requestor_flag_check_dir,
  old_purpose_flag_check_dir,
  old_campus_flag_check_dir,
  old_missing_topics_dir,
  old_missing_dates_dir,
  old_import_check_by_file_dir
)

walk(dirs_to_make, ~ if (!dir.exists(.x)) dir.create(.x, recursive = TRUE))

output_path <- file.path(processed_dir, "humc.csv")
run_timestamp <- format(Sys.time(), "%Y_%m_%d-%H-%M-%S")

archive_existing_files(processed_dir, "^humc\\.csv$", old_master_files_dir, run_timestamp)
archive_existing_files(processed_dir, "^humc_year_mismatch_.*\\.csv$", old_year_mismatch_dir, run_timestamp)
archive_existing_files(processed_dir, "^humc_requestor_flag_check_.*\\.csv$", old_requestor_flag_check_dir, run_timestamp)
archive_existing_files(processed_dir, "^humc_purpose_flag_check_.*\\.csv$", old_purpose_flag_check_dir, run_timestamp)
archive_existing_files(processed_dir, "^humc_campus_flag_check_.*\\.csv$", old_campus_flag_check_dir, run_timestamp)
archive_existing_files(processed_dir, "^humc_missing_topics_.*\\.csv$", old_missing_topics_dir, run_timestamp)
archive_existing_files(processed_dir, "^humc_missing_dates_.*\\.csv$", old_missing_dates_dir, run_timestamp)
archive_existing_files(processed_dir, "^humc_import_check_by_file_.*\\.csv$", old_import_check_by_file_dir, run_timestamp)

clean_text_na <- function(x) {
  x <- as.character(x)
  x <- str_squish(x)
  x[x %in% c("", "NA", "N/A", "NULL", "null", "n/a")] <- NA_character_
  x
}

parse_excel_date <- function(x) {
  if (inherits(x, "Date")) return(as.Date(x))
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.Date(x))
  x_chr <- as.character(x)
  x_chr <- str_squish(x_chr)
  x_chr[x_chr %in% c("", "NA", "N/A", "NULL", "null", "n/a", "Date")] <- NA_character_
  x_num <- suppressWarnings(as.numeric(x_chr))
  parsed_num <- suppressWarnings(as.Date(x_num, origin = "1899-12-30"))
  parsed_mdy <- suppressWarnings(mdy(x_chr))
  parsed <- coalesce(parsed_num, parsed_mdy)
  parsed[!is.na(parsed) & (year(parsed) < 2000 | year(parsed) > 2030)] <- NA
  parsed
}

ensure_col <- function(df, col, default = NA_character_) {
  if (!col %in% names(df)) df[[col]] <- default
  df
}

make_campus_affiliation <- function(humc, carrier, jfk, palisades, network) {
  case_when(
    carrier == 1 ~ "Carrier",
    jfk == 1 ~ "JFK",
    palisades == 1 ~ "Palisades",
    network == 1 ~ "Network",
    humc == 1 ~ "HUMC",
    TRUE ~ NA_character_
  )
}

make_campus_detail <- function(humc, carrier, jfk, palisades, network) {
  campus_list <- c()
  if (!is.na(humc) && humc == 1) campus_list <- c(campus_list, "HUMC")
  if (!is.na(carrier) && carrier == 1) campus_list <- c(campus_list, "Carrier")
  if (!is.na(jfk) && jfk == 1) campus_list <- c(campus_list, "JFK")
  if (!is.na(palisades) && palisades == 1) campus_list <- c(campus_list, "Palisades")
  if (!is.na(network) && network == 1) campus_list <- c(campus_list, "Network")
  if (length(campus_list) == 0) return(NA_character_)
  paste(campus_list, collapse = "; ")
}

read_one_search_log <- function(path) {
  message("Reading: ", basename(path))
  sheets <- readxl::excel_sheets(path)
  sheet_to_read <- sheets[1]
  raw <- readxl::read_excel(path, sheet = sheet_to_read) %>%
    janitor::clean_names() %>%
    dplyr::select(-dplyr::any_of(c(
      "requestor", "requester", "requester_name", "requestor_name", "name", "patron_name"
    )))
  needed_cols <- c(
    "date", "topic", "carrier", "jfk", "palisades", "network",
    "attending", "med_ed", "nurse", "other_provider", "committee", "consumer_health",
    "cme", "icu_rnds", "citation_count", "continuing_education", "patient_care",
    "lecture", "ebp", "research", "grant", "publication", "irb_app", "admin", "policy", "patient_info"
  )
  for (col in needed_cols) raw <- ensure_col(raw, col)
  possible_date_cols <- c("date", "x1", "ff")
  existing_date_cols <- possible_date_cols[possible_date_cols %in% names(raw)]
  if (length(existing_date_cols) == 0) raw$date <- NA_character_ else raw$date <- reduce(raw[existing_date_cols], coalesce)
  if ("attd" %in% names(raw)) raw$attending <- raw$attd
  if ("med_ed_5" %in% names(raw)) raw$med_ed <- raw$med_ed_5
  if ("med_ed_25" %in% names(raw)) raw$med_ed <- coalesce(raw$med_ed, raw$med_ed_25)
  if ("nurs" %in% names(raw)) raw$nurse <- raw$nurs
  if ("dept" %in% names(raw)) raw$other_provider <- raw$dept
  if ("ch" %in% names(raw)) raw$consumer_health <- raw$ch
  if ("cites" %in% names(raw)) raw$citation_count <- raw$cites
  if ("cntng_ed" %in% names(raw)) raw$continuing_education <- raw$cntng_ed
  if ("pt_care" %in% names(raw)) raw$patient_care <- raw$pt_care
  if ("lect" %in% names(raw)) raw$lecture <- raw$lect
  if ("rsch" %in% names(raw)) raw$research <- raw$rsch
  if ("pub" %in% names(raw)) raw$publication <- raw$pub
  if ("irb_sub" %in% names(raw)) raw$irb_app <- raw$irb_sub
  if ("pt_info" %in% names(raw)) raw$patient_info <- raw$pt_info
  year_from_file <- str_extract(basename(path), "\\d{4}")
  cleaned <- raw %>%
    transmute(
      source_file = basename(path),
      source_sheet = sheet_to_read,
      file_year = as.integer(year_from_file),
      date_raw = as.character(date),
      date = parse_excel_date(date),
      date = if_else(!is.na(date) & !is.na(as.integer(year_from_file)) & lubridate::year(date) != as.integer(year_from_file), as.Date(NA), date),
      year = as.integer(year_from_file),
      month = floor_date(date, "month"),
      topic = clean_text_na(topic),
      humc = 1L,
      carrier = flag_to_binary(carrier),
      jfk = flag_to_binary(jfk),
      palisades = flag_to_binary(palisades),
      network = flag_to_binary(network),
      campus_affiliation = pmap_chr(list(humc, carrier, jfk, palisades, network), ~ make_campus_affiliation(..1, ..2, ..3, ..4, ..5)),
      campus_affiliation_detail = pmap_chr(list(humc, carrier, jfk, palisades, network), ~ make_campus_detail(..1, ..2, ..3, ..4, ..5)),
      attending = flag_to_binary(attending),
      med_ed = flag_to_binary(med_ed),
      nurse = flag_to_binary(nurse),
      other_provider = flag_to_binary(other_provider),
      committee = flag_to_binary(committee),
      consumer_health = flag_to_binary(consumer_health),
      cme = flag_to_binary(cme),
      icu_rnds = flag_to_binary(icu_rnds),
      citation_count = suppressWarnings(readr::parse_number(as.character(citation_count))),
      continuing_education = flag_to_binary(continuing_education),
      patient_care = flag_to_binary(patient_care),
      lecture = flag_to_binary(lecture),
      ebp = flag_to_binary(ebp),
      research = flag_to_binary(research),
      grant = flag_to_binary(grant),
      publication = flag_to_binary(publication),
      irb_app = flag_to_binary(irb_app),
      admin = flag_to_binary(admin),
      policy = flag_to_binary(policy),
      patient_info = flag_to_binary(patient_info)
    ) %>%
    filter(!is.na(topic), !str_to_lower(topic) %in% c("topic", month.name %>% str_to_lower()))
  cleaned
}

files <- list.files(raw_humc_dir, pattern = "\\.xlsx?$", full.names = TRUE) %>%
  keep(~ !str_detect(basename(.x), "^~\\$")) %>%
  keep(~ !str_detect(basename(.x), regex("template", ignore_case = TRUE)))

if (length(files) == 0) stop("No Excel files found in: ", raw_humc_dir)

humc_master <- files %>%
  map_dfr(read_one_search_log) %>%
  mutate(request_id = row_number()) %>%
  relocate(request_id, source_file, source_sheet, file_year, date_raw, date, year, month, topic, humc, carrier, jfk, palisades, network, campus_affiliation, campus_affiliation_detail)

check_by_file <- humc_master %>% count(source_file, file_year, year, sort = FALSE)
check_missing_dates <- humc_master %>% filter(is.na(date)) %>% count(source_file, file_year, year, sort = TRUE)
check_missing_topics <- humc_master %>% filter(is.na(topic)) %>% count(source_file, file_year, year, sort = TRUE)
check_year_mismatch <- humc_master %>% filter(!is.na(date), !is.na(file_year), year != file_year) %>% count(source_file, file_year, year, sort = TRUE)
check_campus_flags <- humc_master %>% mutate(n_campus_flags = humc + carrier + jfk + palisades + network) %>% count(n_campus_flags, humc, carrier, jfk, palisades, network, campus_affiliation, campus_affiliation_detail, sort = TRUE)
check_requestor_flags <- humc_master %>% mutate(n_requestor_flags = attending + med_ed + nurse + other_provider + committee + consumer_health) %>% count(n_requestor_flags, sort = TRUE)
check_purpose_flags <- humc_master %>% mutate(n_purpose_flags = continuing_education + patient_care + lecture + ebp + research + grant + publication + irb_app + admin + policy + patient_info) %>% count(n_purpose_flags, sort = TRUE)

write_csv(humc_master, output_path)
write_csv(check_by_file, file.path(processed_dir, paste0("humc_import_check_by_file_", run_timestamp, ".csv")))
write_csv(check_missing_dates, file.path(processed_dir, paste0("humc_missing_dates_", run_timestamp, ".csv")))
write_csv(check_missing_topics, file.path(processed_dir, paste0("humc_missing_topics_", run_timestamp, ".csv")))
write_csv(check_year_mismatch, file.path(processed_dir, paste0("humc_year_mismatch_", run_timestamp, ".csv")))
write_csv(check_campus_flags, file.path(processed_dir, paste0("humc_campus_flag_check_", run_timestamp, ".csv")))
write_csv(check_requestor_flags, file.path(processed_dir, paste0("humc_requestor_flag_check_", run_timestamp, ".csv")))
write_csv(check_purpose_flags, file.path(processed_dir, paste0("humc_purpose_flag_check_", run_timestamp, ".csv")))

message("Wrote new master HUMC CSV to: ", output_path)
message("Rows in master file: ", nrow(humc_master))
message("Files combined: ", length(files))
message("\nRows by file/year:")
print(check_by_file)
message("\nCampus flag check:")
print(check_campus_flags)
message("\nRows with missing dates:")
print(check_missing_dates)
message("\nRows with year mismatch:")
print(check_year_mismatch)
