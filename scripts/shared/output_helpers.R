# scripts/shared/output_helpers.R
# Line endings normalized for GitHub display

library(dplyr)
library(janitor)
library(stringr)
library(scales)
library(openxlsx)
library(stringi)
library(purrr)
library(readr)

clear_output_folder <- function(folder, pattern = NULL) {
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
    return(invisible(NULL))
  }
  files <- list.files(folder, full.names = TRUE)
  if (!is.null(pattern)) {
    files <- files[stringr::str_detect(basename(files), pattern)]
  }
  if (length(files) > 0) file.remove(files)
  invisible(NULL)
}

repair_utf8 <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "UTF-8", sub = "")
  x <- stringi::stri_enc_toutf8(x)
  x[is.na(x)] <- ""
  x
}

pretty_names <- function(df) {
  df %>%
    janitor::clean_names() %>%
    rename_with(~ stringr::str_replace_all(.x, "_", " ")) %>%
    rename_with(stringr::str_to_title)
}

format_export_table <- function(df) {
  df %>%
    mutate(
      across(where(is.numeric), ~ round(.x, 2)),
      across(matches("prop|percent|pct"), ~ scales::percent(as.numeric(.x), accuracy = 0.1))
    ) %>%
    mutate(across(everything(), repair_utf8)) %>%
    pretty_names()
}

write_pretty_csv <- function(df, filename, csv_dir) {
  if (!dir.exists(csv_dir)) dir.create(csv_dir, recursive = TRUE)
  path <- file.path(csv_dir, paste0(filename, ".csv"))
  readr::write_csv(format_export_table(df), path)
  invisible(path)
}

write_pretty_workbook <- function(tables, path) {
  if (!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)
  wb <- openxlsx::createWorkbook()
  for (nm in names(tables)) {
    openxlsx::addWorksheet(wb, substr(nm, 1, 31))
    openxlsx::writeData(wb, nm, format_export_table(tables[[nm]]))
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}
