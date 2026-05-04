# scripts/shared/output_helpers.R

library(dplyr)
library(janitor)
library(stringr)
library(scales)
library(openxlsx)
library(stringi)
library(purrr)
library(readr)

# Core Archiving Logic
archive_and_write <- function(writer_func, current_path, ...) {
  output_dir <- dirname(current_path)
  archive_dir <- file.path(output_dir, "archive")
  
  if (file.exists(current_path)) {
    dir.create(archive_dir, showWarnings = FALSE, recursive = TRUE)
    mod_time <- file.info(current_path)$mtime
    timestamp <- format(mod_time, "%Y_%m_%d-%H-%M-%S")
    base_name <- tools::file_path_sans_ext(basename(current_path))
    extension <- tools::file_ext(current_path)
    archive_filename <- paste0(base_name, "_", timestamp, ".", extension)
    archive_path <- file.path(archive_dir, archive_filename)
    file.rename(from = current_path, to = archive_path)
  }
  
  writer_func(path = current_path, ...)
  invisible(current_path)
}

# Your Existing Helper Functions (Unchanged)
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
  df <- df %>%
    mutate(
      across(where(is.list), ~ purrr::map_chr(.x, ~ paste(repair_utf8(.x), collapse = "; "))),
      across(where(is.numeric), ~ round(.x, 2)),
      across(matches("(^prop$|proportion|percent|pct)"), ~ scales::percent(as.numeric(.x), accuracy = 0.1))
    ) %>%
    mutate(across(everything(), repair_utf8)) %>%
    pretty_names()
  
  names(df) <- repair_utf8(names(df))
  df
}

# New and Improved Write Functions
write_archived_csv <- function(df, filename, csv_dir) {
  current_file_path <- file.path(csv_dir, paste0(filename, ".csv"))
  csv_writer <- function(path, data) {
    df_pretty <- format_export_table(data)
    readr::write_csv(df_pretty, path)
  }
  archive_and_write(writer_func = csv_writer, current_path = current_file_path, data = df)
}

write_archived_workbook <- function(tables, path) {
  workbook_writer <- function(path, table_list) {
    wb <- openxlsx::createWorkbook()
    header_style <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#D9EAF7", border = "Bottom", halign = "center")
    for (sheet_name in names(table_list)) {
      safe_sheet_name <- stringr::str_sub(repair_utf8(sheet_name), 1, 31)
      df <- format_export_table(table_list[[sheet_name]])
      openxlsx::addWorksheet(wb, safe_sheet_name)
      openxlsx::writeData(wb, sheet = safe_sheet_name, x = df, keepNA = FALSE)
      openxlsx::addStyle(wb, sheet = safe_sheet_name, style = header_style, rows = 1, cols = seq_len(ncol(df)), gridExpand = TRUE)
      openxlsx::freezePane(wb, sheet = safe_sheet_name, firstRow = TRUE)
      openxlsx::setColWidths(wb, sheet = safe_sheet_name, cols = seq_len(ncol(df)), widths = "auto")
    }
    openxlsx::saveWorkbook(wb, file = path, overwrite = TRUE)
  }
  archive_and_write(writer_func = workbook_writer, current_path = path, table_list = tables)
}