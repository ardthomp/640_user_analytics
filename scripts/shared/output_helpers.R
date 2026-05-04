library(dplyr)
library(janitor)
library(stringr)
library(scales)
library(openxlsx)
library(stringi)
library(purrr)
library(readr) # Explicitly load readr

# -------------------------------------------------------------------
# ---- Core Archiving Logic (New) ----
# -------------------------------------------------------------------

#' Archive a single file if it exists, then execute a writer function.
#'
#' This is the new engine for all writing functions. It implements the
#' "Current & Archive" model.
#'
#' @param writer_func A function that takes a path as its first argument
#'   and writes a file.
#' @param current_path The full path to the file to be written (e.g., "outputs/data.csv").
#' @param ... Arguments passed on to the `writer_func`.
archive_and_write <- function(writer_func, current_path, ...) {
  
  # 1. Define archive path and create directory if needed
  output_dir <- dirname(current_path)
  archive_dir <- file.path(output_dir, "archive")
  
  # 2. If the "current" file exists, move it to the archive
  if (file.exists(current_path)) {
    
    # Ensure the archive sub-directory exists
    dir.create(archive_dir, showWarnings = FALSE, recursive = TRUE)
    
    # Get the file's last modification time to use in the archive name
    mod_time <- file.info(current_path)$mtime
    timestamp <- format(mod_time, "%Y_%m_%d-%H-%M-%S")
    
    # Get file components
    base_name <- tools::file_path_sans_ext(basename(current_path))
    extension <- tools::file_ext(current_path)
    
    # Define the new, timestamped name for the archived file
    archive_filename <- paste0(base_name, "_", timestamp, ".", extension)
    archive_path <- file.path(archive_dir, archive_filename)
    
    # Move the old file to the archive
    file.rename(from = current_path, to = archive_path)
  }
  
  # 3. Now that the old file is gone, execute the writer function to create the new one
  writer_func(path = current_path, ...)
  
  invisible(current_path)
}


# -------------------------------------------------------------------
# ---- Your Existing Helper Functions (Unchanged) ----
# -------------------------------------------------------------------

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
      across(
        where(is.list),
        ~ purrr::map_chr(.x, ~ paste(repair_utf8(.x), collapse = "; "))
      ),
      across(
        where(is.numeric),
        ~ round(.x, 2)
      ),
      across(
        matches("(^prop$|proportion|percent|pct)"),
        ~ scales::percent(as.numeric(.x), accuracy = 0.1)
      )
    ) %>%
    mutate(
      across(
        everything(),
        repair_utf8
      )
    ) %>%
    pretty_names()
  
  names(df) <- repair_utf8(names(df))
  
  df
}


# -------------------------------------------------------------------
# ---- New and Improved Write Functions ----
# -------------------------------------------------------------------

#' Write a formatted CSV, archiving the previous version first.
#'
#' @param df The data frame to write.
#' @param filename The base name of the file (without extension).
#' @param csv_dir The directory where the CSV should be saved.
write_archived_csv <- function(df, filename, csv_dir) {
  
  # Define the full path for the "current" file
  current_file_path <- file.path(csv_dir, paste0(filename, ".csv"))
  
  # Define the function that does the actual writing of the formatted data
  csv_writer <- function(path, data) {
    df_pretty <- format_export_table(data)
    readr::write_csv(df_pretty, path)
  }
  
  # Use the new archiving engine
  archive_and_write(
    writer_func = csv_writer,
    current_path = current_file_path,
    data = df # Pass the data frame to the writer function
  )
}


#' Write a formatted Excel workbook, archiving the previous version first.
#'
#' @param tables A named list of data frames to write to sheets.
#' @param path The full path for the final .xlsx file.
write_archived_workbook <- function(tables, path) {
  
  # Define the function that does the actual writing
  workbook_writer <- function(path, table_list) {
    wb <- openxlsx::createWorkbook()
    header_style <- openxlsx::createStyle(
      textDecoration = "bold", fgFill = "#D9EAF7", border = "Bottom", halign = "center"
    )
    
    for (sheet_name in names(table_list)) {
      safe_sheet_name <- repair_utf8(sheet_name)
      safe_sheet_name <- stringr::str_sub(safe_sheet_name, 1, 31)
      df <- format_export_table(table_list[[sheet_name]])
      
      openxlsx::addWorksheet(wb, safe_sheet_name)
      openxlsx::writeData(wb, sheet = safe_sheet_name, x = df, keepNA = FALSE)
      openxlsx::addStyle(wb, sheet = safe_sheet_name, style = header_style, rows = 1, cols = seq_len(ncol(df)), gridExpand = TRUE)
      openxlsx::freezePane(wb, sheet = safe_sheet_name, firstRow = TRUE)
      openxlsx::setColWidths(wb, sheet = safe_sheet_name, cols = seq_len(ncol(df)), widths = "auto")
    }
    
    openxlsx::saveWorkbook(wb, file = path, overwrite = TRUE)
  }
  
  # Use the new archiving engine
  archive_and_write(
    writer_func = workbook_writer,
    current_path = path,
    table_list = tables # Pass the list of tables to the writer function
  )
}
