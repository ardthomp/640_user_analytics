# scripts/shared/output_helpers.R
#
# Shared output helpers.
#
# Main idea:
#   The default workflow now overwrites "latest" outputs instead of archiving
#   every run. This prevents the project from creating thousands of old CSV,
#   RDS, workbook, HTML, and figure files.
#
# Notes:
#   - clear_output_folder() is used at the start of scripts to remove previous
#     outputs from a folder.
#   - write_pretty_csv() and write_pretty_workbook() overwrite files.
#   - write_archived_csv() and write_archived_workbook() are kept as aliases so
#     older scripts still run, but they no longer archive by default.
#   - archive_existing_files() is kept for rare cases where you intentionally
#     want to archive files.

library(dplyr)
library(janitor)
library(stringr)
library(scales)
library(openxlsx)
library(stringi)
library(purrr)
library(readr)

# Folder management --------------------------------------------------------

clear_output_folder <- function(folder, pattern = NULL) {
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
    return(invisible(NULL))
  }
  
  files <- list.files(folder, full.names = TRUE)
  
  if (!is.null(pattern)) {
    files <- files[stringr::str_detect(basename(files), pattern)]
  }
  
  if (length(files) > 0) {
    file.remove(files)
  }
  
  invisible(NULL)
}

archive_existing_files <- function(source_dir, pattern, archive_parent_dir, timestamp = NULL) {
  if (!dir.exists(source_dir)) {
    dir.create(source_dir, recursive = TRUE)
    return(invisible(NULL))
  }
  
  files_to_archive <- list.files(
    path = source_dir,
    pattern = pattern,
    full.names = TRUE
  )
  
  if (length(files_to_archive) == 0) {
    return(invisible(NULL))
  }
  
  if (is.null(timestamp)) {
    timestamp <- format(Sys.time(), "%Y_%m_%d-%H-%M-%S")
  }
  
  archive_dir <- file.path(archive_parent_dir, timestamp)
  
  if (!dir.exists(archive_dir)) {
    dir.create(archive_dir, recursive = TRUE)
  }
  
  file.rename(
    from = files_to_archive,
    to = file.path(archive_dir, basename(files_to_archive))
  )
  
  invisible(archive_dir)
}

# Text/encoding helpers ----------------------------------------------------

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
    mutate(across(everything(), repair_utf8)) %>%
    pretty_names()
  
  names(df) <- repair_utf8(names(df))
  
  df
}

# CSV writers --------------------------------------------------------------

write_pretty_csv <- function(df, filename, csv_dir) {
  if (!dir.exists(csv_dir)) {
    dir.create(csv_dir, recursive = TRUE)
  }
  
  if (!stringr::str_detect(filename, "\\.csv$")) {
    filename <- paste0(filename, ".csv")
  }
  
  current_file_path <- file.path(csv_dir, filename)
  
  df_pretty <- format_export_table(df)
  
  readr::write_csv(df_pretty, current_file_path)
  
  invisible(current_file_path)
}

# Backward-compatible name.
# This no longer archives by default. It overwrites the latest file.
write_archived_csv <- function(df, filename, csv_dir) {
  write_pretty_csv(
    df = df,
    filename = filename,
    csv_dir = csv_dir
  )
}

# Workbook writers ---------------------------------------------------------

write_pretty_workbook <- function(tables, path) {
  if (!dir.exists(dirname(path))) {
    dir.create(dirname(path), recursive = TRUE)
  }
  
  tables <- tables[
    purrr::map_lgl(tables, ~ is.data.frame(.x) && ncol(.x) > 0)
  ]
  
  wb <- openxlsx::createWorkbook()
  
  header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    fgFill = "#D9EAF7",
    border = "Bottom",
    halign = "center"
  )
  
  if (length(tables) == 0) {
    openxlsx::addWorksheet(wb, "No Tables")
    openxlsx::writeData(
      wb,
      sheet = "No Tables",
      x = data.frame(Message = "No non-empty tables were available to export.")
    )
  } else {
    used_sheet_names <- character()
    
    for (sheet_name in names(tables)) {
      safe_sheet_name <- repair_utf8(sheet_name)
      safe_sheet_name <- stringr::str_replace_all(safe_sheet_name, "[:\\\\/?*\\[\\]]", " ")
      safe_sheet_name <- stringr::str_squish(safe_sheet_name)
      safe_sheet_name <- stringr::str_sub(safe_sheet_name, 1, 31)
      
      if (is.na(safe_sheet_name) || safe_sheet_name == "") {
        safe_sheet_name <- "Sheet"
      }
      
      original_safe_name <- safe_sheet_name
      counter <- 1
      
      while (safe_sheet_name %in% used_sheet_names) {
        suffix <- paste0("_", counter)
        safe_sheet_name <- paste0(
          stringr::str_sub(original_safe_name, 1, 31 - nchar(suffix)),
          suffix
        )
        counter <- counter + 1
      }
      
      used_sheet_names <- c(used_sheet_names, safe_sheet_name)
      
      df <- format_export_table(tables[[sheet_name]])
      
      openxlsx::addWorksheet(wb, safe_sheet_name)
      openxlsx::writeData(wb, sheet = safe_sheet_name, x = df, keepNA = FALSE)
      
      openxlsx::addStyle(
        wb,
        sheet = safe_sheet_name,
        style = header_style,
        rows = 1,
        cols = seq_len(ncol(df)),
        gridExpand = TRUE
      )
      
      openxlsx::freezePane(wb, sheet = safe_sheet_name, firstRow = TRUE)
      openxlsx::setColWidths(
        wb,
        sheet = safe_sheet_name,
        cols = seq_len(ncol(df)),
        widths = "auto"
      )
    }
  }
  
  openxlsx::saveWorkbook(wb, file = path, overwrite = TRUE)
  
  invisible(path)
}

# Backward-compatible name.
# This no longer archives by default. It overwrites the latest workbook.
write_archived_workbook <- function(tables, path) {
  write_pretty_workbook(
    tables = tables,
    path = path
  )
}

# Optional explicit archive-and-write helper -------------------------------
#
# Use this only if you really want archive behavior for a specific output.
# Most scripts should use write_pretty_csv() or write_pretty_workbook().

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
