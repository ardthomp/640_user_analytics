# scripts/humc/00_build_humc_master_csv.R
#
# This script's sole purpose is to read all the raw annual Excel files,
# combine them, perform basic cleaning, and save a single, unified
# 'humc.csv' file to the 'data/processed' directory.

library(tidyverse)
library(readxl)
library(here)

# --- 1. Setup ---
source(here("scripts", "shared", "helpers.R")) # For flag_to_binary

# Define input and output paths
raw_humc_dir <- here("data", "raw", "humc")
output_path <- here("data", "processed", "humc.csv")

# Find all Excel files in the raw directory
excel_files <- list.files(
  path = raw_humc_dir,
  pattern = "\\.xlsx?$",
  full.names = TRUE,
  recursive = TRUE
)

# --- 2. Load and Combine Data ---
# Read each Excel file and combine them into one large data frame
all_humc_data <- excel_files %>%
  map_dfr(~ read_excel(.x, col_types = "text"), .id = "source_file") %>%
  janitor::clean_names() %>%
  mutate(source_file = basename(source_file))

# --- 3. Clean and Standardize Data ---
# This section performs the essential cleaning steps that were previously
# spread across multiple data quality checks.
humc_cleaned <- all_humc_data %>%
  # Ensure critical columns exist, adding them as NA if they don't
  mutate(
    across(everything(), as.character), # Ensure all columns are character for safe binding
    request_id = row_number() # Create a unique ID for each row
  ) %>%
  # Perform data type conversions and flag cleaning
  transmute(
    request_id,
    source_file,
    date = suppressWarnings(as.Date(as.numeric(date), origin = "1899-12-30")),
    topic,
    campus_affiliation,
    campus_affiliation_detail,
    citation_count = suppressWarnings(as.numeric(citation_count)),
    
    # Convert all flag columns to binary 0/1 using the shared helper
    humc = flag_to_binary(humc),
    carrier = flag_to_binary(carrier),
    jfk = flag_to_binary(jfk),
    palisades = flag_to_binary(palisades),
    network = flag_to_binary(network),
    attending = flag_to_binary(attending),
    med_ed = flag_to_binary(med_ed),
    nurse = flag_to_binary(nurse),
    other_provider = flag_to_binary(other_provider),
    committee = flag_to_binary(committee),
    consumer_health = flag_to_binary(consumer_health),
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
  # Filter out rows that have no date, as they are unusable for time-series analysis
  filter(!is.na(date))

# --- 4. Save the Final Master File ---
write_csv(humc_cleaned, output_path)

cat("--- HUMC Master File Build Complete ---\n")
cat("Successfully combined", length(excel_files), "Excel files.\n")
cat("Final master file saved to:", output_path, "\n")