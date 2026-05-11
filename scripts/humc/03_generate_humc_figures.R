# scripts/humc/03_generate_humc_figures.R
#
# Generate HUMC figures.
#
# Purpose:
#   Load outputs/humc/out.rds and table objects from 02_generate_tables.R,
#   then save the main HUMC analysis figures.

library(here)
library(tidyverse)
library(tidytext)
library(scales)

source(here("scripts", "shared", "paths.R"))
source(here("scripts", "shared", "helpers.R"))
source(here("scripts", "shared", "text_helpers.R"))
source(here("scripts", "shared", "output_helpers.R"))

project_name <- "humc"
paths <- make_output_paths("humc")
output_dir <- paths$output_dir
csv_dir <- paths$csv_dir
figures_dir <- paths$figures_dir
out_path <- file.path(output_dir, "out.rds")

# These RDS files are created by 02_generate_humc_tables.R and are required
# for the chi-square residual and category-purpose figures.
required_rds <- c(file.path(csv_dir, "cat_purp.rds"), file.path(csv_dir, "category_chi_results.rds"), file.path(csv_dir, "purpose_results.rds"))
if (!file.exists(out_path)) stop("Cannot find ", out_path, ". Run 01_build_humc_analysis_object.R first.")
missing_rds <- required_rds[!file.exists(required_rds)]
if (length(missing_rds) > 0) stop("Missing table RDS files:\n", paste(missing_rds, collapse = "\n"), "\nRun 02_generate_humc_tables.R first.")

# Ensure folder exists + clear old figures --------------------------

clear_output_folder(figures_dir, "\\.(png|jpg|jpeg|pdf)$")

# Now load data -----------------------------------------------------

out <- readRDS(out_path)
cat_purp <- readRDS(file.path(csv_dir, "cat_purp.rds"))
category_chi_results <- readRDS(file.path(csv_dir, "category_chi_results.rds"))
purpose_results <- readRDS(file.path(csv_dir, "purpose_results.rds"))

# Local label helper -------------------------------------------------------

clean_submitter_label <- function(x) {
  case_when(
    x == "MedEd" ~ "Resident",
    x %in% c("OtherProvider", "AlliedHealthProfessional") ~
      "Allied Health Professional",
    TRUE ~ x
  )
}

# Prepare plotting datasets ------------------------------------------------

category_order <- out$category_counts %>% 
  arrange(desc(n)) %>% 
  pull(category)

category_by_purpose <- cat_purp$category_by_purpose %>% 
  mutate(
    category = factor(category, levels = category_order), 
    category_label = str_replace_all(as.character(category), "_", " ")
    )

category_prop <- category_by_purpose %>% 
  group_by(purpose) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup()

# Prepare standardized residuals for heatmap figures.
# Positive residuals mean observed counts were higher than expected;
# negative residuals mean observed counts were lower than expected.
chi_residuals_plot <- category_chi_results$chi_residuals %>% 
  mutate(category = factor(category, levels = category_order), 
         submitter_type = factor(
           submitter_type, 
           levels = c(
             "Attending", 
             "MedEd", 
             "Nurse", 
             "OtherProvider", 
             "Unknown"
             )
           ), 
         category_label = str_replace_all(as.character(category), "_", " ")
         )

# Main chi-square interpretation figure for the write-up.
purpose_residuals_plot <- purpose_results$purpose_residuals %>%
  mutate(
    submitter_type = clean_submitter_label(submitter_type),
    submitter_type = factor(
      submitter_type,
      levels = c(
        "Attending",
        "Resident",
        "Nurse",
        "Allied Health Professional",
        "Unknown"
      )
    ),
    purpose = recode(
      purpose,
      "ContinuingEducation" = "Continuing Education",
      "PatientInfo" = "Patient Info",
      "PatientCare" = "Patient Care",
      "IRBApp" = "IRB App",
      .default = purpose
    )
  ) %>%
  filter(!is.na(submitter_type))

# Identify the most frequent lemmas within each submitter group.
top_lemmas_person <- out$lemma_counts_by_person %>%
  filter(!submitter_type %in% c("Committee", "Unknown", "Committee/Unknown")) %>%
  group_by(submitter_type) %>%
  slice_max(n, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(lemma_label = str_replace_all(lemma, "_", " "))

if (nrow(top_lemmas_person) > 0) {
  p_top_lemmas_data <- top_lemmas_person %>%
    mutate(
      submitter_plot = clean_submitter_label(submitter_type),
      submitter_plot = factor(
        submitter_plot,
        levels = c(
          "Attending",
          "Resident",
          "Nurse",
          "Allied Health Professional"
        )
      )
    ) %>%
    filter(!is.na(submitter_plot))
  
  p_top_lemmas <- ggplot(
    p_top_lemmas_data,
    aes(
      x = reorder_within(lemma_label, n, submitter_plot),
      y = n,
      fill = submitter_plot
    )
  ) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ submitter_plot, scales = "free", ncol = 2) +
    scale_x_reordered() +
    coord_flip() +
    labs(
      title = "Top Lemmas by Submitter Type, HUMC 2013–2025",
      x = NULL,
      y = "Count"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold", size = 18)
    )
  
  ggsave(
    file.path(figures_dir, "top_lemmas_by_submitter.png"),
    p_top_lemmas,
    width = 10,
    height = 6,
    dpi = 300
  )
}

if (nrow(purpose_residuals_plot) > 0) {
  p_purpose_residuals <- ggplot(
    purpose_residuals_plot,
    aes(
      x = purpose,
      y = submitter_type,
      fill = std_residual
    )
  ) +
    geom_tile(color = "white", linewidth = 0.4) +
    scale_fill_gradient2(
      low = "#3B6FB6",
      mid = "white",
      high = "#D94F3D",
      midpoint = 0
    ) +
    labs(
      title = "Standardized Residuals for Purpose by Submitter Type, HUMC 2013–2025",
      x = NULL,
      y = NULL,
      fill = "Residual"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1),
      axis.text.y = element_text(face = "bold"),
      plot.title = element_text(face = "bold", size = 16),
      plot.margin = margin(10, 25, 15, 25)
    )
  
  ggsave(
    file.path(figures_dir, "purpose_submitter_residuals.png"),
    p_purpose_residuals,
    width = 11,
    height = 6.5,
    dpi = 300
  )
}

message("Figures saved to: ", figures_dir)
