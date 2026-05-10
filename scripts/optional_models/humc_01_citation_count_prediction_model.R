library(here)
library(tidyverse)
library(broom)
library(ggeffects)
library(gt)
library(patchwork)
library(emmeans)

source(here("scripts", "shared", "paths.R"))
source(here("scripts", "shared", "helpers.R"))
source(here("scripts", "shared", "output_helpers.R"))

# Optional model output paths -----------------------------------------------

humc_paths <- make_output_paths("humc")
humc_output_dir <- humc_paths$output_dir
out_path <- file.path(humc_output_dir, "out.rds")

out <- readRDS(out_path)

model_output_dir <- here(
  "outputs",
  "optional_models",
  "humc_01_citation_count_prediction_model"
)

figures_dir <- file.path(model_output_dir, "figures")
tables_dir  <- file.path(model_output_dir, "tables")

dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

clear_output_folder(
  model_output_dir,
  "\\.(csv|rds|xlsx|html|png|jpg|jpeg|pdf)$"
)

# Build modeling dataset ------------------------------------------

model_data <- out$my_data2 %>%
  mutate(
    purpose_group = case_when(
      patient_care == 1 | patient_info == 1 ~ "Clinical",
      research == 1 | publication == 1 ~ "Research/Publication",
      ebp == 1 ~ "Evidence-Based Practice",
      lecture == 1 | continuing_education == 1 ~ "Education",
      grant == 1 | irb_app == 1 | admin == 1 | policy == 1 ~ "Administrative/Policy",
      TRUE ~ "Other/Unknown"
    ),
    submitter_type = case_when(
      submitter_type %in% c("Committee", "Unknown") ~ "Other",
      submitter_type == "OtherProvider" ~ "Allied Health Professional",
      submitter_type == "MedEd" ~ "Residents/Fellows",
      TRUE ~ as.character(submitter_type)
    ),
    submitter_type = factor(submitter_type),
    purpose_group = factor(purpose_group),
    year = factor(year)
  ) %>%
  filter(
    !is.na(citation_count),
    !is.na(year)
  ) %>%
  mutate(
    submitter_type = relevel(submitter_type, ref = "Attending"),
    purpose_group = relevel(purpose_group, ref = "Clinical"),
    year = relevel(year, ref = "2019")
  )

# Descriptive checks -----------------------------------------------

citation_check <- model_data %>%
  summarize(
    n_requests = n(),
    mean_citation_count = mean(citation_count, na.rm = TRUE),
    variance_citation_count = var(citation_count, na.rm = TRUE),
    overdispersion_ratio = variance_citation_count / mean_citation_count,
    median_citation_count = median(citation_count, na.rm = TRUE),
    max_citation_count = max(citation_count, na.rm = TRUE)
  )

submitter_summary <- model_data %>%
  group_by(submitter_type) %>%
  summarize(
    n_requests = n(),
    mean_citations = mean(citation_count, na.rm = TRUE),
    median_citations = median(citation_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_requests))

purpose_summary <- model_data %>%
  group_by(purpose_group) %>%
  summarize(
    n_requests = n(),
    mean_citations = mean(citation_count, na.rm = TRUE),
    median_citations = median(citation_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_requests))

year_summary <- model_data %>%
  group_by(year) %>%
  summarize(
    n_requests = n(),
    mean_citations = mean(citation_count, na.rm = TRUE),
    median_citations = median(citation_count, na.rm = TRUE),
    .groups = "drop"
  )

# Fit quasi-Poisson model ------------------------------------------

model_quasi <- glm(
  citation_count ~ submitter_type + purpose_group + year,
  data = model_data,
  family = quasipoisson(link = "log")
)

model_summary <- summary(model_quasi)

# Exportable model results ----------------------------------------

model_results <- broom::tidy(
  model_quasi,
  exponentiate = TRUE,
  conf.int = TRUE
) %>%
  mutate(
    term_clean = term %>%
      str_replace("^submitter_type", "Requestor: ") %>%
      str_replace("^purpose_group", "Purpose: ") %>%
      str_replace("^year", "Year: ") %>%
      str_replace_all("_", " "),
    term_clean = case_when(
      term == "(Intercept)" ~ "Reference group: Attending, Clinical, 2019",
      TRUE ~ term_clean
    ),
    interpretation = case_when(
      term == "(Intercept)" ~ "Expected citation count for the reference group",
      TRUE ~ "Rate ratio compared with reference group"
    )
  ) %>%
  dplyr::select(
    term,
    term_clean,
    estimate,
    conf.low,
    conf.high,
    std.error,
    statistic,
    p.value,
    interpretation
  )

model_results_clean <- model_results %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 3))
  )

# Save model objects
saveRDS(model_data, file.path(model_output_dir, "model_data.rds"))
saveRDS(model_quasi, file.path(model_output_dir, "quasipoisson_citation_model.rds"))

# Export CSVs ------------------------------------------------------

write_pretty_csv(citation_check, "model_citation_check", tables_dir)
write_pretty_csv(submitter_summary, "model_submitter_summary", tables_dir)
write_pretty_csv(purpose_summary, "model_purpose_summary", tables_dir)
write_pretty_csv(year_summary, "model_year_summary", tables_dir)
write_pretty_csv(model_results_clean, "quasipoisson_model_results", tables_dir)

# Model figures and formatted outputs --------------------------------------

## Adjusted effects: submitter/requestor ------------------------------------

pred_submitter <- ggpredict(model_quasi, terms = "submitter_type")

p_submitter_effect <- ggplot(
  pred_submitter,
  aes(x = x, y = predicted)
) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0.2
  ) +
  labs(
    title = "Adjusted Citation Counts by Requestor",
    x = "Requestor",
    y = "Predicted Citation Count"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    plot.title = element_text(face = "bold", size = 15)
  )

ggsave(
  file.path(figures_dir, "adjusted_effects_requestor.png"),
  p_submitter_effect,
  width = 8,
  height = 5,
  dpi = 300
)

## Time trends: model predictions + actual yearly means ---------------------

pred_year <- tibble(
  year = levels(model_data$year)
) %>%
  mutate(
    submitter_type = "Attending",
    purpose_group = "Clinical",
    year = factor(year, levels = levels(model_data$year)),
    submitter_type = factor(submitter_type, levels = levels(model_data$submitter_type)),
    purpose_group = factor(purpose_group, levels = levels(model_data$purpose_group))
  )

pred_year_link <- predict(
  model_quasi,
  newdata = pred_year,
  type = "link",
  se.fit = TRUE
)

pred_year <- pred_year %>%
  mutate(
    year_numeric = as.numeric(as.character(year)),
    predicted = exp(pred_year_link$fit),
    conf.low = exp(pred_year_link$fit - 1.96 * pred_year_link$se.fit),
    conf.high = exp(pred_year_link$fit + 1.96 * pred_year_link$se.fit),
    series = "Model-predicted count"
  ) %>%
  arrange(year_numeric)

actual_year <- model_data %>%
  group_by(year) %>%
  summarize(
    year_numeric = as.numeric(as.character(first(year))),
    actual_mean = mean(citation_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(series = "Actual yearly mean")

p_time <- ggplot() +
  geom_ribbon(
    data = pred_year,
    aes(x = year_numeric, ymin = conf.low, ymax = conf.high),
    alpha = 0.10
  ) +
  geom_line(
    data = pred_year,
    aes(x = year_numeric, y = predicted, linetype = series),
    linewidth = 1
  ) +
  geom_point(
    data = pred_year,
    aes(x = year_numeric, y = predicted, shape = series),
    size = 2
  ) +
  geom_line(
    data = actual_year,
    aes(x = year_numeric, y = actual_mean, linetype = series),
    linewidth = 0.9
  ) +
  geom_point(
    data = actual_year,
    aes(x = year_numeric, y = actual_mean, shape = series),
    size = 2
  ) +
  scale_x_continuous(
    breaks = pred_year$year_numeric
  ) +
  labs(
    title = "Citation Counts Over Time",
    subtitle = "Model predictions are for Attending Clinical requests; observed means include all request types",
    x = "Year",
    y = "Citation Count",
    linetype = NULL,
    shape = NULL,
    caption = "Grey ribbon shows the 95% confidence interval around model-predicted counts."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 15),
    legend.position = "bottom"
  )

ggsave(
  file.path(figures_dir, "predicted_vs_actual_citations_over_time.png"),
  p_time,
  width = 11,
  height = 6,
  dpi = 300
)

## Add reference rows so they appear in forest plot -------------------------

reference_rows <- tibble(
  term = c(
    "submitter_typeAttending",
    "year2019",
    "purpose_groupClinical"
  ),
  estimate = c(1, 1, 1),
  conf.low = c(NA_real_, NA_real_, NA_real_),
  conf.high = c(NA_real_, NA_real_, NA_real_),
  std.error = c(NA_real_, NA_real_, NA_real_),
  statistic = c(NA_real_, NA_real_, NA_real_),
  p.value = c(NA_real_, NA_real_, NA_real_),
  interpretation = c(
    "Reference group",
    "Reference group",
    "Reference group"
  )
)

model_results_with_ref <- bind_rows(
  model_results,
  reference_rows
)

## Forest plot styled with grouped sections ---------------------------------

forest_data <- model_results_with_ref %>%
  dplyr::filter(term != "(Intercept)") %>%
  mutate(
    group = case_when(
      str_detect(term, "^submitter[ _]type") ~ "Requestor",
      str_detect(term, "^year") ~ "Year",
      str_detect(term, "^purpose[ _]group") ~ "Purpose",
      TRUE ~ NA_character_
    ),
    clean_term = term %>%
      str_replace("^submitter[ _]type", "") %>%
      str_replace("^purpose[ _]group", "") %>%
      str_replace("^year", "") %>%
      str_replace_all("_", " ") %>%
      str_squish(),
    clean_term = case_when(
      clean_term == "AlliedHealthProfessional" ~ "Allied Health Professional",
      TRUE ~ clean_term
    ),
    is_reference = interpretation == "Reference group"
  ) %>%
  dplyr::filter(!is.na(group))

## Desired display order ----------------------------------------------------

requestor_terms <- c(
  "Other",
  "Allied Health Professional",
  "Attending",
  "Nurse",
  "Residents/Fellows"
)

year_terms <- forest_data %>%
  dplyr::filter(group == "Year") %>%
  pull(clean_term) %>%
  unique() %>%
  sort()

purpose_terms <- c(
  "Clinical",
  "Research/Publication",
  "Evidence-Based Practice",
  "Education",
  "Administrative/Policy",
  "Other/Unknown"
)

display_order <- c(
  "Requestor",
  requestor_terms,
  "Year",
  year_terms,
  "Purpose",
  purpose_terms
)

display_order <- display_order[!duplicated(display_order)]

position_lookup <- tibble(
  clean_term = display_order,
  y_position = rev(seq_along(display_order))
)

forest_plot_data <- forest_data %>%
  left_join(position_lookup, by = "clean_term") %>%
  dplyr::filter(!is.na(y_position))

section_data <- tibble(
  section = c("Requestor", "Year", "Purpose"),
  clean_term = c("Requestor", "Year", "Purpose")
) %>%
  left_join(position_lookup, by = "clean_term")

x_min <- 0.08
x_max <- max(forest_plot_data$conf.high, na.rm = TRUE) * 1.25

## Forest plot --------------------------------------------------------------

p_forest <- ggplot() +
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "gray35",
    linewidth = 0.5
  ) +
  geom_errorbarh(
    data = forest_plot_data %>%
      dplyr::filter(!is.na(conf.low), !is.na(conf.high)),
    aes(
      xmin = conf.low,
      xmax = conf.high,
      y = y_position
    ),
    height = 0.18,
    color = "black",
    linewidth = 0.4
  ) +
  geom_point(
    data = forest_plot_data,
    aes(
      x = estimate,
      y = y_position,
      shape = is_reference
    ),
    size = 2.2,
    color = "black"
  ) +
  geom_segment(
    data = section_data,
    aes(
      x = x_min,
      xend = x_max,
      y = y_position,
      yend = y_position,
      color = section
    ),
    linewidth = 0.8,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = section_data,
    aes(
      x = x_min,
      y = y_position,
      label = section,
      color = section
    ),
    hjust = 0,
    vjust = -0.7,
    fontface = "bold",
    size = 4,
    inherit.aes = FALSE
  ) +
  scale_x_log10(
    limits = c(x_min, x_max),
    breaks = c(0.1, 1, 10),
    labels = c("0.1", "1.0", "10.0")
  ) +
  scale_y_continuous(
    breaks = position_lookup$y_position,
    labels = ifelse(
      position_lookup$clean_term %in% c("Requestor", "Year", "Purpose"),
      "",
      stringr::str_wrap(position_lookup$clean_term, width = 30)
    )
  ) +
  scale_color_manual(
    values = c(
      "Requestor" = "#1F4E79",
      "Year" = "#007A3D",
      "Purpose" = "#8E44AD"
    ),
    guide = "none"
  ) +
  scale_shape_manual(
    values = c(
      `FALSE` = 16,
      `TRUE` = 1
    ),
    guide = "none"
  ) +
  labs(
    title = "Rate Ratios for Citation Count (Quasi-Poisson Model)",
    x = "Rate Ratio (log scale)",
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.text.y = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    plot.margin = margin(10, 20, 10, 10)
  )

ggsave(
  file.path(figures_dir, "citation_rate_ratios_forest_grouped.png"),
  p_forest,
  width = 11,
  height = 8,
  dpi = 300
)

## Combined model figure ----------------------------------------------------

p_model_combined <- (
  (p_submitter_effect / p_time) | p_forest
) +
  patchwork::plot_layout(widths = c(1, 1.9)) +
  patchwork::plot_annotation(
    title = "Quasi-Poisson Model Results for HUMC Citation Counts",
    subtitle = "Adjusted predictions and rate ratios"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12)
  )

ggsave(
  file.path(figures_dir, "combined_model_results_figure.png"),
  p_model_combined,
  width = 21,
  height = 10,
  dpi = 300
)

## Export HTML model table --------------------------------------------------

model_results_table <- model_results %>%
  mutate(
    term = term %>%
      str_replace("^submitter_type", "Requestor: ") %>%
      str_replace("^purpose_group", "Purpose: ") %>%
      str_replace("^year", "Year: ") %>%
      str_replace_all("_", " "),
    term = case_when(
      term == "Requestor: AlliedHealthProfessional" ~ "Requestor: Allied Health Professional",
      TRUE ~ term
    )
  ) %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 3))
  )

gt_model <- model_results_table %>%
  gt() %>%
  tab_header(
    title = "Quasi-Poisson Model Predicting Citation Count"
  ) %>%
  cols_label(
    term = "Term",
    estimate = "Rate Ratio",
    conf.low = "Lower CI",
    conf.high = "Upper CI",
    std.error = "Std. Error",
    statistic = "Statistic",
    p.value = "p-value",
    interpretation = "Interpretation"
  )

gtsave(
  gt_model,
  file.path(model_output_dir, "quasipoisson_model_results.html")
)

# Pairwise comparisons using emmeans ---------------------------------------

## --- Requestor comparisons ------------------------------------------------

emm_requestor <- emmeans(
  model_quasi,
  ~ submitter_type,
  type = "response"
)

pairs_requestor <- contrast(
  emm_requestor,
  method = "pairwise",
  type = "response"
)

requestor_raw <- as.data.frame(
  summary(
    pairs_requestor,
    infer = c(TRUE, TRUE),
    type = "response"
  )
) %>%
  rename_with(~ str_replace_all(.x, "\\.", "_")) %>%
  rename_with(~ str_replace_all(.x, " ", "_"))

names(requestor_raw)

requestor_results <- requestor_raw %>%
  mutate(
    rate_ratio = dplyr::coalesce(
      if ("ratio" %in% names(.)) ratio else NA_real_,
      if ("estimate" %in% names(.)) estimate else NA_real_
    ),
    conf.low = dplyr::coalesce(
      if ("asymp_LCL" %in% names(.)) asymp_LCL else NA_real_,
      if ("lower_CL" %in% names(.)) lower_CL else NA_real_
    ),
    conf.high = dplyr::coalesce(
      if ("asymp_UCL" %in% names(.)) asymp_UCL else NA_real_,
      if ("upper_CL" %in% names(.)) upper_CL else NA_real_
    ),
    comparison = contrast,
    interpretation = "Rate ratio comparing requestor types, adjusted for purpose and year"
  ) %>%
  dplyr::select(
    comparison,
    rate_ratio,
    conf.low,
    conf.high,
    dplyr::any_of(c("SE", "z_ratio", "t_ratio")),
    p_value,
    interpretation
  )

write_pretty_csv(
  requestor_results,
  "model_requestor_pairwise_comparisons",
  tables_dir
)

## Mini figure: requestor pairwise comparisons ------------------------------

requestor_plot_data <- requestor_results %>%
  mutate(
    comparison = str_replace_all(comparison, " - ", " vs. "),
    comparison = str_wrap(comparison, width = 35)
  ) %>%
  arrange(rate_ratio) %>%
  mutate(
    comparison = factor(comparison, levels = comparison)
  )

p_requestor_pairwise <- ggplot(
  requestor_plot_data,
  aes(x = rate_ratio, y = comparison)
) +
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "gray40"
  ) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0.2
  ) +
  geom_point(size = 2.5) +
  scale_x_log10() +
  labs(
    title = "Adjusted Requestor Comparisons",
    subtitle = "Rate ratios from quasi-Poisson model, adjusted for purpose and year",
    x = "Rate Ratio (log scale)",
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

ggsave(
  file.path(figures_dir, "requestor_pairwise_comparisons.png"),
  p_requestor_pairwise,
  width = 9,
  height = 6,
  dpi = 300
)

## --- Purpose comparisons --------------------------------------------------

emm_purpose <- emmeans(
  model_quasi,
  ~ purpose_group,
  type = "response"
)

pairs_purpose <- contrast(
  emm_purpose,
  method = "pairwise",
  type = "response"
)

purpose_raw <- as.data.frame(
  summary(
    pairs_purpose,
    infer = c(TRUE, TRUE),
    type = "response"
  )
) %>%
  rename_with(~ str_replace_all(.x, "\\.", "_")) %>%
  rename_with(~ str_replace_all(.x, " ", "_"))

purpose_results <- purpose_raw %>%
  mutate(
    rate_ratio = dplyr::coalesce(
      if ("ratio" %in% names(.)) ratio else NA_real_,
      if ("estimate" %in% names(.)) estimate else NA_real_
    ),
    conf.low = dplyr::coalesce(
      if ("asymp_LCL" %in% names(.)) asymp_LCL else NA_real_,
      if ("lower_CL" %in% names(.)) lower_CL else NA_real_
    ),
    conf.high = dplyr::coalesce(
      if ("asymp_UCL" %in% names(.)) asymp_UCL else NA_real_,
      if ("upper_CL" %in% names(.)) upper_CL else NA_real_
    ),
    comparison = contrast,
    interpretation = "Rate ratio comparing purpose groups, adjusted for requestor and year"
  ) %>%
  dplyr::select(
    comparison,
    rate_ratio,
    conf.low,
    conf.high,
    dplyr::any_of(c("SE", "z_ratio", "t_ratio")),
    p_value,
    interpretation
  )

write_pretty_csv(
  purpose_results,
  "model_purpose_pairwise_comparisons",
  tables_dir
)

# Export Excel workbook --------------------------------------------

model_tables_to_export <- list(
  "Citation Check" = citation_check,
  "Submitter Summary" = submitter_summary,
  "Purpose Summary" = purpose_summary,
  "Year Summary" = year_summary,
  "Model Results" = model_results_clean,
  "Requestor Pairwise Comparisons" = requestor_results,
  "Purpose Pairwise Comparisons" = purpose_results
)

model_summary_filepath <- file.path(
  model_output_dir,
  "model_summary_report.xlsx"
)

write_pretty_workbook(
  tables = model_tables_to_export,
  path = model_summary_filepath
)

# Print output -----------------------------------------------------

model_summary
citation_check
model_results_clean

message("Model outputs saved to: ", model_output_dir)
message("Model summary workbook saved to: ", model_summary_filepath)