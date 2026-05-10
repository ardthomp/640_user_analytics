# Optional model: Effort level by requestor role ----------------------------
# This script does not run automatically in the main project pipeline.

library(tidyverse)
library(here)
library(janitor)
library(MASS)
library(broom)
library(scales)

source(here("scripts", "shared", "paths.R"))
source(here("scripts", "shared", "output_helpers.R"))

# Output folders ------------------------------------------------------------

model_output_dir <- here(
  "outputs",
  "optional_models",
  "hmh_02_effort_level_by_requestor_model"
)

figures_dir <- file.path(model_output_dir, "figures")
tables_dir  <- file.path(model_output_dir, "tables")

dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

clear_output_folder(
  model_output_dir,
  "\\.(csv|rds|xlsx|html|png|jpg|jpeg)$"
)

# Load HMH data -------------------------------------------------------------

hmh_dat <- read_csv(here("data", "raw", "hmh.csv")) %>%
  clean_names()

# Prepare modeling data -----------------------------------------------------

model_dat <- hmh_dat %>%
  filter(select_question_request_type == "Literature Search") %>%
  mutate(
    time_spent_clean = str_squish(str_to_lower(time_spent_on_searches)),
    
    effort_level = case_when(
      str_detect(time_spent_clean, "1\\s*[-–—]\\s*2") ~ "Low time",
      str_detect(time_spent_clean, "2\\s*[-–—]\\s*5") ~ "Medium time",
      str_detect(time_spent_clean, "more than 5|>\\s*5|5\\+") ~ "High time",
      TRUE ~ NA_character_
    ),
    
    effort_level = factor(
      effort_level,
      levels = c("Low time", "Medium time", "High time"),
      ordered = TRUE
    ),
    
    requestor_group = case_when(
      str_detect(
        str_to_lower(who_requested_this_information),
        "physical therapist|pt|occupational therapist|ot|speech|slp|therapy|therapist|allied"
      ) ~ "Allied Health Professional",
      
      str_detect(
        str_to_lower(who_requested_this_information),
        "nurse practitioner|np|physician assistant|pa"
      ) ~ "Nurse Practitioner/PA",
      
      str_detect(
        str_to_lower(who_requested_this_information),
        "physician|attending"
      ) ~ "Physician",
      
      str_detect(
        str_to_lower(who_requested_this_information),
        "nurse|rn"
      ) ~ "Nurse",
      
      str_detect(
        str_to_lower(who_requested_this_information),
        "resident"
      ) ~ "Resident",
      
      str_detect(
        str_to_lower(who_requested_this_information),
        "fellow"
      ) ~ "Fellow",
      
      str_detect(
        str_to_lower(who_requested_this_information),
        "student"
      ) ~ "Student",
      
      str_detect(
        str_to_lower(who_requested_this_information),
        "corporate"
      ) ~ "Corporate",
      
      str_detect(
        str_to_lower(who_requested_this_information),
        "hospital"
      ) ~ "Hospital",
      
      str_detect(
        str_to_lower(who_requested_this_information),
        "pharmacist|pharmacy"
      ) ~ "Pharmacist",
      
      str_detect(
        str_to_lower(who_requested_this_information),
        "faculty"
      ) ~ "Faculty",
      
      TRUE ~ "Other"
    ),
    
    requestor_group = factor(
      requestor_group,
      levels = c(
        "Physician",
        "Nurse",
        "Allied Health Professional",
        "Resident",
        "Nurse Practitioner/PA",
        "Student",
        "Fellow",
        "Pharmacist",
        "Faculty",
        "Corporate",
        "Hospital",
        "Other"
      )
    )
  ) %>%
  filter(
    !is.na(effort_level),
    !is.na(requestor_group)
  )

# Descriptive table ---------------------------------------------------------

effort_summary <- model_dat %>%
  count(requestor_group, effort_level) %>%
  group_by(requestor_group) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  mutate(
    effort_level = factor(
      effort_level,
      levels = c("Low time", "Medium time", "High time")
    )
  )

write_pretty_csv(
  effort_summary,
  "effort_level_by_requestor_group",
  tables_dir
)

# Model ---------------------------------------------------------------------

effort_model <- MASS::polr(
  effort_level ~ requestor_group,
  data = model_dat,
  Hess = TRUE
)

saveRDS(
  effort_model,
  file.path(model_output_dir, "effort_level_requestor_model.rds")
)

# Model results table -------------------------------------------------------

effort_model_results <- broom::tidy(effort_model) %>%
  mutate(
    odds_ratio = exp(estimate)
  )

write_pretty_csv(
  effort_model_results,
  "effort_level_requestor_model_results",
  tables_dir
)

# Heatmap: effort level by requestor role -----------------------------------

effort_fig <- effort_summary %>%
  mutate(
    percent = prop * 100,
    label = paste0(round(percent), "%")
  ) %>%
  ggplot(
    aes(
      x = effort_level,
      y = requestor_group,
      fill = percent
    )
  ) +
  geom_tile(color = "white", linewidth = 1.5) +
  geom_text(aes(label = label), size = 4.5, fontface = "bold") +
  scale_fill_gradient(
    low = "#FDE0DD",
    high = "#C51B1D",
    labels = scales::percent_format(scale = 1)
  ) +
  labs(
    title = "Effort Level by Requestor Role, HMH 2026",
    subtitle = "Literature search requests",
    x = NULL,
    y = NULL,
    fill = "Percent"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.text.y = element_text(face = "bold", size = 11),
    plot.title = element_text(face = "bold", size = 15),
    legend.position = "right"
  )

ggsave(
  file.path(figures_dir, "effort_level_by_requestor_group_heatmap.png"),
  effort_fig,
  width = 10,
  height = 7,
  dpi = 300
)

# Bar chart: effort level mix by requestor role -----------------------------

effort_bar_fig <- effort_summary %>%
  mutate(
    effort_level = factor(
      effort_level,
      levels = c("Low time", "Medium time", "High time")
    )
  ) %>%
  ggplot(
    aes(
      x = prop,
      y = requestor_group,
      fill = effort_level
    )
  ) +
  geom_col(
    width = 0.75,
    color = "white",
    position = position_stack(reverse = TRUE)
  ) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_manual(
    values = c(
      "Low time" = "#FDE0DD",
      "Medium time" = "#FA9FB5",
      "High time" = "#C51B1D"
    ),
    breaks = c("Low time", "Medium time", "High time")
  ) +
  labs(
    title = "Effort Level Mix by Requestor Role, HMH 2026",
    subtitle = "Literature search requests",
    x = NULL,
    y = NULL,
    fill = "Effort Level"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(face = "bold", size = 11),
    axis.text.x = element_text(face = "bold", size = 11),
    plot.title = element_text(face = "bold", size = 15),
    legend.position = "bottom"
  )

ggsave(
  file.path(figures_dir, "effort_level_mix_by_requestor_group.png"),
  effort_bar_fig,
  width = 11,
  height = 7,
  dpi = 300
)

# Time allocation across effort levels --------------------------------------

time_summary <- model_dat %>%
  count(effort_level) %>%
  mutate(
    prop = n / sum(n),
    effort_level = factor(
      effort_level,
      levels = c("Low time", "Medium time", "High time")
    )
  )

p_effort_distribution <- ggplot(
  time_summary,
  aes(
    x = effort_level,
    y = prop,
    fill = effort_level
  )
) +
  geom_col(width = 0.7, color = "white") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(
    values = c(
      "Low time" = "#FDE0DD",
      "Medium time" = "#FA9FB5",
      "High time" = "#C51B1D"
    ),
    guide = "none"
  ) +
  labs(
    title = "Distribution of Literature Search Effort Levels, HMH 2026",
    subtitle = "Literature search requests",
    x = NULL,
    y = "Percent of Requests"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(face = "bold", size = 12),
    axis.text.y = element_text(face = "bold", size = 11),
    plot.title = element_text(face = "bold", size = 15),
    panel.grid.minor = element_blank()
  )

ggsave(
  file.path(figures_dir, "effort_level_distribution.png"),
  p_effort_distribution,
  width = 8,
  height = 5,
  dpi = 300
)

# Estimated librarian workload by requestor role ----------------------------

workload_hours <- model_dat %>%
  mutate(
    estimated_hours = case_when(
      effort_level == "Low time" ~ 1.5,
      effort_level == "Medium time" ~ 3.5,
      effort_level == "High time" ~ 6
    )
  ) %>%
  group_by(requestor_group) %>%
  summarize(
    total_estimated_hours = sum(estimated_hours, na.rm = TRUE),
    n_requests = n(),
    mean_hours_per_request = mean(estimated_hours, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_estimated_hours))

p_workload <- ggplot(
  workload_hours,
  aes(
    x = reorder(requestor_group, total_estimated_hours),
    y = total_estimated_hours
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Estimated Librarian Time by Requestor Role, HMH 2026",
    subtitle = "Approximate workload based on reported search effort categories",
    x = NULL,
    y = "Estimated Librarian Hours"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.y = element_text(face = "bold", size = 11),
    axis.text.x = element_text(face = "bold", size = 11),
    plot.title = element_text(face = "bold", size = 15),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(
  file.path(figures_dir, "estimated_librarian_time_by_requestor.png"),
  p_workload,
  width = 10,
  height = 6,
  dpi = 300
)

# Add estimated hours -------------------------------------------------------

model_dat_hours <- model_dat %>%
  mutate(
    estimated_hours = case_when(
      effort_level == "Low time" ~ 1.5,
      effort_level == "Medium time" ~ 3.5,
      effort_level == "High time" ~ 6
    )
  )

# Contingency table ---------------------------------------------------------

chi_table <- model_dat_hours %>%
  count(requestor_group, effort_level) %>%
  pivot_wider(
    names_from = effort_level,
    values_from = n,
    values_fill = 0
  )

chi_matrix <- chi_table %>%
  column_to_rownames("requestor_group") %>%
  as.matrix()

chi_result <- chisq.test(chi_matrix)

chi_summary <- tibble(
  statistic = unname(chi_result$statistic),
  df = unname(chi_result$parameter),
  p_value = chi_result$p.value
)

write_pretty_csv(
  chi_summary,
  "chi_squared_effort_by_requestor.csv",
  tables_dir
)

# Standardized residuals ----------------------------------------------------

chi_residuals <- as.data.frame(chi_result$stdres) %>%
  rownames_to_column("requestor_group") %>%
  pivot_longer(
    cols = -requestor_group,
    names_to = "effort_level",
    values_to = "std_residual"
  )

write_pretty_csv(
  chi_residuals,
  "chi_squared_standardized_residuals.csv",
  tables_dir
)

# Heatmap of standardized residuals -----------------------------------------

p_residuals <- ggplot(
  chi_residuals,
  aes(
    x = effort_level,
    y = requestor_group,
    fill = std_residual
  )
) +
  geom_tile(color = "white", linewidth = 1.2) +
  geom_text(
    aes(label = round(std_residual, 2)),
    fontface = "bold",
    size = 4
  ) +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "white",
    high = "#B2182B",
    midpoint = 0
  ) +
  labs(
    title = "Observed vs Expected Search Effort by Requestor Role",
    subtitle = "Standardized residuals from chi-squared test",
    x = NULL,
    y = NULL,
    fill = "Std.\nResidual"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 15)
  )

ggsave(
  file.path(figures_dir, "chi_squared_residual_heatmap.png"),
  p_residuals,
  width = 10,
  height = 7,
  dpi = 300
)

# Request share vs workload share -------------------------------------------

request_vs_workload <- model_dat %>%
  mutate(
    estimated_hours = case_when(
      effort_level == "Low time" ~ 1.5,
      effort_level == "Medium time" ~ 3.5,
      effort_level == "High time" ~ 6
    )
  ) %>%
  group_by(requestor_group) %>%
  summarize(
    n_requests = n(),
    total_hours = sum(estimated_hours, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    request_percent = n_requests / sum(n_requests),
    workload_percent = total_hours / sum(total_hours),
    
    workload_ratio = workload_percent / request_percent,
    
    percent_difference =
      (workload_percent - request_percent) * 100
  ) %>%
  arrange(desc(workload_ratio))

write_pretty_csv(
  request_vs_workload,
  "request_vs_workload_summary.csv",
  tables_dir
)

# Diverging workload figure -------------------------------------------------

p_workload_ratio <- request_vs_workload %>%
  ggplot(
    aes(
      x = workload_ratio,
      y = reorder(requestor_group, workload_ratio)
    )
  ) +
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "gray50"
  ) +
  geom_point(
    size = 5,
    color = "#C51B1D"
  ) +
  geom_segment(
    aes(
      x = 1,
      xend = workload_ratio,
      y = requestor_group,
      yend = requestor_group
    ),
    linewidth = 1.5,
    color = "#C51B1D"
  ) +
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.1)
  ) +
  labs(
    title = "Workload Contribution by Requestor Role, HMH 2026",
    subtitle = "Comparison of share of requests vs share of estimated librarian time",
    x = "Workload Ratio",
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 15)
  )

ggsave(
  file.path(figures_dir, "request_vs_workload_ratio.png"),
  p_workload_ratio,
  width = 10,
  height = 6,
  dpi = 300
)