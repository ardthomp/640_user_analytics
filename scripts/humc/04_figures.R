# I used AI to help write this code
# Figures ----------------------------------------------------------

source(here::here("scripts", "humc", "01_setup_functions.R"))

out <- readRDS(out_path)

cat_purp <- readRDS(file.path(csv_dir, "cat_purp.rds"))
category_chi_results <- readRDS(file.path(csv_dir, "category_chi_results.rds"))
purpose_results <- readRDS(file.path(csv_dir, "purpose_results.rds"))

# Timestamp --------------------------------------------------------

run_timestamp <- format(Sys.time(), "%Y_%m_%d-%H-%M-%S")

# Archive old figures ---------------------------------------------

archive_existing_files(
  source_dir = figures_dir,
  pattern = "\\.(png|jpg|jpeg|pdf)$",
  archive_parent_dir = old_figures_dir,
  timestamp = run_timestamp
)

# Prepare plotting objects ----------------------------------------

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

chi_residuals_plot <- category_chi_results$chi_residuals %>%
  mutate(
    category = factor(category, levels = category_order),
    submitter_type = factor(
      submitter_type,
      levels = c("Attending", "MedEd", "Nurse", "OtherProvider", "Unknown")
    ),
    category_label = str_replace_all(as.character(category), "_", " ")
  )

purpose_residuals_plot <- purpose_results$purpose_residuals %>%
  mutate(
    submitter_type = factor(
      submitter_type,
      levels = c("Attending", "MedEd", "Nurse", "OtherProvider", "Unknown")
    )
  )

top_lemmas_person <- out$lemma_counts_by_person %>%
  filter(submitter_type != "Committee") %>%
  group_by(submitter_type) %>%
  slice_max(n, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(lemma_label = str_replace_all(lemma, "_", " "))

# Figure 1: Top lemmas by submitter type --------------------------

p_top_lemmas <- ggplot(
  top_lemmas_person,
  aes(
    x = reorder_within(lemma_label, n, submitter_type),
    y = n,
    fill = submitter_type
  )
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ submitter_type, scales = "free") +
  scale_x_reordered() +
  coord_flip() +
  labs(
    title = "Top Lemmas by Submitter Type",
    x = NULL,
    y = "Count"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 18)
  )

# Figure 2: Category by submitter residual heatmap ----------------

p_category_resid <- ggplot(
  chi_residuals_plot,
  aes(x = category_label, y = submitter_type, fill = std_residual)
) +
  geom_tile(color = "white", linewidth = 0.6) +
  scale_fill_gradient2(
    low = "#3B6FB6",
    mid = "white",
    high = "#D94F3D",
    midpoint = 0,
    limits = c(-6, 6),
    oob = scales::squish
  ) +
  labs(
    title = "Standardized Residuals for Request Category by Submitter Type",
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
    legend.position = "right"
  )

# Figure 3: Purpose by submitter residual heatmap -----------------

p_purpose_resid <- ggplot(
  purpose_residuals_plot,
  aes(x = purpose, y = submitter_type, fill = std_residual)
) +
  geom_tile(color = "white", linewidth = 0.4) +
  scale_fill_gradient2(
    low = "#3B6FB6",
    mid = "white",
    high = "#D94F3D",
    midpoint = 0
  ) +
  labs(
    title = "Standardized Residuals for Purpose by Submitter Type",
    x = NULL,
    y = NULL,
    fill = "Residual"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 35, hjust = 1),
    axis.text.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 16)
  )

# Figure 4: Category by purpose heatmap ---------------------------

p_cat_purpose_heat <- ggplot(
  category_by_purpose,
  aes(x = category_label, y = purpose, fill = n)
) +
  geom_tile(color = "white", linewidth = 0.4) +
  scale_fill_gradient(low = "white", high = "#2C7FB8") +
  labs(
    title = "Request Categories by Purpose",
    x = NULL,
    y = NULL,
    fill = "Requests"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 35, hjust = 1),
    plot.title = element_text(face = "bold", size = 16)
  )

# Figure 5: Proportion heatmap ------------------------------------

p_cat_purpose_prop <- ggplot(
  category_prop,
  aes(x = category_label, y = purpose, fill = prop)
) +
  geom_tile(color = "white", linewidth = 0.4) +
  scale_fill_gradient(
    low = "white",
    high = "#2C7FB8",
    labels = scales::percent
  ) +
  labs(
    title = "Distribution of Categories Within Each Purpose",
    x = NULL,
    y = NULL,
    fill = "Proportion"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 35, hjust = 1),
    plot.title = element_text(face = "bold", size = 16)
  )

# Figure 6: Category mix within each purpose ----------------------

p_category_mix <- ggplot(
  category_by_purpose,
  aes(x = purpose, y = n, fill = category_label)
) +
  geom_col(
    position = "fill",
    color = "white",
    linewidth = 0.25
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Category Mix Within Each Purpose",
    x = NULL,
    y = "Proportion",
    fill = "Category"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16)
  )

# Figure 7: Purpose mix within each category ----------------------

p_purpose_mix <- ggplot(
  category_by_purpose,
  aes(x = category_label, y = n, fill = purpose)
) +
  geom_col(position = "fill", color = "white", linewidth = 0.2) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Purpose Mix Within Each Request Category",
    x = NULL,
    y = "Proportion of Requests",
    fill = "Purpose"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16)
  )

# Figure 8: Bubble plot -------------------------------------------

p_cat_purpose_bubble <- ggplot(
  category_by_purpose,
  aes(x = category_label, y = purpose, size = n)
) +
  geom_point(alpha = 0.7) +
  labs(
    title = "Category × Purpose Request Volume",
    x = NULL,
    y = NULL,
    size = "Requests"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    plot.title = element_text(face = "bold", size = 16)
  )

# Save figures -----------------------------------------------------

ggsave(
  file.path(figures_dir, "top_lemmas_by_submitter.png"),
  p_top_lemmas,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  file.path(figures_dir, "category_submitter_residuals.png"),
  p_category_resid,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  file.path(figures_dir, "purpose_submitter_residuals.png"),
  p_purpose_resid,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  file.path(figures_dir, "category_by_purpose_heatmap.png"),
  p_cat_purpose_heat,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  file.path(figures_dir, "category_by_purpose_proportion_heatmap.png"),
  p_cat_purpose_prop,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  file.path(figures_dir, "category_mix_within_purpose.png"),
  p_category_mix,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  file.path(figures_dir, "purpose_mix_within_category.png"),
  p_purpose_mix,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  file.path(figures_dir, "category_purpose_bubble_plot.png"),
  p_cat_purpose_bubble,
  width = 9,
  height = 5,
  dpi = 300
)

message("Figures saved to: ", figures_dir)

# Optional quick views --------------------------------------------

# p_top_lemmas
# p_category_resid
# p_purpose_resid
# p_cat_purpose_heat
# p_cat_purpose_prop
# p_category_mix
# p_purpose_mix
# p_cat_purpose_bubble