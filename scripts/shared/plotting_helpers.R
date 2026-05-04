# scripts/shared/plotting_helpers.R
#
# Contains helper functions and themes for creating plots.

library(ggplot2)

# A consistent theme for all plots in the project
theme_project <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = rel(1.2)),
      plot.subtitle = element_text(color = "gray40"),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
}