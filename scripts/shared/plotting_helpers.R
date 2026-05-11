# scripts/shared/plotting_helpers.R
#
# Shared plotting themes and helper functions used across the project.

library(ggplot2)

# Consistent project plotting theme.
theme_project <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = rel(1.25)
      ),
      
      plot.subtitle = element_text(
        size = rel(1)
      ),
      
      axis.title = element_text(
        face = "bold"
      ),
      
      strip.text = element_text(
        face = "bold"
      ),
      
      legend.position = "bottom",
      
      panel.grid.minor = element_blank(),
      
      plot.margin = margin(
        t = 10,
        r = 15,
        b = 10,
        l = 15
      )
    )
}