# --------------------------------------------
# Purpose: Pairwise scatterplots of plot macrostates (N, S, B, E) by region (Figure S1).
# Input Origin Scripts: 02_GetEcosystemMacrostates.R
# Output: Figures/FigureS1.png
# ---------------------------------------------------

suppressMessages(library(tidyverse))
source("Code/Functions/config.R"); check_project_root()

#' Lower-triangle scatterplot matrix of four variables (y = later variable, x = earlier variable);
#' the colour legend fills the empty cell at the end of the second row.
pairwise_lower_triangle <- function(data, vars, color_var = "Region") {
  stopifnot(length(vars) == 4, all(vars %in% names(data)))
  scatter <- function(x, y) {
    ggplot(data, aes(x = .data[[x]], y = .data[[y]], color = .data[[color_var]])) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", formula = y ~ x) +
      theme_minimal(base_size = 10) +
      labs(x = x, y = y) +
      theme(legend.position = "none") +
      ggsci::scale_color_npg()
  }
  blank <- ggplot() + theme_void()
  legend_panel <- cowplot::ggdraw(cowplot::get_legend(
    ggplot(data, aes(x = .data[[vars[1]]], y = .data[[vars[2]]], color = .data[[color_var]])) +
      geom_point() + ggsci::scale_color_npg()))

  panels <- list(scatter(vars[1], vars[2]), blank, blank,
                 scatter(vars[1], vars[3]), scatter(vars[2], vars[3]), legend_panel,
                 scatter(vars[1], vars[4]), scatter(vars[2], vars[4]), scatter(vars[3], vars[4]))
  cowplot::plot_grid(plotlist = panels, ncol = 3, align = "hv")
}

macrostates <- read_required_csv(out_path("Macrostates", MAIN_SUFFIX))
fig <- pairwise_lower_triangle(macrostates, c("N", "S", "B", "E"))
ggsave("Figures/FigureS1.png", fig, bg = "white", height = 8, width = 8)