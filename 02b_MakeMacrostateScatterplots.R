# --------------------------------------------
# Purpose: Supplementary diagnostic figures for the plot-level macrostates:
#          1. Pairwise scatterplots of plot macrostates (N, S, B, E) by region (Figures/FigureS1.png).
#          2. Monte Carlo biomass uncertainty (B_cv) against B, N, E and across regions
#             (Figures/Figure_BiomassCV.png).
# Input Origin Scripts: 02_GetEcosystemMacrostates.R
# Output: Figures/FigureS1.png (SI Appendix, Fig. S1);
#         Figures/Figure_BiomassCV.png (SI Appendix, Fig. S2)
# ---------------------------------------------------

suppressMessages({
  library(tidyverse)
  library(patchwork)
})
source("Code/Functions/config.R"); check_project_root()
source("Code/Functions/plot_helpers.R")

macrostates <- read_required_csv(out_path("Macrostates")) |>
  mutate(Region = factor(Region, levels = REGIONS))

# 1. Pairwise lower-triangle scatterplot matrix of (N, S, B, E) ---------------------------------
fig_s1 <- pairwise_lower_triangle(
  macrostates,
  vars   = c("N", "S", "B", "E"),
  labels = c("Abundance (N)", "Richness (S)", "Biomass (B)", "Metabolic rate (E)")
)
save_figure("FigureS1", fig_s1, height = 7.5, width = 7.5)

# 2. Biomass Monte Carlo CV vs B, N, E and by Region -------------------------------------------
cv_ylab <- "Biomass Monte Carlo CV"
cv_lims <- range(macrostates$B_cv, na.rm = TRUE)

p_cv_b   <- scatter_panel(macrostates, "B", "B_cv", xlab = "Biomass (B)",        ylab = cv_ylab, ylim = cv_lims)
p_cv_n   <- scatter_panel(macrostates, "N", "B_cv", xlab = "Abundance (N)",      ylab = cv_ylab, ylim = cv_lims)
p_cv_e   <- scatter_panel(macrostates, "E", "B_cv", xlab = "Metabolic rate (E)", ylab = cv_ylab, ylim = cv_lims)
p_cv_reg <- region_boxplot_panel(macrostates, "B_cv", ylab = cv_ylab, ylim = cv_lims)

fig_cv <- cowplot::plot_grid(
  p_cv_b, p_cv_n, p_cv_e, p_cv_reg,
  labels = c("A", "B", "C", "D"),
  ncol = 2, align = "hv", axis = "tblr"
)
save_figure("Figure_BiomassCV", fig_cv, height = 6.6, width = 7.5)

message("Saved Figures/FigureS1.png and Figures/Figure_BiomassCV.png")