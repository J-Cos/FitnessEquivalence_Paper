# --------------------------------------------
# Purpose: Supplementary diagnostic figures for the plot-level macrostates:
#          1. Pairwise scatterplots of plot macrostates (N, S, B, E) by region (Figures/FigureS1.png).
#          2. Monte Carlo biomass uncertainty (B_cv) against B, N, E and across regions
#             (Figures/Figure_BiomassCV.png).
# Input Origin Scripts: 02_GetEcosystemMacrostates.R
# Output: Figures/FigureS1.png (SI Appendix, Fig. S1);
#         Figures/Figure_BiomassCV.png (SI Appendix, Fig. S2)
#
# Suggested Caption (PNAS style) for FigureS1.png (Fig. S1):
#   Pairwise relationships among the four ecosystem macrostate variables across the 224 0.25-ha
#   Amazonian forest plots (Jurua, n = 30; Balbina, n = 37; Santarem [STM], n = 63; Paragominas
#   [PGM], n = 94): stem abundance (N), species richness (S), dimensionless aboveground biomass
#   (B = sum_i AGB_i / AGB_ref), and dimensionless total metabolic rate (E = sum_i [DBH_i / 10]^2),
#   normalized to a 10-cm DBH reference stem at study-mean wood density and bioclimatic stress.
#
# Suggested Caption (PNAS style) for Figure_BiomassCV.png (Fig. S2):
#   Plot-level allometric biomass coefficient of variation (Biomass Monte Carlo CV, SD / mean across
#   1,000 BIOMASS::AGBmonteCarlo draws propagating taxonomic wood-density, bioclimatic height-
#   allometry, and Chave et al. 2014 regression error; n = 224 plots) plotted against (A)
#   dimensionless biomass (B), (B) stem abundance (N), (C) dimensionless metabolic rate (E), and
#   (D) study region (boxes: median and interquartile range [IQR]; whiskers: 1.5 x IQR; points:
#   individual plots, providing the regional color key for A-C).
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