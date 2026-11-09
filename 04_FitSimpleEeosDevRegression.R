# --------------------------------------------
# Purpose: H1 - linear regressions of EEoS deviation (signed primary; absolute retained) on forest
#          loss with regional fixed effects, ANCOVA slope-homogeneity test, robustness checks
#          (including inverse-allometric-variance WLS and high-CV trimming), allometric uncertainty
#          benchmark, 2x2 Figure 1 (map, B_obs vs. B_pred with 1:1 line & CV bars, dev vs. forest
#          loss, dev by region), and Figure S3 (absolute deviation).
# Input Origin Scripts: 03_FitEEOS.py
# Output: Outputs/H1_Signed_*.csv; Outputs/H1_Absolute_*.csv; Outputs/H1_AllometricBenchmark.csv;
#         Figures/Figure_H1.png (Fig. 1);
#         Figures/Figure_H1_absolutesupplement.png (SI Appendix, Fig. S3);
#         Figures/diagnostics/
#
# Suggested Caption (PNAS style) for Figure_H1.png (Fig. 1):
#   Landscape forest loss drives systematic negative deviations from the Ecological Equation of
#   State (EEoS) across 224 0.25-ha Amazonian forest plots (stems >= 10 cm DBH). (A) Study regions
#   across the Tropical and Subtropical Moist Broadleaf Forests biome (grey shading; Dinerstein et
#   al., 2017); solid circles mark regional centroids and plot counts in intact Jurua (n = 30),
#   Balbina (n = 37), Santarem (STM; n = 63), and Paragominas (PGM; n = 94). (B) Observed versus
#   zero-parameter METE/EEoS-predicted dimensionless plot biomass in natural-log units [ln(B) vs.
#   ln(B_pred); dotted 1:1 line; vertical bars show +/- 1 sigma_allom log-scale uncertainty across
#   1,000 BIOMASS Monte Carlo draws]. (C) Signed EEoS deviation, ln(B / B_pred), versus disturbed
#   forest cover within a 2,000-m radius (proportion of non-forest cover, 1 - fc2000; intact Jurua
#   fixed at 0; horizontal dotted line marks exact EEoS agreement at zero). The solid black line and
#   grey ribbon show the marginal common-slope ordinary least-squares prediction and 95% confidence
#   interval controlling for regional fixed effects (holding regional composition at sample
#   proportions). (D) Regional distributions of signed EEoS deviation (boxes: median and
#   interquartile range [IQR]; whiskers: 1.5 x IQR; points: individual plots), providing the
#   regional color key across panels (see SI Appendix, Fig. S3 for the corresponding absolute
#   deviation analysis).
#
# Suggested Caption (PNAS style) for Figure_H1_absolutesupplement.png (Fig. S3):
#   Unsigned (absolute) EEoS deviation, |ln(B / B_pred)|, across landscape disturbance and regions
#   (n = 224 plots). (A) Absolute EEoS deviation versus disturbed forest cover within a 2,000-m
#   radius (solid black line and grey ribbon: marginal common-slope ordinary least-squares
#   prediction and 95% confidence interval controlling for regional fixed effects). (B) Regional
#   distributions of absolute EEoS deviation (boxes: median and IQR; whiskers: 1.5 x IQR; points:
#   individual plots), providing the color key for A.
# ---------------------------------------------------

suppressMessages({library(tidyverse); library(lmtest); library(sandwich)})
source("Code/Functions/config.R"); check_project_root()
invisible(lapply(c("data_prep", "regression_helpers", "plot_helpers"),
                 \(f) source(sprintf("Code/Functions/%s.R", f))))

set.seed(1)
plot_data <- load_and_prepare_plot_data()

# 1) Allometric uncertainty benchmark (comparing |dev| against sigma_allom) ----------------------
benchmark_tab <- build_allometric_benchmark_table(plot_data)
write.csv(benchmark_tab, out_path("H1_AllometricBenchmark"), row.names = FALSE)

# 2) Primary (signed deviation: H1_Signed_*) and secondary (absolute deviation: H1_Absolute_*) ---
N_BOOT <- 10000
h1_sig <- fit_h1_regression(plot_data, "dev",    n_boot = N_BOOT, seed = 1) |> write_h1_outputs("H1_Signed")
h1_abs <- fit_h1_regression(plot_data, "absdev", n_boot = N_BOOT, seed = 1) |> write_h1_outputs("H1_Absolute")
print(h1_sig$coef_tab)
print(h1_abs$coef_tab)

# Figures (region colors are directly labelled on the regional boxplot panels) ------------------
fc_xlab <- "Disturbed forest cover (2000 m radius)"

# Figure 1 (2x2): A = region map, B = ln(B_obs) vs ln(B_pred) (1:1 + CV bars),
#                 C = signed dev vs disturbed forest cover, D = signed dev by region
marg_sig <- get_marginal_predictions(plot_data, "dev", "disturbedcover")
dev_lims <- range(c(plot_data$dev, marg_sig$lwr, marg_sig$upr, 0), na.rm = TRUE)

p1_a <- make_region_map()
p1_b <- eeos_obs_pred_panel(plot_data)
p1_c <- mediation_panel(
  plot_data, x = "disturbedcover", y = "dev", color_var = "Region",
  xlab = fc_xlab, ylab = "EEoS deviation",
  show_legend = FALSE,
  extra = list(
    geom_hline(yintercept = 0, linetype = "dotted", linewidth = 0.6, color = "black"),
    scale_y_continuous(limits = dev_lims)
  )
)
p1_d <- region_boxplot_panel(plot_data, y = "dev", ylab = "EEoS deviation",
                             ylim = dev_lims, hline = 0)

fig1 <- cowplot::plot_grid(
  p1_a, p1_b, p1_c, p1_d,
  labels = c("A", "B", "C", "D"),
  ncol = 2, align = "hv", axis = "tblr"
)
save_figure("Figure_H1", fig1, height = 6.6, width = 7.5)

# Figure S3 (1x2): absolute EEoS deviation vs disturbed forest cover and by region
marg_abs <- get_marginal_predictions(plot_data, "absdev", "disturbedcover")
abs_lims <- range(c(0, plot_data$absdev, marg_abs$lwr, marg_abs$upr), na.rm = TRUE)

ps2_a <- mediation_panel(
  plot_data, x = "disturbedcover", y = "absdev", color_var = "Region",
  xlab = fc_xlab, ylab = "EEoS absolute deviation",
  show_legend = FALSE,
  extra = list(scale_y_continuous(limits = abs_lims))
)
ps2_b <- region_boxplot_panel(plot_data, y = "absdev", ylab = "EEoS absolute deviation",
                              ylim = abs_lims)

fig_s2 <- cowplot::plot_grid(
  ps2_a, ps2_b,
  labels = c("A", "B"),
  ncol = 2, align = "hv", axis = "tblr"
)
save_figure("Figure_H1_absolutesupplement", fig_s2, height = 3.5, width = 7.5)

message("H1 model results and figures saved to Outputs/ and Figures/")
