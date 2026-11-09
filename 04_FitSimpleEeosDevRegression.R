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
