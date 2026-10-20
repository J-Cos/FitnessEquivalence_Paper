# --------------------------------------------
# Purpose: H2a - SEMs testing whether CATS trait selection (fitness non-equivalence) mediates the
#          effect of forest loss on absolute EEoS deviation. Figure 2.
# Input Origin Scripts: 03_FitEEOS.py; 05_FitCatsRegressions.R
# Output: Outputs/H2a_{Model_Comparison,Parameter_Estimates,LRT}_<suffix>.csv; Figures/Figure_H2a*.png
# ---------------------------------------------------

suppressMessages(library(tidyverse))
source("Code/Functions/config.R"); check_project_root()
invisible(lapply(c("data_prep", "sem_helpers", "plot_helpers"), \(f) source(sprintf("Code/Functions/%s.R", f))))

suffix <- get_suffix_arg()
set.seed(1)
plot_data <- load_and_prepare_plot_data(suffix, need = "cats")

res <- fit_mediation_suite(plot_data, x = "disturbedcover", m = "CATS_pois", y = "absdev")
print(res$lrt)
print(cor.test(plot_data$absdev, plot_data$CATS_pois))
write_mediation_outputs(res, "H2a", suffix)

expand_01 <- expansion(mult = c(0, 0.1))
fig <- plot_mediation_panels(
  plot_data, "disturbedcover", "CATS_pois", "absdev", color_var = "Region",
  labels = list(a_x = "Disturbed forest cover (2000m radius)", a_y = "CATS regression R2",
                b_x = "CATS regression R2", b_y = "EEOS absolute deviation"),
  extra_a = list(scale_x_continuous(expand = expand_01), scale_y_continuous(expand = expand_01)),
  extra_b = list(scale_x_continuous(expand = expand_01), scale_y_continuous(expand = expand_01)))
save_figure("Figure_H2a", suffix, fig, height = 4, width = 8)

message("H2a SEM results saved to Outputs/ for suffix: ", suffix)
