# --------------------------------------------
# Purpose: H2b - SEMs testing whether the relative abundance of non-neutral taxa mediates the
#          effect of forest loss on EEoS deviation (absolute and signed; co-primary). Figure 3.
# Input Origin Scripts: 03_FitEEOS.py; 08_FitOverallSloanAndExtractNonneutralAbundances.R
# Output: Outputs/H2b[_Signed]_{Model_Comparison,Parameter_Estimates,LRT,IndirectEffect}_<suffix>.csv;
#         Figures/Figure_H2b*.png
# ---------------------------------------------------

suppressMessages(library(tidyverse))
source("Code/Functions/config.R"); check_project_root()
invisible(lapply(c("data_prep", "sem_helpers", "plot_helpers"), \(f) source(sprintf("Code/Functions/%s.R", f))))

suffix <- get_suffix_arg()
set.seed(1)
plot_data <- load_and_prepare_plot_data(suffix, need = "nonneutral")

run_mediation_hypothesis(plot_data, "H2b", m = "proportionNonNeutral", y_abs = "absdev", y_signed = "dev", suffix = suffix)

fig <- plot_mediation_panels(
  plot_data, "disturbedcover", "proportionNonNeutral", "absdev", color_var = "Region",
  labels = list(a_x = "Median disturbed forest cover\n(2000m radius)", a_y = "Relative abundance of non-neutral taxa",
                b_x = "Relative abundance of\nnon-neutral taxa", b_y = "EEOS absolute deviation"),
  extra_a = list(scale_x_continuous(expand = expansion(mult = c(0, 0.075)), limits = c(0, NA)),
                 scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA))),
  extra_b = list(scale_x_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)),
                 scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA))))
save_figure("Figure_H2b", suffix, fig, height = 4, width = 8)

message("H2b SEM results saved to Outputs/ for suffix: ", suffix)
