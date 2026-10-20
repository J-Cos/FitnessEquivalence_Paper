# --------------------------------------------
# Purpose: H2c - SEMs testing whether neutral (Poisson random-sampling) model fit mediates the
#          effect of median forest loss on EEoS RMSE across 43 forest-cover subgroups. Figure 4.
# Input Origin Scripts: 03_FitEEOS.py; 10_FitForestCoverSubgroupSloans.R
# Output: Outputs/H2c_{Model_Comparison,Parameter_Estimates,LRT,IndirectEffect}_<suffix>.csv; Figures/Figure_H2c*.png
# ---------------------------------------------------

suppressMessages(library(tidyverse))
source("Code/Functions/config.R"); check_project_root()
invisible(lapply(c("data_prep", "sem_helpers", "plot_helpers"), \(f) source(sprintf("Code/Functions/%s.R", f))))

suffix <- get_suffix_arg()
set.seed(1)
subgroups <- load_and_prepare_subgroup_data(suffix)

res <- fit_mediation_suite(subgroups, x = "disturbedcover", m = "r2pois", y = "RMSE")
print(res$lrt)
write_mediation_outputs(res, "H2c", suffix)
# Indirect effect a*b of forest loss via the mediator, percentile bootstrap CI (2,000 draws)
write.csv(fit_indirect_effect(subgroups, "disturbedcover", "r2pois", "RMSE"),
          out_path("H2c_IndirectEffect", suffix), row.names = FALSE)

fig <- plot_mediation_panels(
  subgroups, "disturbedcover", "r2pois", "RMSE", color_var = "region", color_name = "Region",
  labels = list(a_x = "Median disturbed forest cover\n(2000m radius)", a_y = "Poisson random sampling R2",
                b_x = "Poisson random\nsampling R2", b_y = "EEOS RMSE"),
  extra_a = list(geom_hline(yintercept = 0, linetype = 2),
                 scale_x_continuous(expand = c(0, 0), limits = c(0, NA))),
  extra_b = list(geom_vline(xintercept = 0, linetype = 2),
                 scale_y_continuous(expand = c(0, 0), limits = c(0, NA))))
save_figure("Figure_H2c", suffix, fig, height = 4, width = 8)

message("H2c SEM results saved to Outputs/ for suffix: ", suffix)
