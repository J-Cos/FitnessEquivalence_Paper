# --------------------------------------------
# Purpose: Fits SEMs for nonneutral abundance mediation (Revised Single-Group Model)
# Input Origin Scripts: 03_FitEEOS.py; 08_FitOverallSloanAndExtractNonneutralAbundances.r
# Output: figure visualising best model
# ---------------------------------------------------

library(tidyverse)
library(cowplot)
library(lavaan)
library(ggsci)

args <- commandArgs(trailingOnly = TRUE)
suffix <- if (length(args) > 0) args[1] else "chave"

source("Code/Functions/revision_helpers.R")

set.seed(1)
plot_data <- load_and_prepare_plot_data(suffix = suffix)

# ---------------------------------------------------
# Model Definitions
# ---------------------------------------------------
model_full <- '
  proportionNonNeutral ~ disturbedcover + Reg_Balbina + Reg_STM + Reg_PGM
  dev ~ proportionNonNeutral + Reg_Balbina + Reg_STM + Reg_PGM
'

model_partial <- '
  proportionNonNeutral ~ disturbedcover + Reg_Balbina + Reg_STM + Reg_PGM
  dev ~ proportionNonNeutral + disturbedcover + Reg_Balbina + Reg_STM + Reg_PGM
'

model_direct <- '
  proportionNonNeutral ~ disturbedcover + Reg_Balbina + Reg_STM + Reg_PGM
  dev ~ disturbedcover + Reg_Balbina + Reg_STM + Reg_PGM
'

model_direct_nocov <- '
  proportionNonNeutral ~ disturbedcover + Reg_Balbina + Reg_STM + Reg_PGM
  dev ~ disturbedcover + Reg_Balbina + Reg_STM + Reg_PGM
  proportionNonNeutral ~~ 0*dev
'

# Fit Models
fit_mediation_only <- sem(model_full, data = plot_data, estimator = "ML", meanstructure = TRUE)
fit_mediation_partial <- sem(model_partial, data = plot_data, estimator = "ML", meanstructure = TRUE)
fit_direct_only <- sem(model_direct, data = plot_data, estimator = "ML", meanstructure = TRUE)
fit_nocov <- sem(model_direct_nocov, data = plot_data, estimator = "ML", meanstructure = TRUE)

# Likelihood Ratio Test
lrt_h2b <- lavTestLRT(fit_mediation_only, fit_mediation_partial)
print(lrt_h2b)

# Print Comparisons and Summaries
comp_df_h2b <- print_sem_comparison(fit_mediation_only, fit_mediation_partial, fit_direct_only, fit_nocov)
summary(fit_mediation_only, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# ---------------------------------------------------
# Make Plot (Figure 3)
# ---------------------------------------------------

# Calculate parallel lines coefficients for both panels
coef_CATS <- get_parallel_lines_coefs(fit_mediation_only, outcome = "proportionNonNeutral", predictor = "disturbedcover")
coef_dev <- get_parallel_lines_coefs(fit_mediation_only, outcome = "dev", predictor = "proportionNonNeutral")

# Calculate marginal predictions (common slope and CI ribbon)
marg_CATS <- get_marginal_predictions(plot_data, outcome = "proportionNonNeutral", predictor = "disturbedcover")
marg_dev <- get_marginal_predictions(plot_data, outcome = "dev", predictor = "proportionNonNeutral")

# panel A
PanelA <- ggplot(plot_data, aes(x = disturbedcover, y = proportionNonNeutral, color = Region)) +
  geom_ribbon(data = marg_CATS, aes(x = x, ymin = lwr, ymax = upr), fill = "black", alpha = 0.15, inherit.aes = FALSE) +
  geom_line(data = marg_CATS, aes(x = x, y = y), color = "black", linetype = "dashed", linewidth = 0.8, inherit.aes = FALSE) +
  geom_point(alpha = 0.8, size = 1) +
  geom_abline(data = coef_CATS,
              aes(intercept = intercept, slope = slope, color = region),
              linewidth = 0.5) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.075)), limits = c(0, NA)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  ylab("Relative abundance of non-neutral taxa") +
  xlab("Median disturbed forest cover\n(2000m radius)") +
  ggsci::scale_color_npg()

# panel B
PanelB <- ggplot(plot_data, aes(x = proportionNonNeutral, y = dev, color = Region)) +
  geom_ribbon(data = marg_dev, aes(x = x, ymin = lwr, ymax = upr), fill = "black", alpha = 0.15, inherit.aes = FALSE) +
  geom_line(data = marg_dev, aes(x = x, y = y), color = "black", linetype = "dashed", linewidth = 0.8, inherit.aes = FALSE) +
  geom_point(alpha = 0.8, size = 1) +
  geom_abline(data = coef_dev,
              aes(intercept = intercept, slope = slope, color = region),
              linewidth = 0.5) +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
  xlab("Relative abundance of\nnon-neutral taxa") +
  ylab("EEOS deviation") +
  ggsci::scale_color_npg()

# Combine and save
cowplot::plot_grid(PanelA, PanelB, labels = c('A', 'B'), rel_widths = c(0.82, 1))
ggsave(paste0("Figures/Figure_H2b_", suffix, ".png"), height = 4, width = 8)

# ---------------------------------------------------
# Save Model Results
# ---------------------------------------------------
dir.create("Outputs", showWarnings = FALSE)
write.csv(comp_df_h2b, paste0("Outputs/H2b_Model_Comparison_", suffix, ".csv"), row.names = FALSE)
pe_h2b <- parameterEstimates(fit_mediation_only, standardized = TRUE, rsquare = TRUE)
write.csv(pe_h2b, paste0("Outputs/H2b_Parameter_Estimates_", suffix, ".csv"), row.names = FALSE)
write.csv(as.data.frame(lrt_h2b), paste0("Outputs/H2b_LRT_", suffix, ".csv"), row.names = FALSE)
cat(paste0("H2b SEM results successfully saved to Outputs/ for suffix: ", suffix, "\n"))

