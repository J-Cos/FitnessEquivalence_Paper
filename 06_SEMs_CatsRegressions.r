# --------------------------------------------
# Purpose: Fits SEMs for CATS regression mediation (Revised Single-Group Model)
# Input Origin Scripts: 03_FitEEOS.py; 05_FitCatsRegressions.r
# Output: figure visualising the best model (mediation only model)
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
  CATS_pois ~ disturbedcover + Reg_Balbina + Reg_STM + Reg_PGM
  dev ~ CATS_pois + Reg_Balbina + Reg_STM + Reg_PGM
'

model_partial <- '
  CATS_pois ~ disturbedcover + Reg_Balbina + Reg_STM + Reg_PGM
  dev ~ CATS_pois + disturbedcover + Reg_Balbina + Reg_STM + Reg_PGM
'

model_direct <- '
  CATS_pois ~ disturbedcover + Reg_Balbina + Reg_STM + Reg_PGM
  dev ~ disturbedcover + Reg_Balbina + Reg_STM + Reg_PGM
'

model_direct_nocov <- '
  CATS_pois ~ disturbedcover + Reg_Balbina + Reg_STM + Reg_PGM
  dev ~ disturbedcover + Reg_Balbina + Reg_STM + Reg_PGM
  CATS_pois ~~ 0*dev
'

# Fit Models
fit_mediation_only <- sem(model_full, data = plot_data, estimator = "ML", meanstructure = TRUE)
fit_mediation_partial <- sem(model_partial, data = plot_data, estimator = "ML", meanstructure = TRUE)
fit_direct_only <- sem(model_direct, data = plot_data, estimator = "ML", meanstructure = TRUE)
fit_nocov <- sem(model_direct_nocov, data = plot_data, estimator = "ML", meanstructure = TRUE)

# Likelihood Ratio Test
lrt_h2a <- lavTestLRT(fit_mediation_only, fit_mediation_partial)
print(lrt_h2a)

# Print Comparisons and Summaries
comp_df_h2a <- print_sem_comparison(fit_mediation_only, fit_mediation_partial, fit_direct_only, fit_nocov)
print(cor.test(plot_data$dev, plot_data$CATS_pois))
summary(fit_mediation_only, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# ---------------------------------------------------
# Make Plot (Figure 2)
# ---------------------------------------------------

# Calculate parallel lines coefficients for both panels
coef_CATS <- get_parallel_lines_coefs(fit_mediation_only, outcome = "CATS_pois", predictor = "disturbedcover")
coef_dev <- get_parallel_lines_coefs(fit_mediation_only, outcome = "dev", predictor = "CATS_pois")

# Calculate marginal predictions (common slope and CI ribbon)
marg_CATS <- get_marginal_predictions(plot_data, outcome = "CATS_pois", predictor = "disturbedcover")
marg_dev <- get_marginal_predictions(plot_data, outcome = "dev", predictor = "CATS_pois")

# panel A
PanelA <- ggplot(plot_data, aes(x = disturbedcover, y = CATS_pois, color = Region)) +
  geom_ribbon(data = marg_CATS, aes(x = x, ymin = lwr, ymax = upr), fill = "black", alpha = 0.15, inherit.aes = FALSE) +
  geom_line(data = marg_CATS, aes(x = x, y = y), color = "black", linetype = "dashed", linewidth = 0.8, inherit.aes = FALSE) +
  geom_point(alpha = 0.8, size = 1) +
  geom_abline(data = coef_CATS,
              aes(intercept = intercept, slope = slope, color = region),
              linewidth = 0.5) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  ylab("CATS regression R2") +
  xlab("Disturbed forest cover (2000m radius)") +
  ggsci::scale_color_npg()

# panel B
PanelB <- ggplot(plot_data, aes(x = CATS_pois, y = dev, color = Region)) +
  geom_ribbon(data = marg_dev, aes(x = x, ymin = lwr, ymax = upr), fill = "black", alpha = 0.15, inherit.aes = FALSE) +
  geom_line(data = marg_dev, aes(x = x, y = y), color = "black", linetype = "dashed", linewidth = 0.8, inherit.aes = FALSE) +
  geom_point(alpha = 0.8, size = 1) +
  geom_abline(data = coef_dev,
              aes(intercept = intercept, slope = slope, color = region),
              linewidth = 0.5) +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  xlab("CATS regression R2") +
  ylab("EEOS deviation") +
  ggsci::scale_color_npg()

# Combine and save
cowplot::plot_grid(PanelA, PanelB, labels = c('A', 'B'), rel_widths = c(0.82, 1))
ggsave(paste0("Figures/Figure_H2a_", suffix, ".png"), height = 4, width = 8)

# ---------------------------------------------------
# Save Model Results
# ---------------------------------------------------
dir.create("Outputs", showWarnings = FALSE)
write.csv(comp_df_h2a, paste0("Outputs/H2a_Model_Comparison_", suffix, ".csv"), row.names = FALSE)
pe_h2a <- parameterEstimates(fit_mediation_only, standardized = TRUE, rsquare = TRUE)
write.csv(pe_h2a, paste0("Outputs/H2a_Parameter_Estimates_", suffix, ".csv"), row.names = FALSE)
write.csv(as.data.frame(lrt_h2a), paste0("Outputs/H2a_LRT_", suffix, ".csv"), row.names = FALSE)
cat(paste0("H2a SEM results successfully saved to Outputs/ for suffix: ", suffix, "\n"))

