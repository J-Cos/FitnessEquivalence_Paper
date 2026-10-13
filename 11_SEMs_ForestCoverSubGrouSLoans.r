# --------------------------------------------
# Purpose: Fits SEMs for forest cover sub group Sloan models (Revised Single-Group Model)
# Input Origin Scripts: 10_FitForestCoverSubgroupSloans.r
# Output: figure visualising best model
# ---------------------------------------------------

library(lavaan)
library(tidyverse)
library(ggsci)
library(cowplot)

args <- commandArgs(trailingOnly = TRUE)
suffix <- if (length(args) > 0) args[1] else "chave"

source("Code/Functions/revision_helpers.R")

set.seed(1)
df_modified <- load_and_prepare_subgroup_data(suffix = suffix)

# ---------------------------------------------------
# Model Definitions
# ---------------------------------------------------
model_full <- '
  r2pois ~ disturbedcover + Reg_Balbina + Reg_STM + Reg_PGM
  devmedian ~ r2pois + Reg_Balbina + Reg_STM + Reg_PGM
'

model_partial <- '
  r2pois ~ disturbedcover + Reg_Balbina + Reg_STM + Reg_PGM
  devmedian ~ r2pois + disturbedcover + Reg_Balbina + Reg_STM + Reg_PGM
'

model_direct <- '
  r2pois ~ disturbedcover + Reg_Balbina + Reg_STM + Reg_PGM
  devmedian ~ disturbedcover + Reg_Balbina + Reg_STM + Reg_PGM
'

model_direct_nocov <- '
  r2pois ~ disturbedcover + Reg_Balbina + Reg_STM + Reg_PGM
  devmedian ~ disturbedcover + Reg_Balbina + Reg_STM + Reg_PGM
  r2pois ~~ 0*devmedian
'

# Fit Models
fit_mediation_only <- sem(model_full, data = df_modified, estimator = "ML", meanstructure = TRUE)
fit_mediation_partial <- sem(model_partial, data = df_modified, estimator = "ML", meanstructure = TRUE)
fit_direct_only <- sem(model_direct, data = df_modified, estimator = "ML", meanstructure = TRUE)
fit_nocov <- sem(model_direct_nocov, data = df_modified, estimator = "ML", meanstructure = TRUE)

# Likelihood Ratio Test
lrt_h2c <- lavTestLRT(fit_mediation_only, fit_mediation_partial)
print(lrt_h2c)

# Print Comparisons and Summaries
comp_df_h2c <- print_sem_comparison(fit_mediation_only, fit_mediation_partial, fit_direct_only, fit_nocov)
summary(fit_mediation_only, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# ---------------------------------------------------
# Make Plot (Figure 4)
# ---------------------------------------------------

# Calculate parallel lines coefficients for both panels
coef_CATS <- get_parallel_lines_coefs(fit_mediation_only, outcome = "r2pois", predictor = "disturbedcover")
coef_devmedian <- get_parallel_lines_coefs(fit_mediation_only, outcome = "devmedian", predictor = "r2pois")

# Calculate marginal predictions (common slope and CI ribbon)
marg_CATS <- get_marginal_predictions(df_modified, outcome = "r2pois", predictor = "disturbedcover")
marg_devmedian <- get_marginal_predictions(df_modified, outcome = "devmedian", predictor = "r2pois")

# panel A
PanelA <- ggplot(df_modified, aes(x = disturbedcover, y = r2pois, color = region)) +
  geom_ribbon(data = marg_CATS, aes(x = x, ymin = lwr, ymax = upr), fill = "black", alpha = 0.15, inherit.aes = FALSE) +
  geom_line(data = marg_CATS, aes(x = x, y = y), color = "black", linetype = "dashed", linewidth = 0.8, inherit.aes = FALSE) +
  geom_point(alpha = 0.8, size = 1) +
  geom_abline(data = coef_CATS,
              aes(intercept = intercept, slope = slope, color = region),
              linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +
  ylab("Poisson random sampling R2") +
  xlab("Median disturbed forest cover\n(2000m radius)") +
  ggsci::scale_color_npg()

# panel B
PanelB <- ggplot(df_modified, aes(x = r2pois, y = devmedian, color = region)) +
  geom_ribbon(data = marg_devmedian, aes(x = x, ymin = lwr, ymax = upr), fill = "black", alpha = 0.15, inherit.aes = FALSE) +
  geom_line(data = marg_devmedian, aes(x = x, y = y), color = "black", linetype = "dashed", linewidth = 0.8, inherit.aes = FALSE) +
  geom_point(alpha = 0.8, size = 1) +
  geom_abline(data = coef_devmedian,
              aes(intercept = intercept, slope = slope, color = region),
              linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  theme_classic() +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
  xlab("Poisson random\nsampling R2") +
  ylab("EEOS median deviation") +
  ggsci::scale_color_npg(name = "Region")

# Combine and save
cowplot::plot_grid(PanelA, PanelB, labels = c('A', 'B'), rel_widths = c(0.82, 1))
ggsave(paste0("Figures/Figure_H2c_", suffix, ".png"), height = 4, width = 8)

# ---------------------------------------------------
# Save Model Results
# ---------------------------------------------------
dir.create("Outputs", showWarnings = FALSE)
write.csv(comp_df_h2c, paste0("Outputs/H2c_Model_Comparison_", suffix, ".csv"), row.names = FALSE)
pe_h2c <- parameterEstimates(fit_mediation_only, standardized = TRUE, rsquare = TRUE)
write.csv(pe_h2c, paste0("Outputs/H2c_Parameter_Estimates_", suffix, ".csv"), row.names = FALSE)
write.csv(as.data.frame(lrt_h2c), paste0("Outputs/H2c_LRT_", suffix, ".csv"), row.names = FALSE)
cat(paste0("H2c SEM results successfully saved to Outputs/ for suffix: ", suffix, "\n"))

