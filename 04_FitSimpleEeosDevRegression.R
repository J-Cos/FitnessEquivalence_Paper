# --------------------------------------------
# Purpose: H1 - linear regressions of EEoS deviation (absolute and signed) on forest loss with
#          regional fixed effects, robustness checks, ANCOVA slope-homogeneity test, Figure 1 and S2.
# Input Origin Scripts: 03_FitEEOS.py
# Output: Outputs/H1_*_<suffix>.csv; Figures/Figure_H1*.png; Figures/diagnostics/
# ---------------------------------------------------

suppressMessages({library(tidyverse); library(lmtest); library(sandwich)})
source("Code/Functions/config.R"); check_project_root()
invisible(lapply(c("data_prep", "plot_helpers"), \(f) source(sprintf("Code/Functions/%s.R", f))))

suffix <- get_suffix_arg()
set.seed(1)
plot_data <- load_and_prepare_plot_data(suffix)
dir.create("Figures/diagnostics", showWarnings = FALSE)

#' Main model, CIs, ANCOVA interaction test and robustness checks for one outcome
fit_h1 <- function(d, outcome) {
  form <- reformulate(c("disturbedcover", "Region"), outcome)
  fit <- lm(form, data = d)

  coef_tab <- as.data.frame(summary(fit)$coefficients)
  ci <- confint(fit)
  coef_tab$CI_2.5 <- ci[, 1]; coef_tab$CI_97.5 <- ci[, 2]
  coef_tab$Term <- rownames(coef_tab)

  ancova <- as.data.frame(anova(fit, lm(reformulate("disturbedcover * Region", outcome), data = d)))

  # Robustness: HC3 sandwich SEs, Huber M-estimation, exclusion of influential plots (Cook's D > 4/n)
  hc3 <- coeftest(fit, vcov = vcovHC(fit, type = "HC3"))
  rlm_fit <- MASS::rlm(form, data = d, maxit = 100)
  influential <- d$Region_Plot[cooks.distance(fit) > 4 / nobs(fit)]
  fit_noinf <- lm(form, data = filter(d, !Region_Plot %in% influential))
  bp <- bptest(fit)
  robust <- tibble(
    Term = rownames(coef_tab), Estimate = coef(fit),
    HC3_SE = hc3[, "Std. Error"], HC3_p = hc3[, "Pr(>|t|)"],
    RLM_Estimate = coef(rlm_fit),
    NoInfluential_Estimate = coef(fit_noinf), NoInfluential_p = summary(fit_noinf)$coefficients[, 4],
    n_influential_excluded = length(influential),
    BP_statistic = unname(bp$statistic), BP_p = bp$p.value
  )

  png(sprintf("Figures/diagnostics/H1_%s_%s_QQ.png", outcome, suffix), width = 900, height = 450)
  par(mfrow = c(1, 2)); plot(fit, which = c(2, 4)); dev.off()

  list(fit = fit, coef_tab = coef_tab, ancova = ancova, robust = robust, influential = influential)
}

h1_abs <- fit_h1(plot_data, "absdev")
h1_sig <- fit_h1(plot_data, "dev")
print(h1_abs$coef_tab); print(h1_sig$coef_tab)

# Save model results --------------------------------------------------------------------------
write.csv(h1_sig$coef_tab, out_path("H1_Model_Signed", suffix), row.names = FALSE)
write.csv(h1_abs$coef_tab, out_path("H1_Model_Absolute", suffix), row.names = FALSE)
write.csv(mutate(h1_abs$ancova, Model = c("Common Slope (absdev)", "Varying Slopes (absdev)")),
          out_path("H1_ANCOVA_InteractionTest_Absolute", suffix), row.names = FALSE)
write.csv(mutate(h1_sig$ancova, Model = c("Common Slope (dev)", "Varying Slopes (dev)")),
          out_path("H1_ANCOVA_InteractionTest_Signed", suffix), row.names = FALSE)
write.csv(h1_abs$robust, out_path("H1_Robustness_Absolute", suffix), row.names = FALSE)
write.csv(h1_sig$robust, out_path("H1_Robustness_Signed", suffix), row.names = FALSE)

# Figures ---------------------------------------------------------------------------------------
deviation_scatter <- function(y, ylab, hline = FALSE, y_expand = TRUE) {
  p <- mediation_panel(plot_data, "disturbedcover", y, "Region", "Disturbed forest cover\n(2000m radius)", ylab,
                       point_size = 0.5, show_legend = FALSE,
                       extra = c(list(scale_x_continuous(expand = expansion(mult = c(0, 0.1)))),
                                 if (y_expand) list(scale_y_continuous(expand = expansion(mult = c(0, 0.1)))),
                                 if (hline) list(geom_hline(yintercept = 0, linetype = "dotted"))))
  p
}
deviation_violin <- function(y, hline = FALSE) {
  ggplot(plot_data, aes(x = Region, y = .data[[y]], fill = Region, color = Region)) +
    (if (hline) geom_hline(yintercept = 0, linetype = "dotted") else NULL) +
    geom_violin() +
    theme_classic() +
    ggsci::scale_fill_npg() + ggsci::scale_color_npg() +
    (if (!hline) scale_y_continuous(expand = expansion(mult = c(0, 0.1))) else NULL) +
    theme(axis.title.y = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.line.y = element_blank()) +
    xlab(" \n  \n ")
}

# Figure 1: map, absolute deviation vs forest loss, absolute deviation by region
fig1 <- cowplot::plot_grid(make_region_map(), deviation_scatter("absdev", "EEOS absolute deviation"),
                           deviation_violin("absdev"), labels = c("A", "B", "C"),
                           rel_widths = c(0.5, 1, 0.5), ncol = 3)
save_figure("Figure_H1", suffix, fig1, height = 4, width = 6, bg = "white", dpi = 1000)

# Figure S2: signed deviation
figS2 <- cowplot::plot_grid(deviation_scatter("dev", "EEOS deviation", hline = TRUE, y_expand = FALSE),
                            deviation_violin("dev", hline = TRUE), labels = c("A", "B"), rel_widths = c(1, 0.5))
save_figure("Figure_H1_signedsupplement", suffix, figS2, height = 4, width = 4.5, bg = "white")

message("H1 model results saved to Outputs/ for suffix: ", suffix)
