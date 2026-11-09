# --------------------------------------------
# Purpose: Visualise H2a-c mediation relationships, multimodel-averaged structural equation models
#          (MSA-SEM; Garrido et al. 2022), and the proportion of the total disturbance effect
#          mediated in a 3x3 publication layout
#          (Figures/Figure_H2_SEMs.png + Figures/Figure_H2_SEMs_absolutesupplement.png).
# Input Origin Scripts: 03_FitEEOS.py; 05_FitCatsRegressions.R; 06_FitRegionalSloanNonNeutral.R;
#                       07_FitSubgroupSloans.R; 09_FitMediationSEMs.R
# Output: Figures/Figure_H2_SEMs.png (Fig. 3);
#         Figures/Figure_H2_SEMs_absolutesupplement.png (SI Appendix, Fig. S7)
# ---------------------------------------------------

suppressMessages({
  library(tidyverse)
  library(patchwork)
})
source("Code/Functions/config.R"); check_project_root()
source("Code/Functions/data_prep.R")
source("Code/Functions/plot_helpers.R")

# 1) Load prepared plot/subgroup data and precomputed 09_FitMediationSEMs.R tables ----------------
plot_data <- load_and_prepare_plot_data(need = c("cats", "nonneutral"))
sub_data  <- load_and_prepare_subgroup_data()

build_h2_sem_figure <- function(tier = c("Signed", "Absolute")) {
  tier <- match.arg(tier)
  is_signed <- tier == "Signed"

  y_plot_var <- if (is_signed) "dev" else "absdev"
  y_sub_var  <- if (is_signed) "devmedian" else "RMSE"
  y_plot_lab <- if (is_signed) "EEoS deviation" else "EEoS absolute deviation"
  y_sub_lab  <- if (is_signed) "Median EEoS deviation" else "EEoS RMSE"
  y_plot_box <- if (is_signed) "EEoS deviation" else "EEoS absolute\ndeviation"
  y_sub_box  <- if (is_signed) "Median EEoS\ndeviation" else "EEoS RMSE"

  marg_a <- get_marginal_predictions(plot_data, y_plot_var, "CATS_nb")
  marg_b <- get_marginal_predictions(plot_data, y_plot_var, "proportionNonNeutral")
  marg_c <- get_marginal_predictions(sub_data,  y_sub_var,  "r2pois")

  ylim_plot <- range(
    c(0, plot_data[[y_plot_var]], marg_a$lwr, marg_a$upr, marg_b$lwr, marg_b$upr),
    na.rm = TRUE
  )
  ylim_sub <- range(
    c(0, sub_data[[y_sub_var]], marg_c$lwr, marg_c$upr),
    na.rm = TRUE
  )

  extra_plot <- c(
    if (is_signed) list(geom_hline(yintercept = 0, linetype = "dotted", linewidth = 0.6, color = "black")) else list(),
    list(scale_y_continuous(limits = ylim_plot))
  )
  extra_sub <- c(
    if (is_signed) list(geom_hline(yintercept = 0, linetype = "dotted", linewidth = 0.6, color = "black")) else list(),
    list(scale_x_reverse(), scale_y_continuous(limits = ylim_sub))
  )

  # Column 1 (A, D, G): Mediation scatter panels (M -> Y controlling for regional fixed effects)
  pA <- mediation_panel(
    plot_data, x = "CATS_nb", y = y_plot_var, color_var = "Region",
    xlab = "CATS model R\u00b2", ylab = y_plot_lab,
    show_legend = FALSE, extra = extra_plot
  )
  pD <- mediation_panel(
    plot_data, x = "proportionNonNeutral", y = y_plot_var, color_var = "Region",
    xlab = "Proportion of trees in non-neutral taxa", ylab = y_plot_lab,
    show_legend = FALSE, extra = extra_plot
  )
  pG <- mediation_panel(
    sub_data, x = "r2pois", y = y_sub_var, color_var = "region",
    xlab = "Neutral model R\u00b2 (reversed)", ylab = y_sub_lab,
    point_size = 1.0, show_legend = FALSE, extra = extra_sub
  )

  # Column 2 (B, E, H): Garrido et al. (2022) multimodel-averaged SEM pathway diagrams
  sem_a <- read_sem_diagram_stats(paste0("H2a_", tier), "CATS_nb",              y_plot_var)
  sem_b <- read_sem_diagram_stats(paste0("H2b_", tier), "proportionNonNeutral", y_plot_var)
  sem_c <- read_sem_diagram_stats(paste0("H2c_", tier), "r2pois",               y_sub_var)

  pB <- plot_sem_diagram(
    sem_a,
    x_label = "Disturbed forest\ncover",
    m_label = "CATS model R\u00b2",
    y_label = y_plot_box
  )
  pE <- plot_sem_diagram(
    sem_b,
    x_label = "Disturbed forest\ncover",
    m_label = "Proportion of trees in\nnon-neutral taxa",
    y_label = y_plot_box
  )
  pH <- plot_sem_diagram(
    sem_c,
    x_label = "Median disturbed\nforest cover",
    m_label = "Neutral model R\u00b2",
    y_label = y_sub_box
  )

  # Column 3 (C, F, I): Proportion of total disturbance effect mediated vs. direct
  pC <- plot_proportion_mediated(sem_a)
  pF <- plot_proportion_mediated(sem_b)
  pI <- plot_proportion_mediated(sem_c)

  # Row headers & shared bottom legend strip aligned beneath each column
  rh1 <- make_row_header("CATS Model Fit")
  rh2 <- make_row_header("Non-neutral Abundance")
  rh3 <- make_row_header("Neutral Model Fit")

  reg_leg_plot <- ggplot(plot_data, aes(x = 1, y = 1, color = Region)) +
    geom_point(size = 2.1) +
    ggsci::scale_color_npg(name = "Region:") +
    theme_boxed(base_size = 10) +
    theme(
      legend.position = "bottom",
      legend.title    = element_text(face = "bold", size = 9.5, margin = margin(r = 4)),
      legend.text     = element_text(size = 9.0),
      legend.key.size = unit(0.42, "cm"),
      legend.margin   = margin(0, 0, 0, 0)
    )
  reg_leg_grob <- extract_legend(reg_leg_plot)
  sem_leg_plot <- make_sem_path_legend()
  bar_leg_grob <- make_mediation_bar_legend()

  design_3x3 <- "
ABCD
EFGH
IJKL
#MNO
"

  rh1 + pA + pB + pC +
    rh2 + pD + pE + pF +
    rh3 + pG + pH + pI +
    patchwork::wrap_elements(panel = reg_leg_grob, clip = FALSE) +
    patchwork::wrap_elements(panel = sem_leg_plot, clip = FALSE) +
    patchwork::wrap_elements(panel = bar_leg_grob, clip = FALSE) +
    patchwork::plot_layout(
      design  = design_3x3,
      widths  = c(0.11, 1.00, 1.20, 0.17),
      heights = c(1, 1, 1, 0.10)
    ) +
    patchwork::plot_annotation(
      tag_levels = list(c("", "A", "B", "C", "", "D", "E", "F", "", "G", "H", "I", "", "", ""))
    )
}

# 2) Render primary signed and supplementary absolute/RMSE 3x3 figures ----------------------------
fig_h2_signed <- build_h2_sem_figure("Signed")
save_figure("Figure_H2_SEMs", fig_h2_signed, width = 13.2, height = 10.6)
message("Saved Figures/Figure_H2_SEMs.png")

fig_h2_abs <- build_h2_sem_figure("Absolute")
save_figure("Figure_H2_SEMs_absolutesupplement", fig_h2_abs, width = 13.2, height = 10.6)
message("Saved Figures/Figure_H2_SEMs_absolutesupplement.png")
