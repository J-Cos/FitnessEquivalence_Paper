# --------------------------------------------
# Purpose: Visualise the exact 3-run additive BIOMASS channel decomposition of H1 (disturbance ->
#          EEoS deviation) and H2a-c (fitness-equivalence mediation of EEoS deviation) in a 2x2
#          publication figure (Figures/Figure_BiomassDecomposition.png).
#
#          Reads precomputed tables from 03_FitEEOS.py and 11_FitBiomassDecomposition.R (zero model
#          or bootstrap refitting).
# Input Origin Scripts: 03_FitEEOS.py; 05_FitCatsRegressions.R; 06_FitRegionalSloanNonNeutral.R;
#                       07_FitSubgroupSloans.R; 11_FitBiomassDecomposition.R
# Output: Figures/Figure_BiomassDecomposition.png (Fig. 4)
# ---------------------------------------------------

suppressMessages({
  library(tidyverse)
  library(patchwork)
})
source("Code/Functions/config.R"); check_project_root()
source("Code/Functions/data_prep.R")
source("Code/Functions/plot_helpers.R")

# 1) Load prepared plot data and precomputed 11_FitBiomassDecomposition.R summary CSVs ------------
plot_data <- load_and_prepare_plot_data()
ols_tab   <- read_required_csv(out_path("BiomassDecomposition_OLS"))
sem_tab   <- read_required_csv(out_path("BiomassDecomposition_SEM_Summary"))

CHANNEL_LABELS <- c(
  Total        = "Total deviation",
  Wood_Density = "Wood density",
  Pure_Size    = "Pure size structure",
  Site_Height  = "Site height"
)
CHANNEL_AXIS_LABELS <- c(
  "Total deviation"     = "Total\ndeviation",
  "Wood density"        = "Wood\ndensity",
  "Pure size structure" = "Pure size\nstructure",
  "Site height"         = "Site\nheight"
)
CHANNEL_COLS <- c(
  "Total deviation"     = "#111111",
  "Wood density"        = "#363636",
  "Pure size structure" = "#686868",
  "Site height"         = "#8E8E8E"
)
CHANNEL_LTYS <- c(
  "Total deviation"     = "solid",
  "Wood density"        = "42",
  "Pure size structure" = "4111",
  "Site height"         = "22"
)
CHANNEL_SHAPES <- c(
  "Total deviation"     = 22,
  "Wood density"        = 21,
  "Pure size structure" = 24,
  "Site height"         = 23
)

# 2) Panel A: Marginal channel trajectories across disturbed forest cover (H1, n = 224) ----------
chan_outcomes <- c(
  "Total deviation"     = "dev",
  "Wood density"        = "w_rho",
  "Pure size structure" = "r_size",
  "Site height"         = "w_H"
)

marg_df <- imap_dfr(chan_outcomes, function(y_var, ch_lab) {
  get_marginal_predictions(plot_data, outcome = y_var, predictor = "disturbedcover") |>
    mutate(Channel_Label = factor(ch_lab, levels = names(CHANNEL_COLS)))
})

# Sparse symbol markers along each trajectory so Panel A matches the line+symbol legend and B-D
marker_idx <- c(1, 25, 50, 75, 100)
marg_pts <- marg_df |>
  group_by(Channel_Label) |>
  slice(marker_idx) |>
  ungroup()

# Direct inline trajectory labels placed in clear space adjacent to each curve
panel_a_labels <- tibble(
  Channel_Label = factor(names(CHANNEL_COLS), levels = names(CHANNEL_COLS)),
  x     = c(0.67, 0.25, 0.20, 0.78),
  y     = c(-0.172, 0.063, -0.105, 0.015),
  angle = c(-14.5, -13.0, 0, -1.2),
  face  = c("bold", "bold", "plain", "plain")
)

pA <- ggplot(
  marg_df,
  aes(x = x, y = y, color = Channel_Label, linetype = Channel_Label, fill = Channel_Label)
) +
  geom_hline(yintercept = 0, linetype = "13", color = "grey40", linewidth = 0.45) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.11, color = NA, show.legend = FALSE) +
  geom_line(linewidth = 0.88, show.legend = FALSE) +
  geom_point(
    data = marg_pts,
    aes(shape = Channel_Label),
    size = 2.15, stroke = 0.60, show.legend = FALSE
  ) +
  geom_text(
    data = panel_a_labels,
    aes(x = x, y = y, label = Channel_Label, angle = angle, fontface = face, color = Channel_Label),
    size = 3.0, show.legend = FALSE, inherit.aes = FALSE
  ) +
  scale_color_manual(values = CHANNEL_COLS) +
  scale_fill_manual(values = CHANNEL_COLS) +
  scale_linetype_manual(values = CHANNEL_LTYS) +
  scale_shape_manual(values = CHANNEL_SHAPES) +
  scale_x_continuous(
    breaks = seq(0, 1, by = 0.25),
    labels = c("0.00", "0.25", "0.50", "0.75", "1.00")
  ) +
  scale_y_continuous(limits = c(-0.24, 0.105), breaks = seq(-0.20, 0.10, by = 0.10)) +
  labs(
    x = "Disturbed forest cover (2000 m radius)",
    y = "EEoS deviation component"
  ) +
  theme_boxed()

# 3) Panel B: Exact additive H1 disturbance slopes (beta +/- 95% CI + % share of total) -----------
h1_df <- ols_tab |>
  filter(Hypothesis == "H1") |>
  mutate(
    Channel_Label = factor(CHANNEL_LABELS[Channel], levels = rev(names(CHANNEL_COLS))),
    Pct_Label     = sprintf("%.0f%%", Pct_of_Total),
    Pct_Face      = if_else(Channel %in% c("Total", "Wood_Density"), "bold", "plain")
  )

pB <- ggplot(h1_df, aes(y = Channel_Label)) +
  geom_hline(yintercept = c(1.5, 2.5, 3.5), color = "grey88", linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "13", color = "grey40", linewidth = 0.5) +
  geom_linerange(
    aes(xmin = CI_2.5, xmax = CI_97.5, color = Channel_Label),
    linewidth = 0.72, show.legend = FALSE
  ) +
  geom_point(
    aes(x = Estimate, shape = Channel_Label, fill = Channel_Label, color = Channel_Label),
    size = 2.65, stroke = 0.65, show.legend = FALSE
  ) +
  geom_text(
    aes(x = 0.058, label = Pct_Label, fontface = Pct_Face),
    hjust = 1, size = 3.05, color = "grey15"
  ) +
  scale_color_manual(values = CHANNEL_COLS) +
  scale_fill_manual(values = CHANNEL_COLS) +
  scale_shape_manual(values = CHANNEL_SHAPES) +
  scale_y_discrete(labels = CHANNEL_AXIS_LABELS) +
  scale_x_continuous(
    limits = c(-0.215, 0.068),
    breaks = c(-0.20, -0.15, -0.10, -0.05, 0.00),
    labels = c("-0.20", "-0.15", "-0.10", "-0.05", "0.00")
  ) +
  labs(
    x = "Disturbance slope (\u03b2 \u00b1 95% CI)",
    y = NULL
  ) +
  theme_boxed() +
  theme(axis.text.y = element_text(lineheight = 0.92))

# 4) Panels C & D: Garrido 8-model MSA-SEM channel mediation across fitness-equivalence metrics ---
hyp_levels <- c("H2a", "H2a_noWD", "H2b", "H2c_median")
hyp_labels <- c(
  H2a        = "CATS\nModel Fit",
  H2a_noWD   = "CATS\n(no WD)",
  H2b        = "Non-neutral\nAbundance",
  H2c_median = "Neutral\nModel Fit"
)

chan_offsets <- c(
  "Total deviation"     =  0.255,
  "Wood density"        =  0.085,
  "Pure size structure" = -0.085,
  "Site height"         = -0.255
)

sem_plot_df <- sem_tab |>
  filter(Hypothesis %in% hyp_levels) |>
  mutate(
    Channel_Label = factor(CHANNEL_LABELS[Channel], levels = names(CHANNEL_COLS)),
    Hyp_Idx       = 5L - match(Hypothesis, hyp_levels),
    y_pos         = Hyp_Idx + unname(chan_offsets[as.character(Channel_Label)]),
    # Reverse sign of Neutral Model Fit R^2 path b so reduced fitness equivalence aligns across rows
    b_disp        = if_else(Hypothesis == "H2c_median", -Avg_std_b, Avg_std_b),
    b_lwr         = b_disp - Uncond_SE_std_b,
    b_upr         = b_disp + Uncond_SE_std_b,
    b_supported   = RelImp_b >= 0.70,
    b_fill        = if_else(b_supported, unname(CHANNEL_COLS[as.character(Channel_Label)]), "white"),
    ind_lwr       = Avg_est_ind - Uncond_SE_ind,
    ind_upr       = Avg_est_ind + Uncond_SE_ind,
    ind_supported = RelImp_ind >= 0.70,
    ind_fill      = if_else(ind_supported, unname(CHANNEL_COLS[as.character(Channel_Label)]), "white"),
    stroke_col    = unname(CHANNEL_COLS[as.character(Channel_Label)])
  )

sep_y <- c(1.5, 2.5, 3.5)

pC <- ggplot(sem_plot_df, aes(y = y_pos)) +
  geom_hline(yintercept = sep_y, color = "grey88", linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "13", color = "grey40", linewidth = 0.5) +
  geom_linerange(
    aes(xmin = b_lwr, xmax = b_upr, color = stroke_col),
    linewidth = 0.68
  ) +
  geom_point(
    aes(x = b_disp, shape = Channel_Label, fill = b_fill, color = stroke_col),
    size = 2.50, stroke = 0.65, show.legend = FALSE
  ) +
  scale_color_identity() +
  scale_fill_identity() +
  scale_shape_manual(values = CHANNEL_SHAPES) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.07))) +
  scale_y_continuous(
    limits = c(0.52, 4.48),
    breaks = 1:4,
    labels = unname(hyp_labels[rev(hyp_levels)]),
    expand = c(0, 0)
  ) +
  labs(
    x = "Mediator \u2192 channel path (b \u00b1 SE)",
    y = NULL
  ) +
  theme_boxed() +
  theme(axis.text.y = element_text(lineheight = 0.92))

pD <- ggplot(sem_plot_df, aes(y = y_pos)) +
  geom_hline(yintercept = sep_y, color = "grey88", linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "13", color = "grey40", linewidth = 0.5) +
  geom_linerange(
    aes(xmin = ind_lwr, xmax = ind_upr, color = stroke_col),
    linewidth = 0.68
  ) +
  geom_point(
    aes(x = Avg_est_ind, shape = Channel_Label, fill = ind_fill, color = stroke_col),
    size = 2.50, stroke = 0.65, show.legend = FALSE
  ) +
  scale_color_identity() +
  scale_fill_identity() +
  scale_shape_manual(values = CHANNEL_SHAPES) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.07))) +
  scale_y_continuous(
    limits = c(0.52, 4.48),
    breaks = 1:4,
    labels = NULL,
    expand = c(0, 0)
  ) +
  labs(
    x = "Mediated indirect effect (a \u00d7 b \u00b1 SE)",
    y = NULL
  ) +
  theme_boxed() +
  theme(axis.ticks.y = element_blank())

# 5) Clean custom bottom legend strip (UTF-8 safe without ggplotGrob PDF device warnings) ---------
make_decomposition_legend <- function() {
  ch_df <- tibble(
    x0    = c(10.0, 27.2, 43.5, 62.8),
    x1    = c(14.0, 31.2, 47.5, 66.8),
    xm    = c(12.0, 29.2, 45.5, 64.8),
    xt    = c(15.0, 32.2, 48.5, 67.8),
    lbl   = names(CHANNEL_COLS),
    col   = unname(CHANNEL_COLS),
    lty   = unname(CHANNEL_LTYS),
    shape = unname(CHANNEL_SHAPES)
  )
  ri_df <- tibble(
    xm   = c(89.3, 97.8),
    xt   = c(90.7, 99.2),
    lbl  = c("RI \u2265 0.70", "RI < 0.70"),
    fill = c("#222222", "white")
  )

  p <- ggplot() +
    annotate("text", x = 1.0, y = 0.5, label = "Channel:",
             hjust = 0, fontface = "bold", size = 3.2, color = "black")
  for (i in seq_len(nrow(ch_df))) {
    r <- ch_df[i, ]
    p <- p +
      geom_segment(data = r, aes(x = x0, xend = x1, y = 0.5, yend = 0.5),
                   color = r$col, linetype = r$lty, linewidth = 0.85) +
      geom_point(data = r, aes(x = xm, y = 0.5),
                 shape = r$shape, fill = r$col, color = r$col, size = 2.4, stroke = 0.65) +
      annotate("text", x = r$xt, y = 0.5, label = r$lbl,
               hjust = 0, size = 3.0, color = "black")
  }
  p +
    annotate("text", x = 78.2, y = 0.5, label = "SEM support:",
             hjust = 0, fontface = "bold", size = 3.2, color = "black") +
    geom_point(data = ri_df, aes(x = xm, y = 0.5, fill = fill),
               shape = 21, color = "#222222", size = 2.4, stroke = 0.65) +
    geom_text(data = ri_df, aes(x = xt, y = 0.5, label = lbl),
              hjust = 0, size = 3.0, color = "black") +
    scale_fill_identity() +
    coord_cartesian(xlim = c(0, 105.5), ylim = c(0, 1), expand = FALSE, clip = "off") +
    theme_void()
}

leg_strip <- make_decomposition_legend()

design_2x2 <- "
AB
CD
EE
"

fig_decomp <- pA + pB + pC + pD +
  patchwork::wrap_elements(full = leg_strip, clip = FALSE) +
  patchwork::plot_layout(
    design  = design_2x2,
    widths  = c(1.04, 0.96),
    heights = c(0.88, 1.12, 0.10)
  ) +
  patchwork::plot_annotation(
    tag_levels = list(c("A", "B", "C", "D", ""))
  )

save_figure("Figure_BiomassDecomposition", fig_decomp, width = 9.8, height = 8.3)
message("Saved Figures/Figure_BiomassDecomposition.png")
