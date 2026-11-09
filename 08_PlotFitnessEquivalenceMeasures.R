# --------------------------------------------
# Purpose: Visualise how all three H2 fitness-equivalence / trait-selection metrics work and vary
#          across disturbance gradients (Figures/Figure_FitnessEquivalence.png + Figures S4-S6).
# Input Origin Scripts: 01_CombinePinhoJuruaData.R; 03_FitEEOS.py; 05_FitCatsRegressions.R;
#                       06_FitRegionalSloanNonNeutral.R; 07_FitSubgroupSloans.R
# Output: Figures/Figure_FitnessEquivalence.png (Fig. 2);
#         Figures/Figure_RegionalSloan_supplement.png (SI Appendix, Fig. S4: A = example fits, B = 100-subsample R^2);
#         Figures/Figure_traitsOfNonneutrals.png (SI Appendix, Fig. S5);
#         Figures/Figure_sloansubsets_supplement.png (SI Appendix, Fig. S6)
# ---------------------------------------------------

suppressMessages({
  library(tidyverse)
  library(patchwork)
})
source("Code/Functions/config.R"); check_project_root()
source("Code/Functions/data_prep.R")
source("Code/Functions/cats_helpers.R")
source("Code/Functions/sloan_helpers.R")
source("Code/Functions/plot_helpers.R")

TRAIT_LABELS <- c(
  WD   = "Wood density",
  lnSM = "Log seed mass",
  LMA  = "Leaf mass per area",
  Hmax = "Maximum height"
)
BETA_COLS <- c(
  WD   = "beta_WD_nb",
  lnSM = "beta_lnSM_nb",
  LMA  = "beta_LMA_nb",
  Hmax = "beta_Hmax_nb"
)

scale_color_disturbed <- function(...) {
  scale_color_viridis_c(option = "plasma", end = 0.88, limits = c(0, 1), ...)
}
scale_fill_disturbed <- function(...) {
  scale_fill_viridis_c(option = "plasma", end = 0.88, limits = c(0, 1), ...)
}

make_row_header <- function(title) {
  ggplot() +
    geom_segment(
      aes(x = 0.88, xend = 0.88, y = 0, yend = 1),
      color = "grey30", linewidth = 0.55
    ) +
    annotate(
      "text", x = 0.38, y = 0.5, label = title,
      angle = 90, fontface = "bold", size = 4.3, color = "black"
    ) +
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    coord_cartesian(clip = "off") +
    theme_void() +
    theme(plot.margin = margin(5.5, 3, 5.5, 4))
}

# 1) Load precomputed outputs from 01, 03, 05, 06, 07 (pure plotting, zero model refitting) ------
comm      <- read_required_csv(out_path("comm"))
plot_data <- load_and_prepare_plot_data(need = c("cats", "nonneutral"))
sub_data  <- load_and_prepare_subgroup_data()
cats_out  <- read_required_csv(out_path("CatsOutput"))
cats_sp   <- readRDS(out_path("CatsSpeciesPredictions", ext = "RDS"))
avg_sloan <- read_required_csv(out_path("SloanRegionalAverageFit"))
sloan_r2  <- read_required_csv(out_path("SloanR2_ByRegion"))
subsets   <- readRDS(out_path("NonOverlappingOrderedPlotSubsets", ext = "RDS"))
sub_objs  <- readRDS(out_path("ForestCoverSubgroupSloanObjects", ext = "RDS"))

# 2) Row 1: CATS Model Fit (H2a) -----------------------------------------------------------------
cats_pres <- cats_sp |> filter(n > 0)

reg_trait_bounds <- cats_sp |>
  distinct(Region, Binomial_correct, WD, lnSM, LMA, Hmax) |>
  pivot_longer(all_of(CATS_TRAITS_4), names_to = "TraitKey", values_to = "val") |>
  group_by(Region, TraitKey) |>
  summarise(
    t_mean = mean(val),
    t_min  = unname(quantile(val, 0.01)),
    t_max  = unname(quantile(val, 0.99)),
    .groups = "drop"
  )

plot_mean_p <- cats_pres |>
  group_by(Region_Plot) |>
  summarise(p_bar = mean(p_obs), .groups = "drop")

# Strict ascending disturbance factor order so ggplot draws least -> most disturbed (most on top)
plot_order_dc <- plot_data |>
  arrange(disturbedcover, Region_Plot) |>
  pull(Region_Plot)

cats_plot_coefs <- cats_out |>
  select(Region, Region_Plot, all_of(unname(BETA_COLS))) |>
  left_join(plot_mean_p, by = "Region_Plot") |>
  left_join(select(plot_data, Region_Plot, disturbedcover), by = "Region_Plot")

# Panel A: All 224 plots' fitted CATS partial trait curves colored by disturbed cover (most on top)
cats_224_curves <- map_dfr(CATS_TRAITS_4, function(tr) {
  bcol <- BETA_COLS[[tr]]
  cats_plot_coefs |>
    transmute(Region, Region_Plot, disturbedcover, p_bar, beta = .data[[bcol]], TraitKey = tr) |>
    left_join(reg_trait_bounds, by = c("Region", "TraitKey")) |>
    rowwise() |>
    reframe({
      xg <- seq(t_min, t_max, length.out = 40)
      tibble(
        Region_Plot    = factor(Region_Plot, levels = plot_order_dc),
        disturbedcover = disturbedcover,
        Trait          = factor(TRAIT_LABELS[[tr]], levels = unname(TRAIT_LABELS)),
        trait_val      = xg,
        p_curve        = p_bar * exp(beta * (xg - t_mean))
      )
    })
}) |>
  arrange(Trait, disturbedcover, Region_Plot, trait_val)

pA <- ggplot(
  cats_224_curves,
  aes(x = trait_val, y = p_curve, group = Region_Plot, color = disturbedcover)
) +
  geom_line(linewidth = 0.36, show.legend = FALSE) +
  scale_color_disturbed() +
  scale_y_log10(
    breaks = c(0.003, 0.01, 0.03, 0.10),
    labels = c("0.003", "0.01", "0.03", "0.10")
  ) +
  coord_cartesian(ylim = c(0.0025, 0.24)) +
  facet_wrap(~ Trait, ncol = 2, scales = "free_x") +
  labs(x = "Trait value", y = "Relative abundance") +
  theme_boxed_facet()

# Panel B: All 224 CATS fits faceted 2x2 by Region, colored by disturbed cover (most on top)
set.seed(1)
cats_sp_dc <- cats_sp |>
  left_join(select(plot_data, Region_Plot, disturbedcover), by = "Region_Plot") |>
  mutate(Region = factor(Region, levels = REGIONS))

cats_pres_ord <- cats_sp_dc |>
  filter(n > 0) |>
  arrange(disturbedcover, Region_Plot)

cats_abs_sample <- cats_sp_dc |>
  filter(n == 0) |>
  group_by(Region) |>
  slice_sample(n = 2000) |>
  ungroup() |>
  arrange(disturbedcover, Region_Plot)

sqrt_brks <- c(0, 0.02, 0.08, 0.20, 0.40)
sqrt_lbls <- c("0", "0.02", "0.08", "0.20", "0.40")

pB <- ggplot() +
  geom_point(
    data = cats_abs_sample,
    aes(x = p_hat_nb, y = p_obs, color = disturbedcover),
    alpha = 0.08, size = 0.32, show.legend = FALSE
  ) +
  geom_point(
    data = cats_pres_ord,
    aes(x = p_hat_nb, y = p_obs, color = disturbedcover),
    alpha = 0.25, size = 0.42, show.legend = FALSE
  ) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", linewidth = 0.55, color = "black") +
  scale_color_disturbed() +
  scale_x_sqrt(breaks = sqrt_brks, labels = sqrt_lbls) +
  scale_y_sqrt(breaks = sqrt_brks, labels = sqrt_lbls) +
  coord_cartesian(xlim = c(0, 0.45), ylim = c(0, 0.45)) +
  facet_wrap_region(ncol = 2) +
  labs(x = "Predicted relative abundance", y = "Observed relative abundance") +
  theme_boxed_facet()

pC <- mediation_panel(
  plot_data,
  x = "disturbedcover", y = "CATS_nb", color_var = "Region",
  xlab = "Disturbed forest cover", ylab = "Model R²",
  show_legend = FALSE
)

# 3) Row 2: Non-neutral Abundance (H2b) ----------------------------------------------------------
avg_sloan_df <- avg_sloan |>
  mutate(
    Region        = factor(Region, levels = REGIONS),
    is_nonneutral = Neutrality != "Neutral"
  ) |>
  arrange(Region, p)

panel_d_labels <- avg_sloan_df |>
  group_by(Region) |>
  summarise(n_nn = sum(is_nonneutral), .groups = "drop") |>
  left_join(select(sloan_r2, Region = region, r2pois_mean), by = "Region") |>
  mutate(
    Region = factor(Region, levels = REGIONS),
    label  = sprintf("R² = %.2f\n%d non-neutral", r2pois_mean, n_nn)
  )

reg_mean_N <- comm |>
  count(Region, Region_Plot, name = "N_plot") |>
  group_by(Region) |>
  summarise(N_bar = mean(N_plot), .groups = "drop")

panel_d_curves <- avg_sloan_df |>
  group_by(Region) |>
  summarise(ln_min = min(log(p)), ln_max = max(log(p)), .groups = "drop") |>
  left_join(reg_mean_N, by = "Region") |>
  rowwise() |>
  reframe({
    ln_grid <- seq(ln_min, ln_max, length.out = 160)
    p_grid  <- exp(ln_grid)
    pred    <- stats::ppois(1 / N_bar, N_bar * p_grid, lower.tail = FALSE)
    ci      <- Hmisc::binconf(pred * SUBSAMPLE_SIZE, SUBSAMPLE_SIZE,
                              alpha = 0.05, method = "wilson", return.df = TRUE)
    tibble(Region = Region, p = p_grid,
           pois.pred = pred, pois.lwr = ci[, "Lower"], pois.upr = ci[, "Upper"])
  }) |>
  mutate(Region = factor(Region, levels = REGIONS))

# Panel D: 2x2 regional 100-subsample average Sloan fits on log10 Relative abundance
pD <- ggplot(avg_sloan_df, aes(x = p, y = freq)) +
  geom_point(
    data = filter(avg_sloan_df, !is_nonneutral),
    color = "grey78", alpha = 0.5, size = 0.55
  ) +
  geom_line(data = panel_d_curves, aes(y = pois.pred), color = "black", linewidth = 0.65) +
  geom_line(data = panel_d_curves, aes(y = pois.lwr),  color = "black", linetype = "dashed", linewidth = 0.42) +
  geom_line(data = panel_d_curves, aes(y = pois.upr),  color = "black", linetype = "dashed", linewidth = 0.42) +
  geom_point(
    data = filter(avg_sloan_df, is_nonneutral),
    color = "black", alpha = 0.92, size = 1.15
  ) +
  geom_text(
    data = panel_d_labels,
    aes(x = 0.0002, y = Inf, label = label),
    hjust = 0, vjust = 1.25, size = 2.55, inherit.aes = FALSE
  ) +
  scale_x_log10(
    breaks = c(0.001, 0.01, 0.05),
    labels = c("0.001", "0.01", "0.05")
  ) +
  scale_y_continuous(limits = c(0, 1.03), breaks = c(0, 0.5, 1)) +
  facet_wrap_region(ncol = 2) +
  labs(x = "Relative abundance", y = "Occurrence frequency") +
  theme_boxed_facet()

# Order plots within each region left-to-right by increasing disturbedcover
ordered_plots <- bind_rows(
  plot_data |> filter(Region == "Jurua") |> arrange(Region_Plot),
  plot_data |> filter(Region != "Jurua") |> arrange(Region, desc(fc2000))
) |>
  group_by(Region) |>
  mutate(
    plot_idx = row_number(),
    n_reg    = n()
  ) |>
  ungroup() |>
  mutate(Region = factor(Region, levels = REGIONS))

# Panel E: Per-plot proportion of trees in non-neutral taxa ordered and colored by disturbedcover
pE <- ggplot(ordered_plots) +
  geom_rect(
    aes(xmin = plot_idx - 0.5, xmax = plot_idx + 0.5,
        ymin = 0, ymax = proportionNonNeutral, fill = disturbedcover),
    alpha = 1.0, color = NA, show.legend = FALSE
  ) +
  scale_fill_disturbed() +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(limits = c(0, 1.01), breaks = c(0, 0.5, 1), expand = expansion(mult = c(0, 0.01))) +
  facet_wrap_region(ncol = 2, scales = "free_x") +
  labs(x = "Plots ordered by disturbed forest cover", y = "Proportion of trees in non-neutral taxa") +
  theme_boxed_facet()

pF <- mediation_panel(
  plot_data,
  x = "disturbedcover", y = "proportionNonNeutral", color_var = "Region",
  xlab = "Disturbed forest cover", ylab = "Proportion of trees in non-neutral taxa",
  show_legend = FALSE
)

# 4) Row 3: Neutral Model Fit (H2c) --------------------------------------------------------------
sub_plot_map <- imap_dfr(subsets, \(pids, sname) tibble(subgroup = sname, Region_Plot = pids))
ordered_with_sub <- ordered_plots |>
  left_join(sub_plot_map, by = "Region_Plot") |>
  left_join(select(sub_data, subgroup, sub_dc = disturbedcover, r2pois), by = "subgroup")

block_df <- ordered_with_sub |>
  filter(!is.na(subgroup)) |>
  group_by(Region, subgroup) |>
  summarise(
    xmin   = min(plot_idx) - 0.5,
    xmax   = max(plot_idx) + 0.5,
    sub_dc = first(sub_dc),
    n_in   = n(),
    span   = max(plot_idx) - min(plot_idx) + 1L,
    .groups = "drop"
  )
stopifnot(nrow(block_df) == nrow(sub_data), all(block_df$n_in == SUBGROUP_SIZE & block_df$span == SUBGROUP_SIZE))

# Panel G: 5-plot grouping process with white background, region-colored banners & plot points
pG <- ggplot() +
  geom_rect(
    data = block_df,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    fill = NA, color = "grey65", linewidth = 0.28
  ) +
  geom_segment(
    data = block_df,
    aes(x = xmin, xend = xmax, y = sub_dc, yend = sub_dc),
    color = "black", linewidth = 0.85
  ) +
  geom_line(
    data = ordered_with_sub,
    aes(x = plot_idx, y = disturbedcover),
    color = "grey55", linewidth = 0.42
  ) +
  geom_point(
    data = filter(ordered_with_sub, is.na(subgroup)),
    aes(x = plot_idx, y = disturbedcover, color = Region),
    shape = 21, fill = "white", size = 1.05, stroke = 0.5, show.legend = FALSE
  ) +
  geom_point(
    data = filter(ordered_with_sub, !is.na(subgroup)),
    aes(x = plot_idx, y = disturbedcover, color = Region),
    size = 1.1, alpha = 0.92, show.legend = FALSE
  ) +
  scale_color_manual(values = REGION_PAL) +
  scale_x_continuous(expand = expansion(mult = c(0.03, 0.03))) +
  scale_y_continuous(limits = c(-0.02, 1.03), breaks = c(0, 0.5, 1)) +
  facet_wrap_region(ncol = 2, scales = "free_x") +
  labs(x = "Plots ordered by disturbed forest cover", y = "Disturbed forest cover") +
  theme_boxed_facet()

sub_order_dc <- sub_data |>
  arrange(disturbedcover, subgroup) |>
  pull(subgroup)

subgroup_curves <- imap_dfr(sub_objs, function(f, sname) {
  N_bar <- f$stats$N
  v <- as_tibble(f$vals, rownames = "sp") |>
    transmute(x = log(N_bar * p), freq = freq)
  fit_clog <- suppressWarnings(glm(freq ~ x, family = quasibinomial(link = "cloglog"), data = v))
  x_grid <- seq(min(v$x), min(max(v$x), 3.2), length.out = 60)
  tibble(
    subgroup = sname,
    x        = x_grid,
    freq_hat = as.numeric(predict(fit_clog, newdata = tibble(x = x_grid), type = "response"))
  )
}) |>
  left_join(select(sub_data, subgroup, Region = region, disturbedcover, r2pois), by = "subgroup") |>
  left_join(reg_mean_N, by = "Region") |>
  mutate(
    p_rel    = exp(x) / N_bar,
    Region   = factor(Region, levels = REGIONS),
    subgroup = factor(subgroup, levels = sub_order_dc)
  ) |>
  arrange(Region, disturbedcover, subgroup, p_rel)

neutral_master_curve <- reg_mean_N |>
  rowwise() |>
  reframe({
    x_grid <- seq(-1.62, 3.2, length.out = 120)
    tibble(
      Region       = Region,
      p_rel        = exp(x_grid) / N_bar,
      freq_neutral = 1 - exp(-exp(x_grid))
    )
  }) |>
  mutate(Region = factor(Region, levels = REGIONS))

# Panel H: 43 subgroup occurrence curves faceted 2x2 by Region on log10 Relative abundance
pH <- ggplot() +
  geom_line(
    data = subgroup_curves,
    aes(x = p_rel, y = freq_hat, group = subgroup, color = disturbedcover),
    linewidth = 0.55, show.legend = FALSE
  ) +
  geom_line(
    data = neutral_master_curve,
    aes(x = p_rel, y = freq_neutral),
    color = "black", linewidth = 0.9
  ) +
  scale_color_disturbed() +
  scale_x_log10(
    breaks = c(0.002, 0.01, 0.05, 0.20),
    labels = c("0.002", "0.01", "0.05", "0.20")
  ) +
  scale_y_continuous(limits = c(0, 1.02), breaks = c(0, 0.5, 1)) +
  coord_cartesian(xlim = c(0.0011, 0.25)) +
  facet_wrap_region(ncol = 2) +
  labs(x = "Relative abundance", y = "Occurrence frequency") +
  theme_boxed_facet()

# Panel I: Subgroup neutral R^2 with reversed y-axis so worsening neutral fit slopes upward like C & F
pI <- mediation_panel(
  sub_data,
  x = "disturbedcover", y = "r2pois", color_var = "region",
  xlab = "Disturbed forest cover", ylab = "Model R² (reversed)",
  point_size = 1.0, show_legend = FALSE,
  extra = list(
    geom_hline(yintercept = 0, linetype = "dotted", linewidth = 0.5, color = "black"),
    scale_y_reverse()
  )
)

# 5) Row headers, centered bottom Disturbed Forest Cover colorbar, & flat patchwork assembly -----
rh1 <- make_row_header("CATS Model Fit")
rh2 <- make_row_header("Non-neutral Abundance")
rh3 <- make_row_header("Neutral Model Fit")

leg_disturbed_plot <- ggplot(
  tibble(dc = c(0, 1)),
  aes(x = dc, y = dc, color = dc)
) +
  geom_point() +
  scale_color_disturbed(
    name   = "Disturbed forest cover",
    breaks = seq(0, 1, by = 0.25),
    labels = c("0.00", "0.25", "0.50", "0.75", "1.00")
  ) +
  guides(color = guide_colorbar(
    title.position = "left",
    title.vjust    = 0.85,
    barwidth       = unit(4.6, "cm"),
    barheight      = unit(0.28, "cm")
  )) +
  theme_boxed(base_size = 10) +
  theme(
    legend.position = "bottom",
    legend.title    = element_text(face = "bold", size = 9.5, margin = margin(r = 8)),
    legend.text     = element_text(size = 8.8)
  )

legend_bar <- extract_legend(leg_disturbed_plot)

design_3x3 <- "
ABCD
EFGH
IJKM
#NNN
"

fig_fe <- rh1 + pA + pB + pC +
  rh2 + pD + pE + pF +
  rh3 + pG + pH + pI +
  patchwork::wrap_elements(full = legend_bar) +
  patchwork::plot_layout(
    design  = design_3x3,
    widths  = c(0.10, 1.16, 1.12, 1.00),
    heights = c(1, 1, 1, 0.09)
  ) +
  patchwork::plot_annotation(
    tag_levels = list(c("", "A", "B", "C", "", "D", "E", "F", "", "G", "H", "I", ""))
  )

save_figure("Figure_FitnessEquivalence", fig_fe, width = 14.9, height = 12.4)
message("Saved Figures/Figure_FitnessEquivalence.png")

# 6) Supplementary diagnostic figures (Figures S4-S6) in journal boxed style ---------------------
plot_sloan_fit_facets <- function(fits_named, labels_df, ncol, x_breaks, x_labels,
                                  pt_size = 0.65, text_size = 2.75) {
  panel_levels <- labels_df$Panel

  # Opaque uniform grey68 points prevent overplotted discrete (p, freq) lattice points from darkening
  pts_df <- imap_dfr(fits_named, function(f, key) {
    as_tibble(f$vals, rownames = "sp") |>
      transmute(key = key, p = p, freq = freq) |>
      distinct(key, p, freq)
  }) |>
    left_join(select(labels_df, key, Panel), by = "key") |>
    mutate(Panel = factor(Panel, levels = panel_levels))

  curves_df <- imap_dfr(fits_named, function(f, key) {
    p_grid <- exp(seq(min(log(f$vals$p)), max(log(f$vals$p)), length.out = 150))
    pred   <- stats::ppois(f$stats$Detect, f$stats$N * p_grid, lower.tail = FALSE)
    ci     <- Hmisc::binconf(pred * f$stats$Samples, f$stats$Samples,
                             alpha = 0.05, method = "wilson", return.df = TRUE)
    tibble(key = key, p = p_grid, pois.pred = pred,
           pois.lwr = ci[, "Lower"], pois.upr = ci[, "Upper"])
  }) |>
    left_join(select(labels_df, key, Panel), by = "key") |>
    mutate(Panel = factor(Panel, levels = panel_levels))

  ann_df <- labels_df |>
    mutate(Panel = factor(Panel, levels = panel_levels), x_pos = min(pts_df$p))

  ggplot(pts_df, aes(x = p, y = freq)) +
    geom_point(color = "grey68", alpha = 1.0, size = pt_size) +
    geom_line(data = curves_df, aes(y = pois.pred), color = "black", linewidth = 0.6) +
    geom_line(data = curves_df, aes(y = pois.lwr),  color = "black", linetype = "dashed", linewidth = 0.4) +
    geom_line(data = curves_df, aes(y = pois.upr),  color = "black", linetype = "dashed", linewidth = 0.4) +
    geom_text(
      data = ann_df,
      aes(x = x_pos, y = Inf, label = annot),
      hjust = 0, vjust = 1.35, size = text_size, inherit.aes = FALSE
    ) +
    scale_x_log10(breaks = x_breaks, labels = x_labels) +
    scale_y_continuous(limits = c(0, 1.03), breaks = c(0, 0.5, 1)) +
    facet_wrap_region(~ Panel, ncol = ncol) +
    labs(x = "Relative abundance", y = "Occurrence frequency") +
    theme_boxed_facet()
}

# Figure S4: Combined 2-panel regional neutral fits (A = single-subsample fits; B = 100-subsample R^2)
example_fits <- readRDS(out_path("SloanExampleFits", ext = "RDS"))[REGIONS]
s3_labels <- imap_dfr(example_fits, function(f, reg) {
  tibble(
    key   = reg,
    Panel = reg,
    annot = sprintf("R² = %.2f", if_else(abs(f$stats$Rsqr.pois) < 0.005, 0, f$stats$Rsqr.pois))
  )
})
p_s3a <- plot_sloan_fit_facets(
  example_fits, s3_labels, ncol = 2,
  x_breaks = c(0.001, 0.01, 0.05),
  x_labels = c("0.001", "0.01", "0.05"),
  pt_size = 0.72, text_size = 2.9
)

sloan_stats <- as_tibble(readRDS(out_path("SloanOutput", ext = "RDS"))) |>
  mutate(Region = factor(region, levels = REGIONS))
p_s3b <- region_boxplot_panel(
  sloan_stats,
  y    = "r2pois",
  ylab = "Neutral model R²"
)

fig_s3 <- p_s3a + p_s3b +
  patchwork::plot_layout(widths = c(1.22, 1.00)) +
  patchwork::plot_annotation(tag_levels = "A")
save_figure("Figure_RegionalSloan_supplement", fig_s3, width = 11.2, height = 5.4)

# Figure S5: Standardized functional trait values of consistently non-neutral taxa by region
trait_z <- read_required_csv(out_path("NonNeutralTraitZScores")) |>
  filter(!is.na(TraitScore)) |>
  mutate(
    Region    = factor(RegionNeutral, levels = REGIONS),
    TraitName = factor(TraitName, levels = CATS_TRAITS_4, labels = unname(TRAIT_LABELS[CATS_TRAITS_4]))
  )
fig_s4 <- ggplot(trait_z, aes(x = TraitName, y = TraitScore, fill = Region)) +
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 0.6, color = "black") +
  geom_boxplot(
    width = 0.62, position = position_dodge(width = 0.72),
    alpha = 0.75, outlier.shape = NA, color = "black", linewidth = 0.42
  ) +
  geom_point(
    position = position_jitterdodge(jitter.width = 0.11, dodge.width = 0.72, seed = 1),
    shape = 21, color = "black", stroke = 0.35, size = 1.4, alpha = 0.9, show.legend = FALSE
  ) +
  ggsci::scale_fill_npg(name = "Region") +
  labs(x = "Functional trait", y = "Standardized trait value") +
  theme_boxed() +
  theme(legend.position = "bottom")
save_figure("Figure_traitsOfNonneutrals", fig_s4, width = 7.5, height = 5.6)

# Figure S6: All 43 five-plot subgroup neutral fits ordered by Region and ascending disturbed cover
s5_labels <- sub_data |>
  mutate(Region = factor(as.character(region), levels = REGIONS)) |>
  left_join(
    ordered_with_sub |>
      filter(!is.na(subgroup)) |>
      group_by(subgroup) |>
      summarise(first_plot_idx = min(plot_idx), .groups = "drop"),
    by = "subgroup"
  ) |>
  arrange(Region, first_plot_idx) |>
  group_by(Region) |>
  mutate(
    group_rank = row_number(),
    Panel      = sprintf("%s %d (%.2f)", as.character(Region), group_rank, disturbedcover),
    annot      = sprintf("R² = %.2f", if_else(abs(r2pois) < 0.005, 0, r2pois))
  ) |>
  ungroup() |>
  select(key = subgroup, Panel, annot)

fig_s5 <- plot_sloan_fit_facets(
  sub_objs[s5_labels$key], s5_labels, ncol = 7,
  x_breaks = c(0.002, 0.01, 0.05, 0.20),
  x_labels = c("0.002", "0.01", "0.05", "0.20"),
  pt_size = 0.65, text_size = 2.65
)
save_figure("Figure_sloansubsets_supplement", fig_s5, width = 14.5, height = 13.5, dpi = 300)
message("Saved supplementary diagnostic figures (Figures S4-S6).")
