# --------------------------------------------
# Purpose: Shared plotting helpers: journal boxed theme, legend extraction, scatter/boxplot/matrix
#          panels, marginal common-slope mediation panels, row headers, Garrido MSA-SEM pathway
#          diagrams, figure saving, and regional map (Fig. 1A).
# ---------------------------------------------------

#' Marginal prediction line and 95% CI for a common slope, with region dummies at their sample means
get_marginal_predictions <- function(data, outcome, predictor, grid_points = 100) {
  formula_str <- paste(outcome, "~", predictor, "+", paste(REGION_DUMMIES, collapse = " + "))
  fit_lm <- stats::lm(stats::as.formula(formula_str), data = data)

  grid_x <- seq(min(data[[predictor]], na.rm = TRUE), max(data[[predictor]], na.rm = TRUE),
                length.out = grid_points)
  nd <- data.frame(x = grid_x,
                   Reg_Balbina = mean(data$Reg_Balbina, na.rm = TRUE),
                   Reg_STM = mean(data$Reg_STM, na.rm = TRUE),
                   Reg_PGM = mean(data$Reg_PGM, na.rm = TRUE))
  colnames(nd)[1] <- predictor

  preds <- stats::predict(fit_lm, newdata = nd, interval = "confidence")
  data.frame(x = grid_x, y = preds[, "fit"], lwr = preds[, "lwr"], upr = preds[, "upr"])
}

#' Journal-style complete 4-sided panel box theme with no interior gridlines
theme_boxed <- function(base_size = 11) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      panel.grid   = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.6),
      plot.margin  = ggplot2::margin(5.5, 9, 5.5, 5.5),
      plot.tag     = ggplot2::element_text(face = "bold", size = 12)
    )
}

#' Compact boxed theme for 2x2 faceted subpanels nested inside a multi-panel grid cell
#' Uses the same base_size (11 pt) as theme_boxed() so axis titles are identical everywhere
theme_boxed_facet <- function(base_size = 11) {
  theme_boxed(base_size = base_size) +
    ggplot2::theme(
      panel.border     = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.5),
      strip.background = ggplot2::element_rect(fill = "grey94", color = "black", linewidth = 0.5),
      strip.text       = ggplot2::element_text(size = 8, face = "bold",
                                               margin = ggplot2::margin(2, 3, 2, 3)),
      panel.spacing.x  = ggplot2::unit(7, "pt"),
      panel.spacing.y  = ggplot2::unit(4, "pt"),
      axis.text        = ggplot2::element_text(size = 7.2)
    )
}

#' Named NPG color vector aligned to REGIONS ("Jurua", "Balbina", "STM", "PGM")
REGION_PAL <- stats::setNames(ggsci::pal_npg("nrc")(length(REGIONS)), REGIONS)

#' facet_wrap(~ Region) with strip banners colored by REGION_PAL while preserving native patchwork alignment
facet_wrap_region <- function(facets = ~ Region, ncol = 2, scales = "fixed", strip_alpha = 0.75) {
  strip_fills <- stats::setNames(scales::alpha(unname(REGION_PAL[REGIONS]), strip_alpha), REGIONS)
  find_label <- function(g) {
    if (!is.null(g$label) && is.character(g$label) && length(g$label) == 1L) return(g$label)
    for (k in c(g$grobs, g$children)) {
      res <- find_label(k)
      if (!is.null(res)) return(res)
    }
    NULL
  }
  base_facet <- ggplot2::facet_wrap(facets, ncol = ncol, scales = scales)
  ggplot2::ggproto(
    NULL, base_facet,
    draw_panels = function(self, panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
      gt <- ggplot2::ggproto_parent(base_facet, self)$draw_panels(
        panels, layout, x_scales, y_scales, ranges, coord, data, theme, params
      )
      for (i in grep("^strip-", gt$layout$name)) {
        sg <- gt$grobs[[i]]
        if (inherits(sg, "zeroGrob")) next
        lbl <- find_label(sg)
        if (!is.null(lbl)) {
          key <- if (lbl %in% names(strip_fills)) lbl else sub("[ .:(].*$", "", lbl)
          if (key %in% names(strip_fills)) {
            sg$grobs[[1]]$children[[1]]$gp$fill <- strip_fills[[key]]
            gt$grobs[[i]] <- sg
          }
        }
      }
      gt
    }
  )
}

#' Robustly extract the non-empty legend grob from a ggplot (compatible with ggplot2 >= 3.5)
extract_legend <- function(plot) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  g <- ggplot2::ggplotGrob(plot)
  idx <- grep("guide-box", vapply(g$grobs, `[[`, character(1), "name"))
  for (i in idx) {
    if (!inherits(g$grobs[[i]], "zeroGrob")) return(g$grobs[[i]])
  }
  NULL
}

#' Build a standalone Region (or categorical) NPG legend grob for right or bottom placement
make_region_legend <- function(data, position = c("right", "bottom"), color_var = "Region",
                               title = "Region", base_size = 11) {
  position <- match.arg(position)
  p <- ggplot2::ggplot(data, ggplot2::aes(x = 1, y = 1, color = .data[[color_var]])) +
    ggplot2::geom_point(size = 2.2) +
    theme_boxed(base_size = base_size) +
    ggsci::scale_color_npg(name = title) +
    ggplot2::theme(legend.position = position)
  extract_legend(p)
}

#' Standard publication point size across all scatter, boxplot-jitter, and mediation panels
POINT_SIZE <- 0.75

#' Boxed scatter panel without a fitted trend line, supporting shared limits and outer-only axes
scatter_panel <- function(data, x, y, color_var = "Region", xlab = NULL, ylab = NULL,
                          show_x = TRUE, show_y = TRUE, xlim = NULL, ylim = NULL,
                          point_size = POINT_SIZE, alpha = 0.8, base_size = 11,
                          show_legend = FALSE, color_name = ggplot2::waiver(), extra = list()) {
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[y]], color = .data[[color_var]])) +
    ggplot2::geom_point(alpha = alpha, size = point_size) +
    theme_boxed(base_size = base_size) +
    ggplot2::labs(x = if (show_x) xlab else NULL, y = if (show_y) ylab else NULL) +
    ggsci::scale_color_npg(name = color_name)
  if (!is.null(xlim)) p <- p + ggplot2::scale_x_continuous(limits = xlim)
  if (!is.null(ylim)) p <- p + ggplot2::scale_y_continuous(limits = ylim)
  for (layer in extra) p <- p + layer
  if (!show_legend) p <- p + ggplot2::theme(legend.position = "none")
  if (!show_x) p <- p + ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                                       axis.text.x  = ggplot2::element_blank())
  if (!show_y) p <- p + ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                                       axis.text.y  = ggplot2::element_blank())
  p
}

#' Boxed regional boxplot + jitter panel in the NPG palette
region_boxplot_panel <- function(data, y, ylab, x = "Region", xlab = "Region",
                                 ylim = NULL, hline = NULL, show_y = TRUE,
                                 point_size = POINT_SIZE, seed = 1, base_size = 11, extra = list()) {
  if (!is.null(seed)) set.seed(seed)
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[y]], fill = .data[[x]]))
  if (!is.null(hline)) {
    p <- p + ggplot2::geom_hline(yintercept = hline, linetype = "dotted",
                                 linewidth = 0.6, color = "black")
  }
  p <- p +
    ggplot2::geom_boxplot(width = 0.55, alpha = 0.75, outlier.shape = NA,
                          color = "black", linewidth = 0.45) +
    ggplot2::geom_jitter(ggplot2::aes(color = .data[[x]]), width = 0.13, height = 0,
                         size = point_size, alpha = 0.8, show.legend = FALSE) +
    theme_boxed(base_size = base_size) +
    ggsci::scale_fill_npg(name = xlab) +
    ggsci::scale_color_npg() +
    ggplot2::labs(x = xlab, y = if (show_y) ylab else NULL) +
    ggplot2::theme(legend.position = "none")
  if (!is.null(ylim)) p <- p + ggplot2::scale_y_continuous(limits = ylim)
  for (layer in extra) p <- p + layer
  if (!show_y) p <- p + ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                                       axis.text.y  = ggplot2::element_blank())
  p
}

#' Boxed Observed vs. Predicted EEoS biomass panel in natural-log units ln(B) with 1:1 line and ±1 CV bars
eeos_obs_pred_panel <- function(data, x = "B_pred", y = "B", sigma_col = "sigma_allom",
                                color_var = "Region",
                                xlab = "Predicted biomass, ln(B)",
                                ylab = "Observed biomass, ln(B)",
                                point_size = POINT_SIZE, base_size = 11, show_legend = FALSE) {
  d <- dplyr::mutate(
    data,
    .lx  = log(.data[[x]]),
    .ly  = log(.data[[y]]),
    .lwr = .ly - .data[[sigma_col]],
    .upr = .ly + .data[[sigma_col]]
  )
  lims <- range(c(d$.lx, d$.lwr, d$.upr), na.rm = TRUE)

  p <- ggplot2::ggplot(d, ggplot2::aes(x = .lx, y = .ly, color = .data[[color_var]])) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dotted",
                         linewidth = 0.6, color = "black") +
    ggplot2::geom_linerange(ggplot2::aes(ymin = .lwr, ymax = .upr),
                            alpha = 0.4, linewidth = 0.32) +
    ggplot2::geom_point(alpha = 0.85, size = point_size) +
    ggplot2::scale_x_continuous(limits = lims) +
    ggplot2::scale_y_continuous(limits = lims) +
    theme_boxed(base_size = base_size) +
    ggsci::scale_color_npg(name = color_var) +
    ggplot2::labs(x = xlab, y = ylab)
  if (!show_legend) p <- p + ggplot2::theme(legend.position = "none")
  p
}

#' Pairwise lower-triangle scatterplot matrix of 4 variables with outer-edge-only axis labels
pairwise_lower_triangle <- function(data, vars, labels = vars, color_var = "Region",
                                    legend_title = color_var, point_size = POINT_SIZE,
                                    base_size = 11) {
  stopifnot(length(vars) == 4, length(labels) == 4, all(vars %in% names(data)))
  names(labels) <- vars
  var_lims <- stats::setNames(lapply(vars, \(v) range(data[[v]], na.rm = TRUE)), vars)

  cell <- function(x, y, show_x = FALSE, show_y = FALSE) {
    scatter_panel(
      data, x = x, y = y, color_var = color_var,
      xlab = labels[[x]], ylab = labels[[y]],
      show_x = show_x, show_y = show_y,
      xlim = var_lims[[x]], ylim = var_lims[[y]],
      point_size = point_size, alpha = 0.8, base_size = base_size
    )
  }

  legend_grob <- make_region_legend(
    data, position = "right", color_var = color_var,
    title = legend_title, base_size = base_size
  )

  cell(vars[1], vars[2], show_x = FALSE, show_y = TRUE) +
    patchwork::plot_spacer() +
    patchwork::plot_spacer() +
    cell(vars[1], vars[3], show_x = FALSE, show_y = TRUE) +
    cell(vars[2], vars[3], show_x = FALSE, show_y = FALSE) +
    patchwork::wrap_elements(full = legend_grob) +
    cell(vars[1], vars[4], show_x = TRUE,  show_y = TRUE) +
    cell(vars[2], vars[4], show_x = TRUE,  show_y = FALSE) +
    cell(vars[3], vars[4], show_x = TRUE,  show_y = FALSE) +
    patchwork::plot_layout(ncol = 3)
}

#' One scatter panel with the marginal common-slope line and CI ribbon
#' @param extra list of extra ggplot layers (scales, reference lines, themes)
mediation_panel <- function(data, x, y, color_var, xlab, ylab, extra = list(),
                            point_size = POINT_SIZE, show_legend = TRUE, color_name = ggplot2::waiver()) {
  marg <- get_marginal_predictions(data, outcome = y, predictor = x)
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[y]], color = .data[[color_var]])) +
    ggplot2::geom_ribbon(data = marg, ggplot2::aes(x = x, ymin = lwr, ymax = upr),
                         fill = "black", alpha = 0.15, inherit.aes = FALSE) +
    ggplot2::geom_line(data = marg, ggplot2::aes(x = x, y = y),
                       color = "black", linewidth = 0.8, inherit.aes = FALSE) +
    ggplot2::geom_point(alpha = 0.8, size = point_size) +
    theme_boxed() +
    ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
    ggsci::scale_color_npg(name = color_name)
  for (layer in extra) p <- p + layer
  if (!show_legend) p <- p + ggplot2::theme(legend.position = "none")
  p
}

#' Two-panel mediation figure: A = x -> m, B = m -> y
plot_mediation_panels <- function(data, x, m, y, color_var, labels, extra_a = list(), extra_b = list(),
                                  color_name = ggplot2::waiver()) {
  pa <- mediation_panel(data, x, m, color_var, labels$a_x, labels$a_y, extra_a, show_legend = FALSE)
  pb <- mediation_panel(data, m, y, color_var, labels$b_x, labels$b_y, extra_b, color_name = color_name)
  cowplot::plot_grid(pa, pb, labels = c("A", "B"), rel_widths = c(0.82, 1))
}

#' Save Figures/<stem>.png with consistent publication defaults
save_figure <- function(stem, plot, ..., dpi = 600, bg = "white") {
  ggplot2::ggsave(file.path("Figures", paste0(stem, ".png")), plot = plot, dpi = dpi, bg = bg, ...)
  invisible(plot)
}

#' Regional map of plot locations (Figure 1A)
make_region_map <- function(env_file = "Data/env.csv", eco_shp = "Data/Ecoregions2017/Ecoregions2017.shp") {
  summary_df <- read_required_csv(env_file) |>
    dplyr::filter(Biome == "Amazonia") |>
    dplyr::group_by(Region) |>
    dplyr::summarise(n = dplyr::n(), lon = mean(X, na.rm = TRUE), lat = mean(Y, na.rm = TRUE),
                     fc2000_mean = mean(fc2000, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(fc2000_mean)) |>
    tibble::add_row(Region = "Jurua", n = JURUA_CENTROID$n, lon = JURUA_CENTROID$lon,
                    lat = JURUA_CENTROID$lat, fc2000_mean = 1) |>
    dplyr::mutate(Region = factor(Region, levels = REGIONS))

  south_america <- terra::vect(rnaturalearth::ne_countries(continent = "South America", returnclass = "sf"))
  eco <- terra::vect(eco_shp)
  rainforest <- eco[eco$BIOME_NAME == "Tropical & Subtropical Moist Broadleaf Forests" &
                      eco$REALM == "Neotropic"] |> terra::crop(south_america)
  pts <- terra::vect(as.data.frame(summary_df), geom = c("lon", "lat"), crs = "EPSG:4326")

  ggplot2::ggplot() +
    tidyterra::geom_spatvector(data = rainforest, fill = "grey", color = "grey", size = 0.2) +
    tidyterra::geom_spatvector(data = south_america, fill = NA, color = "black", linewidth = 0.4) +
    tidyterra::geom_spatvector(data = pts, ggplot2::aes(alpha = fc2000_mean, size = n)) +
    tidyterra::geom_spatvector(data = pts, ggplot2::aes(size = n / 2, color = Region)) +
    ggplot2::scale_size(range = c(2.5, 5.5)) +
    ggplot2::geom_text(data = summary_df, ggplot2::aes(x = lon, y = lat, label = n),
                       color = "black", fontface = "bold", size = 2) +
    ggsci::scale_color_npg() +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), axis.text = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(face = "bold")) +
    ggplot2::guides(size = "none", alpha = "none", color = "none")
}

#' Vertical row-title header with right divider bar for multi-row composite figures
make_row_header <- function(title) {
  ggplot2::ggplot() +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0.88, xend = 0.88, y = 0, yend = 1),
      color = "grey30", linewidth = 0.55
    ) +
    ggplot2::annotate(
      "text", x = 0.38, y = 0.5, label = title,
      angle = 90, fontface = "bold", size = 4.3, color = "black"
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(5.5, 3, 5.5, 4))
}

#' Read precomputed Garrido et al. (2022) MSA-SEM tables for one hypothesis stem
read_sem_diagram_stats <- function(stem_prefix, mediator_var, outcome_var) {
  mmi  <- read_required_csv(out_path(paste0(stem_prefix, "_Multimodel_Inference")))
  comp <- read_required_csv(out_path(paste0(stem_prefix, "_Model_Comparison")))
  pe   <- read_required_csv(out_path(paste0(stem_prefix, "_Parameter_Estimates")))

  w_map <- stats::setNames(comp$Akaike_weight, comp$Model)
  r2_df <- pe[pe$op == "r2" & pe$lhs %in% c(mediator_var, outcome_var), c("Model", "lhs", "est")]
  r2_df$w <- w_map[r2_df$Model]
  r2_m <- sum(r2_df$w[r2_df$lhs == mediator_var] * r2_df$est[r2_df$lhs == mediator_var], na.rm = TRUE)
  r2_y <- sum(r2_df$w[r2_df$lhs == outcome_var]  * r2_df$est[r2_df$lhs == outcome_var],  na.rm = TRUE)

  comp_ord <- comp[order(comp$delta_AICc), ]
  cfi_w <- sum(comp_ord$Akaike_weight * comp_ord$CFI, na.rm = TRUE)

  ind_std <- mmi$Avg_std_full[mmi$Parameter == "ind"]
  dir_std <- mmi$Avg_std_full[mmi$Parameter == "c"]
  tot_std <- mmi$Avg_std_full[mmi$Parameter == "tot"]
  prop_mediated <- abs(ind_std) / (abs(ind_std) + abs(dir_std))

  list(
    mmi = mmi, comp = comp_ord,
    r2_m = r2_m, r2_y = r2_y, cfi_w = cfi_w,
    ind_std = ind_std, dir_std = dir_std, tot_std = tot_std,
    prop_mediated = prop_mediated
  )
}

#' Publication-grade Garrido et al. (2022) multimodel-averaged SEM pathway diagram
plot_sem_diagram <- function(sem_stats, x_label, m_label, y_label,
                             ri_cutoff = 0.70, aspect_wh = 1.75) {
  mmi <- sem_stats$mmi
  fmt_signed <- function(v) {
    rv <- round(v, 2)
    if (rv < 0) sprintf("\u2212%.2f", abs(rv)) else if (rv > 0) sprintf("+%.2f", rv) else "0.00"
  }
  get_row <- function(p) mmi[mmi$Parameter == p, , drop = FALSE]

  # Flush top/bottom alignment with adjacent theme_boxed() panels ([0, 100] = panel height)
  box_poly <- function(id, xmin, xmax, ymin, ymax, fill) {
    data.frame(
      id   = id,
      x    = c(xmin, xmax, xmax, xmin),
      y    = c(ymin, ymin, ymax, ymax),
      fill = fill,
      stringsAsFactors = FALSE
    )
  }
  boxes <- rbind(
    box_poly("X",  1.5, 36.5,  1.5, 26.0, "white"),
    box_poly("M", 26.5, 73.5, 75.0, 99.5, "grey96"),
    box_poly("Y", 63.5, 98.5,  1.5, 26.0, "grey96")
  )

  # Separate shaft from proportional solid closed arrowhead so thick/dashed shafts terminate cleanly
  make_edge <- function(param, x0, y0, x1, y1) {
    r <- get_row(param)
    b_std <- r$Avg_std_full
    se_std <- r$Uncond_SE_std_full
    ri <- r$Relative_Importance
    supported <- !is.na(ri) && ri >= ri_cutoff
    lw <- 0.55 + 2.35 * pmin(abs(b_std), 0.70)
    head_mm <- 2.15 + 0.58 * lw

    dx_phys <- (x1 - x0) * aspect_wh
    dy_phys <- (y1 - y0)
    len_phys <- sqrt(dx_phys^2 + dy_phys^2)
    trim_frac <- min((0.95 * head_mm) / len_phys, 0.28)
    head_frac <- min((1.30 * head_mm) / len_phys, 0.35)

    data.frame(
      param     = param,
      x0        = x0,
      y0        = y0,
      xs_end    = x1 - (x1 - x0) * trim_frac,
      ys_end    = y1 - (y1 - y0) * trim_frac,
      xh_start  = x1 - (x1 - x0) * head_frac,
      yh_start  = y1 - (y1 - y0) * head_frac,
      x1        = x1,
      y1        = y1,
      lw        = lw,
      head_mm   = head_mm,
      col       = if (supported) "#1A1A1A" else "#767676",
      lty       = if (supported) "solid" else "32",
      lbl_main  = sprintf("%s: %s \u00b1 %.2f", param, fmt_signed(b_std), round(se_std, 2)),
      lbl_ri    = sprintf("RI = %.2f", ri),
      supported = supported,
      stringsAsFactors = FALSE
    )
  }

  edges <- rbind(
    make_edge("a", 23.8, 28.2, 40.2, 72.8),
    make_edge("b", 59.8, 72.8, 76.2, 28.2),
    make_edge("c", 38.3, 13.8, 61.7, 13.8)
  )

  m_is_multiline <- grepl("\n", m_label)
  y_is_multiline <- grepl("\n", y_label)

  p <- ggplot2::ggplot() +
    ggforce::geom_shape(
      data = boxes,
      ggplot2::aes(x = x, y = y, group = id, fill = fill),
      radius = grid::unit(3.2, "pt"),
      color = "black", linewidth = 0.55
    ) +
    ggplot2::scale_fill_identity()

  for (i in seq_len(nrow(edges))) {
    e <- edges[i, ]
    p <- p +
      ggplot2::geom_segment(
        data = e,
        ggplot2::aes(x = x0, y = y0, xend = xs_end, yend = ys_end),
        color = e$col, linewidth = e$lw, linetype = e$lty, lineend = "butt"
      ) +
      ggplot2::geom_segment(
        data = e,
        ggplot2::aes(x = xh_start, y = yh_start, xend = x1, yend = y1),
        color = e$col, linewidth = 0.45, linetype = "solid", linejoin = "mitre",
        arrow = grid::arrow(length = grid::unit(e$head_mm, "mm"), type = "closed", angle = 24)
      )
  }

  ea <- edges[edges$param == "a", ]
  eb <- edges[edges$param == "b", ]
  ec <- edges[edges$param == "c", ]

  p +
    # Node titles & model-averaged R^2
    ggplot2::annotate(
      "text", x = 19.0, y = 13.8, label = x_label,
      fontface = "bold", size = 3.25, lineheight = 0.94, color = "black"
    ) +
    ggplot2::annotate(
      "text", x = 50.0, y = if (m_is_multiline) 90.3 else 90.8, label = m_label,
      fontface = "bold", size = 3.25, lineheight = 0.94, color = "black"
    ) +
    ggplot2::annotate(
      "text", x = 50.0, y = if (m_is_multiline) 79.2 else 80.8,
      label = sprintf("R\u00b2 = %.2f", sem_stats$r2_m),
      size = 2.85, color = "grey28"
    ) +
    ggplot2::annotate(
      "text", x = 81.0, y = if (y_is_multiline) 16.8 else 17.3, label = y_label,
      fontface = "bold", size = 3.25, lineheight = 0.94, color = "black"
    ) +
    ggplot2::annotate(
      "text", x = 81.0, y = if (y_is_multiline) 5.7 else 7.3,
      label = sprintf("R\u00b2 = %.2f", sem_stats$r2_y),
      size = 2.85, color = "grey28"
    ) +
    # Path a label (outer-left of X -> M shaft)
    ggplot2::annotate(
      "text", x = 28.0, y = 54.0, label = ea$lbl_main,
      hjust = 1, fontface = if (ea$supported) "bold" else "plain",
      size = 2.95, color = ea$col
    ) +
    ggplot2::annotate(
      "text", x = 28.0, y = 48.2, label = ea$lbl_ri,
      hjust = 1, size = 2.75, color = "grey30"
    ) +
    # Path b label (outer-right of M -> Y shaft)
    ggplot2::annotate(
      "text", x = 72.0, y = 54.0, label = eb$lbl_main,
      hjust = 0, fontface = if (eb$supported) "bold" else "plain",
      size = 2.95, color = eb$col
    ) +
    ggplot2::annotate(
      "text", x = 72.0, y = 48.2, label = eb$lbl_ri,
      hjust = 0, size = 2.75, color = "grey30"
    ) +
    # Path c label (centered cleanly above X -> Y shaft)
    ggplot2::annotate(
      "text", x = 50.0, y = 24.4, label = ec$lbl_main,
      hjust = 0.5, fontface = if (ec$supported) "bold" else "plain",
      size = 2.95, color = ec$col
    ) +
    ggplot2::annotate(
      "text", x = 50.0, y = 18.8, label = ec$lbl_ri,
      hjust = 0.5, size = 2.75, color = "grey30"
    ) +
    # Single goodness-of-fit metric aligned with adjacent x-axis title baseline
    ggplot2::annotate(
      "text", x = 50.0, y = -7.2,
      label = sprintf("Weighted CFI = %.2f", sem_stats$cfi_w),
      size = 3.15, color = "grey20"
    ) +
    ggplot2::coord_cartesian(xlim = c(0, 100), ylim = c(0, 100), expand = FALSE, clip = "off") +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.margin = ggplot2::margin(5.5, 9, 5.5, 5.5),
      plot.tag    = ggplot2::element_text(face = "bold", size = 12)
    )
}

#' Streamlined boxed stacked-bar panel where the bar completely fills the panel box
plot_proportion_mediated <- function(sem_stats, ylab = "Proportion of total effect",
                                     base_size = 11) {
  p_med <- sem_stats$prop_mediated

  bar_df <- data.frame(
    component = factor(c("Direct", "Mediated"), levels = c("Direct", "Mediated")),
    ymin      = c(p_med, 0),
    ymax      = c(1, p_med),
    fill      = c("grey90", "#262626"),
    stringsAsFactors = FALSE
  )

  ggplot2::ggplot(bar_df) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = 0, xmax = 1, ymin = ymin, ymax = ymax, fill = fill),
      color = NA
    ) +
    ggplot2::geom_hline(yintercept = p_med, color = "black", linewidth = 0.55) +
    ggplot2::annotate(
      "text",
      x        = 0.5,
      y        = p_med / 2,
      label    = sprintf("%.2f", round(p_med, 2)),
      color    = "white",
      fontface = "bold",
      size     = 3.0
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.25),
      labels = c("0.00", "0.25", "0.50", "0.75", "1.00"),
      expand = c(0, 0)
    ) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    ggplot2::labs(x = NULL, y = ylab) +
    theme_boxed(base_size = base_size) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x  = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      plot.margin  = ggplot2::margin(5.5, 22, 5.5, 5.5)
    )
}

#' Compact horizontal key explaining SEM path relative importance and width encoding
make_sem_path_legend <- function() {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 7.5, y = 0.5, label = "SEM paths:",
                      hjust = 0, fontface = "bold", size = 3.3, color = "black") +
    ggplot2::geom_segment(ggplot2::aes(x = 25.5, xend = 33.3, y = 0.5, yend = 0.5),
                          color = "#1A1A1A", linewidth = 1.15) +
    ggplot2::geom_segment(ggplot2::aes(x = 31.7, xend = 34.7, y = 0.5, yend = 0.5),
                          color = "#1A1A1A", linewidth = 0.45,
                          arrow = grid::arrow(length = grid::unit(2.2, "mm"), type = "closed", angle = 24)) +
    ggplot2::annotate("text", x = 36.2, y = 0.5, label = "RI \u2265 0.70",
                      hjust = 0, size = 3.1, color = "black") +
    ggplot2::geom_segment(ggplot2::aes(x = 52.5, xend = 60.3, y = 0.5, yend = 0.5),
                          color = "#767676", linewidth = 0.9, linetype = "32") +
    ggplot2::geom_segment(ggplot2::aes(x = 58.7, xend = 61.7, y = 0.5, yend = 0.5),
                          color = "#767676", linewidth = 0.45,
                          arrow = grid::arrow(length = grid::unit(2.1, "mm"), type = "closed", angle = 24)) +
    ggplot2::annotate("text", x = 63.2, y = 0.5, label = "RI < 0.70   (width \u221d |\u03b2|)",
                      hjust = 0, size = 3.1, color = "black") +
    ggplot2::coord_cartesian(xlim = c(0, 100), ylim = c(0, 1), expand = FALSE, clip = "off") +
    ggplot2::theme_void()
}

#' Self-sized legend grob for the Mediated vs. Direct effect-partition bar column
make_mediation_bar_legend <- function() {
  dummy_df <- data.frame(
    x    = c(1, 2),
    y    = c(1, 1),
    comp = factor(c("Mediated", "Direct"), levels = c("Mediated", "Direct"))
  )
  p <- ggplot2::ggplot(dummy_df, ggplot2::aes(x = x, y = y, fill = comp)) +
    ggplot2::geom_col(color = "black", linewidth = 0.45, width = 0.7) +
    ggplot2::scale_fill_manual(
      name   = NULL,
      values = c(Mediated = "#262626", Direct = "grey90")
    ) +
    theme_boxed(base_size = 10) +
    ggplot2::theme(
      legend.position  = "bottom",
      legend.direction = "horizontal",
      legend.text      = ggplot2::element_text(size = 9.0, margin = ggplot2::margin(l = 3, r = 7)),
      legend.key.size  = grid::unit(0.36, "cm"),
      legend.spacing.x = grid::unit(0.10, "cm"),
      legend.margin    = ggplot2::margin(0, 0, 0, 0)
    )
  extract_legend(p)
}


