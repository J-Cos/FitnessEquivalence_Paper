# --------------------------------------------
# Purpose: Shared plotting helpers: marginal common-slope lines, mediation figure panels,
#          figure saving, and the regional map (Figure 1A).
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

#' One scatter panel with the marginal common-slope line and CI ribbon
#' @param scales list of extra ggplot layers (scales, reference lines, themes)
mediation_panel <- function(data, x, y, color_var, xlab, ylab, extra = list(),
                            point_size = 1, show_legend = TRUE, color_name = ggplot2::waiver()) {
  marg <- get_marginal_predictions(data, outcome = y, predictor = x)
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[y]], color = .data[[color_var]])) +
    ggplot2::geom_ribbon(data = marg, ggplot2::aes(x = x, ymin = lwr, ymax = upr),
                         fill = "black", alpha = 0.15, inherit.aes = FALSE) +
    ggplot2::geom_line(data = marg, ggplot2::aes(x = x, y = y),
                       color = "black", linewidth = 0.8, inherit.aes = FALSE) +
    ggplot2::geom_point(alpha = 0.8, size = point_size) +
    ggplot2::theme_classic() +
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

#' Save Figures/<stem>_<suffix>.png, plus the canonical Figures/<stem>.png for the main scaling
save_figure <- function(stem, suffix, plot, ...) {
  ggplot2::ggsave(file.path("Figures", paste0(stem, "_", suffix, ".png")), plot = plot, ...)
  if (identical(suffix, MAIN_SUFFIX)) ggplot2::ggsave(file.path("Figures", paste0(stem, ".png")), plot = plot, ...)
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
