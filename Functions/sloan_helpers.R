# --------------------------------------------
# Purpose: Neutral metacommunity (Sloan) models and non-neutral taxa classification (H2b, H2c).
# sncm_fit() is modified from Burns et al. (2016), "Contribution of neutral processes to the
# assembly of gut microbial communities in the zebrafish over host development".
# It fits the Sloan et al. (2006) neutral model plus binomial and Poisson random-sampling models.
#   spp:  community matrix, local communities (plots) as rows, taxa as columns
#   pool: optional matrix defining the source community (default: spp itself)
#   taxon: optional table of taxonomic calls with taxa as row names
# Returns list(stats = one-row data.frame of fit statistics, vals = per-taxon observed/predicted)
# ---------------------------------------------------

sncm_fit <- function(spp, pool = NULL, taxon = NULL) {
  requireNamespace("minpack.lm"); requireNamespace("Hmisc"); requireNamespace("stats4")

  # Number of individuals per community
  N <- mean(apply(spp, 1, sum))

  # Average relative abundance of each taxon across communities
  p.m <- apply(if (is.null(pool)) spp else pool, 2, mean)
  p.m <- p.m[p.m != 0]
  p <- p.m / N

  # Occurrence frequency of each taxon across communities
  spp.bi <- 1 * (spp > 0)
  freq <- apply(spp.bi, 2, mean)
  freq <- freq[freq != 0]

  # Combine; remove taxa absent in either source pool or local communities
  C <- merge(p, freq, by = 0)
  C <- C[order(C[, 2]), ]
  C <- as.data.frame(C)
  C.0 <- C[!(apply(C, 1, function(y) any(y == 0))), ]
  p <- C.0[, 2]
  freq <- C.0[, 3]
  names(p) <- C.0[, 1]
  names(freq) <- C.0[, 1]

  # Limit of detection
  d <- 1 / N

  # Sloan model: fit m by non-linear least squares
  m.fit <- suppressWarnings(minpack.lm::nlsLM(freq ~ pbeta(d, N * m * p, N * m * (1 - p), lower.tail = FALSE),
                                              start = list(m = 0.1)))
  m.ci <- suppressWarnings(suppressMessages(confint(m.fit, "m", level = 0.95)))

  # Sloan model: fit m by maximum likelihood (Gaussian residuals)
  sncm.LL <- function(m, sigma) {
    R <- freq - pbeta(d, N * m * p, N * m * (1 - p), lower.tail = FALSE)
    R <- dnorm(R, 0, sigma)
    -sum(log(R))
  }
  m.mle <- suppressWarnings(try(stats4::mle(sncm.LL, start = list(m = 0.1, sigma = 0.1),
                                            nobs = length(p), method = "Nelder-Mead"), silent = TRUE))
  if (inherits(m.mle, "try-error")) {
    message("Trying alternative maximum likelihood estimation algorithm from bbmle package")
    m.mle <- suppressWarnings(bbmle::mle2(sncm.LL, start = list(m = 0.1, sigma = 0.1), method = "Nelder-Mead"))
  }
  aic.fit <- AIC(m.mle, k = 2)
  bic.fit <- BIC(m.mle)

  # Sloan model goodness of fit
  freq.pred <- pbeta(d, N * coef(m.fit) * p, N * coef(m.fit) * (1 - p), lower.tail = FALSE)
  Rsqr <- 1 - (sum((freq - freq.pred)^2)) / (sum((freq - mean(freq))^2))
  RMSE <- sqrt(sum((freq - freq.pred)^2) / (length(freq) - 1))
  pred.ci <- Hmisc::binconf(freq.pred * nrow(spp), nrow(spp), alpha = 0.05, method = "wilson", return.df = TRUE)

  # Binomial random-sampling model
  bino.LL <- function(mu, sigma) {
    R <- freq - pbinom(d, floor(N), p, lower.tail = FALSE)
    R <- dnorm(R, mu, sigma)
    -sum(log(R))
  }
  bino.mle <- suppressWarnings(stats4::mle(bino.LL, start = list(mu = 0, sigma = 0.1), nobs = length(p)))
  aic.bino <- AIC(bino.mle, k = 2)
  bic.bino <- BIC(bino.mle)
  bino.pred <- pbinom(d, floor(N), p, lower.tail = FALSE)
  Rsqr.bino <- 1 - (sum((freq - bino.pred)^2)) / (sum((freq - mean(freq))^2))
  RMSE.bino <- sqrt(sum((freq - bino.pred)^2) / (length(freq) - 1))
  bino.pred.ci <- Hmisc::binconf(bino.pred * nrow(spp), nrow(spp), alpha = 0.05, method = "wilson", return.df = TRUE)

  # Poisson random-sampling model
  pois.LL <- function(mu, sigma) {
    R <- freq - ppois(d, N * p, lower.tail = FALSE)
    R <- dnorm(R, mu, sigma)
    -sum(log(R))
  }
  pois.mle <- suppressWarnings(stats4::mle(pois.LL, start = list(mu = 0, sigma = 0.1), nobs = length(p)))
  aic.pois <- AIC(pois.mle, k = 2)
  bic.pois <- BIC(pois.mle)
  pois.pred <- ppois(d, N * p, lower.tail = FALSE)
  Rsqr.pois <- 1 - (sum((freq - pois.pred)^2)) / (sum((freq - mean(freq))^2))
  RMSE.pois <- sqrt(sum((freq - pois.pred)^2) / (length(freq) - 1))
  pois.pred.ci <- Hmisc::binconf(pois.pred * nrow(spp), nrow(spp), alpha = 0.05, method = "wilson", return.df = TRUE)

  # Results
  fitstats <- data.frame(m = numeric(), m.ci = numeric(), m.mle = numeric(), maxLL = numeric(),
                         binoLL = numeric(), poisLL = numeric(), Rsqr = numeric(), Rsqr.bino = numeric(),
                         Rsqr.pois = numeric(), RMSE = numeric(), RMSE.bino = numeric(), RMSE.pois = numeric(),
                         AIC = numeric(), BIC = numeric(), AIC.bino = numeric(), BIC.bino = numeric(),
                         AIC.pois = numeric(), BIC.pois = numeric(), N = numeric(), Samples = numeric(),
                         Richness = numeric(), Detect = numeric())
  fitstats[1, ] <- c(coef(m.fit), coef(m.fit) - m.ci[1], m.mle@coef["m"], m.mle@details$value,
                     bino.mle@details$value, pois.mle@details$value, Rsqr, Rsqr.bino, Rsqr.pois,
                     RMSE, RMSE.bino, RMSE.pois, aic.fit, bic.fit, aic.bino, bic.bino, aic.pois, bic.pois,
                     N, nrow(spp), length(p), d)

  A <- as.data.frame(cbind(p, freq, freq.pred, pred.ci[, 2:3], bino.pred, bino.pred.ci[, 2:3],
                           pois.pred, pois.pred.ci[, 2:3]))
  colnames(A) <- c("p", "freq", "freq.pred", "pred.lwr", "pred.upr", "bino.pred", "bino.lwr", "bino.upr",
                   "pois.pred", "pois.lwr", "pois.upr")
  if (is.null(taxon)) {
    B <- A[order(A[, 1]), ]
  } else {
    B <- merge(A, taxon, by = 0, all = TRUE)
    row.names(B) <- B[, 1]
    B <- B[, -1]
    B <- B[order(B[, 1]), ]
  }
  list(stats = fitstats, vals = B)
}
sncm.fit <- sncm_fit  # backwards-compatible alias

#' Plot x taxon abundance matrix (rows = plots, in the order of `plots`) from stem records
build_community_matrix <- function(comm, plots) {
  wide <- comm |>
    dplyr::filter(Region_Plot %in% plots) |>
    dplyr::count(Region_Plot, Binomial_correct, name = "value") |>
    tidyr::pivot_wider(names_from = Binomial_correct, values_from = value, values_fill = 0)
  m <- as.matrix(dplyr::select(wide, -Region_Plot))
  rownames(m) <- wide$Region_Plot
  m
}

#' One-row summary of a sncm_fit result
extract_sloan_summary <- function(fit) {
  s <- fit$stats
  tibble::tibble(r2 = s$Rsqr, r2binom = s$Rsqr.bino, r2pois = s$Rsqr.pois,
                 dAIC_v_binom = s$AIC - s$AIC.bino, dAIC_v_pois = s$AIC - s$AIC.pois,
                 dAIC_pois_v_binom = s$AIC.pois - s$AIC.bino,
                 m = s$m)
}

#' Per-taxon neutrality class from the Poisson random-sampling 95% interval
#' LFHA = low frequency for its abundance; HFLA = high frequency for its abundance
classify_neutrality <- function(fit) {
  v <- fit$vals
  tibble::tibble(sp = rownames(v), freq = v$freq, lwr = v$pois.lwr, upr = v$pois.upr) |>
    dplyr::mutate(Neutrality = dplyr::case_when(freq < lwr ~ "LFHA",
                                                freq > upr ~ "HFLA",
                                                TRUE ~ "Neutral")) |>
    dplyr::select(sp, Neutrality) |>
    dplyr::arrange(sp)
}

#' Observed occurrence frequency vs relative abundance (log10) with Poisson prediction and 95% interval
plot_sloan_fit <- function(fit, title = NULL) {
  v <- fit$vals
  p_grid <- exp(seq(min(log(v$p)), max(log(v$p)), length.out = 160))
  pred   <- stats::ppois(fit$stats$Detect, fit$stats$N * p_grid, lower.tail = FALSE)
  ci     <- Hmisc::binconf(pred * fit$stats$Samples, fit$stats$Samples,
                           alpha = 0.05, method = "wilson", return.df = TRUE)
  curve_df <- tibble::tibble(p = p_grid, pois.pred = pred,
                             pois.lwr = ci[, "Lower"], pois.upr = ci[, "Upper"])

  ggplot2::ggplot(v, ggplot2::aes(x = p, y = freq)) +
    ggplot2::geom_point(color = "grey68", alpha = 1.0, size = 0.7) +
    ggplot2::geom_line(data = curve_df, ggplot2::aes(y = pois.pred), color = "black", linewidth = 0.65) +
    ggplot2::geom_line(data = curve_df, ggplot2::aes(y = pois.lwr),  color = "black", linetype = "dashed", linewidth = 0.42) +
    ggplot2::geom_line(data = curve_df, ggplot2::aes(y = pois.upr),  color = "black", linetype = "dashed", linewidth = 0.42) +
    ggplot2::annotate("text", x = min(v$p), y = Inf, hjust = 0, vjust = 1.35, size = 3.0,
                      label = sprintf("R² = %.2f", fit$stats$Rsqr.pois)) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_continuous(limits = c(0, 1.03), breaks = c(0, 0.5, 1)) +
    ggplot2::labs(x = "Relative abundance", y = "Occurrence frequency", title = title) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      panel.grid   = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.6),
      plot.title   = ggplot2::element_text(size = 10, face = "bold")
    )
}

#' Non-neutral taxa per region from replicate classifications
#' A taxon is non-neutral in a region if it falls in the same non-neutral class (LFHA or HFLA)
#' in every replicate subsample in which it occurs.
#' @param taxa_long tibble(region, seed, sp, Neutrality)
#' @return list(long = tibble(Region, Binomial_correct, Neutrality), wide = tibble(sp, <region 0/1 columns>))
classify_nonneutral_taxa <- function(taxa_long, regions = REGIONS) {
  long <- taxa_long |>
    dplyr::count(region, sp, Neutrality, name = "n") |>
    dplyr::group_by(region, sp) |>
    dplyr::mutate(n_present = sum(n)) |>
    dplyr::ungroup() |>
    dplyr::filter(Neutrality != "Neutral", n == n_present) |>
    dplyr::transmute(Region = region, Binomial_correct = sp, Neutrality)
  stopifnot(!anyDuplicated(long[, c("Region", "Binomial_correct")]))

  wide <- long |>
    dplyr::mutate(flag = 1) |>
    dplyr::select(sp = Binomial_correct, Region, flag) |>
    tidyr::pivot_wider(names_from = Region, values_from = flag, values_fill = 0)
  for (r in setdiff(regions, names(wide))) wide[[r]] <- 0
  wide <- dplyr::select(wide, sp, dplyr::all_of(regions)) |> dplyr::arrange(sp)
  list(long = long, wide = wide)
}

#' Per-plot proportion of stems belonging to taxa that are non-neutral (and LFHA / HFLA) in the plot's region
nonneutral_proportions <- function(comm, nn_long) {
  comm |>
    dplyr::left_join(
      dplyr::select(nn_long, Region, Binomial_correct, Neutrality),
      by = c("Region", "Binomial_correct"),
      relationship = "many-to-one"
    ) |>
    dplyr::group_by(Region, Region_Plot) |>
    dplyr::summarise(
      proportionNonNeutral = sum(!is.na(Neutrality)) / dplyr::n(),
      proportionLFHA       = sum(Neutrality == "LFHA", na.rm = TRUE) / dplyr::n(),
      proportionHFLA       = sum(Neutrality == "HFLA", na.rm = TRUE) / dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::select(Region_Plot, proportionNonNeutral, proportionLFHA, proportionHFLA)
}

#' Generate replicate equal-effort random subsamples of `subsample_size` plots per region (H2b)
make_regional_plot_subsamples <- function(comm, n_subsamples = N_SUBSAMPLES,
                                          subsample_size = SUBSAMPLE_SIZE) {
  plots <- comm |>
    dplyr::group_by(Region, Region_Plot) |>
    dplyr::summarise(value = dplyr::n(), .groups = "drop")

  purrr::map(seq_len(n_subsamples), function(i) {
    set.seed(i)
    plots |>
      dplyr::group_by(Region) |>
      dplyr::sample_n(subsample_size) |>
      dplyr::ungroup() |>
      dplyr::pull(Region_Plot)
  })
}

#' Split a vector of plot IDs into non-overlapping consecutive sets of `size` (incomplete tail dropped)
split_into_plot_sets <- function(plot_ids, size = SUBGROUP_SIZE) {
  sets <- split(plot_ids, ceiling(seq_along(plot_ids) / size))
  sets[lengths(sets) == size]
}

#' Generate non-overlapping sets of `subgroup_size` plots ordered by forest cover within region (H2c)
make_ordered_forest_cover_subgroups <- function(comm, env, subgroup_size = SUBGROUP_SIZE) {
  plots <- comm |>
    dplyr::group_by(Region, Region_Plot) |>
    dplyr::summarise(value = dplyr::n(), .groups = "drop")

  ordered_sets <- list()
  for (region in c("Balbina", "PGM", "STM")) {
    ordered_ids <- env |>
      dplyr::filter(Region_Plot %in% plots$Region_Plot, Region == region) |>
      dplyr::arrange(fc2000) |>
      dplyr::pull(Region_Plot)
    ordered_sets[[region]] <- split_into_plot_sets(ordered_ids, size = subgroup_size)
  }
  # Intact Jurua has no forest-cover gradient; sets follow plot ID order
  ordered_sets[["Jurua"]] <- split_into_plot_sets(
    dplyr::filter(plots, Region == "Jurua")$Region_Plot,
    size = subgroup_size
  )
  unlist(ordered_sets, recursive = FALSE)
}

#' Fit sncm_fit() across a named list of plot-ID vectors using the same underlying model routine
fit_sloan_over_plot_sets <- function(comm, plot_sets) {
  purrr::map(plot_sets, \(ids) sncm_fit(build_community_matrix(comm, ids)))
}

#' Region-level summary of replicate neutral-model fits and paired differences from reference region
summarise_regional_sloan_r2 <- function(sloan_stats, regions = REGIONS, ref_region = "Jurua") {
  q025 <- \(x) unname(stats::quantile(x, 0.025))
  q975 <- \(x) unname(stats::quantile(x, 0.975))
  ref_df <- sloan_stats |>
    dplyr::filter(region == ref_region) |>
    dplyr::select(seed, r2pois_ref = r2pois)

  sloan_stats |>
    dplyr::left_join(ref_df, by = "seed") |>
    dplyr::mutate(diff_from_jurua = r2pois - r2pois_ref) |>
    dplyr::group_by(region) |>
    dplyr::summarise(
      n_subsamples                = dplyr::n(),
      r2pois_mean                 = mean(r2pois),
      r2pois_q025                 = q025(r2pois),
      r2pois_q975                 = q975(r2pois),
      diff_from_jurua_mean        = mean(diff_from_jurua),
      diff_from_jurua_q025        = q025(diff_from_jurua),
      diff_from_jurua_q975        = q975(diff_from_jurua),
      prop_subsamples_below_jurua = mean(diff_from_jurua < 0),
      .groups = "drop"
    ) |>
    dplyr::arrange(factor(region, levels = regions))
}

#' Average species-level relative abundance, occurrence frequency, and Poisson prediction across replicates
summarise_regional_average_sloan <- function(vals_long, nn_long) {
  vals_long |>
    dplyr::group_by(Region = region, sp) |>
    dplyr::summarise(
      n_reps    = dplyr::n(),
      p         = mean(p),
      freq      = mean(freq),
      pois.pred = mean(pois.pred),
      pois.lwr  = mean(pois.lwr),
      pois.upr  = mean(pois.upr),
      .groups   = "drop"
    ) |>
    dplyr::left_join(dplyr::rename(nn_long, sp = Binomial_correct), by = c("Region", "sp")) |>
    dplyr::mutate(Neutrality = tidyr::replace_na(Neutrality, "Neutral"))
}

#' Standardize traits within each regional species pool and test non-neutral taxa against 0
compute_nonneutral_trait_tests <- function(comm, traits, non_neutrals,
                                           trait_names = c("WD", "lnSM", "LMA", "Hmax"),
                                           regions = REGIONS) {
  pool_z <- comm |>
    dplyr::distinct(Region, Binomial_correct) |>
    dplyr::left_join(
      dplyr::select(traits, Binomial_correct, dplyr::all_of(trait_names)),
      by = "Binomial_correct",
      relationship = "many-to-one"
    ) |>
    tidyr::pivot_longer(dplyr::all_of(trait_names), names_to = "TraitName", values_to = "value") |>
    dplyr::group_by(Region, TraitName) |>
    dplyr::mutate(TraitScore = (value - mean(value, na.rm = TRUE)) / stats::sd(value, na.rm = TRUE)) |>
    dplyr::ungroup()

  nn_long <- non_neutrals |>
    tidyr::pivot_longer(dplyr::all_of(regions), names_to = "Region", values_to = "NeutralScore") |>
    dplyr::filter(NeutralScore == 1) |>
    dplyr::select(Region, Binomial_correct = sp)

  z_scores <- dplyr::inner_join(
    nn_long, pool_z,
    by = c("Region", "Binomial_correct"),
    relationship = "one-to-many"
  ) |>
    dplyr::rename(RegionNeutral = Region)

  safe_p <- function(test) tryCatch(test$p.value, error = function(e) NA_real_)
  tests <- z_scores |>
    dplyr::group_by(TraitName, RegionNeutral) |>
    dplyr::summarise(
      n_taxa   = dplyr::n_distinct(Binomial_correct),
      n        = sum(!is.na(TraitScore)),
      mean     = mean(TraitScore, na.rm = TRUE),
      sd       = stats::sd(TraitScore, na.rm = TRUE),
      t_test_p = if (n >= 2) safe_p(stats::t.test(TraitScore, mu = 0)) else NA_real_,
      wilcox_p = if (n >= 1) safe_p(stats::wilcox.test(TraitScore, mu = 0, exact = FALSE)) else NA_real_,
      .groups  = "drop"
    ) |>
    dplyr::group_by(TraitName) |>
    dplyr::mutate(
      t_p_adj = stats::p.adjust(t_test_p, method = "BH"),
      w_p_adj = stats::p.adjust(wilcox_p, method = "BH")
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      sig = dplyr::case_when(
        is.na(t_p_adj)  ~ "",
        t_p_adj < 0.001 ~ "***",
        t_p_adj < 0.01  ~ "**",
        t_p_adj < 0.05  ~ "*",
        TRUE            ~ ""
      )
    ) |>
    dplyr::arrange(TraitName, RegionNeutral)

  list(z_scores = z_scores, tests = tests)
}
