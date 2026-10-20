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

#' Observed occurrence frequency vs log abundance with Poisson 95% interval
plot_sloan_fit <- function(fit, title) {
  fit$vals |>
    ggplot2::ggplot(ggplot2::aes(y = freq, x = log(p))) +
    ggplot2::geom_point(alpha = 0.3) +
    ggplot2::geom_line(ggplot2::aes(y = pois.upr), color = "red") +
    ggplot2::geom_line(ggplot2::aes(y = pois.lwr), color = "red") +
    ggplot2::annotate("text", x = -6, y = 0.9, label = paste0("R2=", signif(fit$stats$Rsqr.pois, 2), "\n")) +
    ggplot2::theme_classic() +
    ggplot2::ggtitle(title)
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

#' Per-plot proportion of stems belonging to taxa that are non-neutral in the plot's region
nonneutral_proportions <- function(comm, nn_long) {
  comm |>
    dplyr::left_join(dplyr::mutate(dplyr::select(nn_long, Region, Binomial_correct), nonneutral = 1),
                     by = c("Region", "Binomial_correct"), relationship = "many-to-one") |>
    dplyr::group_by(Region, Region_Plot) |>
    dplyr::summarise(proportionNonNeutral = sum(nonneutral, na.rm = TRUE) / dplyr::n(), .groups = "drop") |>
    dplyr::select(Region_Plot, proportionNonNeutral)
}
