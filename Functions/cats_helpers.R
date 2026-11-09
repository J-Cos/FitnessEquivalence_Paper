# --------------------------------------------
# Purpose: Community Assembly via Trait Selection (CATS) regression helpers (H2a).
#          Implements both Warton, Shipley & Hastie's (2015, MEE 6:363-371) Negative Binomial
#          CATS regression (primary; accounting for local stem clumping / overdispersion) and
#          Poisson CATS regression (exact MaxEnt equivalence to Shipley et al. 2006, 2012),
#          with an offset of log regional metacommunity relative abundance.
# ---------------------------------------------------

CATS_TRAITS_4    <- c("WD", "lnSM", "LMA", "Hmax")
CATS_TRAITS_3    <- c("WD", "lnSM", "LMA")
CATS_TRAITS_NOWD <- c("lnSM", "LMA", "Hmax")

#' Regional metacommunity relative abundance (share of regional stems per taxon)
compute_regional_metacommunity <- function(plot_species) {
  meta_by_region <- plot_species |>
    dplyr::group_by(Region, Binomial_correct) |>
    dplyr::summarise(n_stems = sum(n), .groups = "drop_last") |>
    dplyr::mutate(meta = n_stems / sum(n_stems)) |>
    dplyr::ungroup() |>
    dplyr::select(Region, Binomial_correct, meta)
  sums <- tapply(meta_by_region$meta, meta_by_region$Region, sum)
  stopifnot(all(abs(sums - 1) < 1e-12))
  meta_by_region
}

#' Build the full regional-pool species table for one plot (absent regional taxa receive n = 0L)
build_cats_plot_table <- function(plot_id, region, meta_by_region, plot_species, traits) {
  meta_by_region |>
    dplyr::filter(Region == region) |>
    dplyr::left_join(
      dplyr::filter(plot_species, Region_Plot == plot_id) |> dplyr::select(Binomial_correct, n),
      by = "Binomial_correct"
    ) |>
    dplyr::mutate(n = tidyr::replace_na(n, 0L)) |>
    dplyr::left_join(traits, by = "Binomial_correct")
}

cats_formula <- function(trait_names = character(0), offset_var = "meta") {
  rhs <- if (length(trait_names) == 0) "1" else paste(trait_names, collapse = " + ")
  stats::as.formula(sprintf("n ~ %s + offset(log(%s))", rhs, offset_var))
}

#' Coverage of a trait set: number of present species and proportion of plot stems with complete traits
cats_trait_coverage <- function(fitting, trait_names) {
  ok <- stats::complete.cases(fitting[, trait_names, drop = FALSE])
  c(n_species = sum(ok & fitting$n > 0), prop_stems = sum(fitting$n[ok]) / sum(fitting$n))
}

#' Boundary-safe NB2 and Poisson GLM MLE parameterized over (beta, alpha = 1/theta >= 0)
#' Handles the Poisson boundary (alpha = 0, theta = Inf) cleanly without MASS::theta.ml overflow.
fit_nb_glm_exact <- function(formula, data) {
  mf  <- stats::model.frame(formula, data = data)
  y   <- stats::model.response(mf)
  X   <- stats::model.matrix(formula, mf)
  off <- stats::model.offset(mf)
  if (is.null(off)) off <- rep(0, length(y))

  pois <- stats::glm(formula, family = "poisson", data = data)
  ll_pois <- as.numeric(stats::logLik(pois))
  k_pois  <- ncol(X)
  n_obs   <- length(y)

  neg_ll <- function(par) {
    beta  <- par[seq_len(k_pois)]
    alpha <- par[k_pois + 1L]
    mu    <- exp(pmin(as.numeric(X %*% beta) + off, 50))
    if (alpha <= 1e-12) {
      -sum(stats::dpois(y, lambda = mu, log = TRUE))
    } else {
      -sum(stats::dnbinom(y, size = 1 / alpha, mu = mu, log = TRUE))
    }
  }

  nb_mass <- tryCatch(
    suppressWarnings(MASS::glm.nb(formula, data = data, start = stats::coef(pois))),
    error = function(e) NULL
  )
  init_alpha <- if (!is.null(nb_mass) && is.finite(nb_mass$theta) && nb_mass$theta > 0) 1 / nb_mass$theta else 0.5
  init_beta  <- if (!is.null(nb_mass) && all(is.finite(stats::coef(nb_mass)))) stats::coef(nb_mass) else stats::coef(pois)

  lower_b <- c(rep(-Inf, k_pois), 0)
  ctrl    <- list(eval.max = 1000, iter.max = 500)
  opt1 <- stats::nlminb(c(init_beta, init_alpha), neg_ll, lower = lower_b, control = ctrl)
  opt2 <- stats::nlminb(c(stats::coef(pois), 0.5), neg_ll, lower = lower_b, control = ctrl)
  opt  <- if (opt1$objective <= opt2$objective) opt1 else opt2

  on_boundary <- (-opt$objective <= ll_pois + 1e-10)
  ll_nb       <- if (on_boundary) ll_pois else -opt$objective
  alpha_hat   <- if (on_boundary) 0 else opt$par[k_pois + 1L]
  theta_hat   <- if (alpha_hat == 0) Inf else 1 / alpha_hat
  beta_nb     <- if (on_boundary) unname(stats::coef(pois)) else opt$par[seq_len(k_pois)]
  names(beta_nb) <- colnames(X)
  mu_nb <- exp(as.numeric(X %*% beta_nb) + off)

  ll_sat_nb <- if (alpha_hat <= 1e-12) {
    sum(stats::dpois(y, lambda = y, log = TRUE))
  } else {
    sum(ifelse(y == 0, 0, stats::dnbinom(y, size = 1 / alpha_hat, mu = y, log = TRUE)))
  }

  k_nb <- k_pois + 1L
  aic_pois <- -2 * ll_pois + 2 * k_pois
  aic_nb   <- -2 * ll_nb   + 2 * k_nb
  bic_pois <- -2 * ll_pois + log(n_obs) * k_pois
  bic_nb   <- -2 * ll_nb   + log(n_obs) * k_nb

  lrt_stat <- max(0, 2 * (ll_nb - ll_pois))
  # Boundary LRT for H0: alpha = 1/theta = 0 follows a 0.5*chisq_0 + 0.5*chisq_1 mixture
  lrt_p <- if (lrt_stat <= 0) 1 else 0.5 * stats::pchisq(lrt_stat, df = 1, lower.tail = FALSE)
  pearson_disp <- sum(stats::residuals(pois, type = "pearson")^2) / pois$df.residual

  list(
    beta_nb           = beta_nb,
    mu_nb             = mu_nb,
    alpha             = alpha_hat,
    theta             = theta_hat,
    ll_nb             = ll_nb,
    ll_sat_nb         = ll_sat_nb,
    aic_nb            = aic_nb,
    bic_nb            = bic_nb,
    pois              = pois,
    mu_pois           = as.numeric(stats::fitted(pois)),
    ll_pois           = ll_pois,
    aic_pois          = aic_pois,
    bic_pois          = bic_pois,
    dAIC_pois_v_nb    = aic_pois - aic_nb,
    dBIC_pois_v_nb    = bic_pois - bic_nb,
    overdisp_lrt_stat = lrt_stat,
    overdisp_lrt_p    = lrt_p,
    pearson_disp_pois = pearson_disp
  )
}

#' Fit NB and Poisson CATS models against their respective neutral metacommunity-prior null models
#' NB pseudo-R2 uses Cameron & Windmeijer's (1996, J. Econometrics 77:329-342, Eq. 3.9) deviance R2,
#' comparing fitted NB1 (beta_1, theta_1) to refitted neutral-prior NB0 (beta_0, theta_0), which
#' reduces identically to Poisson deviance / KL R2 (1 - D_1 / D_0) when alpha -> 0.
fit_cats_model_pair <- function(fitting, trait_names, offset_var = "meta") {
  sub <- fitting[stats::complete.cases(fitting[, c(trait_names, offset_var), drop = FALSE]), ]
  m1  <- fit_nb_glm_exact(cats_formula(trait_names, offset_var), sub)
  m0  <- fit_nb_glm_exact(cats_formula(character(0), offset_var), sub)

  ll1 <- max(m1$ll_nb, m0$ll_nb)
  r2_nb   <- (ll1 - m0$ll_nb) / (m1$ll_sat_nb - m0$ll_nb)
  r2_pois <- 1 - m1$pois$deviance / m1$pois$null.deviance

  sub$mu_null_nb   <- m0$mu_nb
  sub$mu_null_pois <- m0$mu_pois
  sub$mu_nb        <- m1$mu_nb
  sub$mu_pois      <- m1$mu_pois

  list(
    r2_nb             = r2_nb,
    r2_pois           = r2_pois,
    aic_pois          = m1$aic_pois,
    aic_nb            = m1$aic_nb,
    dAIC_pois_v_nb    = m1$dAIC_pois_v_nb,
    bic_pois          = m1$bic_pois,
    bic_nb            = m1$bic_nb,
    dBIC_pois_v_nb    = m1$dBIC_pois_v_nb,
    theta_nb          = m1$theta,
    alpha_nb          = m1$alpha,
    overdisp_lrt_stat = m1$overdisp_lrt_stat,
    overdisp_lrt_p    = m1$overdisp_lrt_p,
    pearson_disp_pois = m1$pearson_disp_pois,
    fit_nb            = m1,
    null_nb           = m0,
    fitted_df         = sub
  )
}
