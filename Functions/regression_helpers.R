# --------------------------------------------
# Purpose: Reusable regression and benchmark helpers for H1 (EEoS deviation vs. disturbed forest
#          cover, allometric uncertainty benchmark, ANCOVA interaction tests, HC3/WLS/robust/bootstrap
#          checks) and exact additive 3-run BIOMASS channel OLS decompositions.
# ---------------------------------------------------

#' Summarise EEoS deviation against Monte Carlo allometric propagation uncertainty for a plot table
summarise_allometric_benchmark <- function(d, label = NULL) {
  d |>
    dplyr::summarise(
      Region            = if (is.null(label)) as.character(dplyr::first(Region)) else label,
      n                 = dplyr::n(),
      mean_dev          = mean(dev),
      sd_dev            = stats::sd(dev),
      mean_absdev       = mean(absdev),
      median_B_cv       = stats::median(B_cv),
      mean_sigma_allom  = mean(sigma_allom),
      pct_within_1sigma = 100 * mean(absdev <= sigma_allom),
      pct_within_95CI   = 100 * mean(absdev <= 1.96 * sigma_allom),
      R2_log_B          = 1 - sum((log(B) - log(B_pred))^2) / sum((log(B) - mean(log(B)))^2)
    )
}

#' Build regional + study-wide ("All") allometric uncertainty benchmark table
build_allometric_benchmark_table <- function(plot_data) {
  dplyr::bind_rows(
    plot_data |>
      dplyr::group_by(Region) |>
      dplyr::group_map(\(g, k) summarise_allometric_benchmark(dplyr::mutate(g, Region = k$Region))) |>
      purrr::list_rbind(),
    summarise_allometric_benchmark(plot_data, label = "All")
  )
}

#' Fit H1 regional fixed-effects OLS, ANCOVA interaction test, robustness suite, and bootstrap CIs
fit_h1_regression <- function(d, outcome, n_boot = 10000, n_rq_boot = 2000, seed = 1) {
  form <- stats::reformulate(c("disturbedcover", "Region"), outcome)
  fit  <- stats::lm(form, data = d)

  coef_tab <- as.data.frame(summary(fit)$coefficients)
  ci <- stats::confint(fit)
  coef_tab$CI_2.5  <- ci[, 1]
  coef_tab$CI_97.5 <- ci[, 2]
  coef_tab$Term    <- rownames(coef_tab)

  ancova <- as.data.frame(stats::anova(fit, stats::lm(stats::reformulate("disturbedcover * Region", outcome), data = d))) |>
    dplyr::mutate(Model = c(sprintf("Common Slope (%s)", outcome), sprintf("Varying Slopes (%s)", outcome)))

  # Robustness: HC3 sandwich SEs; Huber and MM robust regression; median (quantile) regression;
  # Gamma GLM with log link (positive right-skewed outcomes only); inverse-allometric-variance WLS
  # (1 / sigma_allom^2); top-10% B_cv trimming; and Cook's-D > 4/n trimming
  hc3     <- lmtest::coeftest(fit, vcov = sandwich::vcovHC(fit, type = "HC3"))
  rlm_fit <- MASS::rlm(form, data = d, maxit = 100)
  mm_fit  <- robustbase::lmrob(form, data = d, setting = "KS2014")
  set.seed(seed)
  rq_sum <- suppressWarnings(summary(quantreg::rq(form, tau = 0.5, data = d), se = "boot", R = n_rq_boot))
  gamma_coef <- if (all(d[[outcome]] > 0)) {
    summary(stats::glm(form, family = stats::Gamma(link = "log"), data = d))$coefficients
  } else NULL

  fit_wls     <- stats::lm(form, data = d, weights = 1 / (d$sigma_allom^2))
  cv_cut      <- stats::quantile(d$B_cv, 0.90, na.rm = TRUE)
  fit_trim_cv <- stats::lm(form, data = dplyr::filter(d, B_cv <= cv_cut))

  cooks_d     <- stats::cooks.distance(fit)
  is_inf      <- cooks_d > 4 / stats::nobs(fit)
  influential <- d$Region_Plot[is_inf]
  fit_noinf   <- stats::lm(form, data = dplyr::filter(d, !Region_Plot %in% influential))
  bp          <- lmtest::bptest(fit)

  robust <- tibble::tibble(
    Term                   = rownames(coef_tab),
    Estimate               = stats::coef(fit),
    HC3_SE                 = hc3[, "Std. Error"],
    HC3_p                  = hc3[, "Pr(>|t|)"],
    WLS_InvCV2_Estimate    = stats::coef(fit_wls),
    WLS_InvCV2_p           = summary(fit_wls)$coefficients[, 4],
    TrimTop10CV_Estimate   = stats::coef(fit_trim_cv),
    TrimTop10CV_p          = summary(fit_trim_cv)$coefficients[, 4],
    RLM_Estimate           = stats::coef(rlm_fit),
    MM_Estimate            = stats::coef(mm_fit),
    MM_p                   = summary(mm_fit)$coefficients[, 4],
    Median_Estimate        = rq_sum$coefficients[, 1],
    Median_p               = rq_sum$coefficients[, 4],
    GammaLog_Estimate      = if (is.null(gamma_coef)) NA_real_ else gamma_coef[, 1],
    GammaLog_p             = if (is.null(gamma_coef)) NA_real_ else gamma_coef[, 4],
    NoInfluential_Estimate = stats::coef(fit_noinf),
    NoInfluential_p        = summary(fit_noinf)$coefficients[, 4],
    n_influential_excluded = length(influential),
    BP_statistic           = unname(bp$statistic),
    BP_p                   = bp$p.value
  )

  influential_df <- tibble::tibble(
    Region_Plot    = influential,
    cooks_distance = unname(cooks_d[is_inf])
  )

  # Case-resampling bootstrap CIs (BCa and percentile) for every coefficient
  set.seed(seed)
  b <- boot::boot(d, function(data, i) stats::coef(stats::lm(form, data = data[i, ])), R = n_boot)
  boot_tab <- purrr::map_dfr(seq_along(stats::coef(fit)), function(k) {
    ci_b <- boot::boot.ci(b, type = c("bca", "perc"), index = k)
    tibble::tibble(
      Term     = names(stats::coef(fit))[k],
      Estimate = unname(stats::coef(fit))[k],
      BCa_lwr  = ci_b$bca[4],
      BCa_upr  = ci_b$bca[5],
      Perc_lwr = ci_b$percent[4],
      Perc_upr = ci_b$percent[5],
      R        = n_boot
    )
  })

  list(
    fit            = fit,
    coef_tab       = coef_tab,
    ancova         = ancova,
    robust         = robust,
    influential    = influential,
    influential_df = influential_df,
    boot           = boot_tab
  )
}

#' Write H1 regression CSV outputs and diagnostic QQ / Cook's-distance PNG
write_h1_outputs <- function(res, stem_prefix, diag_dir = "Figures/diagnostics") {
  dir.create(diag_dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(res$influential_df, out_path(paste0(stem_prefix, "_InfluentialPlots")), row.names = FALSE)
  utils::write.csv(res$coef_tab,       out_path(paste0(stem_prefix, "_Model")),            row.names = FALSE)
  utils::write.csv(res$ancova,         out_path(paste0(stem_prefix, "_ANCOVA_InteractionTest")), row.names = FALSE)
  utils::write.csv(res$robust,         out_path(paste0(stem_prefix, "_Robustness")),       row.names = FALSE)
  utils::write.csv(res$boot,           out_path(paste0(stem_prefix, "_BootstrapCI")),      row.names = FALSE)

  grDevices::png(file.path(diag_dir, sprintf("%s_QQ.png", stem_prefix)), width = 900, height = 450)
  graphics::par(mfrow = c(1, 2))
  plot(res$fit, which = c(2, 4))
  grDevices::dev.off()
  invisible(res)
}

#' Fit common-slope regional fixed-effects OLS across a named vector of additive channels
#' @param outcomes_named named character vector mapping channel labels (including `"Total"`) to column names
fit_channel_ols <- function(df, predictor, reg_col, hypothesis, dataset_label, outcomes_named) {
  rows <- purrr::imap_dfr(outcomes_named, function(y_col, chan_name) {
    form <- stats::reformulate(c(predictor, reg_col), y_col)
    fit  <- stats::lm(form, data = df)
    s    <- summary(fit)
    co   <- s$coefficients[predictor, ]
    ci   <- stats::confint(fit)[predictor, ]
    hc3  <- lmtest::coeftest(fit, vcov = sandwich::vcovHC(fit, type = "HC3"))[predictor, ]
    sd_x <- stats::sd(df[[predictor]])
    sd_y <- stats::sd(df[[y_col]])

    tibble::tibble(
      Hypothesis = hypothesis,
      Dataset    = dataset_label,
      N          = stats::nobs(fit),
      Predictor  = predictor,
      Channel    = chan_name,
      Outcome    = y_col,
      Estimate   = unname(co["Estimate"]),
      Std_Error  = unname(co["Std. Error"]),
      CI_2.5     = unname(ci[1]),
      CI_97.5    = unname(ci[2]),
      t_value    = unname(co["t value"]),
      p_value    = unname(co["Pr(>|t|)"]),
      HC3_SE     = unname(hc3["Std. Error"]),
      HC3_p      = unname(hc3["Pr(>|t|)"]),
      Std_Beta   = unname(co["Estimate"]) * (sd_x / sd_y),
      R2         = s$r.squared,
      Adj_R2     = s$adj.r.squared
    )
  })

  tot_est <- rows$Estimate[rows$Channel == "Total"]
  rows |>
    dplyr::mutate(
      Total_Slope    = tot_est,
      Share_of_Total = Estimate / tot_est,
      Pct_of_Total   = 100 * Estimate / tot_est
    )
}
