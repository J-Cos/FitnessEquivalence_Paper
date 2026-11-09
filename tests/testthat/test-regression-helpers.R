simulate_h1_plot_data <- function(n = 120, seed = 42) {
  set.seed(seed)
  reg <- factor(rep(REGIONS, length.out = n), levels = REGIONS)
  dc  <- ifelse(reg == "Jurua", 0, runif(n, 0.05, 0.85))
  reg_shift <- c(Jurua = 0.05, Balbina = 0.02, STM = -0.02, PGM = -0.06)[as.character(reg)]

  w_rho  <- -0.14 * dc + 0.5 * reg_shift + rnorm(n, sd = 0.03)
  r_size <- -0.04 * dc + 0.3 * reg_shift + rnorm(n, sd = 0.02)
  w_H    <- -0.01 * dc + 0.2 * reg_shift + rnorm(n, sd = 0.005)
  dev    <- w_rho + r_size + w_H
  b_pred <- runif(n, 50, 1200)
  b_obs  <- b_pred * exp(dev)
  b_cv   <- runif(n, 0.07, 0.16)

  tibble(
    Region         = reg,
    Region_Plot    = paste0(substr(as.character(reg), 1, 1), "_", seq_len(n)),
    B              = b_obs,
    B_pred         = b_pred,
    dev            = dev,
    absdev         = abs(dev) + 1e-4,
    w_rho          = w_rho,
    r_size         = r_size,
    w_H            = w_H,
    B_cv           = b_cv,
    sigma_allom    = sqrt(log1p(b_cv^2)),
    disturbedcover = dc
  )
}

test_that("build_allometric_benchmark_table summarises all 4 regions plus study-wide All row", {
  d <- simulate_h1_plot_data(n = 80)
  bm <- build_allometric_benchmark_table(d)
  expect_equal(bm$Region, c(REGIONS, "All"))
  expect_equal(bm$n[bm$Region == "All"], 80)
  expect_equal(sum(bm$n[bm$Region != "All"]), 80)
  expect_true(all(bm$pct_within_1sigma >= 0 & bm$pct_within_1sigma <= 100))
  expect_true(all(bm$pct_within_95CI >= bm$pct_within_1sigma))
  expect_true(all(is.finite(bm$R2_log_B) & bm$R2_log_B > 0.8))
})

test_that("fit_h1_regression recovers negative slope, runs robustness suite, and handles Gamma GLM conditionally", {
  d <- simulate_h1_plot_data(n = 120)

  res_sig <- fit_h1_regression(d, "dev", n_boot = 120, n_rq_boot = 100, seed = 1)
  slope_est <- res_sig$coef_tab["disturbedcover", "Estimate"]
  expect_equal(slope_est, -0.19, tolerance = 0.04)
  expect_equal(nrow(res_sig$ancova), 2)
  expect_true(all(is.na(res_sig$robust$GammaLog_Estimate)))   # signed dev takes negative values
  expect_true(all(is.finite(res_sig$robust$HC3_SE)))
  expect_true(all(is.finite(res_sig$robust$WLS_InvCV2_Estimate)))
  expect_true(all(is.finite(res_sig$boot$Perc_lwr) & is.finite(res_sig$boot$Perc_upr)))

  res_abs <- fit_h1_regression(d, "absdev", n_boot = 120, n_rq_boot = 100, seed = 1)
  expect_true(all(is.finite(res_abs$robust$GammaLog_Estimate)))
})

test_that("fit_channel_ols satisfies exact linear slope additivity across additive channels", {
  d <- simulate_h1_plot_data(n = 100)
  chans <- c(Total = "dev", Wood_Density = "w_rho", Pure_Size = "r_size", Site_Height = "w_H")
  tab <- fit_channel_ols(d, "disturbedcover", "Region", "H1_test", "plot", chans)

  expect_equal(nrow(tab), 4)
  tot_slope  <- tab$Estimate[tab$Channel == "Total"]
  sum_parts  <- sum(tab$Estimate[tab$Channel != "Total"])
  sum_shares <- sum(tab$Share_of_Total[tab$Channel != "Total"])
  sum_pcts   <- sum(tab$Pct_of_Total[tab$Channel != "Total"])

  expect_equal(sum_parts, tot_slope, tolerance = 1e-13)
  expect_equal(sum_shares, 1.0, tolerance = 1e-13)
  expect_equal(sum_pcts, 100.0, tolerance = 1e-11)
})
