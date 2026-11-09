simulate_mediation <- function(n = 400, a_slope = 0.6, b_slope = 0.8, direct = 0, seed = 1) {
  set.seed(seed)
  region <- sample(REGIONS, n, replace = TRUE)
  x <- ifelse(region == "Jurua", 0, runif(n))
  m <- a_slope * x + rnorm(n, sd = 0.3)
  y <- b_slope * m + direct * x + rnorm(n, sd = 0.3)
  tibble(disturbedcover = x, med = m, out = y,
         Reg_Balbina = as.numeric(region == "Balbina"), Reg_STM = as.numeric(region == "STM"),
         Reg_PGM = as.numeric(region == "PGM"))
}

test_that("8-model balanced Garrido set shares exact 6x6 covariance matrix S and expected df/npar", {
  d <- simulate_mediation()
  fits <- lapply(build_mediation_models("disturbedcover", "med", "out"), fit_sem, data = d)
  df <- sapply(fits, \(f) as.numeric(lavaan::fitMeasures(f, "df")))
  npar <- sapply(fits, \(f) as.numeric(lavaan::fitMeasures(f, "npar")))
  s_dims <- sapply(fits, \(f) nrow(lavaan::lavInspect(f, "sampstat")$cov))
  expect_equal(unname(df), c(0, 1, 1, 1, 2, 2, 2, 3))
  expect_equal(unname(npar), c(13, 12, 12, 12, 11, 11, 11, 10))
  expect_true(all(s_dims == 6))
})

test_that("8-model Garrido MMI distinguishes full mediation, partial mediation, and independent effects (a = 0)", {
  capture.output(r_full <- fit_mediation_suite(simulate_mediation(direct = 0), "disturbedcover", "med", "out"))
  expect_gt(r_full$lrt$`Pr(>Chisq)`[2], 0.05)
  expect_gt(r_full$mmi$Relative_Importance[r_full$mmi$Parameter == "a"], 0.90)
  expect_gt(r_full$mmi$Relative_Importance[r_full$mmi$Parameter == "b"], 0.90)

  capture.output(r_part <- fit_mediation_suite(simulate_mediation(direct = 0.5), "disturbedcover", "med", "out"))
  expect_lt(r_part$lrt$`Pr(>Chisq)`[2], 0.001)
  expect_gt(r_part$mmi$Relative_Importance[r_part$mmi$Parameter == "a"], 0.90)
  expect_gt(r_part$mmi$Relative_Importance[r_part$mmi$Parameter == "b"], 0.90)
  expect_gt(r_part$mmi$Relative_Importance[r_part$mmi$Parameter == "c"], 0.90)

  capture.output(r_indep <- fit_mediation_suite(simulate_mediation(a_slope = 0, direct = 0.5), "disturbedcover", "med", "out"))
  expect_lt(r_indep$mmi$Relative_Importance[r_indep$mmi$Parameter == "a"], 0.50)
  expect_gt(r_indep$mmi$Relative_Importance[r_indep$mmi$Parameter == "b"], 0.90)
  expect_gt(r_indep$mmi$Relative_Importance[r_indep$mmi$Parameter == "c"], 0.90)
  expect_equal(r_indep$mmi$Best_Model[1], "Independent Effects (No a)")
})

test_that("AICc follows the textbook Garrido Glossary formula", {
  fit <- fit_sem(build_mediation_models("disturbedcover", "med", "out")$full, simulate_mediation())
  fm <- lavaan::fitMeasures(fit, c("aic", "npar", "ntotal"))
  k <- fm[["npar"]]; n <- fm[["ntotal"]]
  expect_equal(calculate_aicc(fit), fm[["aic"]] + 2 * k * (k + 1) / (n - k - 1))
})

test_that("indirect effect point estimate equals a*b from the ML fit", {
  d <- simulate_mediation(n = 200)
  ind <- fit_indirect_effect(d, "disturbedcover", "med", "out", n_boot = 50)
  est <- lavaan::parameterEstimates(fit_sem(build_mediation_models("disturbedcover", "med", "out")$full, d))
  a <- path_est(est, "med", "disturbedcover")$est; b <- path_est(est, "out", "med")$est
  expect_equal(ind$est[ind$label == "ind"], a * b, tolerance = 1e-8)
  expect_true(all(c("ci.lower", "ci.upper") %in% names(ind)))
})

test_that("marginal predictions: grid length and ordered confidence band", {
  d <- simulate_mediation()
  mp <- get_marginal_predictions(d, outcome = "out", predictor = "med", grid_points = 50)
  expect_equal(nrow(mp), 50)
  expect_true(all(mp$lwr <= mp$y & mp$y <= mp$upr))
})

test_that("fit_mediation_suite exports all 8 candidate models and summarise_mediation_run weights sum to 1", {
  d <- simulate_mediation(n = 150, direct = 0)
  capture.output(res <- fit_mediation_suite(d, "disturbedcover", "med", "out"))
  expect_setequal(unique(res$estimates$Model), unname(CANDIDATE_MODEL_LABELS))
  res$indirect <- fit_indirect_effect(d, "disturbedcover", "med", "out", n_boot = 50)
  s <- summarise_mediation_run(res, id = "H2_test", m = "med", y = "out")
  expect_equal(nrow(s), 1)
  w_sum <- s$w_Partial + s$w_Full + s$w_Coresponse + s$w_Independent +
           s$w_X_to_M_Only + s$w_M_to_Y_Only + s$w_Direct_Only + s$w_Null
  expect_equal(w_sum, 1, tolerance = 1e-8)
  expect_true(is.finite(s$ind_full_ci_low) && is.finite(s$ind_full_ci_high))
  expect_true(s$R2_m > 0 && s$R2_y_full > 0)
})

test_that("run_mediation_hypothesis(write_files = FALSE) and extract_mmi_metrics return valid MMI metrics", {
  d <- simulate_mediation(n = 160, a_slope = 0.5, b_slope = -0.6, direct = -0.1)
  capture.output(
    res <- run_mediation_hypothesis(
      d, id = "UnitTest_NoWrite", m = "med", y = "out",
      indirect = TRUE, n_boot = 40, seed = 1, write_files = FALSE
    )
  )
  mmi_wide <- extract_mmi_metrics(res)
  expect_equal(nrow(mmi_wide), 1)
  expect_true(is.finite(mmi_wide$Weighted_CFI) && mmi_wide$Weighted_CFI > 0.9)
  expect_true(is.finite(mmi_wide$Avg_est_ind) && mmi_wide$Avg_est_ind < 0)
  expect_true(mmi_wide$Prop_Mediated >= 0 && mmi_wide$Prop_Mediated <= 1)
})
