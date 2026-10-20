simulate_mediation <- function(n = 400, direct = 0, seed = 1) {
  set.seed(seed)
  region <- sample(REGIONS, n, replace = TRUE)
  x <- ifelse(region == "Jurua", 0, runif(n))
  m <- 0.6 * x + rnorm(n, sd = 0.3)
  y <- 0.8 * m + direct * x + rnorm(n, sd = 0.3)
  tibble(disturbedcover = x, med = m, out = y,
         Reg_Balbina = as.numeric(region == "Balbina"), Reg_STM = as.numeric(region == "STM"),
         Reg_PGM = as.numeric(region == "PGM"))
}

test_that("model strings parse and have the expected degrees of freedom", {
  d <- simulate_mediation()
  fits <- lapply(build_mediation_models("disturbedcover", "med", "out"), fit_sem, data = d)
  df <- sapply(fits, \(f) as.numeric(lavaan::fitMeasures(f, "df")))
  expect_equal(unname(df), c(1, 0, 0, 1))
})

test_that("full-mediation LRT does not reject true full mediation but rejects a direct effect", {
  res_full <- suppressMessages(capture.output(r <- fit_mediation_suite(simulate_mediation(direct = 0), "disturbedcover", "med", "out")))
  expect_gt(r$lrt$`Pr(>Chisq)`[2], 0.05)
  capture.output(r2 <- fit_mediation_suite(simulate_mediation(direct = 0.5), "disturbedcover", "med", "out"))
  expect_lt(r2$lrt$`Pr(>Chisq)`[2], 0.001)
})

test_that("AICc follows the textbook formula", {
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
