test_that("CATS NB deviance R2 reduces identically to Poisson deviance R2 on Poisson boundary (alpha = 0)", {
  set.seed(42)
  S <- 40
  meta <- runif(S, 0.5, 5); meta <- meta / sum(meta)
  wd <- seq(0.3, 0.9, length.out = S)
  # Underdispersed / exact deterministic-like counts near expected Poisson mean so alpha_hat = 0
  mu_true <- 150 * meta * exp(-1.5 * (wd - mean(wd)))
  n <- round(mu_true)
  df <- tibble(Binomial_correct = paste0("sp", seq_len(S)), meta = meta, n = as.integer(n), WD = wd)

  res <- fit_cats_model_pair(df, trait_names = "WD")
  expect_equal(res$alpha_nb, 0)
  expect_true(is.infinite(res$theta_nb))
  expect_equal(res$r2_nb, res$r2_pois, tolerance = 1e-10)
  expect_true(res$r2_nb > 0.5 && res$r2_nb <= 1)
})

test_that("CATS NB model detects overdispersion and outperforms Poisson when counts are NB-clumped", {
  set.seed(7)
  S <- 80
  meta <- rexp(S, rate = 1); meta <- meta / sum(meta)
  wd <- runif(S, 0.3, 0.95)
  lma <- runif(S, 50, 150)
  mu <- 140 * meta * exp(-2.0 * (wd - 0.6))
  n <- rnbinom(S, size = 0.5, mu = mu)
  df <- tibble(Binomial_correct = paste0("sp", seq_len(S)), meta = meta, n = as.integer(n),
               WD = wd, LMA = lma)

  res <- fit_cats_model_pair(df, trait_names = c("WD", "LMA"))
  expect_true(res$dAIC_pois_v_nb > 10)
  expect_true(res$overdisp_lrt_p < 0.001)
  expect_true(res$r2_nb >= 0 && res$r2_nb <= 1)
  expect_true(res$r2_pois >= 0 && res$r2_pois <= 1)
  expect_equal(nrow(res$fitted_df), S)
})
