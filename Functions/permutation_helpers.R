# --------------------------------------------
# Purpose: Blocked permutation test of an ordinal trend in a response across regions,
#          with permutation of region order within each replicate subsample.
# ---------------------------------------------------

#' @param dat tibble with columns seed (replicate), dv (response), order (ordinal region rank)
#' @return list(observed_mean_slope, p_value (one-sided, >=), boot_ci)
run_permutation_test <- function(dat, n_perm = 1000) {
  replicate_slopes <- function(d) {
    d |>
      dplyr::group_by(seed) |>
      dplyr::reframe(beta = unname(coef(lm(dv ~ order))["order"]))
  }
  slopes_obs <- replicate_slopes(dat)
  beta_obs_mean <- mean(slopes_obs$beta)

  beta_perm_means <- vapply(seq_len(n_perm), function(b) {
    set.seed(b)
    perm <- dat |> dplyr::group_by(seed) |> dplyr::mutate(order = sample(order)) |> dplyr::ungroup()
    mean(replicate_slopes(perm)$beta)
  }, numeric(1))
  p_value <- mean(beta_perm_means >= beta_obs_mean)

  set.seed(1)
  boot_means <- replicate(5000, mean(sample(slopes_obs$beta, size = 100, replace = TRUE)))
  ci <- stats::quantile(boot_means, c(0.025, 0.975))
  cat("Observed mean slope:", round(beta_obs_mean, 4), "\nPermutation P-value:", p_value,
      "\nBootstrap confidence interval", ci, "\n")
  list(observed_mean_slope = beta_obs_mean, p_value = p_value, boot_ci = ci)
}