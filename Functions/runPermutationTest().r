
runPermutationTest<-function(dat, n_perm =12) {
  # 1. Compute the observed slopes for each replicate
  slopes_obs <- dat %>%
    group_by(seed) %>%
    do({
      fit <- lm(dv ~ order, data = .)
      data.frame(beta = coef(fit)["order"])
    })

  # Average observed slope
  beta_obs_mean <- mean(slopes_obs$beta)

  # 2. Blocked permutation
  beta_perm_means <- numeric(n_perm)

  for (b in seq_len(n_perm)) {
    set.seed(b)  # for reproducibility

    # Permute within each replicate
    df_perm <-  dat %>%
      group_by(seed) %>%
      mutate(
        order = sample(order)  # shuffle region labels
      ) %>%
      ungroup()
    
    # Recompute slopes on permuted labels
    slopes_b <- df_perm %>%
      group_by(seed) %>%
    do({
      fit <- lm(dv ~ order, data = .)
      data.frame(beta = coef(fit)["order"])
    })
    
    # Store the mean slope for this permutation
    beta_perm_means[b] <- mean(slopes_b$beta)
    print(b)
  }

  # 3. Compute one‐sided p‐value
  p_value <- mean(beta_perm_means >= beta_obs_mean)

  # 4. Results

  cat("Observed mean slope: ", round(beta_obs_mean, 4), "\n")
  cat("Permutation P-value: ", p_value, "\n")

  set.seed(1)
  B <- 5000
  boot_means <- replicate(B, {
    sample_betas <- sample(slopes_obs$beta, size = 100, replace = TRUE)
    mean(sample_betas)
  })
  cat("Bootstrap confidence interval ", quantile(boot_means, c(0.025, 0.975)))


}