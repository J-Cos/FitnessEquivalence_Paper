simulate_metacommunity <- function(n_plots = 20, n_species = 30, n_per_plot = 100, seed = 1) {
  set.seed(seed)
  p <- rev(sort(rgamma(n_species, shape = 0.5)))
  p <- p / sum(p)
  m <- t(sapply(seq_len(n_plots), \(i) as.vector(rmultinom(1, n_per_plot, p))))
  colnames(m) <- paste0("sp", seq_len(n_species))
  m
}

test_that("sncm_fit returns valid statistics and predictions", {
  fit <- sncm_fit(simulate_metacommunity())
  v <- fit$vals
  expect_true(all(v$pois.pred >= 0 & v$pois.pred <= 1))
  expect_true(all(v$bino.pred >= 0 & v$bino.pred <= 1))
  expect_true(all(is.finite(unlist(fit$stats[c("AIC", "AIC.bino", "AIC.pois")]))))
  expect_lte(fit$stats$Rsqr.pois, 1)
})

test_that("binomial likelihood uses observed-minus-predicted residuals (regression test for 3.1)", {
  # Under pure random sampling the binomial model fits well, so its residual SD (ML) must be small
  # and close to sd(freq - bino.pred); with the old bug (R = -pred) it was ~sd(pred).
  fit <- sncm_fit(simulate_metacommunity(n_plots = 40))
  v <- fit$vals
  sigma_ml <- sqrt(mean((v$freq - v$bino.pred - mean(v$freq - v$bino.pred))^2))
  loglik_expected <- sum(dnorm(v$freq - v$bino.pred, mean(v$freq - v$bino.pred), sigma_ml, log = TRUE))
  expect_equal(-fit$stats$binoLL, loglik_expected, tolerance = 0.02)
  # Poisson and binomial random sampling are near-identical for small p
  expect_lt(abs(fit$stats$AIC.pois - fit$stats$AIC.bino), 10)
})

test_that("classify_neutrality labels taxa by the Poisson interval", {
  fake <- list(vals = data.frame(freq = c(0.1, 0.5, 0.9), pois.lwr = c(0.2, 0.4, 0.5), pois.upr = c(0.3, 0.6, 0.8),
                                 row.names = c("low", "mid", "high")))
  cl <- classify_neutrality(fake)
  expect_equal(cl$Neutrality[match(c("low", "mid", "high"), cl$sp)], c("LFHA", "Neutral", "HFLA"))
})

test_that("non-neutral classification requires the same class in every replicate where present", {
  taxa_long <- tibble(
    region = c(rep("PGM", 4), rep("Balbina", 3), rep("PGM", 2)),
    seed = c(1, 2, 3, 4, 1, 2, 3, 1, 2),
    sp = c(rep("x", 4), rep("x", 3), "y", "y"),
    Neutrality = c("HFLA", "HFLA", "HFLA", "HFLA", "HFLA", "HFLA", "HFLA", "HFLA", "Neutral"))
  nn <- classify_nonneutral_taxa(taxa_long)
  expect_equal(nrow(nn$long), 2)                      # x in PGM and in Balbina; y is not always non-neutral
  expect_equal(nn$wide$sp, "x")
  expect_equal(unname(unlist(nn$wide[1, REGIONS])), c(0, 1, 0, 1))
})

test_that("non-neutral proportions do not duplicate stems and partition cleanly into LFHA + HFLA", {
  nn_long <- tibble(Region = c("PGM", "PGM", "Jurua"),
                    Binomial_correct = c("a", "c", "a"),
                    Neutrality = c("HFLA", "LFHA", "HFLA"))
  prop <- nonneutral_proportions(toy_comm, nn_long)
  expect_equal(nrow(prop), n_distinct(toy_comm$Region_Plot))
  expect_equal(prop$proportionHFLA[prop$Region_Plot == "P1"], 1 / 3)
  expect_equal(prop$proportionLFHA[prop$Region_Plot == "P1"], 2 / 3)
  expect_equal(prop$proportionNonNeutral, prop$proportionLFHA + prop$proportionHFLA)
  expect_equal(prop$proportionNonNeutral[prop$Region_Plot == "J1"], 1 / 3)
  expect_equal(prop$proportionNonNeutral[prop$Region_Plot == "S1"], 0)   # 'a' not non-neutral in STM
})

test_that("community matrix has plots as rows and stem counts as entries", {
  m <- build_community_matrix(toy_comm, c("J1", "P1"))
  expect_equal(sort(rownames(m)), c("J1", "P1"))
  expect_equal(sum(m), 6)
  expect_equal(unname(m["J1", "b"]), 2)
})

test_that("ordered subgroup builder orders by fc2000 and drops incomplete trailing sets", {
  sub_comm <- tibble(
    Region = rep(c("PGM", "Jurua"), each = 3),
    Region_Plot = c("P1", "P2", "P3", "J1", "J2", "J3"),
    Binomial_correct = "a"
  )
  sub_env <- tibble(Region = "PGM", Region_Plot = c("P1", "P2", "P3"), fc2000 = c(0.9, 0.2, 0.5))
  sets <- make_ordered_forest_cover_subgroups(sub_comm, sub_env, subgroup_size = 2)
  expect_equal(sets[["PGM.1"]], c("P2", "P3"))
  expect_equal(sets[["Jurua.1"]], c("J1", "J2"))
  expect_equal(length(sets), 2)
})

test_that("regional Sloan R2 and average species fit summaries aggregate replicates accurately", {
  stats_df <- tibble(
    seed   = rep(1:3, each = 4),
    region = rep(REGIONS, times = 3),
    r2pois = c(0.82, 0.76, 0.74, 0.70,
               0.80, 0.75, 0.72, 0.68,
               0.84, 0.77, 0.73, 0.69)
  )
  r2_sum <- summarise_regional_sloan_r2(stats_df)
  expect_equal(as.character(r2_sum$region), REGIONS)
  expect_equal(r2_sum$diff_from_jurua_mean[r2_sum$region == "Jurua"], 0)
  expect_equal(r2_sum$prop_subsamples_below_jurua[r2_sum$region == "PGM"], 1)

  vals_long <- tibble(
    region    = c("PGM", "PGM", "PGM"),
    seed      = c(1, 2, 1),
    sp        = c("spA", "spA", "spB"),
    p         = c(0.02, 0.04, 0.01),
    freq      = c(0.10, 0.20, 0.25),
    pois.pred = c(0.45, 0.55, 0.24),
    pois.lwr  = c(0.30, 0.40, 0.12),
    pois.upr  = c(0.60, 0.70, 0.38)
  )
  nn_long <- tibble(Region = "PGM", Binomial_correct = "spA", Neutrality = "LFHA")
  avg_fit <- summarise_regional_average_sloan(vals_long, nn_long)
  expect_equal(nrow(avg_fit), 2)
  expect_equal(avg_fit$p[avg_fit$sp == "spA"], 0.03)
  expect_equal(avg_fit$Neutrality[avg_fit$sp == "spA"], "LFHA")
  expect_equal(avg_fit$Neutrality[avg_fit$sp == "spB"], "Neutral")
})
