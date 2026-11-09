# End-to-end integration checks on the real pipeline outputs and publication figures.
# Skipped cleanly when Outputs/ is empty (before run_all.R has been executed).
out <- function(f) file.path(PROJECT_ROOT, "Outputs", f)
fig <- function(f) file.path(PROJECT_ROOT, "Figures", f)

test_that("harmonised stem and trait tables (01) and macrostate / EEoS predictions (02-03) satisfy exact invariants", {
  skip_if_not(file.exists(out("EEOSPreds.csv")), "pipeline outputs not present")

  comm   <- read.csv(out("comm.csv"))
  traits <- read.csv(out("traits.csv"))
  macros <- read.csv(out("Macrostates.csv"))
  preds  <- read.csv(out("EEOSPreds.csv"))

  expect_equal(nrow(comm), 28199)
  expect_true(all(comm$DBH >= DBH_MIN_CM))
  expect_equal(anyDuplicated(traits$Binomial_correct), 0L)
  expect_true(all(unique(comm$Binomial_correct) %in% traits$Binomial_correct))

  expect_equal(nrow(macros), 224)
  expect_equal(nrow(preds), 224)
  expect_equal(sum(preds$N), 28199)
  expect_true(all(preds$E > preds$N))
  expect_true(all(preds$B_cv > 0 & is.finite(preds$B_cv)))
  expect_true(all(abs(preds$con_N) < 1e-3 & abs(preds$con_E) < 1e-3))

  # Exact 3-run telescoping channel decomposition: dev = w_rho + w_H + r_size
  expect_true(all(is.finite(preds$dev)))
  expect_lt(max(abs((preds$w_rho + preds$w_H + preds$r_size) - preds$dev)), 1e-12)
})

test_that("H1 regressions, fitness-equivalence metrics (04-07), and H2 MSA-SEMs (09) are complete and well-formed", {
  skip_if_not(file.exists(out("H2_Mediation_Summary.csv")), "pipeline outputs not present")

  bm <- read.csv(out("H1_AllometricBenchmark.csv"))
  expect_equal(nrow(bm), 5)
  expect_equal(bm$n[bm$Region == "All"], 224)

  h1_sig <- read.csv(out("H1_Signed_Model.csv"))
  expect_true("disturbedcover" %in% h1_sig$Term)
  expect_lt(h1_sig$Estimate[h1_sig$Term == "disturbedcover"], 0)

  cats <- read.csv(out("CatsOutput.csv"))
  expect_equal(nrow(cats), 224)
  expect_true(all(cats$CATS_nb >= 0 & cats$CATS_nb <= 1))
  expect_true(all(cats$CATS_pois >= 0 & cats$CATS_pois <= 1))

  expect_equal(nrow(read.csv(out("NonNeutralTaxaTable.csv"))), 34)
  nn_prop <- read.csv(out("NonNeutralProportions.csv"))
  expect_equal(nrow(nn_prop), 224)
  expect_true(all(nn_prop$proportionHFLA == 0))
  expect_lt(max(abs(nn_prop$proportionNonNeutral - nn_prop$proportionLFHA)), 1e-12)

  expect_equal(nrow(read.csv(out("ForestCoverSubgroupSloanFits.csv"))), 43)

  h2_sum <- read.csv(out("H2_Mediation_Summary.csv"))
  expect_equal(nrow(h2_sum), 10)
  w_sums <- h2_sum$w_Partial + h2_sum$w_Full + h2_sum$w_Coresponse + h2_sum$w_Independent +
            h2_sum$w_X_to_M_Only + h2_sum$w_M_to_Y_Only + h2_sum$w_Direct_Only + h2_sum$w_Null
  expect_lt(max(abs(w_sums - 1)), 1e-8)
  expect_true(all(h2_sum$RelImp_ind >= 0 & h2_sum$RelImp_ind <= 1))
})

test_that("3-run BIOMASS channel decomposition outputs (11) satisfy exact OLS and SEM additivity", {
  skip_if_not(file.exists(out("BiomassDecomposition_SEM_Summary.csv")), "pipeline outputs not present")

  nowd <- read.csv(out("BiomassDecomposition_NoWD_Cats.csv"))
  expect_equal(nrow(nowd), 224)
  expect_true(all(is.finite(nowd$CATS_nb_noWD)))

  ols <- read.csv(out("BiomassDecomposition_OLS.csv"))
  ols_check <- ols |>
    filter(Hypothesis %in% c("H1", "H2a", "H2a_noWD", "H2b", "H2c_mean")) |>
    group_by(Hypothesis) |>
    summarise(err = abs(sum(Estimate[Channel != "Total"]) - Estimate[Channel == "Total"]), .groups = "drop")
  expect_lt(max(ols_check$err), 1e-12)

  sem <- read.csv(out("BiomassDecomposition_SEM_Summary.csv"))
  sem_check <- sem |>
    filter(Hypothesis %in% c("H2a", "H2a_noWD", "H2b", "H2c_mean")) |>
    group_by(Hypothesis) |>
    summarise(
      ind_err = abs(sum(ind_part_est[Channel != "Total"]) - ind_part_est[Channel == "Total"]),
      c_err   = abs(sum(c_part_est[Channel != "Total"])   - c_part_est[Channel == "Total"]),
      .groups = "drop"
    )
  expect_lt(max(sem_check$ind_err), 1e-10)
  expect_lt(max(sem_check$c_err), 1e-10)
})

test_that("all 11 publication figures exist in Figures/ and no stray Rplots.pdf exists", {
  skip_if_not(file.exists(fig("Figure_BiomassDecomposition.png")), "pipeline figures not present")

  expected_figs <- c(
    "FigureS1.png",
    "Figure_BiomassCV.png",
    "Figure_H1.png",
    "Figure_H1_absolutesupplement.png",
    "Figure_FitnessEquivalence.png",
    "Figure_RegionalSloan_supplement.png",
    "Figure_traitsOfNonneutrals.png",
    "Figure_sloansubsets_supplement.png",
    "Figure_H2_SEMs.png",
    "Figure_H2_SEMs_absolutesupplement.png",
    "Figure_BiomassDecomposition.png"
  )
  for (f in expected_figs) {
    p <- fig(f)
    expect_true(file.exists(p), info = f)
    expect_gt(file.info(p)$size, 10000)
  }
  expect_false(file.exists(file.path(PROJECT_ROOT, "Rplots.pdf")))
})
