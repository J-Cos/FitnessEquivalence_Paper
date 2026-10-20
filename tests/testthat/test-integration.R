# Integration checks on the real pipeline outputs; skipped unless run_all.R has produced them.
out <- function(f) file.path(PROJECT_ROOT, "Outputs", f)

test_that("pipeline outputs have the expected dimensions", {
  skip_if_not(file.exists(out("EEOSPreds_harte_wd.csv")), "pipeline outputs not present")
  preds <- read.csv(out("EEOSPreds_harte_wd.csv"))
  expect_equal(nrow(preds), 224)
  expect_equal(sum(preds$N), 28199)
  expect_true(all(abs(preds$con_N) < 1e-3 & abs(preds$con_E) < 1e-3))
  expect_equal(nrow(read.csv(out("ForestCoverSubgroupSloanFits.csv"))), 43)
  expect_equal(nrow(read.csv(out("NonNeutralTaxaTable.csv"))), 34)
  expect_equal(nrow(read.csv(out("CatsOutput.csv"))), 224)
})

test_that("every scaling yields 224 plots with finite deviations", {
  skip_if_not(file.exists(out("EEOSPreds_chave.csv")), "pipeline outputs not present")
  for (s in SCALING_SUFFIXES) {
    p <- read.csv(out(sprintf("EEOSPreds_%s.csv", s)))
    expect_equal(nrow(p), 224, info = s)
    expect_true(all(is.finite(p$dev)), info = s)
  }
})
