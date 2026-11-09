write_fixture <- function(df) { f <- tempfile(fileext = ".csv"); write.csv(df, f, row.names = FALSE); f }

preds_fixture <- tibble(Region = c("Jurua", "Balbina", "STM", "PGM"),
                        Region_Plot = c("J1", "B1", "S1", "P1"),
                        N = 10, S = 5, B = c(20, 21, 22, 23), E = 15, B_pred = 20,
                        dev = c(0.01, -0.05, 0.1, -0.2), B_cv = c(0.08, 0.12, 0.10, 0.09))
env_fixture <- tibble(Region = c("Balbina", "STM", "PGM"), Region_Plot = c("B1", "S1", "P1"),
                      fc2000 = c(0.9, 0.7, 0.6))
cats_fixture <- tibble(Region_Plot = c("J1", "B1", "S1", "P1"),
                       CATS_nb = c(0.03, 0.06, 0.08, 0.14),
                       CATS_pois = c(0.05, 0.1, 0.12, 0.2))

test_that("plot data: Jurua has zero forest loss, dummies, absdev and sigma_allom are correct", {
  d <- load_and_prepare_plot_data(preds_file = write_fixture(preds_fixture), env_file = write_fixture(env_fixture),
                                  cats_file = write_fixture(cats_fixture), need = "cats")
  expect_equal(d$disturbedcover[d$Region == "Jurua"], 0)
  expect_equal(d$disturbedcover[d$Region == "PGM"], 0.4)
  expect_equal(levels(d$Region), REGIONS)
  expect_equal(d$Reg_STM, as.numeric(d$Region == "STM"))
  expect_equal(d$absdev, abs(d$dev))
  expect_equal(d$sigma_allom, sqrt(log1p(preds_fixture$B_cv^2)))
  expect_equal(d$CATS_nb, cats_fixture$CATS_nb)
  expect_equal(d$CATS_pois, cats_fixture$CATS_pois)
})

test_that("plot data: missing required files and incomplete joins fail loudly", {
  pf <- write_fixture(preds_fixture); ef <- write_fixture(env_fixture)
  expect_error(load_and_prepare_plot_data(preds_file = pf, env_file = ef, cats_file = "nope.csv", need = "cats"),
               "Required file not found")
  expect_error(load_and_prepare_plot_data(preds_file = pf, env_file = ef,
                                          cats_file = write_fixture(cats_fixture[-2, ]), need = "cats"),
               "NA values")
  expect_error(load_and_prepare_plot_data(preds_file = write_fixture(rbind(preds_fixture, preds_fixture[1, ])),
                                          env_file = ef), "Duplicate")
})

test_that("subgroup EEoS summaries compute medians, means, and exact additive channel means", {
  preds_chan <- preds_fixture |>
    mutate(
      w_rho  = c(0.02, -0.03, 0.06, -0.12),
      w_H    = c(0.00, -0.01, 0.01, -0.02),
      r_size = dev - w_rho - w_H
    )
  subsets <- list(Jurua1 = "J1", Balbina1 = c("B1"), STM1 = c("S1", "P1"))
  s <- summarise_subgroup_eeos(subsets, preds_chan, env_fixture)
  expect_equal(s$subgroup, names(subsets))
  expect_true(is.na(s$fcmedian[1]))                       # Jurua absent from env
  expect_equal(s$fcmedian[3], median(c(0.7, 0.6)))
  expect_equal(s$RMSE[3], sqrt(mean(c(0.1, -0.2)^2)))
  expect_equal(s$devmedian[3], median(c(0.1, -0.2)))
  expect_equal(s$dev_mean, s$w_rho_mean + s$w_H_mean + s$r_size_mean, tolerance = 1e-14)
})

test_that("raw stem and trait harmonisation fixes typos, filters DBH >= 10, and merges Jurua medians", {
  expect_equal(
    fix_taxon_names(c(" Copaifera guyanensis ", "Licaria rodriguesii", "Eschweilera coriacea")),
    c("Copaifera guianensis", "Licania rodriguesii", "Eschweilera coriacea")
  )
  expect_equal(hamming_distance("Licania", "Licaria"), 1)
  expect_equal(hamming_distance("short", "longer"), Inf)
  expect_setequal(
    strings_with_n_diff(c("Licania", "Licaria", "Protium"), 1),
    c("Licania", "Licaria")
  )

  pinho_raw <- tibble(
    X                = 1:3,
    Region           = c("PGM", "STM", "OtherReg"),
    Region_Plot      = c("P1", "S1", "O1"),
    Binomial_correct = c("Copaifera guyanensis", "Licania rodriguesii", "Other sp"),
    Genus            = c("Copaifera", "Licania", "Other"),
    family           = c("Fabaceae", "Chrysobalanaceae", "Other"),
    DBH              = c(15.0, 22.5, 30.0)
  )
  jurua_raw <- tibble(
    parcel  = c("J1", "J1", "J2"),
    species = c("Licaria macrophylla", "Jurua endemic", "Jurua endemic"),
    genus   = c("Licania", "Jurua", "Jurua"),
    family  = c("Chrysobalanaceae", "Rubiaceae", "Rubiaceae"),
    DBH     = c(9.5, 12.0, 28.0),    # first stem < 10 cm must be excluded
    WD      = c(0.80, 0.60, 0.70),
    SM      = c(10.0, exp(2.0), exp(4.0)),
    LMA     = c(80.0, 70.0, 90.0)
  )

  stems <- harmonise_stem_data(pinho_raw, jurua_raw, dbh_min = 10)
  expect_equal(nrow(stems), 4)
  expect_equal(stems$X, 1:4)
  expect_true(all(stems$DBH >= 10))
  expect_false("OtherReg" %in% stems$Region)
  expect_true("Copaifera guianensis" %in% stems$Binomial_correct)

  pinho_traits <- tibble(
    Binomial_correct = c("Copaifera guianensis", "Licania rodriguesii", "Licaria rodriguesii"),
    WD               = c(0.62, 0.85, 0.55),
    lnSM             = c(5.1, 6.2, 4.0),
    LMA              = c(72, 88, 65),
    Hmax             = c(35, 28, 20),
    DS               = c("EndoZoo", "EndoZoo", "Anemo")
  )
  tr <- harmonise_trait_table(pinho_traits, jurua_raw, dbh_min = 10)
  expect_false("Licaria rodriguesii" %in% tr$Binomial_correct)
  expect_false("Licania macrophylla" %in% tr$Binomial_correct)  # only occurred on DBH = 9.5 stem
  expect_true(!anyDuplicated(tr$Binomial_correct))
  j_row <- filter(tr, Binomial_correct == "Jurua endemic")
  expect_equal(j_row$WD, 0.65)
  expect_equal(j_row$lnSM, 3.0)
  expect_equal(j_row$LMA, 80.0)
  expect_true(is.na(j_row$Hmax) && is.na(j_row$DS))
})
