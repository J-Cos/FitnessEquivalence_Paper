write_fixture <- function(df) { f <- tempfile(fileext = ".csv"); write.csv(df, f, row.names = FALSE); f }

preds_fixture <- tibble(Region = c("Jurua", "Balbina", "STM", "PGM"),
                        Region_Plot = c("J1", "B1", "S1", "P1"),
                        N = 10, S = 5, B = c(20, 21, 22, 23), E = 15, B_pred = 20,
                        dev = c(0.01, -0.05, 0.1, -0.2))
env_fixture <- tibble(Region = c("Balbina", "STM", "PGM"), Region_Plot = c("B1", "S1", "P1"),
                      fc2000 = c(0.9, 0.7, 0.6))
cats_fixture <- tibble(Region_Plot = c("J1", "B1", "S1", "P1"), CATS_pois = c(0.05, 0.1, 0.12, 0.2))

test_that("plot data: Jurua has zero forest loss, dummies and absdev are correct", {
  d <- load_and_prepare_plot_data(preds_file = write_fixture(preds_fixture), env_file = write_fixture(env_fixture),
                                  cats_file = write_fixture(cats_fixture), need = "cats")
  expect_equal(d$disturbedcover[d$Region == "Jurua"], 0)
  expect_equal(d$disturbedcover[d$Region == "PGM"], 0.4)
  expect_equal(levels(d$Region), REGIONS)
  expect_equal(d$Reg_STM, as.numeric(d$Region == "STM"))
  expect_equal(d$absdev, abs(d$dev))
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

test_that("subgroup EEoS summaries are computed per subgroup", {
  subsets <- list(Jurua1 = "J1", Balbina1 = c("B1"), STM1 = c("S1", "P1"))
  s <- summarise_subgroup_eeos(subsets, preds_fixture, env_fixture)
  expect_equal(s$subgroup, names(subsets))
  expect_true(is.na(s$fcmedian[1]))                       # Jurua absent from env
  expect_equal(s$fcmedian[3], median(c(0.7, 0.6)))
  expect_equal(s$RMSE[3], sqrt(mean(c(0.1, -0.2)^2)))
  expect_equal(s$devmedian[3], median(c(0.1, -0.2)))
})
