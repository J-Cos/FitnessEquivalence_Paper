# Loaded automatically by testthat before the tests (working directory = Code/tests/testthat).
suppressMessages({library(dplyr); library(tidyr); library(tibble); library(purrr)})
PROJECT_ROOT <- normalizePath(file.path(getwd(), "..", "..", ".."))
for (f in c("config", "macrostates", "data_prep", "regression_helpers",
            "sem_helpers", "plot_helpers", "sloan_helpers", "cats_helpers")) {
  source(file.path(PROJECT_ROOT, "Code", "Functions", paste0(f, ".R")))
}
options(BIOMASS.cache = normalizePath(file.path(PROJECT_ROOT, BIOMASS_CACHE_DIR), mustWork = FALSE))

# Toy community: 3 plots, 8 stems, 4 species (DBH in cm, minimum 10)
toy_comm <- tibble(
  Region = c("Jurua", "Jurua", "Jurua", "PGM", "PGM", "PGM", "STM", "STM"),
  Region_Plot = c("J1", "J1", "J1", "P1", "P1", "P1", "S1", "S1"),
  Binomial_correct = c("a", "b", "b", "a", "c", "c", "d", "a"),
  DBH = c(10, 20, 35, 12, 50, 15, 80, 10)
)
toy_traits <- tibble(Binomial_correct = c("a", "b", "c", "d"), WD = c(0.5, 0.7, 0.4, 0.9))
# Jurua plot J1 intentionally absent, matching Data/env.csv
toy_env <- tibble(
  Region = c("PGM", "STM"),
  Region_Plot = c("P1", "S1"),
  X = c(-47.5, -54.9),
  Y = c(-3.0, -2.9)
)
