# --------------------------------------------
# Purpose: Golden-master check. Compares every CSV/RDS output of the frozen baseline
#          (Archive/Baseline_3e9b007_Outputs, produced by the pre-refactor code) with the
#          current Outputs/. Used to prove that behaviour-preserving refactors change nothing.
# Usage (from project root): Rscript Code/tests/compare_to_baseline.R [baseline_dir]
# Exit status: 0 if all compared files match, 1 otherwise.
# ---------------------------------------------------

suppressMessages(library(tidyverse))
source("Code/Functions/config.R")
source("Code/Functions/data_prep.R")

args <- commandArgs(trailingOnly = TRUE)
BASE <- if (length(args)) args[1] else "Archive/Baseline_3e9b007_Outputs"
CUR  <- "Outputs"
TOL_DEFAULT <- 1e-8
TOL_EEOS    <- 1e-6   # numerical integration outputs (Python env may differ)
ABS_ZERO    <- 1e-6   # |values| below this in both files are treated as zero
EEOS_COLS   <- c("B_pred", "dev", "absdev", "RMSE", "devmedian", "R2")

# baseline name -> current name
rename_file <- function(f) {
  f |>
    sub("^MacrostatesHarteWD_(.*)\\.csv$", "Macrostates_\\1.csv", x = _) |>
    sub("^EEOSPredsHarteWD_(.*)\\.csv$", "EEOSPreds_\\1.csv", x = _)
}
SKIP <- c("^MacrostatesHarteWD\\.csv$", "^EEOSPredsHarteWD\\.csv$", "^ForestCoverSubgroupSloanFits\\.csv$",
          "^SensitivityAnalysisResults\\.csv$", "^H1_ANCOVA_InteractionTest_harte_wd\\.csv$",
          "_chave\\.csv$", "_sens\\.csv$", "^Old", "^logs", "^08b_stdout",
          # replaced by the subgroup check below
          "^ForestCoverSubgroupSloanFits_harte_wd\\.csv$")
# Optional extra skips via env var (regex, comma-separated), for files a Phase 3 step changes on purpose
extra <- Sys.getenv("COMPARE_SKIP"); if (nzchar(extra)) SKIP <- c(SKIP, strsplit(extra, ",")[[1]])

# Row keys used to align rows before comparing (only where row order is not meaningful)
ROW_KEYS <- list("^NonNeutralTaxa\\.csv$" = "sp", "^NonNeutralTaxaTable\\.csv$" = "Binomial",
                 "^NonNeutralProportions\\.csv$" = "Region_Plot", "^CatsOutput\\.csv$" = "Region_Plot",
                 "^(Macrostates|EEOSPreds)_" = "Region_Plot")

compare_frames <- function(a, b, name) {
  a <- as.data.frame(a); b <- as.data.frame(b)
  a$X <- NULL; b$X <- NULL
  key <- ROW_KEYS[vapply(names(ROW_KEYS), grepl, logical(1), x = name)]
  if (length(key)) {
    k <- key[[1]]
    if (!all(k %in% names(a)) || !all(k %in% names(b))) return(paste("missing key", k))
    if (!setequal(a[[k]], b[[k]])) return(sprintf("key sets differ (%d vs %d)", nrow(a), nrow(b)))
    a <- a[order(a[[k]]), , drop = FALSE]; b <- b[order(b[[k]]), , drop = FALSE]
  }
  if (nrow(a) != nrow(b)) return(sprintf("rows %d vs %d", nrow(a), nrow(b)))
  missing <- setdiff(names(a), names(b))
  if (length(missing)) return(paste("columns missing in current:", paste(missing, collapse = ",")))
  msgs <- character(0)
  for (col in names(a)) {
    x <- a[[col]]; y <- b[[col]]
    if (is.numeric(x) && is.numeric(y)) {
      tol <- if (col %in% EEOS_COLS) TOL_EEOS else TOL_DEFAULT
      # values that are zero up to optimizer noise (e.g. SRMR of saturated SEMs) count as equal
      near_zero <- !is.na(x) & !is.na(y) & abs(x) < ABS_ZERO & abs(y) < ABS_ZERO
      eq <- all.equal(unname(x[!near_zero]), unname(y[!near_zero]), tolerance = tol)
      if (!isTRUE(eq)) msgs <- c(msgs, sprintf("%s: %s", col, eq[1]))
    } else if (!identical(as.character(x), as.character(y))) {
      msgs <- c(msgs, sprintf("%s: values differ (first: '%s' vs '%s')", col,
                              as.character(x)[which(as.character(x) != as.character(y))[1]],
                              as.character(y)[which(as.character(x) != as.character(y))[1]]))
    }
  }
  if (length(msgs)) paste(msgs, collapse = " | ") else "PASS"
}

results <- list()
for (f in list.files(BASE)) {
  if (dir.exists(file.path(BASE, f)) || any(vapply(SKIP, grepl, logical(1), x = f))) next
  g <- rename_file(f)
  cur_file <- file.path(CUR, g)
  if (!file.exists(cur_file)) { results[[f]] <- "MISSING in current Outputs/"; next }
  if (grepl("\\.csv$", f)) {
    results[[f]] <- compare_frames(read.csv(file.path(BASE, f), check.names = FALSE),
                                   read.csv(cur_file, check.names = FALSE), g)
  } else if (grepl("\\.RDS$", f, ignore.case = TRUE)) {
    a <- readRDS(file.path(BASE, f)); b <- readRDS(cur_file)
    if (is.data.frame(a)) {
      results[[f]] <- compare_frames(a, b[, intersect(names(a), names(b)), drop = FALSE], g)
    } else {
      eq <- all.equal(a, b, tolerance = TOL_DEFAULT, check.attributes = FALSE)
      results[[f]] <- if (isTRUE(eq)) "PASS" else paste(eq, collapse = " | ")
    }
  }
}

# H2c subgroup table: legacy per-suffix file vs suffix-free fits + EEoS summaries
legacy_sub <- file.path(BASE, "ForestCoverSubgroupSloanFits_harte_wd.csv")
if (file.exists(legacy_sub) && !any(vapply(SKIP, grepl, logical(1), x = "SUBGROUP_CHECK"))) {
  res <- tryCatch({
    old <- read.csv(legacy_sub) |> dplyr::rename(subgroup = seed) |> dplyr::select(-dAIC)
    fits <- read.csv(out_path("ForestCoverSubgroupSloanFits"))
    eeos <- summarise_subgroup_eeos(readRDS(out_path("NonOverlappingOrderedPlotSubsets", ext = "RDS")),
                                    read.csv(out_path("EEOSPreds", MAIN_SUFFIX)), read.csv("Data/env.csv"))
    new <- dplyr::left_join(fits, eeos, by = "subgroup") |>
      dplyr::mutate(fcmedian = dplyr::if_else(is.na(fcmedian), 1, fcmedian))
    compare_frames(old[, intersect(names(old), names(new))], new[, intersect(names(old), names(new))],
                   "ForestCoverSubgroupSloanFits")
  }, error = function(e) paste("ERROR:", conditionMessage(e)))
  results[["ForestCoverSubgroupSloanFits_harte_wd.csv (rebuilt)"]] <- res
}

out <- tibble(file = names(results), status = unlist(results))
out$ok <- out$status == "PASS"
print(as.data.frame(out[, c("ok", "file", "status")]), right = FALSE, row.names = FALSE)
cat(sprintf("\n%d/%d files match the baseline.\n", sum(out$ok), nrow(out)))
if (!all(out$ok)) quit(status = 1)
