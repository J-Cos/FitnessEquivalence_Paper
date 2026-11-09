# --------------------------------------------
# Purpose: Run the full analysis pipeline end to end, each step in a fresh process.
# Usage (from the project root, the folder containing Data/ and Code/):
#   Rscript Code/run_all.R            # all steps (01 through 12)
#   Rscript Code/run_all.R 04 10      # only the steps whose script number is listed
# Output: everything in Outputs/ and Figures/; per-step logs and versions in Outputs/logs/
# ---------------------------------------------------

source("Code/Functions/config.R"); check_project_root()
Sys.setenv(PYTHONNOUSERSITE = "1")   # keep ~/.local packages out of the pinned conda environment
dir.create("Outputs/logs", recursive = TRUE, showWarnings = FALSE)
PY <- get_python()

steps <- list(
  c("Rscript", "Code/01_CombinePinhoJuruaData.R"),
  c("Rscript", "Code/02_GetEcosystemMacrostates.R"),
  c("Rscript", "Code/02b_MakeMacrostateScatterplots.R"),
  c(PY,        "Code/03_FitEEOS.py"),
  c("Rscript", "Code/04_FitSimpleEeosDevRegression.R"),
  c("Rscript", "Code/05_FitCatsRegressions.R"),
  c("Rscript", "Code/06_FitRegionalSloanNonNeutral.R"),
  c("Rscript", "Code/07_FitSubgroupSloans.R"),
  c("Rscript", "Code/08_PlotFitnessEquivalenceMeasures.R"),
  c("Rscript", "Code/09_FitMediationSEMs.R"),
  c("Rscript", "Code/10_PlotMediationSEMs.R"),
  c("Rscript", "Code/11_FitBiomassDecomposition.R"),
  c("Rscript", "Code/12_PlotBiomassDecomposition.R")
)

only <- commandArgs(trailingOnly = TRUE)
t0 <- Sys.time()
for (step in steps) {
  script <- basename(step[2])
  key <- sub("_.*", "", script)
  if (length(only) && !key %in% only) next
  log_file <- file.path("Outputs/logs", paste0(tools::file_path_sans_ext(script), ".log"))
  message(format(Sys.time(), "%H:%M:%S"), "  ", paste(c(script, step[-(1:2)]), collapse = " "))
  status <- system2(step[1], step[-1], stdout = log_file, stderr = log_file)
  if (status != 0) stop("Step failed: ", script, " (exit ", status, "); see ", log_file)
}
if (file.exists("Rplots.pdf")) warning("A script wrote Rplots.pdf (an unsaved base-graphics plot).")

writeLines(capture.output(sessionInfo()), "Outputs/logs/R_sessionInfo.txt")
system2(
  PY,
  c("-c", shQuote(paste(
    "import sys, numpy, scipy, pandas;",
    "print(sys.version);",
    "print('numpy', numpy.__version__);",
    "print('scipy', scipy.__version__);",
    "print('pandas', pandas.__version__)"
  ))),
  stdout = "Outputs/logs/python_versions.txt"
)
message(sprintf("Pipeline finished in %.1f min.", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
