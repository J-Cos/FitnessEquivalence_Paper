# --------------------------------------------
# Purpose: Single source of project-wide constants, paths and small utilities.
#          Every pipeline script sources this first and calls check_project_root().
# ---------------------------------------------------

REGIONS        <- c("Jurua", "Balbina", "STM", "PGM")   # factor order; Jurua (undisturbed) = reference
REGION_ABBR    <- c(Jur = "Jurua", Bal = "Balbina", STM = "STM", PGM = "PGM")
REGION_DUMMIES <- c("Reg_Balbina", "Reg_STM", "Reg_PGM")

MAIN_SUFFIX      <- "harte_wd"
SCALING_SUFFIXES <- c("harte_nowd", "harte_wd", "quad_nowd", "quad_wd", "mte_wd", "chave")
SCALING_LABELS   <- c(harte_nowd = "Option 1: Linear, No WD",
                      harte_wd   = "Option 2: Linear, With WD (Original)",
                      quad_nowd  = "Option 3: Quadratic, No WD",
                      quad_wd    = "Option 4: Quadratic, With WD (Metab-First)",
                      mte_wd     = "Option 5: Biomass-First Power Law (MTE)",
                      chave      = "Option 6: Chave et al. (2014) Allometry, With WD")

# Jurua plots are absent from Data/env.csv; centroid used only for the regional map (Figure 1A)
JURUA_CENTROID <- list(lon = -67.71, lat = -5.56, n = 30)

N_SUBSAMPLES   <- 100  # random plot subsamples per region (H2b)
SUBSAMPLE_SIZE <- 29   # one fewer than the smallest region (Jurua, 30 plots)
SUBGROUP_SIZE  <- 5    # plots per forest-cover-ordered subgroup (H2c)

#' Stop unless the working directory is the project root (contains Data/ and Code/)
check_project_root <- function() {
  if (!dir.exists("Data") || !dir.exists("Code")) {
    stop("Run from the project root (the folder containing Data/ and Code/). getwd() = ", getwd())
  }
  dir.create("Outputs", showWarnings = FALSE)
  dir.create("Figures", showWarnings = FALSE)
  invisible(TRUE)
}

#' Read the macrostate-scaling suffix from the command line
get_suffix_arg <- function(default = MAIN_SUFFIX) {
  args <- commandArgs(trailingOnly = TRUE)
  suffix <- if (length(args) > 0) args[1] else default
  if (!suffix %in% SCALING_SUFFIXES) stop("Unknown suffix '", suffix, "'. Use one of: ",
                                          paste(SCALING_SUFFIXES, collapse = ", "))
  suffix
}

#' Locate the Python interpreter for the EEoS fits (env var EEOS_PYTHON overrides)
get_python <- function() {
  candidates <- c(Sys.getenv("EEOS_PYTHON"),
                  "/home/j/miniconda3/envs/EEOS-env/bin/python",
                  unname(Sys.which("python3")))
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(candidates) == 0) stop("No Python interpreter found; set EEOS_PYTHON.")
  candidates[1]
}

#' Path to an output file: Outputs/<stem>[_<suffix>].<ext>
out_path <- function(stem, suffix = NULL, ext = "csv") {
  file.path("Outputs", paste0(stem, if (!is.null(suffix)) paste0("_", suffix), ".", ext))
}

#' Read a CSV that must exist, as a tibble
read_required_csv <- function(path) {
  if (!file.exists(path)) stop("Required file not found: ", path, " (has the upstream script been run?)")
  tibble::as_tibble(utils::read.csv(path))
}
