# --------------------------------------------
# Purpose: Single source of project-wide constants, paths and small utilities.
#          Every pipeline script sources this first and calls check_project_root().
# ---------------------------------------------------

REGIONS        <- c("Jurua", "Balbina", "STM", "PGM")   # factor order; Jurua (undisturbed) = reference
REGION_ABBR    <- c(Jur = "Jurua", Bal = "Balbina", STM = "STM", PGM = "PGM")
REGION_DUMMIES <- c("Reg_Balbina", "Reg_STM", "Reg_PGM")

DBH_MIN_CM        <- 10.0   # census stem diameter threshold (cm); reference stem has e = 1
BIOMASS_CACHE_DIR <- "Data/BIOMASS_cache"

# Jurua plots are absent from Data/env.csv; regional centroid used for BIOMASS::computeAGB and Figure 1A
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
  dir.create(BIOMASS_CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
  options(BIOMASS.cache = normalizePath(BIOMASS_CACHE_DIR))
  invisible(TRUE)
}

#' Locate the Python interpreter for the EEoS fits (env var EEOS_PYTHON overrides)
get_python <- function(env_name = "EEOS-env") {
  conda_bin  <- unname(Sys.which("conda"))
  conda_root <- if (nzchar(conda_bin)) dirname(dirname(conda_bin)) else ""
  home_roots <- file.path(path.expand("~"), c("miniconda3", "miniforge3", "mambaforge", "anaconda3"))
  env_dirs   <- unique(c(
    if (basename(Sys.getenv("CONDA_PREFIX")) == env_name) Sys.getenv("CONDA_PREFIX"),
    if (nzchar(conda_root)) file.path(conda_root, "envs", env_name),
    file.path(home_roots, "envs", env_name)
  ))
  env_py <- unlist(lapply(env_dirs, function(d) {
    c(file.path(d, "bin", "python"), file.path(d, "python.exe"))
  }))

  candidates <- c(
    Sys.getenv("EEOS_PYTHON"),
    env_py,
    unname(Sys.which("python3")),
    unname(Sys.which("python"))
  )
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(candidates) == 0) stop("No Python interpreter found; activate ", env_name, " or set EEOS_PYTHON.")
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
