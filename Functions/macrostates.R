# --------------------------------------------
# Purpose: Organism-level metabolic rate (e) and mass (m) under six macrostate scalings,
#          and plot-level macrostates (S, N, E, B). Single source of truth for 02 and 12.
# ---------------------------------------------------

#' Centre species wood density so the stem-weighted mean across all stems is exactly 1
center_wd <- function(wd) {
  if (anyNA(wd)) stop("Wood density contains NA; every stem needs a WD value.")
  wd - (mean(wd) - 1)
}

#' Chave et al. (2014, eq. 7) above-ground mass, up to a constant factor (constants cancel on rescaling).
#' dbh_cm must be in centimetres: the quadratic log term is not scale invariant.
chave_mass <- function(dbh_cm, wd) wd^0.976 * exp(2.673 * log(dbh_cm) - 0.0299 * log(dbh_cm)^2)

# Each scaling takes a tibble with DBH (cm) and WD and adds e (min 1 over all stems) and m.
SCALINGS <- list(
  harte_nowd = function(d) dplyr::mutate(d, e = DBH / min(DBH), m = e^(4/3)),
  harte_wd   = function(d) dplyr::mutate(d, e = DBH / min(DBH), m = e^(4/3) * center_wd(WD)),
  quad_nowd  = function(d) dplyr::mutate(d, e = (DBH / min(DBH))^2, m = e^(4/3)),
  quad_wd    = function(d) dplyr::mutate(d, e = (DBH / min(DBH))^2, m = e^(4/3) * center_wd(WD)),
  mte_wd     = function(d) dplyr::mutate(d, m = (DBH / min(DBH))^(8/3) * center_wd(WD), e = m^(3/4)),
  chave      = function(d) {
    m_ref <- chave_mass(min(d$DBH), 1)                      # smallest stem at mean (centred) WD
    dplyr::mutate(d,
                  m = chave_mass(DBH, center_wd(WD)) / m_ref,
                  e = (chave_mass(DBH, 1) / m_ref)^(3/4))    # WD-free mass^(3/4), so min(e) = 1
  }
)

#' Organism-level e and m for one scaling
compute_organism_stats <- function(comm, traits, suffix) {
  if (!suffix %in% names(SCALINGS)) stop("Unknown scaling: ", suffix)
  d <- dplyr::left_join(comm, dplyr::select(traits, Binomial_correct, WD),
                        by = "Binomial_correct", relationship = "many-to-one")
  stopifnot(nrow(d) == nrow(comm))
  SCALINGS[[suffix]](d) |>
    dplyr::select(Region, Region_Plot, Binomial_correct, m, e, WD)
}

#' Plot-level macrostates
summarise_macrostates <- function(org) {
  org |>
    dplyr::group_by(Region, Region_Plot) |>
    dplyr::summarise(N = dplyr::n(),
                     S = dplyr::n_distinct(Binomial_correct),
                     B = sum(m),
                     E = sum(e),
                     .groups = "drop")
}
