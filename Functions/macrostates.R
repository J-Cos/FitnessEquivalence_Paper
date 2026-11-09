# --------------------------------------------
# Purpose: Organism-level metabolic rate (e) and aboveground biomass (b) via the BIOMASS package
#          (Réjou-Méchain et al. 2017, Methods Ecol. Evol. 8:1163-1167; Chave et al. 2014,
#          Global Change Biology 20:3177-3190), plus the exact 3-run decomposition of plot
#          biomass into pure size-structure, site bioclimatic height, and wood-density channels.
#
# Formulation (all dimensionless quantities expressed relative to a reference threshold-sized tree
# at DBH_min = 10 cm with fixed study-wide stem-mean wood density rho_ref and pooled study-mean
# bioclimatic stress E_clim,ref):
#   1. Metabolic rate from stem basal area:
#        e_i = (DBH_i / DBH_min)^2,   E = sum_i e_i     (e_i >= 1 for all stems with DBH_i >= DBH_min)
#   2. Aboveground biomass from BIOMASS::computeAGB (three runs, switching off one source of
#      variation at a time):
#        - agb_Mg:      actual species WD (Outputs/traits.csv), plot coordinates (lon, lat)
#        - agb_rho_Mg:  fixed reference rho_ref,                plot coordinates (lon, lat)
#        - agb_size_Mg: fixed reference rho_ref,                pooled study-wide E_clim,ref
#      Dividing each run by agb_ref_Mg (the AGB [Mg] of a DBH_min = 10 cm tree at rho_ref and
#      E_clim,ref) gives b_i, b_rho,i, and b_size,i (with b_size = 1 at DBH = DBH_min).
#   3. Exact multiplicative plot-level telescoping split (yielding dev = w_rho + w_H + r_size):
#        B / B_pred = (B / B_rho) * (B_rho / B_size) * (B_size / B_pred)
#      with W_rho = B / B_rho, W_H = B_rho / B_size, and r_size = ln(B_size / B_pred).
# ---------------------------------------------------

#' Build a validated plot-coordinate lookup (Region, Region_Plot, lon, lat).
#' Jurua plots are absent from Data/env.csv and receive the regional centroid from config.R.
prepare_plot_coords <- function(comm, env, jurua_centroid = JURUA_CENTROID) {
  env_coords <- env |>
    dplyr::select(Region, Region_Plot, lon = X, lat = Y)
  if (anyDuplicated(env_coords$Region_Plot)) stop("Duplicate Region_Plot in env table.")

  plots <- comm |>
    dplyr::distinct(Region, Region_Plot) |>
    dplyr::left_join(env_coords, by = c("Region", "Region_Plot"), relationship = "one-to-one") |>
    dplyr::mutate(
      lon = dplyr::if_else(Region == "Jurua" & is.na(lon), jurua_centroid$lon, lon),
      lat = dplyr::if_else(Region == "Jurua" & is.na(lat), jurua_centroid$lat, lat)
    )

  if (any(!is.finite(plots$lon) | !is.finite(plots$lat))) {
    missing_plots <- plots$Region_Plot[!is.finite(plots$lon) | !is.finite(plots$lat)]
    stop("Missing or non-finite coordinates (lon, lat) for plots: ",
         paste(missing_plots, collapse = ", "))
  }
  plots
}

#' Compute stem-level metabolic rate (e) and 3-run BIOMASS aboveground biomass (b, b_rho, b_size)
compute_organism_stats <- function(comm, traits, env, dbh_min = DBH_MIN_CM) {
  if (length(dbh_min) != 1 || !is.finite(dbh_min) || dbh_min <= 0) {
    stop("dbh_min must be a single positive number (cm).")
  }

  stems  <- dplyr::select(comm, -dplyr::any_of("X"))
  coords <- prepare_plot_coords(stems, env)

  d <- stems |>
    dplyr::mutate(.orig_row = dplyr::row_number()) |>
    dplyr::left_join(dplyr::select(traits, Binomial_correct, WD),
                     by = "Binomial_correct", relationship = "many-to-one") |>
    dplyr::left_join(coords, by = c("Region", "Region_Plot"), relationship = "many-to-one")
  stopifnot(nrow(d) == nrow(stems))

  if (anyNA(d$DBH) || any(d$DBH < dbh_min)) {
    stop("Every stem must have finite DBH >= dbh_min (", dbh_min, " cm) so METE's bound e >= 1 holds.")
  }
  if (anyNA(d$WD) || any(d$WD <= 0)) {
    stop("Wood density contains NA or non-positive values; every stem needs WD > 0 (g cm^-3).")
  }

  # CRAN BIOMASS::computeE() (v2.2.7.1) joins coord[coord_unique, on = c("long", "lat")], which
  # groups the returned E vector by unique coordinates instead of preserving input row order when
  # duplicate coordinates are non-contiguous. Ordering stems into contiguous (lon, lat) blocks
  # before calling computeAGB() and restoring .orig_row afterwards guarantees exact row alignment.
  coord_key <- paste(d$lon, d$lat, sep = "_")
  d$.coord_id <- match(coord_key, unique(coord_key))
  d <- dplyr::arrange(d, .coord_id, .orig_row)

  rho_ref <- mean(d$WD)
  stem_xy <- cbind(d$lon, d$lat)

  # Run 1: actual species WD + site coordinates
  agb_Mg <- BIOMASS::computeAGB(D = d$DBH, WD = d$WD, coord = stem_xy)
  # Run 2: fixed reference rho_ref + site coordinates
  agb_rho_Mg <- BIOMASS::computeAGB(D = d$DBH, WD = rep(rho_ref, nrow(d)), coord = stem_xy)

  # Threshold-tree AGB [Mg] at each unique coordinate pair, mapped back to stems.
  # Because site bioclimate enters Chave's pantropical equation multiplicatively, the geometric mean
  # across all stems equals AGB(DBH_min, rho_ref, E_clim,ref = mean(E_clim)), and rescaling agb_rho_Mg
  # by agb_ref_Mg / agb_10_site_Mg yields Run 3 (fixed rho_ref, pooled study-wide E_clim,ref) without
  # hardcoding internal allometric coefficients.
  uniq_xy <- dplyr::distinct(d, .coord_id, lon, lat)
  uniq_xy$agb_10_site_Mg <- BIOMASS::computeAGB(
    D     = rep(dbh_min, nrow(uniq_xy)),
    WD    = rep(rho_ref, nrow(uniq_xy)),
    coord = cbind(uniq_xy$lon, uniq_xy$lat)
  )
  d <- dplyr::left_join(d, dplyr::select(uniq_xy, .coord_id, agb_10_site_Mg),
                        by = ".coord_id", relationship = "many-to-one")

  agb_ref_Mg  <- exp(mean(log(d$agb_10_site_Mg)))
  agb_size_Mg <- agb_rho_Mg * (agb_ref_Mg / d$agb_10_site_Mg)
  e_val       <- (d$DBH / dbh_min)^2
  if (min(e_val) < 1 - 1e-12) stop("METE lower bound violated: min(e) = ", min(e_val))

  d |>
    dplyr::mutate(
      e           = e_val,
      b           = agb_Mg / agb_ref_Mg,
      b_rho       = agb_rho_Mg / agb_ref_Mg,
      b_size      = agb_size_Mg / agb_ref_Mg,
      agb_Mg      = agb_Mg,
      agb_rho_Mg  = agb_rho_Mg,
      agb_size_Mg = agb_size_Mg,
      agb_ref_Mg  = agb_ref_Mg,
      rho_ref     = rho_ref
    ) |>
    dplyr::arrange(.orig_row) |>
    dplyr::select(
      Region, Region_Plot, Binomial_correct, DBH, WD,
      e, b, b_rho, b_size, agb_Mg, agb_rho_Mg, agb_size_Mg, agb_ref_Mg, rho_ref
    )
}

#' Summarise stem-level quantities to plot-level macrostates (S, N, E, B) and 3-run split factors
summarise_macrostates <- function(org) {
  org |>
    dplyr::group_by(Region, Region_Plot) |>
    dplyr::summarise(
      N          = dplyr::n(),
      S          = dplyr::n_distinct(Binomial_correct),
      E          = sum(e),
      B          = sum(b),
      B_rho      = sum(b_rho),
      B_size     = sum(b_size),
      Q_obs      = (B / N) / ((E / N)^(4 / 3)),
      Q_size     = (B_size / N) / ((E / N)^(4 / 3)),
      W_H        = B_rho / B_size,
      W_rho      = B / B_rho,
      AGB_Mg     = sum(agb_Mg),
      AGB_ref_Mg = dplyr::first(agb_ref_Mg),
      rho_ref    = dplyr::first(rho_ref),
      .groups    = "drop"
    )
}

#' Estimate plot-level biomass uncertainty (CV = SD / mean) via BIOMASS::AGBmonteCarlo,
#' propagating wood-density, bioclimatic-height, and allometric error (no DBH measurement error).
compute_biomass_mc_cv <- function(comm, traits, env, n_sim = 1000, seed = 1) {
  stems  <- dplyr::select(comm, -dplyr::any_of("X"))
  coords <- prepare_plot_coords(stems, env)

  d <- stems |>
    dplyr::left_join(dplyr::select(traits, Binomial_correct, WD),
                     by = "Binomial_correct", relationship = "many-to-one") |>
    dplyr::left_join(coords, by = c("Region", "Region_Plot"), relationship = "many-to-one")
  stopifnot(nrow(d) == nrow(stems))

  if (anyNA(d$WD) || any(d$WD <= 0)) {
    stop("Wood density contains NA or non-positive values; every stem needs WD > 0 (g cm^-3).")
  }

  # Taxonomic wood-density SD prior (species / genus / family / stand) from BIOMASS::getWoodDensity
  sp_parts <- strsplit(as.character(d$Binomial_correct), "[ _]+")
  genus    <- vapply(sp_parts, `[`, character(1), 1)
  species  <- vapply(sp_parts, function(x) if (length(x) >= 2) x[2] else "sp", character(1))
  fam_vec  <- if ("family" %in% names(d)) d$family else NULL
  err_wd   <- BIOMASS::getWoodDensity(
    genus   = genus,
    species = species,
    stand   = d$Region_Plot,
    family  = fam_vec,
    verbose = FALSE
  )$sdWD

  if (!is.null(seed)) set.seed(seed)
  mc <- BIOMASS::AGBmonteCarlo(
    D       = d$DBH,
    WD      = d$WD,
    errWD   = err_wd,
    coord   = cbind(d$lon, d$lat),
    Dpropag = NULL,   # propagate WD + bioclimatic height + allometric error only (no diameter error)
    n       = n_sim
  )

  plot_simu <- rowsum(mc$AGB_simu, d$Region_Plot, reorder = FALSE)
  mc_mean   <- rowMeans(plot_simu)
  mc_sd     <- apply(plot_simu, 1, stats::sd)

  tibble::tibble(
    Region_Plot    = rownames(plot_simu),
    AGB_mc_mean_Mg = unname(mc_mean),
    AGB_mc_sd_Mg   = unname(mc_sd),
    B_cv           = unname(mc_sd / mc_mean)
  )
}
