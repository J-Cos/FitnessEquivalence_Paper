# --------------------------------------------
# Purpose: Harmonise raw stem and trait datasets (Pinho et al. + Scabin et al. Jurua) and
#          load/validate the plot-level and subgroup-level analysis tables
#          (EEoS deviation + forest cover + fitness-equivalence metrics).
# ---------------------------------------------------

# Taxonomic typos identified by inspecting near-identical names across Pinho and Jurua tables
NAME_FIXES <- c(
  "Copaifera guyanensis" = "Copaifera guianensis",
  "Licaria macrophylla"  = "Licania macrophylla",
  "Licaria rodriguesii"  = "Licania rodriguesii"
)

#' Trim whitespace and standardise known binomial spelling variants
fix_taxon_names <- function(x) {
  x <- stringr::str_trim(x)
  hit <- x %in% names(NAME_FIXES)
  x[hit] <- unname(NAME_FIXES[x[hit]])
  x
}
fix_names <- fix_taxon_names

#' Hamming distance between two equal-length strings (Inf if lengths differ)
hamming_distance <- function(a, b) {
  if (nchar(a) != nchar(b)) return(Inf)
  sum(strsplit(a, "")[[1]] != strsplit(b, "")[[1]])
}

#' Unique strings in `vec` that differ from another element by exactly `n` characters
strings_with_n_diff <- function(vec, n) {
  vec <- unique(vec)
  if (length(vec) < 2) return(character(0))
  combs <- expand.grid(a = vec, b = vec, stringsAsFactors = FALSE) |>
    dplyr::filter(a != b)
  combs$dist <- mapply(hamming_distance, combs$a, combs$b)
  unique(unlist(combs[combs$dist == n, c("a", "b")]))
}

#' Combine and harmonise Pinho (Balbina, PGM, STM) and Jurua stem census records
harmonise_stem_data <- function(pinho, jurua, dbh_min = DBH_MIN_CM) {
  pinho_sub <- pinho |>
    dplyr::filter(Region %in% c("Balbina", "PGM", "STM"), DBH >= dbh_min) |>
    dplyr::select(X, Region, Region_Plot, Binomial_correct, Genus, family, DBH)
  jurua_sub <- jurua |>
    dplyr::filter(DBH >= dbh_min) |>
    dplyr::transmute(
      X                = NA_integer_,
      Region           = "Jurua",
      Region_Plot      = parcel,
      Binomial_correct = species,
      Genus            = genus,
      family           = family,
      DBH              = DBH
    )
  dplyr::bind_rows(pinho_sub, jurua_sub) |>
    dplyr::mutate(
      X                = dplyr::row_number(),
      Binomial_correct = fix_taxon_names(Binomial_correct)
    )
}

#' Merge Pinho species trait table with median-summarised Jurua-only taxa (removing misspelt duplicate)
harmonise_trait_table <- function(pinho_traits, jurua, dbh_min = DBH_MIN_CM) {
  jurua_traits <- jurua |>
    dplyr::filter(DBH >= dbh_min) |>
    dplyr::transmute(
      Binomial_correct = fix_taxon_names(species),
      WD               = WD,
      lnSM             = log(SM),
      LMA              = LMA
    )
  traits_needed <- jurua_traits |>
    dplyr::filter(!Binomial_correct %in% pinho_traits$Binomial_correct) |>
    dplyr::group_by(Binomial_correct) |>
    dplyr::summarise(
      WD   = stats::median(WD),
      lnSM = stats::median(lnSM),
      LMA  = stats::median(LMA),
      Hmax = NA_real_,
      DS   = NA_character_,
      .groups = "drop"
    )
  traits_new <- pinho_traits |>
    dplyr::filter(Binomial_correct != "Licaria rodriguesii") |>
    dplyr::bind_rows(traits_needed)
  if (anyDuplicated(traits_new$Binomial_correct)) {
    stop("Duplicate Binomial_correct in harmonised trait table.")
  }
  traits_new
}

#' Plot-level table for H1 / H2a / H2b and the channel decomposition
#' @param need which fitness-equivalence metrics must be joined: "cats", "nonneutral"
load_and_prepare_plot_data <- function(need = character(0),
                                       preds_file = out_path("EEOSPreds"),
                                       env_file = "Data/env.csv",
                                       cats_file = out_path("CatsOutput"),
                                       non_neutral_file = out_path("NonNeutralProportions")) {
  preds <- read_required_csv(preds_file)
  env   <- read_required_csv(env_file)
  if (anyDuplicated(preds$Region_Plot)) stop("Duplicate Region_Plot in ", preds_file)

  plot_data <- dplyr::left_join(preds, env, by = c("Region", "Region_Plot"),
                                relationship = "one-to-one")

  if ("cats" %in% need) {
    cats <- read_required_csv(cats_file) |>
      dplyr::select(Region_Plot, dplyr::starts_with("CATS_"))
    plot_data <- dplyr::left_join(plot_data, cats, by = "Region_Plot", relationship = "one-to-one")
  }
  if ("nonneutral" %in% need) {
    nn <- read_required_csv(non_neutral_file) |> dplyr::select(Region_Plot, proportionNonNeutral)
    plot_data <- dplyr::left_join(plot_data, nn, by = "Region_Plot", relationship = "one-to-one")
  }

  plot_data <- plot_data |>
    dplyr::mutate(
      fc2000 = dplyr::if_else(Region == "Jurua", 1.0, fc2000),  # intact region: no forest loss, no noise
      disturbedcover = 1.0 - fc2000,
      absdev = abs(dev),
      Region = factor(Region, levels = REGIONS),
      Reg_Balbina = dplyr::if_else(Region == "Balbina", 1, 0),
      Reg_STM = dplyr::if_else(Region == "STM", 1, 0),
      Reg_PGM = dplyr::if_else(Region == "PGM", 1, 0)
    )
  if ("B_cv" %in% names(plot_data)) {
    plot_data <- dplyr::mutate(plot_data, sigma_allom = sqrt(log1p(B_cv^2)))
  }

  required <- c("dev", "disturbedcover", "Region",
                if ("cats" %in% need) c("CATS_nb", "CATS_pois"),
                if ("nonneutral" %in% need) "proportionNonNeutral")
  bad <- required[vapply(required, function(v) anyNA(plot_data[[v]]), logical(1))]
  if (length(bad)) stop("NA values after joins in: ", paste(bad, collapse = ", "))
  plot_data
}

#' Per-subgroup forest cover and EEoS summaries (H2c)
#' @param subsets named list of Region_Plot vectors (Outputs/NonOverlappingOrderedPlotSubsets.RDS)
summarise_subgroup_eeos <- function(subsets, preds, env) {
  has_channels <- all(c("r_size", "w_H", "w_rho") %in% names(preds))
  purrr::imap_dfr(subsets, function(plots, name) {
    p <- dplyr::filter(preds, Region_Plot %in% plots)
    e <- dplyr::filter(env, Region_Plot %in% plots)
    out <- tibble::tibble(
      subgroup  = name,
      fcmedian  = if (nrow(e)) stats::median(e$fc2000) else NA_real_,
      devmedian = stats::median(p$dev),
      dev_mean  = mean(p$dev),
      RMSE      = sqrt(mean(p$dev^2)),
      R2        = 1 - sum((p$B_pred - p$B)^2) / sum((p$B - mean(p$B))^2)
    )
    if (has_channels) {
      out <- dplyr::mutate(
        out,
        r_size_median = stats::median(p$r_size),
        w_H_median    = stats::median(p$w_H),
        w_rho_median  = stats::median(p$w_rho),
        r_size_mean   = mean(p$r_size),
        w_H_mean      = mean(p$w_H),
        w_rho_mean    = mean(p$w_rho)
      )
    }
    out
  })
}

#' Subgroup-level table for H2c: Sloan fits + EEoS summaries
load_and_prepare_subgroup_data <- function(fits_file = out_path("ForestCoverSubgroupSloanFits"),
                                           subsets_file = out_path("NonOverlappingOrderedPlotSubsets", ext = "RDS"),
                                           preds_file = out_path("EEOSPreds"),
                                           env_file = "Data/env.csv") {
  fits <- read_required_csv(fits_file)
  if (!file.exists(subsets_file)) stop("Required file not found: ", subsets_file)
  eeos <- summarise_subgroup_eeos(readRDS(subsets_file), read_required_csv(preds_file),
                                  read_required_csv(env_file))
  df <- dplyr::left_join(fits, eeos, by = "subgroup", relationship = "one-to-one")
  stopifnot(nrow(df) == nrow(fits), !anyNA(df$RMSE))

  df |>
    dplyr::mutate(
      fcmedian = dplyr::if_else(region == "Jur", 1.0, fcmedian),
      disturbedcover = 1.0 - fcmedian,
      region = factor(unname(REGION_ABBR[region]), levels = REGIONS),
      Reg_Balbina = dplyr::if_else(region == "Balbina", 1, 0),
      Reg_STM = dplyr::if_else(region == "STM", 1, 0),
      Reg_PGM = dplyr::if_else(region == "PGM", 1, 0)
    )
}
