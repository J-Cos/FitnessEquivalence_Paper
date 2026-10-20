# --------------------------------------------
# Purpose: Load and validate the plot-level and subgroup-level analysis tables
#          (EEoS deviation + forest cover + fitness-equivalence metrics).
# ---------------------------------------------------

#' Plot-level table for H1 / H2a / H2b and the sensitivity analysis
#' @param need which fitness-equivalence metrics must be joined: "cats", "nonneutral"
load_and_prepare_plot_data <- function(suffix = MAIN_SUFFIX,
                                       need = character(0),
                                       preds_file = out_path("EEOSPreds", suffix),
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
      dplyr::select(Region_Plot, dplyr::starts_with("CATS_pois"))
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

  required <- c("dev", "disturbedcover", "Region",
                if ("cats" %in% need) "CATS_pois",
                if ("nonneutral" %in% need) "proportionNonNeutral")
  bad <- required[vapply(required, function(v) anyNA(plot_data[[v]]), logical(1))]
  if (length(bad)) stop("NA values after joins in: ", paste(bad, collapse = ", "))
  plot_data
}

#' Per-subgroup forest cover and EEoS summaries (H2c)
#' @param subsets named list of Region_Plot vectors (Outputs/NonOverlappingOrderedPlotSubsets.RDS)
summarise_subgroup_eeos <- function(subsets, preds, env) {
  purrr::imap_dfr(subsets, function(plots, name) {
    p <- dplyr::filter(preds, Region_Plot %in% plots)
    e <- dplyr::filter(env, Region_Plot %in% plots)
    tibble::tibble(
      subgroup  = name,
      fcmedian  = if (nrow(e)) stats::median(e$fc2000) else NA_real_,
      devmedian = stats::median(p$dev),
      RMSE      = sqrt(mean(p$dev^2)),
      R2        = 1 - sum((p$B_pred - p$B)^2) / sum((p$B - mean(p$B))^2)
    )
  })
}

#' Subgroup-level table for H2c: suffix-free Sloan fits + suffix-specific EEoS summaries
load_and_prepare_subgroup_data <- function(suffix = MAIN_SUFFIX,
                                           fits_file = out_path("ForestCoverSubgroupSloanFits"),
                                           subsets_file = out_path("NonOverlappingOrderedPlotSubsets", ext = "RDS"),
                                           preds_file = out_path("EEOSPreds", suffix),
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
