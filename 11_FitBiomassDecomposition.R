# --------------------------------------------
# Purpose: Exact 3-run BIOMASS channel decomposition of H1 (disturbance -> EEoS deviation) and
#          H2a-c (fitness-equivalence mediation of EEoS deviation) across the three orthogonal
#          additive channels from 02_GetEcosystemMacrostates.R and 03_FitEEOS.py:
#
#            dev = ln(B / B_pred) = w_rho + w_H + r_size
#
#          via the exact telescoping product B / B_pred = (B / B_rho) * (B_rho / B_size) * (B_size / B_pred):
#            - w_rho  = ln(B / B_rho)       : Functional wood-density channel
#            - w_H    = ln(B_rho / B_size)  : Site bioclimatic height-allometry channel
#            - r_size = ln(B_size / B_pred) : Pure MaxEnt size-structure channel
#
#          Computes and verifies:
#            1. WD-free CATS non-circularity check (Outputs/BiomassDecomposition_NoWD_Cats.csv):
#               Fits Negative Binomial and Poisson CATS per plot strictly excluding wood density
#               (using only lnSM + LMA + Hmax) to verify that H2a's link to w_rho reflects
#               co-evolution along the tropical fast-slow life-history axis rather than shared WD.
#            2. Exact additive OLS channel regressions (Outputs/BiomassDecomposition_OLS.csv):
#               Common-slope regional fixed-effects regressions (Y ~ Predictor + Region, with 95%
#               CIs and HC3 robust SEs) across Total, w_rho, r_size, and w_H for H1, H2a,
#               H2a_noWD, H2b, H2c_median, and H2c_mean (verifying exact slope additivity).
#            3. Channel-specific Garrido 8-model MSA-SEMs (Outputs/BiomassDecomposition_SEM_Summary.csv
#               and Outputs/BiomassDecomposition_SEM_MMI.csv):
#               Evaluates the balanced 2^3 = 8-model recursive SEM candidate set and 2,000-draw
#               percentile bootstrap indirect effects on each individual channel (verifying exact
#               additivity of partial-mediation indirect [a * b] and direct [c] channel effects).
#
#          Pure computation (publication figures rendered in 12_PlotBiomassDecomposition.R).
# Input Origin Scripts: 01_CombinePinhoJuruaData.R; 03_FitEEOS.py; 05_FitCatsRegressions.R;
#                       06_FitRegionalSloanNonNeutral.R; 07_FitSubgroupSloans.R
# Output: Outputs/BiomassDecomposition_NoWD_Cats.csv;
#         Outputs/BiomassDecomposition_OLS.csv;
#         Outputs/BiomassDecomposition_SEM_Summary.csv;
#         Outputs/BiomassDecomposition_SEM_MMI.csv
# ---------------------------------------------------

suppressMessages({
  library(tidyverse)
  library(lmtest)
  library(sandwich)
})
source("Code/Functions/config.R"); check_project_root()
source("Code/Functions/cats_helpers.R")
source("Code/Functions/data_prep.R")
source("Code/Functions/regression_helpers.R")
source("Code/Functions/sem_helpers.R")

set.seed(1)

# 1) Load plot & subgroup tables and verify exact additive channel identities --------------------
plot_data <- load_and_prepare_plot_data(need = c("cats", "nonneutral"))
sub_data  <- load_and_prepare_subgroup_data()

plot_ident_err <- max(abs((plot_data$w_rho + plot_data$r_size + plot_data$w_H) - plot_data$dev))
sub_ident_err  <- max(abs((sub_data$w_rho_mean + sub_data$r_size_mean + sub_data$w_H_mean) - sub_data$dev_mean))
stopifnot(plot_ident_err < 1e-12, sub_ident_err < 1e-12)
message(sprintf(
  "Verified exact additive identity: plot max |w_rho + r_size + w_H - dev| = %.2e; subgroup mean = %.2e",
  plot_ident_err, sub_ident_err
))

# 2) Fit WD-free CATS regressions (lnSM + LMA + Hmax; strictly excluding WD) ---------------------
comm   <- read_required_csv(out_path("comm"))
traits <- read_required_csv(out_path("traits")) |> select(-DS)

plot_species   <- comm |> count(Region, Region_Plot, Binomial_correct, name = "n")
meta_by_region <- compute_regional_metacommunity(plot_species)

cats_nowd <- plot_data |>
  select(Region, Region_Plot) |>
  mutate(Region = as.character(Region)) |>
  pmap_dfr(function(Region, Region_Plot) {
    fitting <- build_cats_plot_table(Region_Plot, Region, meta_by_region, plot_species, traits)
    res_nw  <- fit_cats_model_pair(fitting, CATS_TRAITS_NOWD)
    cov_nw  <- cats_trait_coverage(fitting, CATS_TRAITS_NOWD)
    tibble(
      Region          = Region,
      Region_Plot     = Region_Plot,
      CATS_nb_noWD    = res_nw$r2_nb,
      CATS_pois_noWD  = res_nw$r2_pois,
      n_species_noWD  = cov_nw[["n_species"]],
      prop_stems_noWD = cov_nw[["prop_stems"]]
    )
  })
stopifnot(nrow(cats_nowd) == nrow(plot_data), !anyNA(cats_nowd$CATS_nb_noWD))
write.csv(cats_nowd, out_path("BiomassDecomposition_NoWD_Cats"), row.names = FALSE)

plot_data <- plot_data |>
  left_join(select(cats_nowd, Region_Plot, CATS_nb_noWD, CATS_pois_noWD),
            by = "Region_Plot", relationship = "one-to-one")

# 3) Exact additive OLS channel decomposition across H1 and H2a-c --------------------------------
plot_channels <- c(
  Total        = "dev",
  Wood_Density = "w_rho",
  Pure_Size    = "r_size",
  Site_Height  = "w_H"
)
sub_median_channels <- c(
  Total        = "devmedian",
  Wood_Density = "w_rho_median",
  Pure_Size    = "r_size_median",
  Site_Height  = "w_H_median"
)
sub_mean_channels <- c(
  Total        = "dev_mean",
  Wood_Density = "w_rho_mean",
  Pure_Size    = "r_size_mean",
  Site_Height  = "w_H_mean"
)

ols_tab <- bind_rows(
  fit_channel_ols(plot_data, "disturbedcover",       "Region", "H1",         "plot",            plot_channels),
  fit_channel_ols(plot_data, "CATS_nb",              "Region", "H2a",        "plot",            plot_channels),
  fit_channel_ols(plot_data, "CATS_nb_noWD",         "Region", "H2a_noWD",   "plot",            plot_channels),
  fit_channel_ols(plot_data, "proportionNonNeutral", "Region", "H2b",        "plot",            plot_channels),
  fit_channel_ols(sub_data,  "r2pois",               "region", "H2c_median", "subgroup_median", sub_median_channels),
  fit_channel_ols(sub_data,  "r2pois",               "region", "H2c_mean",   "subgroup_mean",   sub_mean_channels)
)

# Verify exact OLS slope additivity across all linear-sum datasets
exact_add_check <- ols_tab |>
  filter(Hypothesis %in% c("H1", "H2a", "H2a_noWD", "H2b", "H2c_mean")) |>
  group_by(Hypothesis) |>
  summarise(
    tot  = Estimate[Channel == "Total"],
    csum = sum(Estimate[Channel != "Total"]),
    err  = abs(csum - tot),
    .groups = "drop"
  )
stopifnot(max(exact_add_check$err) < 1e-12)
write.csv(ols_tab, out_path("BiomassDecomposition_OLS"), row.names = FALSE)

cat("\n=== Exact Additive OLS Channel Decomposition ===\n")
print(
  ols_tab |>
    select(Hypothesis, Predictor, Channel, Outcome, Estimate, Std_Error, p_value, Pct_of_Total),
  n = 30, width = Inf
)

# 4) Channel-specific Garrido 8-model MSA-SEMs + 2,000-draw percentile bootstrap -----------------
sem_channel_specs <- bind_rows(
  imap_dfr(plot_channels,       \(y, ch) tibble(Hypothesis = "H2a",        Dataset = "plot",     Mediator = "CATS_nb",              Channel = ch, Outcome = y)),
  imap_dfr(plot_channels,       \(y, ch) tibble(Hypothesis = "H2a_noWD",   Dataset = "plot",     Mediator = "CATS_nb_noWD",         Channel = ch, Outcome = y)),
  imap_dfr(plot_channels,       \(y, ch) tibble(Hypothesis = "H2b",        Dataset = "plot",     Mediator = "proportionNonNeutral", Channel = ch, Outcome = y)),
  imap_dfr(sub_median_channels, \(y, ch) tibble(Hypothesis = "H2c_median", Dataset = "subgroup", Mediator = "r2pois",               Channel = ch, Outcome = y)),
  imap_dfr(sub_mean_channels,   \(y, ch) tibble(Hypothesis = "H2c_mean",   Dataset = "subgroup", Mediator = "r2pois",               Channel = ch, Outcome = y))
) |>
  mutate(Hypothesis_ID = paste("Decomp", Hypothesis, Channel, sep = "_"), .before = 1)

sem_runs <- pmap(sem_channel_specs, function(Hypothesis_ID, Hypothesis, Dataset, Mediator, Channel, Outcome) {
  d <- if (Dataset == "plot") plot_data else sub_data
  res <- run_mediation_hypothesis(
    d, id = Hypothesis_ID, m = Mediator, y = Outcome,
    indirect = TRUE, n_boot = 2000, seed = 1, write_files = FALSE
  )
  sum_row <- res$summary |>
    mutate(Hypothesis = Hypothesis, Channel = Channel, .after = Hypothesis_ID) |>
    bind_cols(extract_mmi_metrics(res))
  mmi_rows <- res$mmi |>
    mutate(
      Hypothesis_ID = Hypothesis_ID,
      Hypothesis    = Hypothesis,
      Channel       = Channel,
      Mediator      = Mediator,
      Outcome       = Outcome,
      .before       = 1
    )
  list(summary = sum_row, mmi = mmi_rows)
})

sem_summary_tab <- map_dfr(sem_runs, "summary") |>
  group_by(Hypothesis) |>
  mutate(
    Total_ind_part       = ind_part_est[Channel == "Total"],
    Share_ind_part_Total = ind_part_est / Total_ind_part,
    Pct_ind_part_Total   = 100 * ind_part_est / Total_ind_part,
    Total_c_part         = c_part_est[Channel == "Total"],
    Share_c_part_Total   = c_part_est / Total_c_part,
    Pct_c_part_Total     = 100 * c_part_est / Total_c_part
  ) |>
  ungroup()

sem_mmi_tab <- map_dfr(sem_runs, "mmi")

# Verify exact additivity of Partial-Mediation indirect (a * b_part) and direct (c_part) effects
sem_add_check <- sem_summary_tab |>
  filter(Hypothesis %in% c("H2a", "H2a_noWD", "H2b", "H2c_mean")) |>
  group_by(Hypothesis) |>
  summarise(
    ind_err = abs(sum(ind_part_est[Channel != "Total"]) - ind_part_est[Channel == "Total"]),
    c_err   = abs(sum(c_part_est[Channel != "Total"])   - c_part_est[Channel == "Total"]),
    .groups = "drop"
  )
stopifnot(max(sem_add_check$ind_err) < 1e-10, max(sem_add_check$c_err) < 1e-10)

write.csv(sem_summary_tab, out_path("BiomassDecomposition_SEM_Summary"), row.names = FALSE)
write.csv(sem_mmi_tab,     out_path("BiomassDecomposition_SEM_MMI"),     row.names = FALSE)

cat("\n=== Channel-Specific Garrido MSA-SEM Summary ===\n")
print(
  sem_summary_tab |>
    select(
      Hypothesis, Channel, Outcome, Weighted_CFI,
      RelImp_a, RelImp_b, RelImp_c, RelImp_ind,
      Avg_std_b, Avg_est_ind, ind_part_est, ind_part_ci_low, ind_part_ci_high,
      Pct_ind_part_Total, Prop_Mediated
    ),
  n = 25, width = Inf
)
message("Saved BiomassDecomposition_{NoWD_Cats,OLS,SEM_Summary,SEM_MMI}.csv to Outputs/")
