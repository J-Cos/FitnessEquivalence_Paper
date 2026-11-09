# --------------------------------------------
# Purpose: Community Assembly via Trait Selection (CATS) regressions per plot (H2a).
#          Fits Negative Binomial (primary; Warton, Shipley & Hastie 2015, accounting for local
#          stem overdispersion) and Poisson (exact MaxEnt equivalence to Shipley et al. 2006, 2012)
#          GLMs of species stem counts on functional traits with an offset of log regional
#          metacommunity relative abundance. Deviance R2 against the neutral metacommunity-prior
#          null model measures the strength of local trait selection (fitness non-equivalence).
# Input Origin Scripts: 01_CombinePinhoJuruaData.R
# Output: Outputs/CatsOutput.csv and Outputs/CatsSpeciesPredictions.RDS:
#   CATS_nb / CATS_nb_3trait      NB deviance R2 (Cameron & Windmeijer 1996, NB1 vs refitted NB0)
#   CATS_pois / CATS_pois_3trait  Poisson deviance (KL) R2 (4-trait and 3-trait without Hmax)
#   beta0_nb, beta_*_nb           Fitted 4-trait NB CATS partial regression coefficients
#   aic_*, bic_*, dAIC_pois_v_nb, dBIC_pois_v_nb, theta_nb, alpha_nb, overdisp_lrt_*, pearson_disp_pois
#   n_species_*, prop_stems_*     species and stem coverage of each model
# ---------------------------------------------------

suppressMessages(library(tidyverse))
source("Code/Functions/config.R"); check_project_root()
source("Code/Functions/cats_helpers.R")

comm   <- read_required_csv(out_path("comm"))
traits <- read_required_csv(out_path("traits")) |> select(-DS)

plot_species   <- comm |> count(Region, Region_Plot, Binomial_correct, name = "n")
meta_by_region <- compute_regional_metacommunity(plot_species)

fit_cats_plot <- function(plot_id, region) {
  fitting <- build_cats_plot_table(plot_id, region, meta_by_region, plot_species, traits)
  res4 <- fit_cats_model_pair(fitting, CATS_TRAITS_4)
  res3 <- fit_cats_model_pair(fitting, CATS_TRAITS_3)
  cov4 <- cats_trait_coverage(fitting, CATS_TRAITS_4)
  cov3 <- cats_trait_coverage(fitting, CATS_TRAITS_3)
  b4   <- res4$fit_nb$beta_nb

  sp_df <- res4$fitted_df |>
    transmute(
      Region           = region,
      Region_Plot      = plot_id,
      Binomial_correct = Binomial_correct,
      n                = n,
      meta             = meta,
      WD               = WD,
      lnSM             = lnSM,
      LMA              = LMA,
      Hmax             = Hmax,
      mu_nb            = mu_nb,
      p_obs            = n / sum(n),
      p_hat_nb         = mu_nb / sum(mu_nb)
    )

  list(
    summary = tibble(
      Region            = region,
      Region_Plot       = plot_id,
      CATS_nb           = res4$r2_nb,
      CATS_nb_3trait    = res3$r2_nb,
      CATS_pois         = res4$r2_pois,
      CATS_pois_3trait  = res3$r2_pois,
      beta0_nb          = b4[["(Intercept)"]],
      beta_WD_nb        = b4[["WD"]],
      beta_lnSM_nb      = b4[["lnSM"]],
      beta_LMA_nb       = b4[["LMA"]],
      beta_Hmax_nb      = b4[["Hmax"]],
      aic_pois          = res4$aic_pois,
      aic_nb            = res4$aic_nb,
      dAIC_pois_v_nb    = res4$dAIC_pois_v_nb,
      bic_pois          = res4$bic_pois,
      bic_nb            = res4$bic_nb,
      dBIC_pois_v_nb    = res4$dBIC_pois_v_nb,
      theta_nb          = res4$theta_nb,
      alpha_nb          = res4$alpha_nb,
      overdisp_lrt_stat = res4$overdisp_lrt_stat,
      overdisp_lrt_p    = res4$overdisp_lrt_p,
      pearson_disp_pois = res4$pearson_disp_pois,
      n_species_4trait  = cov4[["n_species"]],
      prop_stems_4trait = cov4[["prop_stems"]],
      n_species_3trait  = cov3[["n_species"]],
      prop_stems_3trait = cov3[["prop_stems"]]
    ),
    species = sp_df
  )
}

plots    <- distinct(plot_species, Region, Region_Plot) |> arrange(Region, Region_Plot)
cats_all <- map2(plots$Region_Plot, plots$Region, fit_cats_plot)
cats     <- map_dfr(cats_all, "summary")
cats_sp  <- map_dfr(cats_all, "species")

write.csv(cats, out_path("CatsOutput"), row.names = FALSE)
saveRDS(cats_sp, out_path("CatsSpeciesPredictions", ext = "RDS"))

ct <- cor.test(cats$CATS_nb, cats$CATS_pois)
message(sprintf(
  paste0("NB vs Poisson CATS: NB lower AIC in %d/%d plots (%.1f%%; dAIC > 2 in %.1f%%; median dAIC = %.1f); ",
         "overdispersion LRT p < 0.05 in %.1f%%; R2 correlation r = %.3f [%.3f, %.3f]"),
  sum(cats$dAIC_pois_v_nb > 0), nrow(cats),
  100 * mean(cats$dAIC_pois_v_nb > 0),
  100 * mean(cats$dAIC_pois_v_nb > 2),
  median(cats$dAIC_pois_v_nb),
  100 * mean(cats$overdisp_lrt_p < 0.05),
  ct$estimate, ct$conf.int[1], ct$conf.int[2]
))
print(
  cats |>
    group_by(Region) |>
    summarise(
      n = n(),
      CATS_nb = mean(CATS_nb),
      CATS_nb_3trait = mean(CATS_nb_3trait),
      CATS_pois = mean(CATS_pois),
      median_dAIC = median(dAIC_pois_v_nb),
      pct_overdisp_p05 = 100 * mean(overdisp_lrt_p < 0.05),
      prop_stems_4trait = mean(prop_stems_4trait),
      prop_stems_3trait = mean(prop_stems_3trait)
    ),
  width = Inf
)