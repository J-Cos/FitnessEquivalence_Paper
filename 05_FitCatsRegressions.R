# --------------------------------------------
# Purpose: Community Assembly via Trait Selection (CATS) regressions per plot (H2a).
#          Poisson GLM of species abundance on traits with a metacommunity offset
#          (equivalent to Shipley et al. 2006 MaxEnt CATS; Warton et al. 2015).
#          Deviance R2 = strength of trait selection (fitness non-equivalence).
# Input Origin Scripts: 01_CombinePinhoJuruaData.R
# Output: Outputs/CatsOutput.csv
# ---------------------------------------------------

suppressMessages(library(tidyverse))
source("Code/Functions/config.R"); check_project_root()

comm   <- read_required_csv(out_path("comm"))
traits <- read_required_csv(out_path("traits")) |> select(-DS)

# Stems per species per plot
plot_species <- comm |> count(Region, Region_Plot, Binomial_correct, name = "n")

# Metacommunity weight of each species in each region: share of the region's plot occurrences
meta_by_region <- plot_species |>
  count(Region, Binomial_correct, name = "n_occ") |>
  group_by(Region) |>
  mutate(meta = n_occ / sum(n_occ)) |>
  ungroup() |>
  select(Region, Binomial_correct, meta)

deviance_r2 <- function(fit) 1 - fit$deviance / fit$null.deviance

#' Fit Poisson and negative-binomial CATS models for one plot
fit_cats_plot <- function(plot_id, region) {
  observed <- plot_species |> filter(Region_Plot == plot_id) |> mutate(rel = n / sum(n))
  fitting <- meta_by_region |>
    filter(Region == region) |>
    left_join(select(observed, Binomial_correct, rel), by = "Binomial_correct") |>
    mutate(rel = replace_na(rel, 0)) |>
    left_join(traits, by = "Binomial_correct")

  form <- rel ~ WD + lnSM + LMA + Hmax + offset(log(meta))
  pois <- suppressWarnings(glm(form, family = "poisson", data = fitting))
  nb <- tryCatch(suppressWarnings(MASS::glm.nb(form, data = fitting, start = coef(pois))),
                 error = function(e) NULL)
  tibble(Region = region, Region_Plot = plot_id,
         CATS_pois = deviance_r2(pois),
         CATS_nb = if (is.null(nb)) NA_real_ else deviance_r2(nb))
}

plots <- distinct(plot_species, Region, Region_Plot) |> arrange(Region, Region_Plot)
cats <- map2_dfr(plots$Region_Plot, plots$Region, fit_cats_plot)

write.csv(cats, out_path("CatsOutput"))
ct <- cor.test(cats$CATS_pois, cats$CATS_nb)
message(sprintf("Poisson vs NB CATS R2: r = %.3f, 95%% CI [%.3f, %.3f]; NB failures: %d",
                ct$estimate, ct$conf.int[1], ct$conf.int[2], sum(is.na(cats$CATS_nb))))