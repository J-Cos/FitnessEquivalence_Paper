# --------------------------------------------
# Purpose: Community Assembly via Trait Selection (CATS) regressions per plot (H2a).
#          Poisson GLM of species abundance (stem counts) on traits with an offset of log
#          metacommunity relative abundance (the neutral prior of Shipley et al. 2006; equivalent
#          MaxEnt/GLM formulation of Warton et al. 2015). The metacommunity of a plot is all stems
#          recorded in its region. Deviance R2 = strength of trait selection (fitness non-equivalence).
# Input Origin Scripts: 01_CombinePinhoJuruaData.R
# Output: Outputs/CatsOutput.csv with, per plot:
#   CATS_pois / CATS_nb        main 4-trait model (WD, lnSM, LMA, Hmax); Poisson and negative binomial
#   CATS_pois_3trait           3-trait model (WD, lnSM, LMA) - Hmax is unavailable for Jurua-only taxa
#   n_species_*, prop_stems_*  species and stem coverage of each model (taxa with missing traits drop)
# ---------------------------------------------------

suppressMessages(library(tidyverse))
source("Code/Functions/config.R"); check_project_root()

comm   <- read_required_csv(out_path("comm"))
traits <- read_required_csv(out_path("traits")) |> select(-DS)

TRAITS_4 <- c("WD", "lnSM", "LMA", "Hmax")
TRAITS_3 <- c("WD", "lnSM", "LMA")

# Stems per species per plot
plot_species <- comm |> count(Region, Region_Plot, Binomial_correct, name = "n")

# Metacommunity relative abundance: each species' share of all stems recorded in the region
meta_by_region <- plot_species |>
  group_by(Region, Binomial_correct) |>
  summarise(n_stems = sum(n), .groups = "drop_last") |>
  mutate(meta = n_stems / sum(n_stems)) |>
  ungroup() |>
  select(Region, Binomial_correct, meta)
stopifnot(all(abs(tapply(meta_by_region$meta, meta_by_region$Region, sum) - 1) < 1e-12))

deviance_r2 <- function(fit) 1 - fit$deviance / fit$null.deviance

cats_formula <- function(trait_names) {
  as.formula(sprintf("n ~ %s + offset(log(meta))", paste(trait_names, collapse = " + ")))
}

#' Coverage of a model: species and stems with complete traits
coverage <- function(fitting, trait_names) {
  ok <- complete.cases(fitting[, trait_names])
  c(n_species = sum(ok & fitting$n > 0), prop_stems = sum(fitting$n[ok]) / sum(fitting$n))
}

#' Fit the CATS models for one plot; species of the regional pool absent from the plot have n = 0
fit_cats_plot <- function(plot_id, region) {
  fitting <- meta_by_region |>
    filter(Region == region) |>
    left_join(filter(plot_species, Region_Plot == plot_id) |> select(Binomial_correct, n),
              by = "Binomial_correct") |>
    mutate(n = replace_na(n, 0L)) |>
    left_join(traits, by = "Binomial_correct")

  pois  <- glm(cats_formula(TRAITS_4), family = "poisson", data = fitting)
  pois3 <- glm(cats_formula(TRAITS_3), family = "poisson", data = fitting)
  nb <- tryCatch(suppressWarnings(MASS::glm.nb(cats_formula(TRAITS_4), data = fitting, start = coef(pois))),
                 error = function(e) NULL)
  cov4 <- coverage(fitting, TRAITS_4); cov3 <- coverage(fitting, TRAITS_3)
  tibble(Region = region, Region_Plot = plot_id,
         CATS_pois = deviance_r2(pois),
         CATS_nb = if (is.null(nb)) NA_real_ else deviance_r2(nb),
         CATS_pois_3trait = deviance_r2(pois3),
         n_species_4trait = cov4[["n_species"]], prop_stems_4trait = cov4[["prop_stems"]],
         n_species_3trait = cov3[["n_species"]], prop_stems_3trait = cov3[["prop_stems"]])
}

plots <- distinct(plot_species, Region, Region_Plot) |> arrange(Region, Region_Plot)
cats <- map2_dfr(plots$Region_Plot, plots$Region, fit_cats_plot)
write.csv(cats, out_path("CatsOutput"))

ct <- cor.test(cats$CATS_pois, cats$CATS_nb)
message(sprintf("Poisson vs NB CATS R2: r = %.3f, 95%% CI [%.3f, %.3f]; NB failures: %d",
                ct$estimate, ct$conf.int[1], ct$conf.int[2], sum(is.na(cats$CATS_nb))))
print(cats |> group_by(Region) |>
        summarise(across(c(CATS_pois, CATS_pois_3trait, prop_stems_4trait, prop_stems_3trait), mean)))