# --------------------------------------------
# Purpose: Fitness-equivalence Measure 2 (H2b) - Regional neutral metacommunity models and
#          per-plot relative abundance of non-neutral taxa, plus regional z-scored trait tests.
#          Draws 100 equal-effort random subsamples of 29 plots per region, fits Sloan / binomial /
#          Poisson random-sampling neutral models via sloan_helpers.R, classifies taxa consistently
#          outside the 95% Poisson neutral interval (LFHA or HFLA across all replicates where
#          present), computes per-plot non-neutral stem proportions, and tests non-neutral trait
#          departures from regional species-pool means. Pure computation (figures in 08).
# Input Origin Scripts: 01_CombinePinhoJuruaData.R
# Output: Outputs/PlotSubsets.RDS; Outputs/SloanOutput.RDS; Outputs/SloanExampleFits.RDS;
#         Outputs/SloanR2_ByRegion.csv; Outputs/SloanRegionalAverageFit.csv;
#         Outputs/NonNeutralTaxa.csv; Outputs/NonNeutralTaxaTable.csv;
#         Outputs/NonNeutralProportions.csv; Outputs/NonNeutralTraitTests.csv;
#         Outputs/NonNeutralTraitZScores.csv
# ---------------------------------------------------

suppressMessages(library(tidyverse))
source("Code/Functions/config.R"); check_project_root()
source("Code/Functions/sloan_helpers.R")

comm   <- read_required_csv(out_path("comm"))
traits <- read_required_csv(out_path("traits"))
region_of_plot <- distinct(comm, Region, Region_Plot)

# 1) 100 equal-effort random subsamples of 29 plots per region ----------------------------------
plot_subsets <- make_regional_plot_subsamples(comm)
saveRDS(plot_subsets, out_path("PlotSubsets", ext = "RDS"))

# 2) Replicate regional neutral model fits ------------------------------------------------------
fit_replicate <- function(i) {
  plots_i <- plot_subsets[[i]]
  reg_sets <- map(set_names(REGIONS[order(REGIONS)]), function(region) {
    filter(region_of_plot, Region == region, Region_Plot %in% plots_i)$Region_Plot
  })
  fits <- fit_sloan_over_plot_sets(comm, reg_sets)
  list(
    stats = imap_dfr(fits, \(f, r) mutate(extract_sloan_summary(f), seed = i, region = r)),
    taxa  = imap_dfr(fits, \(f, r) mutate(classify_neutrality(f),   seed = i, region = r)),
    vals  = imap_dfr(fits, \(f, r) tibble::rownames_to_column(f$vals, "sp") |>
                       select(sp, p, freq, pois.pred, pois.lwr, pois.upr) |>
                       mutate(seed = i, region = r)),
    fits  = fits[REGIONS]
  )
}
replicates <- map(seq_len(N_SUBSAMPLES), fit_replicate)

sloan_stats <- map_dfr(replicates, "stats") |>
  select(seed, region, r2, m, r2binom, r2pois, dAIC_v_binom, dAIC_v_pois, dAIC_pois_v_binom)
saveRDS(as.data.frame(sloan_stats), out_path("SloanOutput", ext = "RDS"))
saveRDS(replicates[[N_SUBSAMPLES]]$fits, out_path("SloanExampleFits", ext = "RDS"))
print(summary(select(sloan_stats, dAIC_v_binom, dAIC_v_pois, dAIC_pois_v_binom)))

r2_by_region <- summarise_regional_sloan_r2(sloan_stats)
print(r2_by_region, width = Inf)
write.csv(r2_by_region, out_path("SloanR2_ByRegion"), row.names = FALSE)

# 3) Non-neutral taxa classification & per-plot stem proportions --------------------------------
taxa_long <- map_dfr(replicates, "taxa")
nn <- classify_nonneutral_taxa(taxa_long)

avg_regional_fit <- summarise_regional_average_sloan(map_dfr(replicates, "vals"), nn$long)
write.csv(avg_regional_fit, out_path("SloanRegionalAverageFit"), row.names = FALSE)

nn_taxa <- nn$wide |>
  left_join(
    summarise(group_by(nn$long, sp = Binomial_correct),
              Neutrality = paste(sort(unique(Neutrality)), collapse = "/")),
    by = "sp"
  ) |>
  left_join(traits, by = join_by(sp == Binomial_correct))
stopifnot(!anyNA(nn_taxa$WD), !anyNA(nn_taxa$LMA))   # Hmax is unavailable for Jurua-only taxa
write.csv(nn_taxa, out_path("NonNeutralTaxa"), row.names = FALSE)
write.csv(rename(select(nn$wide, sp, all_of(REGIONS)), Binomial = sp),
          out_path("NonNeutralTaxaTable"), row.names = FALSE)

proportions <- nonneutral_proportions(comm, nn$long)
stopifnot(nrow(proportions) == n_distinct(comm$Region_Plot))
write.csv(proportions, out_path("NonNeutralProportions"), row.names = FALSE)
message(sprintf("%d non-neutral taxa identified across regions.", nrow(nn$wide)))

# 4) Regional species-pool z-scored trait departures of non-neutral taxa ------------------------
trait_res <- compute_nonneutral_trait_tests(comm, traits, nn_taxa)
print(trait_res$tests, n = Inf, width = Inf)
write.csv(trait_res$tests,    out_path("NonNeutralTraitTests"),   row.names = FALSE)
write.csv(trait_res$z_scores, out_path("NonNeutralTraitZScores"), row.names = FALSE)
