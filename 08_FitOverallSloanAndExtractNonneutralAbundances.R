# --------------------------------------------
# Purpose: Fit neutral metacommunity (Sloan) models to 100 random 29-plot subsamples per region,
#          classify non-neutral taxa, and compute the per-plot relative abundance of
#          non-neutral taxa (fitness-equivalence metric for H2b).
# Input Origin Scripts: 01_CombinePinhoJuruaData.R; 07_GetPlotSubsets.R
# Output: Outputs/SloanOutput.RDS, NonNeutralTaxa.csv, NonNeutralTaxaTable.csv,
#         NonNeutralProportions.csv, SloanR2_ByRegion.csv; Figures S3, S4
# ---------------------------------------------------

suppressMessages(library(tidyverse))
source("Code/Functions/config.R"); check_project_root()
source("Code/Functions/sloan_helpers.R")

comm <- read_required_csv(out_path("comm"))
plot_subsets <- readRDS(out_path("PlotSubsets", ext = "RDS"))
region_of_plot <- distinct(comm, Region, Region_Plot)

# 1) Replicate neutral model fits ---------------------------------------------------------------
fit_replicate <- function(i) {
  plots_i <- plot_subsets[[i]]
  fits <- map(set_names(REGIONS[order(REGIONS)]), function(region) {
    ids <- filter(region_of_plot, Region == region, Region_Plot %in% plots_i)$Region_Plot
    sncm_fit(build_community_matrix(comm, ids))
  })
  list(stats = imap_dfr(fits, \(f, r) mutate(extract_sloan_summary(f), seed = i, region = r)),
       taxa  = imap_dfr(fits, \(f, r) mutate(classify_neutrality(f), seed = i, region = r)),
       fits  = fits)
}
replicates <- map(seq_len(N_SUBSAMPLES), fit_replicate)

sloan_stats <- map_dfr(replicates, "stats") |>
  select(seed, region, r2, m, r2binom, r2pois, dAIC_v_binom, dAIC_v_pois, dAIC_pois_v_binom)
saveRDS(as.data.frame(sloan_stats), out_path("SloanOutput", ext = "RDS"))
print(summary(select(sloan_stats, dAIC_v_binom, dAIC_v_pois, dAIC_pois_v_binom)))

# 2) Figures: example fits (last replicate; Figure S3) and R2 distribution by region (Figure S4) ---
last_fits <- replicates[[N_SUBSAMPLES]]$fits
ggsave("Figures/Figure_ExampleSLoanFits.png",
       cowplot::plot_grid(plotlist = imap(last_fits, plot_sloan_fit)))

p_avg <- sloan_stats |>
  mutate(region = factor(region, levels = REGIONS)) |>
  ggplot(aes(x = region, y = r2pois, color = region, fill = region)) +
  geom_violin(alpha = 0.5) +
  theme_classic() +
  ggsci::scale_color_npg() + ggsci::scale_fill_npg() +
  labs(x = "Region", y = "Poisson random sampling R2") +
  theme(legend.position = "none")
ggsave("Figures/Figure_SloanAvgFit.png", p_avg, height = 8, width = 8)

# Region-level summary of neutral-model fit (no ordinal ranking of regions assumed):
# mean and 95% interval across the replicate subsamples, and the paired (same-seed)
# difference from the undisturbed reference region (Jurua)
q025 <- \(x) unname(quantile(x, 0.025)); q975 <- \(x) unname(quantile(x, 0.975))
r2_by_region <- sloan_stats |>
  left_join(select(filter(sloan_stats, region == "Jurua"), seed, r2pois_jurua = r2pois), by = "seed") |>
  mutate(diff_from_jurua = r2pois - r2pois_jurua) |>
  group_by(region) |>
  summarise(n_subsamples = n(),
            r2pois_mean = mean(r2pois), r2pois_q025 = q025(r2pois), r2pois_q975 = q975(r2pois),
            diff_from_jurua_mean = mean(diff_from_jurua),
            diff_from_jurua_q025 = q025(diff_from_jurua), diff_from_jurua_q975 = q975(diff_from_jurua),
            prop_subsamples_below_jurua = mean(diff_from_jurua < 0),
            .groups = "drop") |>
  arrange(factor(region, levels = REGIONS))
print(r2_by_region, width = Inf)
write.csv(r2_by_region, out_path("SloanR2_ByRegion"), row.names = FALSE)

# 3) Non-neutral taxa ---------------------------------------------------------------------------
taxa_long <- map_dfr(replicates, "taxa")
nn <- classify_nonneutral_taxa(taxa_long)
trait_table <- read_required_csv(out_path("traits"))
nn_taxa <- nn$wide |>
  left_join(summarise(group_by(nn$long, sp = Binomial_correct), Neutrality = paste(sort(unique(Neutrality)), collapse = "/")),
            by = "sp") |>
  left_join(trait_table, by = join_by(sp == Binomial_correct))
stopifnot(!anyNA(nn_taxa$WD), !anyNA(nn_taxa$LMA))   # Hmax is unavailable for Jurua-only taxa
write.csv(nn_taxa, out_path("NonNeutralTaxa"))
write.csv(rename(select(nn$wide, sp, all_of(REGIONS)), Binomial = sp), out_path("NonNeutralTaxaTable"))

# 4) Per-plot relative abundance of non-neutral taxa --------------------------------------------
proportions <- nonneutral_proportions(comm, nn$long)
stopifnot(nrow(proportions) == n_distinct(comm$Region_Plot))
write.csv(proportions, out_path("NonNeutralProportions"))
message(sprintf("%d non-neutral taxa identified.", nrow(nn$wide)))
