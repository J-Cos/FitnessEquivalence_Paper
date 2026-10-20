# --------------------------------------------
# Purpose: Fit neutral metacommunity (Sloan) models to 100 random 29-plot subsamples per region,
#          classify non-neutral taxa, and compute the per-plot relative abundance of
#          non-neutral taxa (fitness-equivalence metric for H2b).
# Input Origin Scripts: 01_CombinePinhoJuruaData.R; 07_GetPlotSubsets.R
# Output: Outputs/SloanOutput.RDS, NonNeutralTaxa.csv, NonNeutralTaxaTable.csv,
#         NonNeutralProportions.csv; Figures S3, S4, S5
# ---------------------------------------------------

suppressMessages(library(tidyverse))
source("Code/Functions/config.R"); check_project_root()
source("Code/Functions/sloan_helpers.R")
source("Code/Functions/permutation_helpers.R")

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

region_rank_plot <- c(PGM = 1, STM = 2, Balbina = 3, Jurua = 4)
p_avg <- sloan_stats |>
  mutate(order = region_rank_plot[region]) |>
  ggplot(aes(x = reorder(region, order), y = r2pois, color = region, fill = region)) +
  geom_violin(alpha = 0.5) +
  theme_classic() +
  theme(axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), ends = "both")),
        axis.title.x = element_text(angle = 0)) +
  ggsci::scale_color_npg() + ggsci::scale_fill_npg() +
  labs(x = "More Degraded                                                                   Less Degraded")
ggsave("Figures/Figure_SloanAvgFit.png", p_avg, height = 8, width = 8)

# Blocked permutation test of an ordinal regional trend in R2
region_rank_test <- c(STM = 1, PGM = 1, Balbina = 2, Jurua = 3)
perm <- sloan_stats |> mutate(order = region_rank_test[region], dv = r2binom) |> run_permutation_test(1000)

# 3) Non-neutral taxa ---------------------------------------------------------------------------
taxa_long <- map_dfr(replicates, "taxa")
nn <- classify_nonneutral_taxa(taxa_long)
trait_table <- read_required_csv("Data/traits.csv")
nn_taxa <- nn$wide |>
  left_join(summarise(group_by(nn$long, sp = Binomial_correct), Neutrality = paste(sort(unique(Neutrality)), collapse = "/")),
            by = "sp") |>
  left_join(trait_table, by = join_by(sp == Binomial_correct))
write.csv(nn_taxa, out_path("NonNeutralTaxa"))
write.csv(rename(select(nn$wide, sp, all_of(REGIONS)), Binomial = sp), out_path("NonNeutralTaxaTable"))

# Trait distribution of non-neutral taxa (Figure S5)
p_traits <- nn_taxa |>
  pivot_longer(all_of(REGIONS), names_to = "RegionNeutral", values_to = "NeutralScore") |>
  filter(NeutralScore == 1) |>
  mutate(across(c(WD, lnSM, LMA, Hmax), \(x) as.numeric(scale(x)))) |>
  pivot_longer(c(WD, lnSM, LMA, Hmax), names_to = "TraitName", values_to = "TraitScore") |>
  ggplot() +
  geom_boxplot(aes(x = TraitName, y = TraitScore, fill = RegionNeutral)) +
  ggsci::scale_fill_npg() +
  geom_hline(yintercept = 0, linetype = 2) +
  theme_classic()
ggsave("Figures/Figure_traitsOfNonneutrals.png", p_traits, height = 8, width = 8)

# 4) Per-plot relative abundance of non-neutral taxa --------------------------------------------
proportions <- nonneutral_proportions(comm, nn$long)
stopifnot(nrow(proportions) == n_distinct(comm$Region_Plot))
write.csv(proportions, out_path("NonNeutralProportions"))
message(sprintf("%d non-neutral taxa identified.", nrow(nn$wide)))
