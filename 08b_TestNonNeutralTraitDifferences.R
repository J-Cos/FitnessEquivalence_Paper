# --------------------------------------------
# Purpose: Test whether non-neutral taxa differ in traits from the average taxon of their region.
#          Each trait is z-scored against the species pool of the focal region (all taxa recorded in
#          that region, one value per taxon) BEFORE selecting the non-neutral taxa, so a mean of 0
#          means "typical of the regional species pool". One-sample t-tests and Wilcoxon tests
#          against 0, BH-adjusted within trait. Figure S5 shows the same standardised values.
# Input Origin Scripts: 01_CombinePinhoJuruaData.R; 08_FitOverallSloanAndExtractNonneutralAbundances.R
# Output: Outputs/NonNeutralTraitTests.csv; Figures/Figure_traitsOfNonneutrals.png (Figure S5)
# ---------------------------------------------------

suppressMessages(library(tidyverse))
source("Code/Functions/config.R"); check_project_root()

TRAITS <- c("WD", "lnSM", "LMA", "Hmax")
comm   <- read_required_csv(out_path("comm"))
traits <- read_required_csv(out_path("traits"))
non_neutrals <- read_required_csv(out_path("NonNeutralTaxa"))

# Regional species pools with traits z-scored within region
pool_z <- comm |>
  distinct(Region, Binomial_correct) |>
  left_join(select(traits, Binomial_correct, all_of(TRAITS)), by = "Binomial_correct",
            relationship = "many-to-one") |>
  pivot_longer(all_of(TRAITS), names_to = "TraitName", values_to = "value") |>
  group_by(Region, TraitName) |>
  mutate(TraitScore = (value - mean(value, na.rm = TRUE)) / sd(value, na.rm = TRUE)) |>
  ungroup()

nn_long <- non_neutrals |>
  pivot_longer(all_of(REGIONS), names_to = "Region", values_to = "NeutralScore") |>
  filter(NeutralScore == 1) |>
  select(Region, Binomial_correct = sp)

df_long <- inner_join(nn_long, pool_z, by = c("Region", "Binomial_correct"), relationship = "one-to-many") |>
  rename(RegionNeutral = Region)

safe_p <- function(test) tryCatch(test$p.value, error = function(e) NA_real_)
ttests <- df_long |>
  group_by(TraitName, RegionNeutral) |>
  summarise(n_taxa = n_distinct(Binomial_correct),
            n = sum(!is.na(TraitScore)),
            mean = mean(TraitScore, na.rm = TRUE),
            sd = sd(TraitScore, na.rm = TRUE),
            t_test_p = if (n >= 2) safe_p(t.test(TraitScore, mu = 0)) else NA_real_,
            wilcox_p = if (n >= 1) safe_p(wilcox.test(TraitScore, mu = 0, exact = FALSE)) else NA_real_,
            .groups = "drop") |>
  group_by(TraitName) |>
  mutate(t_p_adj = p.adjust(t_test_p, method = "BH"),
         w_p_adj = p.adjust(wilcox_p, method = "BH")) |>
  ungroup() |>
  mutate(sig = case_when(is.na(t_p_adj) ~ "", t_p_adj < 0.001 ~ "***", t_p_adj < 0.01 ~ "**",
                         t_p_adj < 0.05 ~ "*", TRUE ~ "")) |>
  arrange(TraitName, RegionNeutral)

print(ttests, n = Inf, width = Inf)
write.csv(ttests, out_path("NonNeutralTraitTests"), row.names = FALSE)

# Figure S5: standardised traits of non-neutral taxa by region (0 = regional species-pool mean)
p_traits <- df_long |>
  filter(!is.na(TraitScore)) |>
  mutate(RegionNeutral = factor(RegionNeutral, levels = REGIONS)) |>
  ggplot() +
  geom_boxplot(aes(x = TraitName, y = TraitScore, fill = RegionNeutral)) +
  ggsci::scale_fill_npg(name = "Region") +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "Trait", y = "Trait value (z-score within regional species pool)") +
  theme_classic()
ggsave("Figures/Figure_traitsOfNonneutrals.png", p_traits, height = 8, width = 8)
