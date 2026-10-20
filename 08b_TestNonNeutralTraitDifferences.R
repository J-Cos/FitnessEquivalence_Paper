# --------------------------------------------
# Purpose: Test whether the traits of non-neutral taxa differ from average trait values, per region
#          (one-sample t-tests and Wilcoxon tests of standardised traits against 0; BH within trait).
# Input Origin Scripts: 08_FitOverallSloanAndExtractNonneutralAbundances.R
# Output: Outputs/NonNeutralTraitTests.csv
# ---------------------------------------------------

suppressMessages(library(tidyverse))
source("Code/Functions/config.R"); check_project_root()

TRAITS <- c("WD", "lnSM", "LMA", "Hmax")
non_neutrals <- read_required_csv(out_path("NonNeutralTaxa"))

df_long <- non_neutrals |>
  pivot_longer(all_of(REGIONS), names_to = "RegionNeutral", values_to = "NeutralScore") |>
  filter(NeutralScore == 1) |>
  mutate(across(all_of(TRAITS), \(x) as.numeric(scale(x)))) |>
  pivot_longer(all_of(TRAITS), names_to = "TraitName", values_to = "TraitScore")

safe_p <- function(test) tryCatch(test$p.value, error = function(e) NA_real_)
ttests <- df_long |>
  group_by(TraitName, RegionNeutral) |>
  summarise(n = sum(!is.na(TraitScore)),
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

print(ttests, n = Inf)
write.csv(ttests, out_path("NonNeutralTraitTests"), row.names = FALSE)
