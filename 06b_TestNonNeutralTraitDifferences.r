library(tidyverse)
library(ggplot2)
 library(ggsci)  # optional for nicer fill palette

# 1. prepare data (same as before)
df_long <- NonNeutrals %>%
  pivot_longer(cols = c(Balbina, Jurua, PGM, STM),
               names_to = "RegionNeutral",
               values_to = "NeutralScore") %>%
  filter(NeutralScore == 1) %>%
  mutate(
    WD   = as.numeric(scale(WD)),
    lnSM = as.numeric(scale(lnSM)),
    LMA  = as.numeric(scale(LMA)),
    Hmax = as.numeric(scale(Hmax)),
    RegionNeutral = factor(RegionNeutral)
  ) %>%
  pivot_longer(cols = c(WD, lnSM, LMA, Hmax),
               names_to = "TraitName",
               values_to = "TraitScore")

# 2. Run one-sample t-tests (trait mean vs 0) for each TraitName x RegionNeutral
ttests <- df_long %>%
  group_by(TraitName, RegionNeutral) %>%
  summarise(
    n = sum(!is.na(TraitScore)),
    mean = mean(TraitScore, na.rm = TRUE),
    sd   = sd(TraitScore, na.rm = TRUE),
    # run t-test if n >= 2 (otherwise NA)
    t_test_p = ifelse(n >= 2, t.test(TraitScore, mu = 0)$p.value, NA_real_),
    # also compute a Wilcoxon signed-rank p (non-parametric) for reference
    wilcox_p = ifelse(n >= 1, tryCatch(wilcox.test(TraitScore, mu = 0, exact = FALSE)$p.value, error = function(e) NA_real_), NA_real_),
    .groups = "drop"
  )

# 3. Adjust p-values: here I adjust within each TraitName (BH per trait).
ttests <- ttests %>%
  group_by(TraitName) %>%
  mutate(
    t_p_adj = p.adjust(t_test_p, method = "BH"),
    w_p_adj = p.adjust(wilcox_p, method = "BH")
  ) %>%
  ungroup() %>%
  mutate(
    # choose which adjusted p to use for the stars: t-test by default
    p_adj = t_p_adj,
    # create significance stars
    sig = case_when(
      is.na(p_adj) ~ "",
      p_adj < 0.001 ~ "***",
      p_adj < 0.01  ~ "**",
      p_adj < 0.05  ~ "*",
      TRUE          ~ ""
    )
  )
  
# 7. Print table of results for inspection
ttests %>% select(TraitName, RegionNeutral, n, mean, sd, t_test_p, t_p_adj, wilcox_p, sig) %>% arrange(TraitName, RegionNeutral) %>% print(n = Inf)
