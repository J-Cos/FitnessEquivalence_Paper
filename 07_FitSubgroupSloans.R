# --------------------------------------------
# Purpose: Fitness-equivalence Measure 3 (H2c) - Neutral metacommunity (Sloan / Poisson
#          random-sampling) model fit across non-overlapping forest-cover-ordered 5-plot subgroups
#          within each region, sharing the exact same underlying fitting functions in sloan_helpers.R
#          as 06_FitRegionalSloanNonNeutral.R. Pure computation (figures in 08).
# Input Origin Scripts: 01_CombinePinhoJuruaData.R
# Output: Outputs/NonOverlappingOrderedPlotSubsets.RDS; Outputs/ForestCoverSubgroupSloanFits.csv;
#         Outputs/ForestCoverSubgroupSloanObjects.RDS
# ---------------------------------------------------

suppressMessages(library(tidyverse))
source("Code/Functions/config.R"); check_project_root()
source("Code/Functions/sloan_helpers.R")

comm <- read_required_csv(out_path("comm"))
env  <- read_required_csv("Data/env.csv")

# 1) Non-overlapping 5-plot subgroups ordered by forest cover within region ---------------------
subsets <- make_ordered_forest_cover_subgroups(comm, env)
saveRDS(subsets, out_path("NonOverlappingOrderedPlotSubsets", ext = "RDS"))

# 2) Neutral metacommunity fits per subgroup ----------------------------------------------------
fits <- fit_sloan_over_plot_sets(comm, subsets)
saveRDS(fits, out_path("ForestCoverSubgroupSloanObjects", ext = "RDS"))

fit_table <- imap_dfr(fits, \(f, name) mutate(
  extract_sloan_summary(f),
  subgroup = name,
  region   = substr(name, 1, 3),
  .before  = 1
))
print(summary(select(fit_table, dAIC_v_binom, dAIC_v_pois, dAIC_pois_v_binom)))
write.csv(fit_table, out_path("ForestCoverSubgroupSloanFits"), row.names = FALSE)
message(sprintf("Saved %d subgroup neutral-model fits to Outputs/ForestCoverSubgroupSloanFits.csv", nrow(fit_table)))
