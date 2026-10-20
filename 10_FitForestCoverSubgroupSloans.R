# --------------------------------------------
# Purpose: Fit neutral metacommunity (Sloan / random-sampling) models to each forest-cover-ordered
#          subgroup of 5 plots (H2c). Fits do not depend on the macrostate scaling; the
#          suffix-specific EEoS summaries are joined later by load_and_prepare_subgroup_data().
# Input Origin Scripts: 01_CombinePinhoJuruaData.R; 07_GetPlotSubsets.R
# Output: Outputs/ForestCoverSubgroupSloanFits.csv; Figure S6
# ---------------------------------------------------

suppressMessages(library(tidyverse))
source("Code/Functions/config.R"); check_project_root()
source("Code/Functions/sloan_helpers.R")

comm <- read_required_csv(out_path("comm"))
subsets <- readRDS(out_path("NonOverlappingOrderedPlotSubsets", ext = "RDS"))

fits <- map(subsets, \(ids) sncm_fit(build_community_matrix(comm, ids)))

fit_table <- imap_dfr(fits, \(f, name) mutate(extract_sloan_summary(f), subgroup = name,
                                              region = substr(name, 1, 3), .before = 1))
print(summary(select(fit_table, dAIC_v_binom, dAIC_v_pois, dAIC_pois_v_binom)))
write.csv(fit_table, out_path("ForestCoverSubgroupSloanFits"), row.names = FALSE)

# Figure S6: all subgroup fits with Poisson random-sampling 95% intervals
ggsave("Figures/Figure_sloansubsets_supplement.png",
       cowplot::plot_grid(plotlist = imap(fits, plot_sloan_fit)), height = 20, width = 20)
