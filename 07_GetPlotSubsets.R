# --------------------------------------------
# Purpose: Plot subsets for the neutral metacommunity models.
#          (1) 100 random subsamples of 29 plots per region (H2b);
#          (2) non-overlapping sets of 5 plots ordered by forest cover within region (H2c).
# Input Origin Scripts: 01_CombinePinhoJuruaData.R
# Output: Outputs/PlotSubsets.RDS, Outputs/NonOverlappingOrderedPlotSubsets.RDS
# ---------------------------------------------------

suppressMessages(library(tidyverse))
source("Code/Functions/config.R"); check_project_root()

comm <- read_required_csv(out_path("comm"))
env  <- read_required_csv("Data/env.csv")

plots <- comm |>
  group_by(Region, Region_Plot) |>
  summarise(value = n(), .groups = "drop")

# (1) random subsamples; seed i for replicate i (sample_n kept so draws match the submission)
plot_subsets <- map(seq_len(N_SUBSAMPLES), function(i) {
  set.seed(i)
  plots |>
    group_by(Region) |>
    sample_n(SUBSAMPLE_SIZE) |>
    ungroup() |>
    pull(Region_Plot)
})
saveRDS(plot_subsets, out_path("PlotSubsets", ext = "RDS"))

# (2) non-overlapping consecutive sets of SUBGROUP_SIZE plots; incomplete final sets dropped
split_into_sets <- function(plot_ids, size = SUBGROUP_SIZE) {
  sets <- split(plot_ids, ceiling(seq_along(plot_ids) / size))
  sets[lengths(sets) == size]
}

ordered_sets <- list()
for (region in c("Balbina", "PGM", "STM")) {
  ordered_ids <- env |>
    filter(Region_Plot %in% comm$Region_Plot, Region == region) |>
    arrange(fc2000) |>
    pull(Region_Plot)
  ordered_sets[[region]] <- split_into_sets(ordered_ids)
}
# Jurua has no forest-cover gradient (intact); sets follow plot ID order
ordered_sets[["Jurua"]] <- split_into_sets(filter(plots, Region == "Jurua")$Region_Plot)

saveRDS(unlist(ordered_sets, recursive = FALSE), out_path("NonOverlappingOrderedPlotSubsets", ext = "RDS"))
