# --------------------------------------------
# Purpose: Calculates plot-level ecosystem macrostates (S, N, E, B), the 3-run BIOMASS
#          decomposition quantities (B_rho, B_size, Q_obs, Q_size, W_H, W_rho), and plot-level
#          biomass uncertainty (B_cv) via BIOMASS::AGBmonteCarlo (WD + height + allometric error,
#          excluding diameter measurement error).
# Input Origin Scripts: 01_CombinePinhoJuruaData.R (plus Data/env.csv for plot coordinates)
# Output: Outputs/Macrostates.csv
# ---------------------------------------------------

suppressMessages({ library(tidyverse); library(BIOMASS) })
source("Code/Functions/config.R"); check_project_root()
source("Code/Functions/macrostates.R")

comm   <- read_required_csv(out_path("comm"))
traits <- read_required_csv(out_path("traits"))
env    <- read_required_csv("Data/env.csv")

macrostates <- compute_organism_stats(comm, traits, env) |>
  summarise_macrostates()

mc_cv <- compute_biomass_mc_cv(comm, traits, env, n_sim = 1000, seed = 1)
macrostates <- dplyr::left_join(macrostates, mc_cv, by = "Region_Plot", relationship = "one-to-one")

stopifnot(
  nrow(macrostates) == n_distinct(comm$Region_Plot),
  sum(macrostates$N) == nrow(comm),
  all(macrostates$E > macrostates$N),   # every plot strictly inside METE's domain (E > N)
  max(abs(macrostates$Q_size * macrostates$W_H * macrostates$W_rho - macrostates$Q_obs)) < 1e-12,
  !anyNA(macrostates$B_cv),
  all(macrostates$B_cv > 0)
)

write.csv(macrostates, out_path("Macrostates"), row.names = FALSE)
message(sprintf(
  "Macrostates written to %s (%d plots, %d stems, rho_ref = %.4f g/cm^3, AGB_ref = %.2f kg, median B_cv = %.3f)",
  out_path("Macrostates"), nrow(macrostates), sum(macrostates$N),
  macrostates$rho_ref[1], 1000 * macrostates$AGB_ref_Mg[1], median(macrostates$B_cv)
))