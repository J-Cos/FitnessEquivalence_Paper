# --------------------------------------------
# Purpose: Calculates plot-level ecosystem macrostates (S, N, E, B) under all six
#          organism-level scalings (see Functions/macrostates.R; main analysis = harte_wd).
# Input Origin Scripts: 01_CombinePinhoJuruaData.R
# Output: Outputs/Macrostates_<suffix>.csv for each scaling
# ---------------------------------------------------

suppressMessages(library(tidyverse))
source("Code/Functions/config.R"); check_project_root()
source("Code/Functions/macrostates.R")

comm   <- read_required_csv(out_path("comm"))
traits <- read_required_csv(out_path("traits"))

for (suffix in SCALING_SUFFIXES) {
  macrostates <- compute_organism_stats(comm, traits, suffix) |> summarise_macrostates()
  stopifnot(nrow(macrostates) == n_distinct(comm$Region_Plot),
            sum(macrostates$N) == nrow(comm),
            all(macrostates$E >= macrostates$N))   # every stem has e >= 1
  write.csv(macrostates, out_path("Macrostates", suffix), row.names = FALSE)
  message("Macrostates written for ", suffix)
}