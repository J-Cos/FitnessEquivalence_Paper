# --------------------------------------------
# Purpose: Combines the Jurua data with the Pinho et al. data (Balbina, PGM, STM),
#          harmonising taxon names and filling missing taxa in the trait table.
# Input Origin Scripts: None (reads Data/)
# Output: Outputs/comm.csv (one row per stem) and Outputs/traits.csv (one row per taxon)
# ---------------------------------------------------

suppressMessages(library(tidyverse))
source("Code/Functions/config.R"); check_project_root()
source("Code/Functions/data_prep.R")

# Load raw datasets ----------------------------------------------------------------------------
jurua  <- read_required_csv("Data/Jurua_Flora_FullDB.csv")
pinho  <- read_required_csv("Data/comm.csv")
traits <- read_required_csv("Data/traits.csv")

# Community (stems) ----------------------------------------------------------------------------
comm_new <- harmonise_stem_data(pinho, jurua, dbh_min = DBH_MIN_CM)

if (interactive()) {
  print(table(comm_new$Region)); print(table(comm_new$Region_Plot))
  binoms <- str_trim(comm_new$Binomial_correct)
  print(table(binoms[binoms %in% strings_with_n_diff(unique(binoms), 1)]))
  genera <- str_trim(comm_new$Genus)
  print(table(genera[genera %in% strings_with_n_diff(unique(genera), 1)]))
  print(table(str_trim(comm_new$family)))
  print(summary(comm_new$DBH)); print(comm_new[comm_new$DBH > 120, ])
}
write.csv(comm_new, out_path("comm"), row.names = FALSE)

# Traits (taxa) --------------------------------------------------------------------------------
if (interactive()) {
  jurua_traits <- jurua |>
    filter(DBH >= DBH_MIN_CM) |>
    transmute(Binomial_correct = fix_taxon_names(species), WD, lnSM = log(SM), LMA)
  # taxa lacking seed mass, and taxa with non-identical replicate trait records
  jurua_traits |> filter(!Binomial_correct %in% traits$Binomial_correct) |>
    group_by(Binomial_correct) |> summarise(across(everything(), mean)) |>
    filter(is.na(WD) | is.na(lnSM) | is.na(LMA)) |> print(n = 999)
  jurua_traits |> filter(!Binomial_correct %in% traits$Binomial_correct) |>
    group_by(Binomial_correct) |> summarise(across(everything(), sd)) |>
    filter(!WD == 0 | !lnSM == 0 | !LMA == 0) |> print(n = 999)
}

traits_new <- harmonise_trait_table(traits, jurua, dbh_min = DBH_MIN_CM)
stopifnot(!anyDuplicated(traits_new$Binomial_correct),
          all(unique(comm_new$Binomial_correct) %in% traits_new$Binomial_correct))
write.csv(traits_new, out_path("traits"), row.names = FALSE)
