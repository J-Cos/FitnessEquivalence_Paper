# --------------------------------------------
# Purpose: Combines the Jurua data with the Pinho et al. data (Balbina, PGM, STM),
#          harmonising taxon names and filling missing taxa in the trait table.
# Input Origin Scripts: None (reads Data/)
# Output: Outputs/comm.csv (one row per stem) and Outputs/traits.csv (one row per taxon)
# ---------------------------------------------------

suppressMessages(library(tidyverse))
source("Code/Functions/config.R"); check_project_root()

# Pairs of names differing by exactly n characters (same length; Hamming distance) -------------
hamming_distance <- function(a, b) {
  if (nchar(a) != nchar(b)) return(Inf)
  sum(strsplit(a, "")[[1]] != strsplit(b, "")[[1]])
}
strings_with_n_diff <- function(vec, n) {
  combs <- expand.grid(a = vec, b = vec, stringsAsFactors = FALSE) |> filter(a != b)
  combs$dist <- mapply(hamming_distance, combs$a, combs$b)
  unique(unlist(combs[combs$dist == n, c("a", "b")]))
}

# Typos identified by inspecting near-identical names (see interactive checks below)
NAME_FIXES <- c("Copaifera guyanensis" = "Copaifera guianensis",
                "Licaria macrophylla"  = "Licania macrophylla",
                "Licaria rodriguesii"  = "Licania rodriguesii")
fix_names <- function(x) {
  x <- str_trim(x)
  hit <- x %in% names(NAME_FIXES)
  x[hit] <- NAME_FIXES[x[hit]]
  x
}

# Load data ------------------------------------------------------------------------------------
jurua  <- read_required_csv("Data/Jurua_Flora_FullDB.csv") |> filter(DBH >= 10)
pinho  <- read_required_csv("Data/comm.csv") |> filter(Region %in% c("Balbina", "PGM", "STM"))
traits <- read_required_csv("Data/traits.csv")

# Community (stems) ----------------------------------------------------------------------------
comm_new <- bind_rows(
  pinho |> select(X, Region, Region_Plot, Binomial_correct, Genus, family, DBH),
  jurua |>
    transmute(X = NA, Region = "Jurua", Region_Plot = parcel, Binomial_correct = species,
              Genus = genus, family, DBH)
) |>
  mutate(X = row_number(), Binomial_correct = fix_names(Binomial_correct))

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
# Jurua traits: Hmax and dispersal syndrome (DS) are not recorded in the Jurua database
jurua_traits <- jurua |>
  transmute(Binomial_correct = fix_names(species), WD, lnSM = log(SM), LMA, Hmax = NA, DS = NA)

# Jurua taxa absent from the Pinho trait table; replicated records are summarised by the median
traits_needed <- jurua_traits |>
  filter(!Binomial_correct %in% traits$Binomial_correct) |>
  group_by(Binomial_correct) |>
  summarise(across(everything(), median))

if (interactive()) {
  # taxa lacking seed mass, and taxa with non-identical replicate trait records
  jurua_traits |> filter(!Binomial_correct %in% traits$Binomial_correct) |>
    group_by(Binomial_correct) |> summarise(across(everything(), mean)) |>
    filter(is.na(WD) | is.na(lnSM) | is.na(LMA)) |> print(n = 999)
  jurua_traits |> filter(!Binomial_correct %in% traits$Binomial_correct) |>
    group_by(Binomial_correct) |> summarise(across(everything(), sd)) |>
    filter(!WD == 0 | !lnSM == 0 | !LMA == 0) |> print(n = 999)
}

# Remove an erroneous imputed trait row for a misspelt taxon in the Pinho table, then combine
traits_new <- traits |>
  filter(Binomial_correct != "Licaria rodriguesii") |>
  bind_rows(traits_needed)
stopifnot(!anyDuplicated(traits_new$Binomial_correct),
          all(unique(comm_new$Binomial_correct) %in% traits_new$Binomial_correct))
write.csv(traits_new, out_path("traits"), row.names = FALSE)
