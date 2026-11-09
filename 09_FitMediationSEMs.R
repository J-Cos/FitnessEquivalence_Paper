# --------------------------------------------
# Purpose: Fit all H2a-c causal mediation Structural Equation Models (SEMs) following the 5-step
#          Model Selection Approach to SEM (MSA-SEM) of Garrido et al. (2022, Methods Ecol. Evol.
#          13:42-53). Pure computation (pathway diagrams and mediation scatter figures in 10).
#
#          Evaluates the complete, balanced 2^3 = 8-model recursive candidate set spanning all
#          inclusion/exclusion subsets of paths a (X -> M), b (M -> Y), and c (X -> Y) over the
#          exact same 6x6 empirical covariance matrix S (0* constraints on omitted focal paths,
#          region fixed effects with Jurua = reference, and 2,000-draw percentile bootstrap 95% CIs):
#            1. Primary (Signed EEoS deviation):
#                 - H2a_Signed: disturbedcover -> CATS_nb -> dev (N = 224 plots)
#                 - H2b_Signed: disturbedcover -> proportionNonNeutral -> dev (N = 224 plots)
#                 - H2c_Signed: disturbedcover -> r2pois -> devmedian (N = 43 five-plot subgroups)
#            2. Secondary (Absolute / RMSE EEoS deviation):
#                 - H2a_Absolute: disturbedcover -> CATS_nb -> absdev (N = 224 plots)
#                 - H2b_Absolute: disturbedcover -> proportionNonNeutral -> absdev (N = 224 plots)
#                 - H2c_Absolute: disturbedcover -> r2pois -> RMSE (N = 43 five-plot subgroups)
#            3. CATS formulation sensitivity (3-trait excluding Hmax, and classical Poisson CATS):
#                 - H2a_3trait_Signed, H2a_3trait_Absolute
#                 - H2a_Poisson_Signed, H2a_Poisson_3trait_Signed
# Input Origin Scripts: 03_FitEEOS.py; 05_FitCatsRegressions.R; 06_FitRegionalSloanNonNeutral.R;
#                       07_FitSubgroupSloans.R
# Output: Outputs/H2{a,b,c}_{Signed,Absolute}_{Model_Comparison,Parameter_Estimates,LRT,
#           Multimodel_Inference,IndirectEffect}.csv;
#         Outputs/H2a_{3trait_Signed,3trait_Absolute,Poisson_Signed,Poisson_3trait_Signed}_*.csv;
#         Outputs/H2_Mediation_Summary.csv
# ---------------------------------------------------

suppressMessages(library(tidyverse))
source("Code/Functions/config.R"); check_project_root()
source("Code/Functions/data_prep.R")
source("Code/Functions/sem_helpers.R")

set.seed(1)
plot_data <- load_and_prepare_plot_data(need = c("cats", "nonneutral"))
sub_data  <- load_and_prepare_subgroup_data()

# Verify that all regional non-neutral taxa are LFHA (so proportionLFHA == proportionNonNeutral)
nn_props <- read_required_csv(out_path("NonNeutralProportions"))
stopifnot(max(abs(nn_props$proportionNonNeutral - nn_props$proportionLFHA)) < 1e-12,
          all(nn_props$proportionHFLA == 0))

sem_specs <- tribble(
  ~id,                         ~tier,         ~dataset,   ~m,                     ~y,
  "H2a_Signed",                "Primary",     "plot",     "CATS_nb",              "dev",
  "H2b_Signed",                "Primary",     "plot",     "proportionNonNeutral", "dev",
  "H2c_Signed",                "Primary",     "subgroup", "r2pois",               "devmedian",
  "H2a_Absolute",              "Secondary",   "plot",     "CATS_nb",              "absdev",
  "H2b_Absolute",              "Secondary",   "plot",     "proportionNonNeutral", "absdev",
  "H2c_Absolute",              "Secondary",   "subgroup", "r2pois",               "RMSE",
  "H2a_3trait_Signed",         "Sensitivity", "plot",     "CATS_nb_3trait",       "dev",
  "H2a_3trait_Absolute",       "Sensitivity", "plot",     "CATS_nb_3trait",       "absdev",
  "H2a_Poisson_Signed",        "Sensitivity", "plot",     "CATS_pois",            "dev",
  "H2a_Poisson_3trait_Signed", "Sensitivity", "plot",     "CATS_pois_3trait",     "dev"
)

runs <- pmap(sem_specs, function(id, tier, dataset, m, y) {
  d <- if (dataset == "plot") plot_data else sub_data
  res <- run_mediation_hypothesis(d, id = id, m = m, y = y, indirect = TRUE, n_boot = 2000, seed = 1)
  mutate(res$summary, Tier = tier, .after = Hypothesis_ID)
})

summary_tab <- bind_rows(runs)
write.csv(summary_tab, out_path("H2_Mediation_Summary"), row.names = FALSE)

cat("\n=== Consolidated H2 Mediation Summary (8-Model Balanced Garrido Set) ===\n")
print(
  summary_tab |>
    select(Hypothesis_ID, Tier, N, Mediator, Outcome,
           a_est, a_p, b_full_est, b_full_p, b_part_est, b_part_p, c_part_est, c_part_p,
           ind_full_est, ind_full_ci_low, ind_full_ci_high,
           Best_Model_AICc, w_Partial, w_Full, w_Coresponse, w_Independent,
           RelImp_a, RelImp_b, RelImp_c, RelImp_ind, FullMed_p),
  width = Inf
)
message("Saved all H2a-c SEM tables and Outputs/H2_Mediation_Summary.csv")
