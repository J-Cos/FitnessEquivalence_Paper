# -------------------------------------------------------------------
# Purpose: Unit tests for revision_helpers.R
#          Verifies data formats, constraints, and helper functions.
# -------------------------------------------------------------------

source("Code/Functions/revision_helpers.R")

cat("=== Starting Unit Tests ===\n\n")

# -------------------------------------------------------------------
# Test 1: load_and_prepare_plot_data
# -------------------------------------------------------------------
cat("Test 1: Testing load_and_prepare_plot_data...\n")
plot_data <- load_and_prepare_plot_data()

# Asserts
stopifnot(nrow(plot_data) == 224)
stopifnot(all(c("disturbedcover", "absdev", "Reg_Balbina", "Reg_STM", "Reg_PGM") %in% colnames(plot_data)))
stopifnot(all(plot_data$disturbedcover[plot_data$Region == "Jurua"] == 0.0))
stopifnot(all(plot_data$Reg_Balbina[plot_data$Region == "Balbina"] == 1))
stopifnot(all(plot_data$Reg_Balbina[plot_data$Region != "Balbina"] == 0))
stopifnot(levels(plot_data$Region) == c("Jurua", "Balbina", "STM", "PGM"))
cat("  -> Test 1 passed successfully.\n\n")

# -------------------------------------------------------------------
# Test 2: load_and_prepare_subgroup_data
# -------------------------------------------------------------------
cat("Test 2: Testing load_and_prepare_subgroup_data...\n")
sub_data <- load_and_prepare_subgroup_data()

# Asserts
stopifnot(nrow(sub_data) == 43)
stopifnot(all(c("disturbedcover", "Reg_Balbina", "Reg_STM", "Reg_PGM") %in% colnames(sub_data)))
stopifnot(all(sub_data$disturbedcover[sub_data$region == "Jurua"] == 0.0))
stopifnot(levels(sub_data$region) == c("Jurua", "Balbina", "STM", "PGM"))
cat("  -> Test 2 passed successfully.\n\n")

# -------------------------------------------------------------------
# Test 3: get_parallel_lines_coefs
# -------------------------------------------------------------------
cat("Test 3: Testing get_parallel_lines_coefs with a dummy fit...\n")
model_test <- '
  CATS_pois ~ disturbedcover + Reg_Balbina + Reg_STM + Reg_PGM
  absdev ~ CATS_pois + Reg_Balbina + Reg_STM + Reg_PGM
'
fit_test <- sem(model_test, data = plot_data, estimator = "ML")

coefs_cats <- get_parallel_lines_coefs(fit_test, outcome = "CATS_pois", predictor = "disturbedcover")
coefs_absdev <- get_parallel_lines_coefs(fit_test, outcome = "absdev", predictor = "CATS_pois")

# Asserts
stopifnot(nrow(coefs_cats) == 4)
stopifnot(all(coefs_cats$region == c("Jurua", "Balbina", "STM", "PGM")))
stopifnot(length(unique(coefs_cats$slope)) == 1) # Parallel lines must have identical slopes
stopifnot(nrow(coefs_absdev) == 4)
stopifnot(length(unique(coefs_absdev$slope)) == 1)

cat("  -> Test 3 passed successfully.\n\n")

# -------------------------------------------------------------------
# Test 4: calculate_aicc & print_sem_comparison
# -------------------------------------------------------------------
cat("Test 4: Testing calculate_aicc and print_sem_comparison...\n")
model_partial <- '
  CATS_pois ~ disturbedcover + Reg_Balbina + Reg_STM + Reg_PGM
  absdev ~ CATS_pois + disturbedcover + Reg_Balbina + Reg_STM + Reg_PGM
'
model_direct <- '
  absdev ~ disturbedcover + Reg_Balbina + Reg_STM + Reg_PGM
  CATS_pois ~ disturbedcover + Reg_Balbina + Reg_STM + Reg_PGM
'
fit_partial <- sem(model_partial, data = plot_data, estimator = "ML")
fit_direct <- sem(model_direct, data = plot_data, estimator = "ML")

aicc_val <- calculate_aicc(fit_test)
stopifnot(!is.na(aicc_val) && is.numeric(aicc_val))

# Print comparison to console to verify formatting (no crash check)
print_sem_comparison(fit_test, fit_partial, fit_direct)
cat("  -> Test 4 passed successfully.\n\n")

# -------------------------------------------------------------------
# Test 5: get_marginal_predictions
# -------------------------------------------------------------------
cat("Test 5: Testing get_marginal_predictions...\n")
marg_preds <- get_marginal_predictions(plot_data, outcome = "absdev", predictor = "CATS_pois", grid_points = 50)
stopifnot(nrow(marg_preds) == 50)
stopifnot(all(c("x", "y", "lwr", "upr") %in% colnames(marg_preds)))
stopifnot(all(marg_preds$upr >= marg_preds$y))
stopifnot(all(marg_preds$y >= marg_preds$lwr))
cat("  -> Test 5 passed successfully.\n\n")

cat("=== All Unit Tests Passed! ===\n")
