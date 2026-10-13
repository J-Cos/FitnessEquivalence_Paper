# -------------------------------------------------------------------
# Purpose: Run comprehensive sensitivity analysis across 6 scaling models
# -------------------------------------------------------------------

library(tidyverse)
library(lavaan)

source("Code/Functions/revision_helpers.R")

comm <- read.csv("Outputs/comm.csv") %>% as_tibble()
traits <- read.csv("Outputs/traits.csv") %>% as_tibble()

# Define the 6 scaling options

get_e_m_option1 <- function(comm, traits) {
  comm %>%
    left_join(select(traits, Binomial_correct, WD), by = "Binomial_correct") %>%
    mutate(minDBH = min(DBH, na.rm = TRUE)) %>%
    mutate(e = (DBH / minDBH)) %>%
    mutate(m = e^(4/3)) %>%
    select(Region, Region_Plot, Binomial_correct, m, e, WD)
}

get_e_m_option2 <- function(comm, traits) {
  comm %>%
    left_join(select(traits, Binomial_correct, WD), by = "Binomial_correct") %>%
    mutate(minDBH = min(DBH, na.rm = TRUE)) %>%
    mutate(e = (DBH / minDBH)) %>%
    mutate(scaledWD = scale(WD, scale=FALSE, center=(mean(WD, na.rm=TRUE)-1))) %>%
    mutate(m = e^(4/3) * scaledWD) %>%
    select(Region, Region_Plot, Binomial_correct, m, e, WD)
}

get_e_m_option3 <- function(comm, traits) {
  comm %>%
    left_join(select(traits, Binomial_correct, WD), by = "Binomial_correct") %>%
    mutate(minDBH = min(DBH, na.rm = TRUE)) %>%
    mutate(e = (DBH / minDBH)^2) %>%
    mutate(m = e^(4/3)) %>%
    select(Region, Region_Plot, Binomial_correct, m, e, WD)
}

get_e_m_option4 <- function(comm, traits) {
  comm %>%
    left_join(select(traits, Binomial_correct, WD), by = "Binomial_correct") %>%
    mutate(minDBH = min(DBH, na.rm = TRUE)) %>%
    mutate(e = (DBH / minDBH)^2) %>%
    mutate(scaledWD = scale(WD, scale=FALSE, center=(mean(WD, na.rm=TRUE)-1))) %>%
    mutate(m = e^(4/3) * scaledWD) %>%
    select(Region, Region_Plot, Binomial_correct, m, e, WD)
}

get_e_m_option5 <- function(comm, traits) {
  comm %>%
    left_join(select(traits, Binomial_correct, WD), by = "Binomial_correct") %>%
    mutate(minDBH = min(DBH, na.rm = TRUE)) %>%
    mutate(scaledWD = scale(WD, scale=FALSE, center=(mean(WD, na.rm=TRUE)-1))) %>%
    mutate(m = (DBH / minDBH)^(8/3) * scaledWD) %>%
    mutate(e = m^(3/4)) %>%
    select(Region, Region_Plot, Binomial_correct, m, e, WD)
}

get_e_m_option6 <- function(comm, traits) {
  comm %>%
    left_join(select(traits, Binomial_correct, WD), by = "Binomial_correct") %>%
    mutate(minDBH = min(DBH, na.rm = TRUE)) %>%
    mutate(scaledWD = scale(WD, scale=FALSE, center=(mean(WD, na.rm=TRUE)-1))) %>%
    mutate(D = DBH / minDBH) %>%
    mutate(m = scaledWD * D^2.67 * exp(-0.0299 * (log(D))^2)) %>%
    mutate(e = D^2 * exp(-0.0224 * (log(D))^2)) %>%
    select(Region, Region_Plot, Binomial_correct, m, e, WD)
}

options_list <- list(
  "Option 1: Linear, No WD" = get_e_m_option1,
  "Option 2: Linear, With WD (Original)" = get_e_m_option2,
  "Option 3: Quadratic, No WD" = get_e_m_option3,
  "Option 4: Quadratic, With WD (Metab-First)" = get_e_m_option4,
  "Option 5: Biomass-First Power Law (MTE)" = get_e_m_option5,
  "Option 6: Volume-Corrected Chave Hybrid" = get_e_m_option6
)

results <- list()

for (opt_name in names(options_list)) {
  cat("\n=========================================================\n")
  cat("Running", opt_name, "...\n")
  cat("=========================================================\n")
  
  # 1. Recalculate macrostates
  func <- options_list[[opt_name]]
  stats_df <- func(comm, traits)
  
  macrostates <- stats_df %>%
    group_by(Region, Region_Plot) %>%
    summarise(
      N = n(),
      S = length(unique(Binomial_correct)),
      B = sum(m, na.rm = TRUE),
      E = sum(e, na.rm = TRUE),
      .groups = "drop"
    )
  
  write.csv(macrostates, "Outputs/MacrostatesHarteWD_sens.csv", row.names = FALSE)
  
  # 2. Run EEOS python script
  python_status <- system2("/home/j/miniconda3/envs/EEOS-env/bin/python", c("Code/03_FitEEOS.py", "sens"))
  if (python_status != 0) stop("EEOS python script failed!")
  
  # 3. Refit subgroup Sloans
  sloan_status <- system2("Rscript", c("Code/10_FitForestCoverSubgroupSloans.r", "sens"), stdout = NULL)
  if (sloan_status != 0) stop("Sloan subgroup fitting script failed!")
  
  # 4. Load data for regressions and SEMs
  plot_data <- load_and_prepare_plot_data(suffix = "sens")
  df_modified <- load_and_prepare_subgroup_data(suffix = "sens")
  
  # 5. Baseline EEoS accuracy in undisturbed Jurua
  jurua_df <- plot_data %>% filter(Region == "Jurua")
  jurua_mean_dev <- mean(jurua_df$dev, na.rm = TRUE)
  jurua_mean_absdev <- mean(jurua_df$absdev, na.rm = TRUE)

  # 6. Fit simple deviation regressions (H1: Signed and Absolute)
  fit_signed <- lm(dev ~ disturbedcover + Region, data = plot_data)
  coef_signed <- summary(fit_signed)$coefficients
  h1_sig_slope <- coef_signed["disturbedcover", "Estimate"]
  h1_sig_p <- coef_signed["disturbedcover", "Pr(>|t|)"]

  fit_abs <- lm(absdev ~ disturbedcover + Region, data = plot_data)
  coef_abs <- summary(fit_abs)$coefficients
  h1_abs_slope <- coef_abs["disturbedcover", "Estimate"]
  h1_abs_p <- coef_abs["disturbedcover", "Pr(>|t|)"]
  
  # 7. Fit CATS Regression SEMs (H2a: Signed and Absolute)
  fit_h2a_sig <- sem('CATS_pois ~ disturbedcover + Reg_Balbina + Reg_STM + Reg_PGM\n dev ~ CATS_pois + Reg_Balbina + Reg_STM + Reg_PGM', data = plot_data, estimator = "ML", meanstructure = TRUE)
  est_h2a_sig <- parameterEstimates(fit_h2a_sig)
  p1_h2a_slope <- est_h2a_sig %>% filter(lhs == "CATS_pois", op == "~", rhs == "disturbedcover") %>% pull(est)
  p1_h2a_p <- est_h2a_sig %>% filter(lhs == "CATS_pois", op == "~", rhs == "disturbedcover") %>% pull(pvalue)
  p2_h2a_sig_slope <- est_h2a_sig %>% filter(lhs == "dev", op == "~", rhs == "CATS_pois") %>% pull(est)
  p2_h2a_sig_p <- est_h2a_sig %>% filter(lhs == "dev", op == "~", rhs == "CATS_pois") %>% pull(pvalue)
  fm_h2a_sig_p <- as.numeric(fitMeasures(fit_h2a_sig, "pvalue"))

  fit_h2a_abs <- sem('CATS_pois ~ disturbedcover + Reg_Balbina + Reg_STM + Reg_PGM\n absdev ~ CATS_pois + Reg_Balbina + Reg_STM + Reg_PGM', data = plot_data, estimator = "ML", meanstructure = TRUE)
  est_h2a_abs <- parameterEstimates(fit_h2a_abs)
  p2_h2a_abs_slope <- est_h2a_abs %>% filter(lhs == "absdev", op == "~", rhs == "CATS_pois") %>% pull(est)
  p2_h2a_abs_p <- est_h2a_abs %>% filter(lhs == "absdev", op == "~", rhs == "CATS_pois") %>% pull(pvalue)
  fm_h2a_abs_p <- as.numeric(fitMeasures(fit_h2a_abs, "pvalue"))
  
  # 8. Fit Non-Neutral % SEMs (H2b: Signed and Absolute)
  fit_h2b_sig <- sem('proportionNonNeutral ~ disturbedcover + Reg_Balbina + Reg_STM + Reg_PGM\n dev ~ proportionNonNeutral + Reg_Balbina + Reg_STM + Reg_PGM', data = plot_data, estimator = "ML", meanstructure = TRUE)
  est_h2b_sig <- parameterEstimates(fit_h2b_sig)
  p1_h2b_slope <- est_h2b_sig %>% filter(lhs == "proportionNonNeutral", op == "~", rhs == "disturbedcover") %>% pull(est)
  p1_h2b_p <- est_h2b_sig %>% filter(lhs == "proportionNonNeutral", op == "~", rhs == "disturbedcover") %>% pull(pvalue)
  p2_h2b_sig_slope <- est_h2b_sig %>% filter(lhs == "dev", op == "~", rhs == "proportionNonNeutral") %>% pull(est)
  p2_h2b_sig_p <- est_h2b_sig %>% filter(lhs == "dev", op == "~", rhs == "proportionNonNeutral") %>% pull(pvalue)
  fm_h2b_sig_p <- as.numeric(fitMeasures(fit_h2b_sig, "pvalue"))

  fit_h2b_abs <- sem('proportionNonNeutral ~ disturbedcover + Reg_Balbina + Reg_STM + Reg_PGM\n absdev ~ proportionNonNeutral + Reg_Balbina + Reg_STM + Reg_PGM', data = plot_data, estimator = "ML", meanstructure = TRUE)
  est_h2b_abs <- parameterEstimates(fit_h2b_abs)
  p2_h2b_abs_slope <- est_h2b_abs %>% filter(lhs == "absdev", op == "~", rhs == "proportionNonNeutral") %>% pull(est)
  p2_h2b_abs_p <- est_h2b_abs %>% filter(lhs == "absdev", op == "~", rhs == "proportionNonNeutral") %>% pull(pvalue)
  fm_h2b_abs_p <- as.numeric(fitMeasures(fit_h2b_abs, "pvalue"))
  
  # 9. Fit Subgroup Sloan SEMs (H2c: Signed devmedian and RMSE)
  fit_h2c_sig <- sem('r2pois ~ disturbedcover + Reg_Balbina + Reg_STM + Reg_PGM\n devmedian ~ r2pois + Reg_Balbina + Reg_STM + Reg_PGM', data = df_modified, estimator = "ML", meanstructure = TRUE)
  est_h2c_sig <- parameterEstimates(fit_h2c_sig)
  p1_h2c_slope <- est_h2c_sig %>% filter(lhs == "r2pois", op == "~", rhs == "disturbedcover") %>% pull(est)
  p1_h2c_p <- est_h2c_sig %>% filter(lhs == "r2pois", op == "~", rhs == "disturbedcover") %>% pull(pvalue)
  p2_h2c_sig_slope <- est_h2c_sig %>% filter(lhs == "devmedian", op == "~", rhs == "r2pois") %>% pull(est)
  p2_h2c_sig_p <- est_h2c_sig %>% filter(lhs == "devmedian", op == "~", rhs == "r2pois") %>% pull(pvalue)
  fm_h2c_sig_p <- as.numeric(fitMeasures(fit_h2c_sig, "pvalue"))

  fit_h2c_abs <- sem('r2pois ~ disturbedcover + Reg_Balbina + Reg_STM + Reg_PGM\n RMSE ~ r2pois + Reg_Balbina + Reg_STM + Reg_PGM', data = df_modified, estimator = "ML", meanstructure = TRUE)
  est_h2c_abs <- parameterEstimates(fit_h2c_abs)
  p2_h2c_abs_slope <- est_h2c_abs %>% filter(lhs == "RMSE", op == "~", rhs == "r2pois") %>% pull(est)
  p2_h2c_abs_p <- est_h2c_abs %>% filter(lhs == "RMSE", op == "~", rhs == "r2pois") %>% pull(pvalue)
  fm_h2c_abs_p <- as.numeric(fitMeasures(fit_h2c_abs, "pvalue"))
  
  # Store results
  results[[opt_name]] <- list(
    Jurua_dev = jurua_mean_dev, Jurua_absdev = jurua_mean_absdev,
    H1_Sig_Slope = h1_sig_slope, H1_Sig_P = h1_sig_p,
    H1_Abs_Slope = h1_abs_slope, H1_Abs_P = h1_abs_p,
    H2a_P1_Slope = p1_h2a_slope, H2a_P1_P = p1_h2a_p,
    H2a_P2_Sig_Slope = p2_h2a_sig_slope, H2a_P2_Sig_P = p2_h2a_sig_p, H2a_FM_Sig_P = fm_h2a_sig_p,
    H2a_P2_Abs_Slope = p2_h2a_abs_slope, H2a_P2_Abs_P = p2_h2a_abs_p, H2a_FM_Abs_P = fm_h2a_abs_p,
    H2b_P1_Slope = p1_h2b_slope, H2b_P1_P = p1_h2b_p,
    H2b_P2_Sig_Slope = p2_h2b_sig_slope, H2b_P2_Sig_P = p2_h2b_sig_p, H2b_FM_Sig_P = fm_h2b_sig_p,
    H2b_P2_Abs_Slope = p2_h2b_abs_slope, H2b_P2_Abs_P = p2_h2b_abs_p, H2b_FM_Abs_P = fm_h2b_abs_p,
    H2c_P1_Slope = p1_h2c_slope, H2c_P1_P = p1_h2c_p,
    H2c_P2_Sig_Slope = p2_h2c_sig_slope, H2c_P2_Sig_P = p2_h2c_sig_p, H2c_FM_Sig_P = fm_h2c_sig_p,
    H2c_P2_Abs_Slope = p2_h2c_abs_slope, H2c_P2_Abs_P = p2_h2c_abs_p, H2c_FM_Abs_P = fm_h2c_abs_p
  )
}

# Compile and print summary tables
df_abs <- do.call(rbind, lapply(names(results), function(n) {
  r <- results[[n]]
  data.frame(
    Model = n,
    Jurua_mean_dev = sprintf("%+.4f", r$Jurua_dev),
    H1_abs_slope = sprintf("%+.4f (p=%.4f)", r$H1_Abs_Slope, r$H1_Abs_P),
    H2a_path1 = sprintf("%+.4f (p=%.4f)", r$H2a_P1_Slope, r$H2a_P1_P),
    H2a_path2_abs = sprintf("%+.4f (p=%.4f; FM p=%.3f)", r$H2a_P2_Abs_Slope, r$H2a_P2_Abs_P, r$H2a_FM_Abs_P),
    H2b_path1 = sprintf("%+.4f (p=%.4f)", r$H2b_P1_Slope, r$H2b_P1_P),
    H2b_path2_abs = sprintf("%+.4f (p=%.4f; FM p=%.3f)", r$H2b_P2_Abs_Slope, r$H2b_P2_Abs_P, r$H2b_FM_Abs_P),
    H2c_path1 = sprintf("%+.4f (p=%.4f)", r$H2c_P1_Slope, r$H2c_P1_P),
    H2c_path2_RMSE = sprintf("%+.4f (p=%.4f; FM p=%.3f)", r$H2c_P2_Abs_Slope, r$H2c_P2_Abs_P, r$H2c_FM_Abs_P),
    stringsAsFactors = FALSE
  )
}))

df_sig <- do.call(rbind, lapply(names(results), function(n) {
  r <- results[[n]]
  data.frame(
    Model = n,
    Jurua_mean_dev = sprintf("%+.4f", r$Jurua_dev),
    H1_sig_slope = sprintf("%+.4f (p=%.4f)", r$H1_Sig_Slope, r$H1_Sig_P),
    H2a_path1 = sprintf("%+.4f (p=%.4f)", r$H2a_P1_Slope, r$H2a_P1_P),
    H2a_path2_sig = sprintf("%+.4f (p=%.4f; FM p=%.3f)", r$H2a_P2_Sig_Slope, r$H2a_P2_Sig_P, r$H2a_FM_Sig_P),
    H2b_path1 = sprintf("%+.4f (p=%.4f)", r$H2b_P1_Slope, r$H2b_P1_P),
    H2b_path2_sig = sprintf("%+.4f (p=%.4f; FM p=%.3f)", r$H2b_P2_Sig_Slope, r$H2b_P2_Sig_P, r$H2b_FM_Sig_P),
    H2c_path1 = sprintf("%+.4f (p=%.4f)", r$H2c_P1_Slope, r$H2c_P1_P),
    H2c_path2_sig = sprintf("%+.4f (p=%.4f; FM p=%.3f)", r$H2c_P2_Sig_Slope, r$H2c_P2_Sig_P, r$H2c_FM_Sig_P),
    stringsAsFactors = FALSE
  )
}))

cat("\n\n=========================================================\n")
cat("SENSITIVITY ANALYSIS RESULTS (ABSOLUTE DEVIATION / RMSE)\n")
cat("=========================================================\n\n")
print(df_abs)

cat("\n\n=========================================================\n")
cat("SENSITIVITY ANALYSIS RESULTS (SIGNED DEVIATION)\n")
cat("=========================================================\n\n")
print(df_sig)

write.csv(df_abs, "Outputs/SensitivityAnalysisResults_Absolute.csv", row.names = FALSE)
write.csv(df_sig, "Outputs/SensitivityAnalysisResults_Signed.csv", row.names = FALSE)
write.csv(df_sig, "Outputs/SensitivityAnalysisResults.csv", row.names = FALSE)
