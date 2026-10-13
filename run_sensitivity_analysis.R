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
  
  # 5. Fit simple deviation regression (H1)
  fit_signed <- lm(dev ~ disturbedcover + Region, data = plot_data)
  s_signed <- summary(fit_signed)
  coef_signed <- s_signed$coefficients
  h1_slope <- coef_signed["disturbedcover", "Estimate"]
  h1_p <- coef_signed["disturbedcover", "Pr(>|t|)"]
  
  # 6. Fit CATS Regression SEM (H2a)
  model_h2a <- '
    CATS_pois ~ disturbedcover + Reg_Balbina + Reg_STM + Reg_PGM
    dev ~ CATS_pois + Reg_Balbina + Reg_STM + Reg_PGM
  '
  fit_h2a <- sem(model_h2a, data = plot_data, estimator = "ML", meanstructure = TRUE)
  est_h2a <- parameterEstimates(fit_h2a)
  p1_h2a_slope <- est_h2a %>% filter(lhs == "CATS_pois", op == "~", rhs == "disturbedcover") %>% pull(est)
  p1_h2a_p <- est_h2a %>% filter(lhs == "CATS_pois", op == "~", rhs == "disturbedcover") %>% pull(pvalue)
  p2_h2a_slope <- est_h2a %>% filter(lhs == "dev", op == "~", rhs == "CATS_pois") %>% pull(est)
  p2_h2a_p <- est_h2a %>% filter(lhs == "dev", op == "~", rhs == "CATS_pois") %>% pull(pvalue)
  
  # 7. Fit Non-Neutral % SEM (H2b)
  model_h2b <- '
    proportionNonNeutral ~ disturbedcover + Reg_Balbina + Reg_STM + Reg_PGM
    dev ~ proportionNonNeutral + Reg_Balbina + Reg_STM + Reg_PGM
  '
  fit_h2b <- sem(model_h2b, data = plot_data, estimator = "ML", meanstructure = TRUE)
  est_h2b <- parameterEstimates(fit_h2b)
  p1_h2b_slope <- est_h2b %>% filter(lhs == "proportionNonNeutral", op == "~", rhs == "disturbedcover") %>% pull(est)
  p1_h2b_p <- est_h2b %>% filter(lhs == "proportionNonNeutral", op == "~", rhs == "disturbedcover") %>% pull(pvalue)
  p2_h2b_slope <- est_h2b %>% filter(lhs == "dev", op == "~", rhs == "proportionNonNeutral") %>% pull(est)
  p2_h2b_p <- est_h2b %>% filter(lhs == "dev", op == "~", rhs == "proportionNonNeutral") %>% pull(pvalue)
  
  # 8. Fit Subgroup Sloan SEM (H2c)
  model_h2c <- '
    r2pois ~ disturbedcover + Reg_Balbina + Reg_STM + Reg_PGM
    devmedian ~ r2pois + Reg_Balbina + Reg_STM + Reg_PGM
  '
  fit_h2c <- sem(model_h2c, data = df_modified, estimator = "ML", meanstructure = TRUE)
  est_h2c <- parameterEstimates(fit_h2c)
  p1_h2c_slope <- est_h2c %>% filter(lhs == "r2pois", op == "~", rhs == "disturbedcover") %>% pull(est)
  p1_h2c_p <- est_h2c %>% filter(lhs == "r2pois", op == "~", rhs == "disturbedcover") %>% pull(pvalue)
  p2_h2c_slope <- est_h2c %>% filter(lhs == "devmedian", op == "~", rhs == "r2pois") %>% pull(est)
  p2_h2c_p <- est_h2c %>% filter(lhs == "devmedian", op == "~", rhs == "r2pois") %>% pull(pvalue)
  
  # Store results
  results[[opt_name]] <- list(
    H1_Slope = h1_slope, H1_P = h1_p,
    H2a_P1_Slope = p1_h2a_slope, H2a_P1_P = p1_h2a_p,
    H2a_P2_Slope = p2_h2a_slope, H2a_P2_P = p2_h2a_p,
    H2b_P1_Slope = p1_h2b_slope, H2b_P1_P = p1_h2b_p,
    H2b_P2_Slope = p2_h2b_slope, H2b_P2_P = p2_h2b_p,
    H2c_P1_Slope = p1_h2c_slope, H2c_P1_P = p1_h2c_p,
    H2c_P2_Slope = p2_h2c_slope, H2c_P2_P = p2_h2c_p
  )
}

# Compile and print summary table
cat("\n\n=========================================================\n")
cat("SENSITIVITY ANALYSIS RESULTS (SIGNED DEVIATION)\n")
cat("=========================================================\n\n")

df_results <- do.call(rbind, lapply(names(results), function(n) {
  r <- results[[n]]
  data.frame(
    Model = n,
    H1_slope = sprintf("%.4f (p=%.4f)", r$H1_Slope, r$H1_P),
    H2a_path1 = sprintf("%.4f (p=%.4f)", r$H2a_P1_Slope, r$H2a_P1_P),
    H2a_path2 = sprintf("%.4f (p=%.4f)", r$H2a_P2_Slope, r$H2a_P2_P),
    H2b_path1 = sprintf("%.4f (p=%.4f)", r$H2b_P1_Slope, r$H2b_P1_P),
    H2b_path2 = sprintf("%.4f (p=%.4f)", r$H2b_P2_Slope, r$H2b_P2_P),
    H2c_path1 = sprintf("%.4f (p=%.4f)", r$H2c_P1_Slope, r$H2c_P1_P),
    H2c_path2 = sprintf("%.4f (p=%.4f)", r$H2c_P2_Slope, r$H2c_P2_P),
    stringsAsFactors = FALSE
  )
}))

print(df_results)
write.csv(df_results, "Outputs/SensitivityAnalysisResults.csv", row.names = FALSE)
