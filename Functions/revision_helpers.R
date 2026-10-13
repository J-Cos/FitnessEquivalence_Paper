# -------------------------------------------------------------------
# Purpose: Shared helper functions for the paper revision.
#          Implements Approach A (Single-group SEM with dummy controls)
#          and Approach C (Hold-out validation).
#          Respects DRY across the codebase.
# -------------------------------------------------------------------

library(tidyverse)
library(lavaan)

#' Load and prepare plot-level data
#' Sets Juruá forest cover to 1.0 (0% disturbed) with no simulated noise.
#' Dummy codes regions with Juruá as reference.
load_and_prepare_plot_data <- function(suffix = "harte_wd",
                                      preds_file = NULL,
                                      env_file = "Data/env.csv",
                                      cats_file = "Outputs/CatsOutput.csv",
                                      non_neutral_file = "Outputs/NonNeutralProportions.csv") {
  
  if (is.null(preds_file)) {
    preds_file <- paste0("Outputs/EEOSPredsHarteWD_", suffix, ".csv")
  }
  if (!file.exists(preds_file)) stop(paste("File not found:", preds_file))
  if (!file.exists(env_file)) stop(paste("File not found:", env_file))
  
  d <- read.csv(preds_file) %>% as_tibble()
  env <- read.csv(env_file) %>% as_tibble()
  
  plot_data <- d %>% left_join(env, by = c("Region", "Region_Plot"))
  
  # Optional joins (only if files exist)
  if (!is.null(cats_file) && file.exists(cats_file)) {
    cats <- read.csv(cats_file) %>% dplyr::select(Region_Plot, CATS_pois) %>% as_tibble()
    plot_data <- plot_data %>% left_join(cats, by = "Region_Plot")
  }
  
  if (!is.null(non_neutral_file) && file.exists(non_neutral_file)) {
    non_neutral <- read.csv(non_neutral_file) %>% dplyr::select(Region_Plot, proportionNonNeutral) %>% as_tibble()
    plot_data <- plot_data %>% left_join(non_neutral, by = "Region_Plot")
  }
  
  # Prepare variables
  plot_data <- plot_data %>%
    mutate(
      fc2000 = if_else(Region == "Jurua", 1.0, fc2000),
      disturbedcover = 1.0 - fc2000,
      absdev = abs(dev),
      Region = factor(Region, levels = c("Jurua", "Balbina", "STM", "PGM")),
      Reg_Balbina = if_else(Region == "Balbina", 1, 0),
      Reg_STM = if_else(Region == "STM", 1, 0),
      Reg_PGM = if_else(Region == "PGM", 1, 0)
    )
  
  return(plot_data)
}

#' Load and prepare subgroup-level data (for H2c)
load_and_prepare_subgroup_data <- function(suffix = "harte_wd", subgroup_file = NULL) {
  if (is.null(subgroup_file)) {
    subgroup_file <- paste0("Outputs/ForestCoverSubgroupSloanFits_", suffix, ".csv")
  }
  if (!file.exists(subgroup_file)) stop(paste("File not found:", subgroup_file))
  
  df <- read.csv(subgroup_file) %>% as_tibble()
  
  df_modified <- df %>%
    mutate(
      fcmedian = if_else(region == "Jur", 1.0, fcmedian),
      disturbedcover = 1.0 - fcmedian,
      region = recode(region, "Jur" = "Jurua", "Bal" = "Balbina", "STM" = "STM", "PGM" = "PGM"),
      region = factor(region, levels = c("Jurua", "Balbina", "STM", "PGM")),
      Reg_Balbina = if_else(region == "Balbina", 1, 0),
      Reg_STM = if_else(region == "STM", 1, 0),
      Reg_PGM = if_else(region == "PGM", 1, 0)
    )
  
  return(df_modified)
}

#' Calculate sample-size corrected AIC (AICc) for lavaan model
calculate_aicc <- function(fit) {
  mes <- fitMeasures(fit, c("aic", "npar", "ntotal"))
  mes[1] + 2 * mes[2] * (mes[2] + 1) / (mes[3] - mes[2] - 1)
}

#' Calculate group-specific intercepts and slope for plotting parallel regression lines
get_parallel_lines_coefs <- function(fit, outcome, predictor, regions = c("Jurua", "Balbina", "STM", "PGM")) {
  pe <- parameterEstimates(fit)
  
  base_int <- pe %>% dplyr::filter(lhs == outcome & op == "~1") %>% pull(est)
  slope <- pe %>% dplyr::filter(lhs == outcome & op == "~" & rhs == predictor) %>% pull(est)
  
  b_balbina <- pe %>% dplyr::filter(lhs == outcome & op == "~" & rhs == "Reg_Balbina") %>% pull(est)
  b_stm <- pe %>% dplyr::filter(lhs == outcome & op == "~" & rhs == "Reg_STM") %>% pull(est)
  b_pgm <- pe %>% dplyr::filter(lhs == outcome & op == "~" & rhs == "Reg_PGM") %>% pull(est)
  
  # Fallback to 0 if coefficients not in the model
  if(length(base_int) == 0) base_int <- 0
  if(length(slope) == 0) slope <- 0
  if(length(b_balbina) == 0) b_balbina <- 0
  if(length(b_stm) == 0) b_stm <- 0
  if(length(b_pgm) == 0) b_pgm <- 0
  
  intercepts <- c(
    "Jurua" = base_int,
    "Balbina" = base_int + b_balbina,
    "STM" = base_int + b_stm,
    "PGM" = base_int + b_pgm
  )
  
  data.frame(
    region = regions,
    intercept = intercepts[regions],
    slope = rep(slope, length(regions))
  )
}

#' Print model comparison table
print_sem_comparison <- function(fit_full, fit_partial, fit_direct, fit_nocov = NULL) {
  aicc_full <- calculate_aicc(fit_full)
  aicc_partial <- calculate_aicc(fit_partial)
  aicc_direct <- calculate_aicc(fit_direct)
  
  models <- list(
    "Full Mediation" = fit_full,
    "Partial Mediation" = fit_partial,
    "Direct Only" = fit_direct
  )
  
  aicc_vals <- c(aicc_full, aicc_partial, aicc_direct)
  
  if (!is.null(fit_nocov)) {
    models[["Direct Only (No Cov)"]] <- fit_nocov
    aicc_vals <- c(aicc_vals, calculate_aicc(fit_nocov))
  }
  
  comp_df <- data.frame(
    Model = names(models),
    AIC = sapply(models, function(f) fitMeasures(f, "aic")),
    BIC = sapply(models, function(f) fitMeasures(f, "bic")),
    AICc = aicc_vals,
    logL = sapply(models, function(f) fitMeasures(f, "logl")),
    npar = sapply(models, function(f) fitMeasures(f, "npar")),
    Chisq = sapply(models, function(f) fitMeasures(f, "chisq")),
    df = sapply(models, function(f) fitMeasures(f, "df")),
    p_value = sapply(models, function(f) fitMeasures(f, "pvalue")),
    CFI = sapply(models, function(f) fitMeasures(f, "cfi")),
    RMSEA = sapply(models, function(f) fitMeasures(f, "rmsea")),
    SRMR = sapply(models, function(f) fitMeasures(f, "srmr"))
  )
  
  # Calculate relative likelihoods & Akaike weights based on AICc
  min_aicc <- min(comp_df$AICc)
  delta_aicc <- comp_df$AICc - min_aicc
  rel_lik <- exp(-delta_aicc / 2)
  akaike_w <- rel_lik / sum(rel_lik)
  
  comp_df$delta_AICc <- delta_aicc
  comp_df$Akaike_weight <- akaike_w
  
  print(as.data.frame(comp_df), row.names = FALSE)
  cat("\n")
  return(comp_df)
}

#' Calculate marginal prediction line and 95% CI ribbon (Option B)
#' Sets region dummy covariates to their sample means to isolate the common slope.
get_marginal_predictions <- function(data, outcome, predictor, grid_points = 100) {
  formula_str <- paste(outcome, "~", predictor, "+ Reg_Balbina + Reg_STM + Reg_PGM")
  fit_lm <- lm(as.formula(formula_str), data = data)
  
  grid_x <- seq(min(data[[predictor]], na.rm = TRUE), max(data[[predictor]], na.rm = TRUE), length.out = grid_points)
  
  nd <- data.frame(
    predictor = grid_x,
    Reg_Balbina = mean(data$Reg_Balbina, na.rm = TRUE),
    Reg_STM = mean(data$Reg_STM, na.rm = TRUE),
    Reg_PGM = mean(data$Reg_PGM, na.rm = TRUE)
  )
  colnames(nd)[1] <- predictor
  
  preds <- predict(fit_lm, newdata = nd, interval = "confidence")
  
  data.frame(
    x = grid_x,
    y = preds[, "fit"],
    lwr = preds[, "lwr"],
    upr = preds[, "upr"]
  )
}

