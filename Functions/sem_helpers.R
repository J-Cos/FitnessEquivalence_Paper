# --------------------------------------------
# Purpose: Mediation SEM suite (full / partial mediation, common cause) shared by the
#          H2a, H2b, H2c scripts and the sensitivity analysis.
# ---------------------------------------------------

#' Sample-size corrected AIC for a lavaan fit
calculate_aicc <- function(fit) {
  mes <- lavaan::fitMeasures(fit, c("aic", "npar", "ntotal"))
  as.numeric(mes[1] + 2 * mes[2] * (mes[2] + 1) / (mes[3] - mes[2] - 1))
}

#' Model strings for x -> m -> y with regional fixed effects
#' full: m ~ x; y ~ m (df = 1, tests y _||_ x | m, region)
#' partial: adds y ~ x (saturated); direct: common cause (saturated, residual cov m~~y);
#' direct_nocov: common cause with m~~y fixed to 0 (df = 1)
build_mediation_models <- function(x, m, y, covars = REGION_DUMMIES, label_paths = FALSE) {
  cv <- paste(covars, collapse = " + ")
  full <- if (label_paths) {
    sprintf("%s ~ a*%s + %s\n%s ~ b*%s + %s\nind := a*b", m, x, cv, y, m, cv)
  } else {
    sprintf("%s ~ %s + %s\n%s ~ %s + %s", m, x, cv, y, m, cv)
  }
  list(
    full         = full,
    partial      = sprintf("%s ~ %s + %s\n%s ~ %s + %s + %s", m, x, cv, y, m, x, cv),
    direct       = sprintf("%s ~ %s + %s\n%s ~ %s + %s", m, x, cv, y, x, cv),
    direct_nocov = sprintf("%s ~ %s + %s\n%s ~ %s + %s\n%s ~~ 0*%s", m, x, cv, y, x, cv, m, y)
  )
}

fit_sem <- function(model, data, ...) {
  lavaan::sem(model, data = data, estimator = "ML", meanstructure = TRUE, ...)
}

#' Model comparison table (AIC, BIC, AICc, fit indices, Akaike weights on AICc)
print_sem_comparison <- function(fit_full, fit_partial, fit_direct, fit_nocov = NULL) {
  models <- list("Full Mediation" = fit_full, "Partial Mediation" = fit_partial, "Direct Only" = fit_direct)
  if (!is.null(fit_nocov)) models[["Direct Only (No Cov)"]] <- fit_nocov
  fm <- function(f, what) as.numeric(lavaan::fitMeasures(f, what))

  comp_df <- data.frame(
    Model = names(models),
    AIC = sapply(models, fm, "aic"),
    BIC = sapply(models, fm, "bic"),
    AICc = sapply(models, calculate_aicc),
    logL = sapply(models, fm, "logl"),
    npar = sapply(models, fm, "npar"),
    Chisq = sapply(models, fm, "chisq"),
    df = sapply(models, fm, "df"),
    p_value = sapply(models, fm, "pvalue"),
    CFI = sapply(models, fm, "cfi"),
    RMSEA = sapply(models, fm, "rmsea"),
    SRMR = sapply(models, fm, "srmr")
  )
  comp_df$delta_AICc <- comp_df$AICc - min(comp_df$AICc)
  rel_lik <- exp(-comp_df$delta_AICc / 2)
  comp_df$Akaike_weight <- rel_lik / sum(rel_lik)
  print(comp_df, row.names = FALSE)
  cat("\n")
  comp_df
}

#' Fit all four models and collect the outputs reported in the paper
fit_mediation_suite <- function(data, x, m, y) {
  fits <- lapply(build_mediation_models(x, m, y), fit_sem, data = data)
  list(
    fits = fits,
    lrt = lavaan::lavTestLRT(fits$full, fits$partial),
    comparison = print_sem_comparison(fits$full, fits$partial, fits$direct, fits$direct_nocov),
    estimates = lavaan::parameterEstimates(fits$full, standardized = TRUE, rsquare = TRUE)
  )
}

#' Indirect effect a*b with a percentile bootstrap CI (paths keep ML point estimates)
fit_indirect_effect <- function(data, x, m, y, n_boot = 2000, seed = 1) {
  set.seed(seed)
  fit <- fit_sem(build_mediation_models(x, m, y, label_paths = TRUE)$full, data,
                 se = "bootstrap", bootstrap = n_boot)
  lavaan::parameterEstimates(fit, boot.ci.type = "perc", level = 0.95) |>
    dplyr::filter(label %in% c("a", "b", "ind")) |>
    dplyr::select(label, lhs, op, rhs, est, se, pvalue, ci.lower, ci.upper) |>
    dplyr::mutate(n_boot = n_boot)
}

#' Estimate and p-value of a regression path from parameterEstimates()
path_est <- function(est, lhs, rhs) {
  row <- est[est$lhs == lhs & est$op == "~" & est$rhs == rhs, ]
  stopifnot(nrow(row) == 1)
  list(est = row$est, p = row$pvalue)
}

#' Write the three standard outputs for one hypothesis (file names as in the submission)
write_mediation_outputs <- function(res, id, suffix) {
  utils::write.csv(res$comparison, out_path(paste0(id, "_Model_Comparison"), suffix), row.names = FALSE)
  utils::write.csv(res$estimates, out_path(paste0(id, "_Parameter_Estimates"), suffix), row.names = FALSE)
  utils::write.csv(as.data.frame(res$lrt), out_path(paste0(id, "_LRT"), suffix), row.names = FALSE)
}
