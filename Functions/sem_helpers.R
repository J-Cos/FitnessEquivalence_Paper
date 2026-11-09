# --------------------------------------------
# Purpose: Mediation SEM suite following the 5-step MSA-SEM (Model Selection Approach to SEM) road map of
#          Garrido, Hansen, Yaari, & Hawlena (2022, Methods in Ecology and Evolution 13:42-53):
#            Step 1 (Causal analysis, Sec. 3.1): A priori causal mediation hypotheses (H2a: CATS_nb, H2b: proportionNonNeutral, H2c: r2pois)
#            Step 2 (SEM specification, Sec. 3.2 & Box 1): Saturated SEM meta-model (Partial Mediation {a,b,c}, df = 0) and
#                    the complete, balanced 2^3 = 8-model nested candidate set spanning all inclusion/exclusion combinations
#                    of focal paths a (X -> M), b (M -> Y), and c (X -> Y). Omitted focal paths are fixed via 0* constraints
#                    (with m ~~ 0*y) so all 8 models are calibrated over the exact same empirical covariance matrix S and
#                    each focal path (a, b, c) appears in 4 of 8 models (exact 0.50 prior inclusion weight).
#            Step 3 (Statistical validation, Sec. 3.3): Global goodness-of-fit (Chisq, CFI > 0.90, RMSEA, SRMR),
#                    multicollinearity check |r(x, m)| < 0.60 (pooled and within-region partial; Glossary, p. 43),
#                    ML estimation (meanstructure = TRUE, fixed.x = TRUE) + 2,000-draw nonparametric percentile bootstrap,
#                    and selection of AICc and BIC (Sec. 2.3 & Fig. S5).
#            Step 4 (Model selection, Sec. 3.4): Rank candidate models by AICc and BIC, compute model weights (w_i),
#                    and specify the 'best' SEM model set via the recommended cumulative weight sum(w_i) >= 55% and
#                    delta_AICc <= 2 criteria (alongside w_i >= 10%).
#            Step 5 (Inference, Sec. 3.5): Evidence Ratios (ER), balanced path relative importance (sum of w_i),
#                    standardized and unstandardized coefficients for the best model(s) and weighted-average model,
#                    decomposition into direct (c), indirect (a*b), and total (c + a*b) effects, and LRT / d-separation tests.
# ---------------------------------------------------

CANDIDATE_MODEL_LABELS <- c(
  partial     = "Partial Mediation",
  full        = "Full Mediation",
  coresponse  = "Co-response (No b)",
  independent = "Independent Effects (No a)",
  x_to_m_only = "Stressor -> Mediator Only",
  m_to_y_only = "Mediator -> Deviation Only",
  direct_only = "Direct Only",
  null        = "Null (Region Only)"
)

#' Sample-size corrected AIC (AICc; Glossary p. 43 & official Figshare code of Garrido et al. 2022)
calculate_aicc <- function(fit) {
  mes <- lavaan::fitMeasures(fit, c("aic", "npar", "ntotal"))
  as.numeric(mes[1] + 2 * mes[2] * (mes[2] + 1) / (mes[3] - mes[2] - 1))
}

#' Step 2 & 3 (Garrido et al. 2022, Glossary p. 43 & Sec. 3.2-3.3):
#' Check multicollinearity (|r| < 0.60 pooled and within-region), missingness, and observations per parameter
check_garrido_assumptions <- function(data, x, m, y, covars = REGION_DUMMIES, k_max = 13, k_eq_max = 7) {
  vars <- c(x, m, y, intersect(covars, names(data)))
  if (anyNA(data[, vars])) stop("Missing values in SEM variables: ", paste(vars, collapse = ", "))
  r_xm <- stats::cor(data[[x]], data[[m]])
  cv_present <- intersect(covars, names(data))
  if (length(cv_present) > 0) {
    rx <- stats::resid(stats::lm(stats::reformulate(cv_present, x), data = data))
    rm <- stats::resid(stats::lm(stats::reformulate(cv_present, m), data = data))
    r_xm_partial <- stats::cor(rx, rm)
  } else {
    r_xm_partial <- r_xm
  }
  if (max(abs(r_xm), abs(r_xm_partial)) >= 0.60) {
    warning(sprintf(
      "Garrido et al. (2022) Glossary: |r(%s, %s)| = %.3f (within-region %.3f) >= 0.60 multicollinearity threshold.",
      x, m, abs(r_xm), abs(r_xm_partial)
    ))
  }
  n <- nrow(data)
  tibble::tibble(
    n                    = n,
    k_saturated          = k_max,
    n_per_par            = round(n / k_max, 2),
    n_per_par_ge_5       = (n / k_max) >= 5,
    n_per_par_eq         = round(n / k_eq_max, 2),
    n_per_par_eq_ge_5    = (n / k_eq_max) >= 5,
    use_AICc             = (n / k_max) < 40,
    cor_x_m              = round(r_xm, 4),
    partial_cor_x_m      = round(r_xm_partial, 4),
    multicollinear_gt_06 = max(abs(r_xm), abs(r_xm_partial)) >= 0.60
  )
}

#' Step 2 (Garrido et al. 2022, Sec. 3.2 & Box 1): Complete, balanced 2^3 = 8-model recursive candidate set for x -> m -> y.
#' All 8 models retain m and y as endogenous variables and fix omitted focal paths via 0* constraints (plus m ~~ 0*y)
#' so every model is calibrated against the exact same 6x6 empirical covariance matrix S, and each focal path
#' a (m ~ x), b (y ~ m), and c (y ~ x) appears in 4 of 8 models (0.50 prior inclusion weight):
#'   - partial     {a,b,c}: m ~ a*x; y ~ b*m + c*x  (df = 0; saturated SEM meta-model / SEMM)
#'   - full        {a,b}  : m ~ a*x; y ~ b*m + 0*x  (df = 1; tests c = 0, i.e. y _||_ x | m, region)
#'   - coresponse  {a,c}  : m ~ a*x; y ~ 0*m + c*x  (df = 1; tests b = 0, i.e. y _||_ m | x, region)
#'   - independent {b,c}  : m ~ 0*x; y ~ b*m + c*x  (df = 1; tests a = 0, i.e. m _||_ x | region)
#'   - x_to_m_only {a}    : m ~ a*x; y ~ 0*m + 0*x  (df = 2; tests b = 0 and c = 0)
#'   - m_to_y_only {b}    : m ~ 0*x; y ~ b*m + 0*x  (df = 2; tests a = 0 and c = 0)
#'   - direct_only {c}    : m ~ 0*x; y ~ 0*m + c*x  (df = 2; tests a = 0 and b = 0)
#'   - null        {}     : m ~ 0*x; y ~ 0*m + 0*x  (df = 3; tests a = 0, b = 0, and c = 0)
build_mediation_models <- function(x, m, y, covars = REGION_DUMMIES, label_paths = FALSE) {
  cv <- paste(covars, collapse = " + ")
  mk <- function(a_on, b_on, c_on) {
    a_term <- if (a_on) (if (label_paths) paste0("a*", x) else x) else paste0("0*", x)
    b_term <- if (b_on) (if (label_paths) paste0("b*", m) else m) else paste0("0*", m)
    c_term <- if (c_on) (if (label_paths) paste0("c*", x) else x) else paste0("0*", x)
    base <- sprintf("%s ~ %s + %s\n%s ~ %s + %s + %s\n%s ~~ 0*%s",
                    m, a_term, cv, y, b_term, c_term, cv, m, y)
    if (!label_paths) return(base)
    defs <- character(0)
    if (a_on && b_on) defs <- c(defs, "ind := a*b")
    if (a_on && b_on && c_on) {
      defs <- c(defs, "tot := c + a*b")
    } else if (a_on && b_on) {
      defs <- c(defs, "tot := a*b")
    } else if (c_on) {
      defs <- c(defs, "tot := c")
    }
    if (length(defs)) paste(c(base, defs), collapse = "\n") else base
  }
  list(
    partial     = mk(TRUE,  TRUE,  TRUE),
    full        = mk(TRUE,  TRUE,  FALSE),
    coresponse  = mk(TRUE,  FALSE, TRUE),
    independent = mk(FALSE, TRUE,  TRUE),
    x_to_m_only = mk(TRUE,  FALSE, FALSE),
    m_to_y_only = mk(FALSE, TRUE,  FALSE),
    direct_only = mk(FALSE, FALSE, TRUE),
    null        = mk(FALSE, FALSE, FALSE)
  )
}

fit_sem <- function(model, data, ...) {
  lavaan::sem(model, data = data, estimator = "ML", meanstructure = TRUE, ...)
}

#' Step 3 & 4 (Garrido et al. 2022, Sec. 3.3-3.4):
#' Candidate model comparison table (AICc, BIC, w_i, Evidence Ratio, best-set flags, and global goodness-of-fit)
print_sem_comparison <- function(fits) {
  models <- stats::setNames(fits, unname(CANDIDATE_MODEL_LABELS[names(fits)]))
  fm <- function(f, what) as.numeric(lavaan::fitMeasures(f, what))

  comp_df <- data.frame(
    Model          = names(models),
    N              = sapply(models, fm, "ntotal"),
    npar           = sapply(models, fm, "npar"),
    n_per_par      = round(sapply(models, fm, "ntotal") / sapply(models, fm, "npar"), 2),
    AIC            = sapply(models, fm, "aic"),
    BIC            = sapply(models, fm, "bic"),
    AICc           = sapply(models, calculate_aicc),
    logL           = sapply(models, fm, "logl"),
    Chisq          = sapply(models, fm, "chisq"),
    df             = sapply(models, fm, "df"),
    p_value        = sapply(models, fm, "pvalue"),
    CFI            = sapply(models, fm, "cfi"),
    RMSEA          = sapply(models, fm, "rmsea"),
    RMSEA_ci_lower = sapply(models, fm, "rmsea.ci.lower"),
    RMSEA_ci_upper = sapply(models, fm, "rmsea.ci.upper"),
    SRMR           = sapply(models, fm, "srmr"),
    stringsAsFactors = FALSE
  )
  comp_df$delta_AICc     <- comp_df$AICc - min(comp_df$AICc)
  rel_lik                <- exp(-comp_df$delta_AICc / 2)
  comp_df$Akaike_weight  <- rel_lik / sum(rel_lik)
  comp_df$Evidence_ratio <- max(comp_df$Akaike_weight) / comp_df$Akaike_weight

  comp_df$delta_BIC      <- comp_df$BIC - min(comp_df$BIC)
  rel_lik_bic            <- exp(-comp_df$delta_BIC / 2)
  comp_df$BIC_weight     <- rel_lik_bic / sum(rel_lik_bic)

  # Garrido et al. (2022, Sec. 2.3 & 3.4, pp. 47, 49, 51) 'best' SEM model set criteria:
  # (1) cumulative weight sum(w_i) >= 55% (minimal top-ranked set reaching >= 0.55 cumulative weight)
  # (2) delta_AICc <= 2
  ord <- order(comp_df$delta_AICc)
  cum_w <- cumsum(comp_df$Akaike_weight[ord])
  k_best55 <- which(cum_w >= 0.55)[1]
  if (is.na(k_best55)) k_best55 <- length(ord)
  best55_idx <- ord[seq_len(k_best55)]
  comp_df$in_best_set_cum55  <- seq_len(nrow(comp_df)) %in% best55_idx
  comp_df$in_best_set_dAICc2 <- comp_df$delta_AICc <= 2.0

  print(comp_df[ord, ], row.names = FALSE)
  cat("\n")
  comp_df
}

#' Step 5 (Garrido et al. 2022, Sec. 3.5):
#' Multimodel inference across the balanced 8-model candidate set:
#'   - Relative Importance (sum of w_i across models containing each path/effect; prior = 0.50 for a, b, c)
#'   - Model-averaged unstandardized and standardized direct (c), indirect (a*b), and total (c + a*b)
#'     effects across (i) the 'best' SEM set (sum(w_i) >= 55%), (ii) w_i >= 10%, and (iii) the full candidate set.
compute_garrido_mmi <- function(fits_labeled, comp_df) {
  names(fits_labeled) <- unname(CANDIDATE_MODEL_LABELS[names(fits_labeled)])
  weights <- stats::setNames(comp_df$Akaike_weight, comp_df$Model)
  top_mask <- weights >= 0.10
  top_weights <- weights[top_mask] / sum(weights[top_mask])

  ord_best <- order(comp_df$delta_AICc)
  best55_models <- comp_df$Model[ord_best][comp_df$in_best_set_cum55[ord_best]]
  best55_mask <- names(weights) %in% best55_models
  best55_weights <- weights[best55_mask] / sum(weights[best55_mask])

  path_membership <- list(
    a   = unname(CANDIDATE_MODEL_LABELS[c("partial", "full", "coresponse", "x_to_m_only")]),
    b   = unname(CANDIDATE_MODEL_LABELS[c("partial", "full", "independent", "m_to_y_only")]),
    c   = unname(CANDIDATE_MODEL_LABELS[c("partial", "coresponse", "independent", "direct_only")]),
    ind = unname(CANDIDATE_MODEL_LABELS[c("partial", "full")]),
    tot = unname(CANDIDATE_MODEL_LABELS[c("partial", "full", "coresponse", "independent", "direct_only")])
  )

  pe_list <- lapply(fits_labeled, function(f) {
    pe <- lavaan::parameterEstimates(f, standardized = TRUE)
    ss <- lavaan::standardizedSolution(f)
    pe$se_std <- ss$se[match(paste(pe$lhs, pe$op, pe$rhs, pe$label),
                             paste(ss$lhs, ss$op, ss$rhs, ss$label))]
    pe
  })

  get_param <- function(pe, lbl) {
    row <- pe[pe$label == lbl, ]
    if (nrow(row) == 0) {
      return(list(est = 0, se = 0, std = 0, se_std = 0, p = NA_real_))
    }
    row <- row[1, ]
    se_val <- if (is.na(row$se) || row$se == 0) 0 else row$se
    se_std <- if (is.na(row$se_std) || row$se_std == 0) 0 else row$se_std
    list(est = row$est, se = se_val, std = row$std.all, se_std = se_std, p = row$pvalue)
  }

  best_name <- comp_df$Model[which.min(comp_df$AICc)]
  rows <- lapply(names(path_membership), function(lbl) {
    pars <- lapply(pe_list[names(weights)], get_param, lbl = lbl)
    ests <- sapply(pars, `[[`, "est")
    ses  <- sapply(pars, `[[`, "se")
    stds <- sapply(pars, `[[`, "std")
    se_stds <- sapply(pars, `[[`, "se_std")

    active <- intersect(path_membership[[lbl]], names(weights))
    rel_imp <- sum(weights[active])

    avg_est_all <- sum(weights * ests)
    uncond_se_all <- sum(weights * sqrt(ses^2 + (ests - avg_est_all)^2))
    avg_std_all <- sum(weights * stds)
    uncond_se_std_all <- sum(weights * sqrt(se_stds^2 + (stds - avg_std_all)^2))

    avg_est_best55 <- sum(best55_weights * ests[best55_mask])
    uncond_se_best55 <- sum(best55_weights * sqrt(ses[best55_mask]^2 + (ests[best55_mask] - avg_est_best55)^2))
    avg_std_best55 <- sum(best55_weights * stds[best55_mask])
    uncond_se_std_best55 <- sum(best55_weights * sqrt(se_stds[best55_mask]^2 + (stds[best55_mask] - avg_std_best55)^2))

    avg_est_top <- sum(top_weights * ests[top_mask])
    uncond_se_top <- sum(top_weights * sqrt(ses[top_mask]^2 + (ests[top_mask] - avg_est_top)^2))
    avg_std_top <- sum(top_weights * stds[top_mask])
    uncond_se_std_top <- sum(top_weights * sqrt(se_stds[top_mask]^2 + (stds[top_mask] - avg_std_top)^2))

    best_p <- pars[[best_name]]
    z_val <- if (uncond_se_all > 0) avg_est_all / uncond_se_all else NA_real_
    p_uncond <- if (!is.na(z_val)) 2 * stats::pnorm(-abs(z_val)) else NA_real_

    tibble::tibble(
      Parameter            = lbl,
      Best_Model           = best_name,
      Best_Set_Cum55       = paste(best55_models, collapse = " + "),
      Relative_Importance  = rel_imp,
      Important_gt_70pct   = rel_imp >= 0.70,
      Best_est             = best_p$est,
      Best_se              = best_p$se,
      Best_p               = best_p$p,
      Best_std             = best_p$std,
      Avg_est_best55       = avg_est_best55,
      Uncond_SE_best55     = uncond_se_best55,
      Avg_std_best55       = avg_std_best55,
      Uncond_SE_std_best55 = uncond_se_std_best55,
      Avg_est_full         = avg_est_all,
      Uncond_SE_full       = uncond_se_all,
      CI_lower_full        = avg_est_all - 1.96 * uncond_se_all,
      CI_upper_full        = avg_est_all + 1.96 * uncond_se_all,
      Avg_std_full         = avg_std_all,
      Uncond_SE_std_full   = uncond_se_std_all,
      Avg_est_top10pct     = avg_est_top,
      Uncond_SE_top10pct   = uncond_se_top,
      Avg_std_top10pct     = avg_std_top,
      Uncond_SE_std_top10  = uncond_se_std_top,
      Uncond_p             = p_uncond
    )
  })
  dplyr::bind_rows(rows)
}

#' Fit all eight balanced candidate models and collect the outputs reported in the paper
fit_mediation_suite <- function(data, x, m, y) {
  assumptions <- check_garrido_assumptions(data, x, m, y)
  fits <- lapply(build_mediation_models(x, m, y), fit_sem, data = data)
  fits_labeled <- lapply(build_mediation_models(x, m, y, label_paths = TRUE), fit_sem, data = data)
  comp <- print_sem_comparison(fits)
  mmi  <- compute_garrido_mmi(fits_labeled, comp)

  estimates <- dplyr::bind_rows(lapply(names(CANDIDATE_MODEL_LABELS), function(k) {
    dplyr::mutate(
      lavaan::parameterEstimates(fits[[k]], standardized = TRUE, rsquare = TRUE),
      Model = CANDIDATE_MODEL_LABELS[[k]],
      .before = 1
    )
  }))

  list(
    assumptions = assumptions,
    fits        = fits,
    lrt         = lavaan::lavTestLRT(fits$full, fits$partial),
    comparison  = comp,
    mmi         = mmi,
    estimates   = estimates
  )
}

#' Indirect effect a*b with a percentile bootstrap CI (paths keep ML point estimates)
#' Computes bootstrap CIs for both Full Mediation (a, b, ind) and Partial Mediation (a_part, b_part, c_part, ind_part, tot_part)
fit_indirect_effect <- function(data, x, m, y, n_boot = 2000, seed = 1) {
  set.seed(seed)
  mods <- build_mediation_models(x, m, y, label_paths = TRUE)
  fit_f <- fit_sem(mods$full, data, se = "bootstrap", bootstrap = n_boot)
  res_f <- lavaan::parameterEstimates(fit_f, boot.ci.type = "perc", level = 0.95) |>
    dplyr::filter(label %in% c("a", "b", "ind")) |>
    dplyr::select(label, lhs, op, rhs, est, se, pvalue, ci.lower, ci.upper) |>
    dplyr::mutate(Model = "Full Mediation", n_boot = n_boot)

  set.seed(seed)
  fit_p <- fit_sem(mods$partial, data, se = "bootstrap", bootstrap = n_boot)
  res_p <- lavaan::parameterEstimates(fit_p, boot.ci.type = "perc", level = 0.95) |>
    dplyr::filter(label %in% c("a", "b", "c", "ind", "tot")) |>
    dplyr::mutate(label = paste0(label, "_partial")) |>
    dplyr::select(label, lhs, op, rhs, est, se, pvalue, ci.lower, ci.upper) |>
    dplyr::mutate(Model = "Partial Mediation", n_boot = n_boot)

  dplyr::bind_rows(res_f, res_p)
}

#' Estimate, SE, standardized estimate, and p-value of a free regression path from parameterEstimates()
path_est <- function(est, lhs, rhs, model = "Full Mediation") {
  if ("Model" %in% names(est)) {
    est_m <- est[est$Model == model, ]
    row_m <- est_m[est_m$lhs == lhs & est_m$op == "~" & est_m$rhs == rhs & !is.na(est_m$pvalue), ]
    if (nrow(row_m) == 1) {
      est <- est_m
    } else {
      est <- est[est$Model == "Partial Mediation", ]
    }
  }
  row <- est[est$lhs == lhs & est$op == "~" & est$rhs == rhs, ]
  stopifnot(nrow(row) == 1)
  list(
    est = row$est,
    se  = row$se,
    std = if ("std.all" %in% names(row)) row$std.all else NA_real_,
    p   = row$pvalue
  )
}

#' Write the standard + Garrido multimodel inference outputs for one hypothesis
write_mediation_outputs <- function(res, id) {
  utils::write.csv(res$comparison, out_path(paste0(id, "_Model_Comparison")), row.names = FALSE)
  utils::write.csv(res$estimates, out_path(paste0(id, "_Parameter_Estimates")), row.names = FALSE)
  utils::write.csv(as.data.frame(res$lrt), out_path(paste0(id, "_LRT")), row.names = FALSE)
  if (!is.null(res$mmi)) {
    utils::write.csv(res$mmi, out_path(paste0(id, "_Multimodel_Inference")), row.names = FALSE)
  }
  if (!is.null(res$indirect)) {
    utils::write.csv(res$indirect, out_path(paste0(id, "_IndirectEffect")), row.names = FALSE)
  }
}

#' Extract a one-row tidy summary across full/partial mediation, balanced 8-model Garrido MMI, LRT, and bootstrap CIs
summarise_mediation_run <- function(res, id, m, y, x = "disturbedcover") {
  est  <- res$estimates
  comp <- res$comparison
  mmi  <- res$mmi
  ind  <- res$indirect

  p_a      <- path_est(est, m, x, model = "Partial Mediation")
  p_b_full <- path_est(est, y, m, model = "Full Mediation")
  p_b_part <- path_est(est, y, m, model = "Partial Mediation")
  p_c_part <- path_est(est, y, x, model = "Partial Mediation")
  p_c_dir  <- path_est(est, y, x, model = "Direct Only")

  get_r2 <- function(mod, var) {
    r <- est[est$Model == mod & est$lhs == var & est$op == "r2", ]
    if (nrow(r) == 1) r$est else NA_real_
  }
  get_w <- function(mod) {
    w <- comp$Akaike_weight[comp$Model == mod]
    if (length(w) == 1) w else NA_real_
  }
  get_ind <- function(lbl, col) {
    if (is.null(ind)) return(NA_real_)
    r <- ind[ind$label == lbl, ]
    if (nrow(r) == 1) r[[col]] else NA_real_
  }

  tibble::tibble(
    Hypothesis_ID     = id,
    N                 = res$assumptions$n,
    Predictor         = x,
    Mediator          = m,
    Outcome           = y,
    cor_x_m           = res$assumptions$cor_x_m,
    partial_cor_x_m   = res$assumptions$partial_cor_x_m,
    a_est             = p_a$est,
    a_se              = p_a$se,
    a_std             = p_a$std,
    a_p               = p_a$p,
    b_full_est        = p_b_full$est,
    b_full_se         = p_b_full$se,
    b_full_std        = p_b_full$std,
    b_full_p          = p_b_full$p,
    b_part_est        = p_b_part$est,
    b_part_se         = p_b_part$se,
    b_part_std        = p_b_part$std,
    b_part_p          = p_b_part$p,
    c_part_est        = p_c_part$est,
    c_part_se         = p_c_part$se,
    c_part_std        = p_c_part$std,
    c_part_p          = p_c_part$p,
    c_direct_est      = p_c_dir$est,
    c_direct_se       = p_c_dir$se,
    c_direct_std      = p_c_dir$std,
    c_direct_p        = p_c_dir$p,
    ind_full_est      = get_ind("ind", "est"),
    ind_full_ci_low   = get_ind("ind", "ci.lower"),
    ind_full_ci_high  = get_ind("ind", "ci.upper"),
    ind_part_est      = get_ind("ind_partial", "est"),
    ind_part_ci_low   = get_ind("ind_partial", "ci.lower"),
    ind_part_ci_high  = get_ind("ind_partial", "ci.upper"),
    tot_part_est      = get_ind("tot_partial", "est"),
    tot_part_ci_low   = get_ind("tot_partial", "ci.lower"),
    tot_part_ci_high  = get_ind("tot_partial", "ci.upper"),
    Best_Model_AICc   = comp$Model[which.min(comp$AICc)],
    Best_Set_Cum55    = mmi$Best_Set_Cum55[1],
    w_Partial         = get_w("Partial Mediation"),
    w_Full            = get_w("Full Mediation"),
    w_Coresponse      = get_w("Co-response (No b)"),
    w_Independent     = get_w("Independent Effects (No a)"),
    w_X_to_M_Only     = get_w("Stressor -> Mediator Only"),
    w_M_to_Y_Only     = get_w("Mediator -> Deviation Only"),
    w_Direct_Only     = get_w("Direct Only"),
    w_Null            = get_w("Null (Region Only)"),
    RelImp_a          = mmi$Relative_Importance[mmi$Parameter == "a"],
    RelImp_b          = mmi$Relative_Importance[mmi$Parameter == "b"],
    RelImp_c          = mmi$Relative_Importance[mmi$Parameter == "c"],
    RelImp_ind        = mmi$Relative_Importance[mmi$Parameter == "ind"],
    FullMed_Chisq     = as.numeric(res$lrt$Chisq[2]),
    FullMed_df        = as.numeric(res$lrt$Df[2]),
    FullMed_p         = as.numeric(res$lrt$`Pr(>Chisq)`[2]),
    R2_m              = get_r2("Partial Mediation", m),
    R2_y_full         = get_r2("Full Mediation", y),
    R2_y_partial      = get_r2("Partial Mediation", y)
  )
}

#' Run one mediation hypothesis and optionally write per-hypothesis CSV outputs.
#' @param indirect also estimate the bootstrapped indirect effect (`n_boot` draws)
#' @param write_files write the five `<id>_*.csv` files to `Outputs/` (default `TRUE`)
#' @return mediation suite list (with `$indirect` and `$summary`), invisibly
run_mediation_hypothesis <- function(data, id, m, y, x = "disturbedcover",
                                     indirect = TRUE, n_boot = 2000, seed = 1,
                                     write_files = TRUE) {
  cat(sprintf("\n=== %s (x = %s -> m = %s -> y = %s; N = %d) ===\n", id, x, m, y, nrow(data)))
  res <- fit_mediation_suite(data, x, m, y)
  cat(id, ": full-mediation LRT\n"); print(res$lrt)
  cat(id, ": Garrido Step 4-5 Multimodel Inference (8-model balanced set)\n"); print(as.data.frame(res$mmi), digits = 4)
  res$indirect <- if (indirect) fit_indirect_effect(data, x, m, y, n_boot = n_boot, seed = seed) else NULL
  res$summary  <- summarise_mediation_run(res, id = id, m = m, y = y, x = x)
  if (write_files) write_mediation_outputs(res, id)
  invisible(res)
}

#' Extract wide one-row multimodel-inference path estimates, unconditional SEs, weighted CFI, and mediated share
extract_mmi_metrics <- function(res) {
  mmi  <- res$mmi
  comp <- res$comparison
  grab <- function(param, col) mmi[[col]][mmi$Parameter == param]

  cfi_w    <- sum(comp$Akaike_weight * comp$CFI)
  ind_std  <- grab("ind", "Avg_std_full")
  c_std    <- grab("c",   "Avg_std_full")
  denom    <- abs(ind_std) + abs(c_std)
  prop_med <- if (denom > 0) abs(ind_std) / denom else NA_real_

  tibble::tibble(
    Weighted_CFI        = cfi_w,
    Avg_est_a           = grab("a",   "Avg_est_full"),
    Uncond_SE_a         = grab("a",   "Uncond_SE_full"),
    Avg_std_a           = grab("a",   "Avg_std_full"),
    Uncond_SE_std_a     = grab("a",   "Uncond_SE_std_full"),
    Avg_est_b           = grab("b",   "Avg_est_full"),
    Uncond_SE_b         = grab("b",   "Uncond_SE_full"),
    Avg_std_b           = grab("b",   "Avg_std_full"),
    Uncond_SE_std_b     = grab("b",   "Uncond_SE_std_full"),
    Avg_est_c           = grab("c",   "Avg_est_full"),
    Uncond_SE_c         = grab("c",   "Uncond_SE_full"),
    Avg_std_c           = grab("c",   "Avg_std_full"),
    Uncond_SE_std_c     = grab("c",   "Uncond_SE_std_full"),
    Avg_est_ind         = grab("ind", "Avg_est_full"),
    Uncond_SE_ind       = grab("ind", "Uncond_SE_full"),
    CI_lower_ind_uncond = grab("ind", "CI_lower_full"),
    CI_upper_ind_uncond = grab("ind", "CI_upper_full"),
    Avg_std_ind         = ind_std,
    Uncond_SE_std_ind   = grab("ind", "Uncond_SE_std_full"),
    Prop_Mediated       = prop_med
  )
}
