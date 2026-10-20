# --------------------------------------------
# Purpose: Sensitivity of H1 and H2a-c to the macrostate scaling (six formulations; SI Table S2).
#          Uses the same macrostates (02), EEoS fits (03), fitness-equivalence metrics (05, 08, 10)
#          and model code as the main analysis - no scaling logic is duplicated here.
# Input Origin Scripts: 03_FitEEOS.py; 05_FitCatsRegressions.R;
#                       08_FitOverallSloanAndExtractNonneutralAbundances.R; 10_FitForestCoverSubgroupSloans.R
# Output: Outputs/SensitivityAnalysisResults_{Absolute,Signed}.csv (formatted, Table S2A/B)
#         Outputs/SensitivityAnalysisResults_long.csv (all estimates, unformatted)
# ---------------------------------------------------

suppressMessages(library(tidyverse))
source("Code/Functions/config.R"); check_project_root()
invisible(lapply(c("data_prep", "sem_helpers"), \(f) source(sprintf("Code/Functions/%s.R", f))))

# Full-mediation SEM for one mediator/outcome pair: path 1, path 2 and the chi-square(1) test p
mediation_paths <- function(data, m, y) {
  fit <- fit_sem(build_mediation_models("disturbedcover", m, y)$full, data)
  est <- lavaan::parameterEstimates(fit)
  p1 <- path_est(est, m, "disturbedcover"); p2 <- path_est(est, y, m)
  tibble(mediator = m, outcome = y, p1_est = p1$est, p1_p = p1$p, p2_est = p2$est, p2_p = p2$p,
         fm_p = as.numeric(lavaan::fitMeasures(fit, "pvalue")))
}

h1_slope <- function(data, y) {
  s <- summary(lm(reformulate(c("disturbedcover", "Region"), y), data = data))$coefficients
  tibble(outcome = y, h1_est = s["disturbedcover", "Estimate"], h1_p = s["disturbedcover", "Pr(>|t|)"])
}

run_option <- function(suffix) {
  message("Sensitivity: ", SCALING_LABELS[[suffix]])
  d <- load_and_prepare_plot_data(suffix, need = c("cats", "nonneutral"))
  g <- load_and_prepare_subgroup_data(suffix)
  sem <- bind_rows(
    mediation_paths(d, "CATS_pois", "dev"), mediation_paths(d, "CATS_pois", "absdev"),
    mediation_paths(d, "proportionNonNeutral", "dev"), mediation_paths(d, "proportionNonNeutral", "absdev"),
    mediation_paths(g, "r2pois", "devmedian"), mediation_paths(g, "r2pois", "RMSE"),
    mediation_paths(d, "CATS_pois_3trait", "dev"), mediation_paths(d, "CATS_pois_3trait", "absdev")
  ) |> mutate(hypothesis = rep(c("H2a", "H2b", "H2c", "H2a_3trait"), each = 2), .before = 1)
  list(suffix = suffix,
       jurua_dev = mean(d$dev[d$Region == "Jurua"]),
       jurua_absdev = mean(d$absdev[d$Region == "Jurua"]),
       h1 = bind_rows(h1_slope(d, "dev"), h1_slope(d, "absdev")),
       sem = sem)
}

results <- map(SCALING_SUFFIXES, run_option)

# Long, unformatted table -----------------------------------------------------------------------
long <- map_dfr(results, function(r) {
  bind_rows(
    tibble(term = "Jurua_mean_dev", outcome = "dev", est = r$jurua_dev, p = NA_real_),
    tibble(term = "Jurua_mean_absdev", outcome = "absdev", est = r$jurua_absdev, p = NA_real_),
    transmute(r$h1, term = "H1_slope", outcome, est = h1_est, p = h1_p),
    transmute(r$sem, term = paste0(hypothesis, "_path1"), outcome, est = p1_est, p = p1_p),
    transmute(r$sem, term = paste0(hypothesis, "_path2"), outcome, est = p2_est, p = p2_p),
    transmute(r$sem, term = paste0(hypothesis, "_fullmediation_chisq"), outcome, est = NA_real_, p = fm_p)
  ) |> mutate(suffix = r$suffix, Model = SCALING_LABELS[[r$suffix]], .before = 1)
})
write.csv(long, out_path("SensitivityAnalysisResults_long"), row.names = FALSE)

# Formatted Table S2A (absolute) and S2B (signed) -------------------------------------------------
fmt_slope <- function(est, p) sprintf("%+.4f (p=%.4f)", est, p)
fmt_path2 <- function(est, p, fm) sprintf("%+.4f (p=%.4f; FM p=%.3f)", est, p, fm)
sem_row <- function(r, hyp, y) filter(r$sem, hypothesis == hyp, outcome == y)

format_table <- function(type) {
  map_dfr(results, function(r) {
    ys <- if (type == "abs") c(h1 = "absdev", h2ab = "absdev", h2c = "RMSE") else c(h1 = "dev", h2ab = "dev", h2c = "devmedian")
    h1 <- filter(r$h1, outcome == ys[["h1"]])
    a <- sem_row(r, "H2a", ys[["h2ab"]]); b <- sem_row(r, "H2b", ys[["h2ab"]]); cc <- sem_row(r, "H2c", ys[["h2c"]])
    out <- tibble(Model = SCALING_LABELS[[r$suffix]],
                  Jurua_mean_dev = sprintf("%+.4f", r$jurua_dev),
                  H1 = fmt_slope(h1$h1_est, h1$h1_p),
                  H2a_path1 = fmt_slope(a$p1_est, a$p1_p), H2a_path2 = fmt_path2(a$p2_est, a$p2_p, a$fm_p),
                  H2b_path1 = fmt_slope(b$p1_est, b$p1_p), H2b_path2 = fmt_path2(b$p2_est, b$p2_p, b$fm_p),
                  H2c_path1 = fmt_slope(cc$p1_est, cc$p1_p), H2c_path2 = fmt_path2(cc$p2_est, cc$p2_p, cc$fm_p))
    names(out) <- if (type == "abs") {
      c("Model", "Jurua_mean_dev", "H1_abs_slope", "H2a_path1", "H2a_path2_abs", "H2b_path1",
        "H2b_path2_abs", "H2c_path1", "H2c_path2_RMSE")
    } else {
      c("Model", "Jurua_mean_dev", "H1_sig_slope", "H2a_path1", "H2a_path2_sig", "H2b_path1",
        "H2b_path2_sig", "H2c_path1", "H2c_path2_sig")
    }
    out
  })
}

df_abs <- format_table("abs")
df_sig <- format_table("sig")
cat("\nSENSITIVITY ANALYSIS RESULTS (ABSOLUTE DEVIATION / RMSE)\n"); print(as.data.frame(df_abs))
cat("\nSENSITIVITY ANALYSIS RESULTS (SIGNED DEVIATION)\n"); print(as.data.frame(df_sig))
write.csv(df_abs, out_path("SensitivityAnalysisResults_Absolute"), row.names = FALSE)
write.csv(df_sig, out_path("SensitivityAnalysisResults_Signed"), row.names = FALSE)
