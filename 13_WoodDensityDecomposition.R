# --------------------------------------------
# Purpose: Wood-density decomposition of the EEoS deviation response to forest loss (SI Note S3).
#          Tests whether the forest-loss effect on EEoS deviation operates through the shift in
#          community (stem-mean) wood density.
# Input Origin Scripts: 01_CombinePinhoJuruaData.R; 03_FitEEOS.py
# Output: Outputs/WoodDensityDecomposition.csv
# ---------------------------------------------------

suppressMessages(library(tidyverse))
source("Code/Functions/config.R"); check_project_root()
source("Code/Functions/data_prep.R")

comm   <- read_required_csv(out_path("comm"))
traits <- read_required_csv(out_path("traits"))

cwm <- comm |>
  left_join(select(traits, Binomial_correct, WD), by = "Binomial_correct", relationship = "many-to-one") |>
  group_by(Region_Plot) |>
  summarise(cwmWD = mean(WD), .groups = "drop")
stopifnot(!anyNA(cwm$cwmWD))

tidy_term <- function(fit, term, model, suffix, outcome) {
  s <- summary(fit)$coefficients
  ci <- confint(fit)
  tibble(suffix = suffix, outcome = outcome, model = model, term = term,
         estimate = s[term, "Estimate"], std_error = s[term, "Std. Error"],
         ci_lower = ci[term, 1], ci_upper = ci[term, 2], p_value = s[term, "Pr(>|t|)"],
         n = nobs(fit))
}

results <- map_dfr(c("harte_wd", "harte_nowd"), function(suffix) {
  d <- load_and_prepare_plot_data(suffix) |> left_join(cwm, by = "Region_Plot", relationship = "one-to-one")
  bind_rows(
    tidy_term(lm(cwmWD ~ disturbedcover + Region, d), "disturbedcover", "cwmWD ~ forest loss + region", suffix, "cwmWD"),
    map_dfr(c("dev", "absdev"), function(y) bind_rows(
      tidy_term(lm(reformulate(c("disturbedcover", "Region"), y), d), "disturbedcover",
                "y ~ forest loss + region", suffix, y),
      tidy_term(lm(reformulate(c("disturbedcover", "cwmWD", "Region"), y), d), "disturbedcover",
                "y ~ forest loss + cwmWD + region", suffix, y),
      tidy_term(lm(reformulate(c("disturbedcover", "cwmWD", "Region"), y), d), "cwmWD",
                "y ~ forest loss + cwmWD + region", suffix, y)
    ))
  )
})

print(as.data.frame(results), digits = 4)
write.csv(results, out_path("WoodDensityDecomposition"), row.names = FALSE)
