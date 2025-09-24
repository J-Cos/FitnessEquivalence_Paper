# --------------------------------------------
# Sript fits SEMs for forest cover sub group slon models (start rfom script before 08_fit forest cover subgroup sloans)
# Outputs is a figure visualising the best model (mediation only model)
# ---------------------------------------------------

library(lavaan)


df_modified <- df %>%
  mutate(
    fcmedian = if_else(region == "Jur", fcmedian - runif(n = sum(region == "Jur"), min = 0.0001, max = 0.01), fcmedian)
  ) %>%
  mutate(fcmedian_c =scale(fcmedian, scale = FALSE)*100 ) %>%
  mutate(
    disturbedcover = 1- fcmedian
  ) %>%
  mutate(region= factor(region, levels = c("Jur", "Bal","STM", "PGM"))) %>%
  mutate(r2poisINV=1-r2pois)


mediation_only <- '
  r2pois ~ disturbedcover
  RMSE    ~ r2pois
'

fit_mediation_only <- sem(mediation_only, data=df_modified,
                         group="region", meanstructure=TRUE,
                         group.equal="regressions",
                         estimator="ML")
# --------------------------------------

mediation_partial <- '
  r2pois ~ disturbedcover
  RMSE    ~ r2pois + disturbedcover
'

fit_mediation_partial <- sem(mediation_partial, data=df_modified,
                         group="region", meanstructure=TRUE,
                         group.equal="regressions",
                         estimator="ML")

lavTestLRT(fit_mediation_only, fit_mediation_partial)


# --------------------------------------

direct_only <- '
  RMSE ~ disturbedcover
  r2pois    ~ disturbedcover
'

fit_direct_only <- sem(direct_only, data=df_modified,
                         group="region", meanstructure=TRUE,
                         group.equal=c("regressions", "residual.covariances" ),
                         estimator="ML")

# basic fit measures to compare
fitMeasures(fit_mediation_only, c("aic", "bic", "bic2", "logl","npar","ntotal"))
fitMeasures(fit_mediation_partial, c("aic","bic", "bic2","logl","npar","ntotal"))
fitMeasures(fit_direct_only, c("aic","bic", "bic2", "logl","npar","ntotal"))

#AICc - not in lavaan so performed manually - note this is standard linear model formula
correctAIC<-function(fit){
  mes<-fitMeasures(fit, c("aic","npar","ntotal"))
  mes[1] + 2*mes[2]*(mes[2]+1) / (mes[3]-mes[2]-1)
}

rl_med<-exp(-(correctAIC(fit_mediation_only)-correctAIC(fit_mediation_only))/2)
rl_par<-exp(-(correctAIC(fit_mediation_partial)-correctAIC(fit_mediation_only))/2)
rl_dir<-exp(-(correctAIC(fit_direct_only)-correctAIC(fit_mediation_only))/2)

rl_med/(rl_med+rl_par+rl_dir)

AIC(fit_mediation_only, fit_mediation_partial, fit_direct_only)


summary(fit_mediation_only, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)




# Make Plot

# get parameter table
pe <- parameterEstimates(fit_mediation_only, standardized = TRUE)
pe_df <- as.data.frame(pe)

# regressions (op == "~") => slopes
regs <- pe_df %>%
  filter(op == "~") %>%
  select(group, lhs, rhs, est, std.all) %>%
  rename(outcome = lhs, predictor = rhs, slope = est, slope_std = std.all)

# intercepts (op == "~1") => intercepts for each observed variable
inters <- pe_df %>%
  filter(op == "~1") %>%
  select(group, lhs, est) %>%
  rename(outcome = lhs, intercept = est)

# defensive: remove any leading '.' in names (summary sometimes shows .CATS_pois)
regs <- regs %>% mutate(
  outcome   = sub("^\\.", "", outcome),
  predictor = sub("^\\.", "", predictor)
)
inters <- inters %>% mutate(outcome = sub("^\\.", "", outcome))

# join slopes with matching intercept for the same outcome and group
coef_df <- regs %>%
  left_join(inters, by = c("group", "outcome")) %>%
  mutate(region = lavInspect(fit_mediation_only, "group.label")[group]) %>%
  select(region, group, outcome, predictor, slope, slope_std, intercept)


# panel A
coef_CATS <- coef_df %>%
  filter(outcome == "r2pois")

# scatter + group-specific regression lines
PanelA<-ggplot(df_modified, aes(x = disturbedcover, y = r2pois, color = region)) +
  geom_point(alpha = 0.8, size=2) +
  geom_abline(data = coef_CATS,
              aes(intercept = intercept, slope = slope, color = region),
              size = 0.5) +
  geom_hline(yintercept=0, linetype=2)+
  theme_classic()+
  theme(legend.position = "none") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +
  #scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  ylab("Poisson random sampling R2")+
  xlab("Median disturbed forest cover in 2000m radius")+
  ggsci::scale_color_npg()

# panel B
coef_absdev <- coef_df %>%
  filter(outcome == "RMSE")

# scatter + group-specific regression lines
PanelB<-ggplot(df_modified, aes(x = r2pois, y = RMSE, color = region)) +
  geom_point(alpha = 0.8, size=2) +
  geom_abline(data = coef_absdev,
              aes(intercept = intercept, slope = slope, color = region),
              size = 0.5) +
  geom_vline(xintercept=0, linetype=2)+
  theme_classic() +
  #scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  xlab("Poisson random sampling R2")+
  ylab("EEOS RMSE")+
  ggsci::scale_color_npg()



cowplot::plot_grid(PanelA, PanelB, labels = c('A', 'B'), rel_widths=c(0.82, 1))
ggsave("Figures/Figure_H2c.png", height=6, width=12)
