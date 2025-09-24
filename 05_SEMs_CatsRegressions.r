# --------------------------------------------
# Sript fits SEMs for CATS regression mediation
# Outputs is a figure visualising the best model (mediation only model)
# ---------------------------------------------------


library(tidyverse)
library(cowplot)


d<-read.csv("Outputs/EEOSPredsHarteWD.csv") %>% as_tibble
CATS_df<-read.csv("Outputs/CatsOutput.csv") %>% select(Region_Plot, CATS_pois)

plot_data<-d %>%
    left_join(., read.csv("Data/env.csv")) %>%
    left_join(CATS_df)

plot_data[is.na(plot_data$fc2000),]$fc2000<-1

plot_data <- plot_data %>%
  mutate(
    fc2000 = if_else(Region == "Jurua", fc2000 - runif(n = sum(Region == "Jurua"), min = 0.0001, max = 0.01), fc2000)
  ) %>%
  mutate(
    disturbedcover = 1- fc2000
  ) %>%
  mutate(absdev=abs(dev))%>% 
  mutate(Region=factor(Region, levels=c("Jurua", "Balbina", "STM", "PGM"))) 


  library(MuMIn)




# 11/9/25 - simple hypothesis testing structure
library(lavaan)

mediation_only <- '
  CATS_pois ~ disturbedcover
  absdev    ~ CATS_pois
'

fit_mediation_only <- sem(mediation_only, data=plot_data,
                         group="Region", meanstructure=TRUE,
                         group.equal="regressions",
                         estimator="ML")
# --------------------------------------

mediation_partial <- '
  CATS_pois ~ disturbedcover
  absdev    ~ CATS_pois + disturbedcover
'

fit_mediation_partial <- sem(mediation_partial, data=plot_data,
                         group="Region", meanstructure=TRUE,
                         group.equal="regressions",
                         estimator="ML")

lavTestLRT(fit_mediation_only, fit_mediation_partial)


# --------------------------------------

direct_only <- '
  absdev ~ disturbedcover
  CATS_pois    ~ disturbedcover
'

fit_direct_only <- sem(direct_only, data=plot_data,
                         group="Region", meanstructure=TRUE,
                         group.equal=c("regressions", "residual.covariances" ),
                         estimator="ML")

# basic fit measures to compare
fitMeasures(fit_mediation_only, c("aic","bic","logl","npar","ntotal"))
fitMeasures(fit_mediation_partial, c("aic","bic","logl","npar","ntotal"))
fitMeasures(fit_direct_only, c("aic","bic","logl","npar","ntotal"))


#AICc - not in lavaan so performed manually - note this is standard linear model formula
correctAIC<-function(fit){
  mes<-fitMeasures(fit, c("aic","npar","ntotal"))
  mes[1] + 2*mes[2]*(mes[2]+1) / (mes[3]-mes[2]-1)
}

rl_med<-exp(-(correctAIC(fit_mediation_only)-correctAIC(fit_mediation_only))/2)
rl_par<-exp(-(correctAIC(fit_mediation_partial)-correctAIC(fit_mediation_only))/2)
rl_dir<-exp(-(correctAIC(fit_direct_only)-correctAIC(fit_mediation_only))/2)

rl_med/(rl_med+rl_par+rl_dir)


cor.test(plot_data$absdev, plot_data$CATS_pois)

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
  filter(outcome == "CATS_pois")

# scatter + group-specific regression lines
PanelA<-ggplot(plot_data, aes(x = disturbedcover, y = CATS_pois, color = Region)) +
  geom_point(alpha = 0.8, size=1) +
  geom_abline(data = coef_CATS,
              aes(intercept = intercept, slope = slope, color = region),
              size = 0.5) +
  theme_classic()+
  theme(legend.position = "none")+
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
  ylab("CATS regression R2")+
  xlab("Disturbed forest cover in 2000m radius")+
  ggsci::scale_color_npg()

# panel B
coef_absdev <- coef_df %>%
  filter(outcome == "absdev")

# scatter + group-specific regression lines
PanelB<-ggplot(plot_data, aes(x = CATS_pois, y = absdev, color = Region)) +
  geom_point(alpha = 0.8, size=1) +
  geom_abline(data = coef_absdev,
              aes(intercept = intercept, slope = slope, color = region),
              size = 0.5) +
  theme_classic()+
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
  xlab("CATS regression R2")+
  ylab("EEOS absolute deviation")+
  ggsci::scale_color_npg()



cowplot::plot_grid(PanelA, PanelB, labels = c('A', 'B'), rel_widths=c(0.82, 1))
ggsave("Figures/Figure_H2a.png", height=6, width=12)
