# --------------------------------------------
# Sript fits a simple regressions between absolute deviation and forest disturbance
# Outputs is a figure visualising the model
# ---------------------------------------------------


library(tidyverse)
library(cowplot)
library(car)
library(lmtest)
library(sandwich)
library(boot)
library(MASS)



set.seed(1)
d<-read.csv("Outputs/EEOSPredsHarteWD.csv") %>% as_tibble


plot_data<-d %>%
    left_join(., read.csv("Data/env.csv"))

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



# deviation magnitude model
#plot_data<-plot_data[-c(8,34),] # remove major qqplot outliers and confirm estimates stable
fit <- lm(absdev ~ disturbedcover + Region, data = plot_data)
summary(fit)
confint(fit)
car::qqPlot(fit)

# influence
hatvals <- hatvalues(fit)
cooks <- cooks.distance(fit)
which(hatvals > 2*mean(hatvals))
which(cooks > 4/length(cooks))


# heteroscedasticity test
lmtest::bptest(fit)    # Breusch-Pagan

# robust SEs (sandwich)
vc <- vcovHC(fit, type = "HC3")        # HC3 recommended for small/moderate n
coeftest(fit, vcov = vc)

# robust (Wald) 95% CIs using normal/t or coefci helper:
coefci(fit, vcov. = vc, level = 0.95)
# or manual normal approx
est <- coef(fit); se <- sqrt(diag(vc))
cbind(est - 1.96*se, est + 1.96*se)

# bootstrap CIs (basic example)
coeffun <- function(data, i) coef(lm(absdev ~ disturbedcover + Region, data = data[i, ]))
b <- boot(plot_data, coeffun, R = 10000)
boot.ci(b, type =  c("perc","bca","basic"), index = 5)  # index for coefficient of interest

# robust regression
rlm_fit <- rlm(absdev ~ disturbedcover + Region, data = plot_data)
summary(rlm_fit)



# supplementary signed deviation model
fit_signed <- lm(dev ~ disturbedcover + Region, data = plot_data)
summary(fit_signed)
confint(fit_signed)
car::qqPlot(fit_signed)

# influence
hatvals <- hatvalues(fit_signed)
cooks <- cooks.distance(fit_signed)
which(hatvals > 2*mean(hatvals))
which(cooks > 4/length(cooks))


# heteroscedasticity test
lmtest::bptest(fit_signed)    # Breusch-Pagan

# robust SEs (sandwich)
vc <- vcovHC(fit_signed, type = "HC3")        # HC3 recommended for small/moderate n
coeftest(fit_signed, vcov = vc)

# robust (Wald) 95% CIs using normal/t or coefci helper:
coefci(fit_signed, vcov. = vc, level = 0.95)
# or manual normal approx
est <- coef(fit_signed); se <- sqrt(diag(vc))
cbind(est - 1.96*se, est + 1.96*se)

# bootstrap CIs (basic example)
coeffun <- function(data, i) coef(lm(dev ~ disturbedcover + Region, data = data[i, ]))
b <- boot(plot_data, coeffun, R = 10000)
boot.ci(b, type =  c("perc","bca","basic"), index = 5)  # index for coefficient of interest

# robust regression
rlm_fit <- rlm(dev ~ disturbedcover + Region, data = plot_data)
summary(rlm_fit)



#############################
getCoefDataframe<-function(fit){
  regions <- unique(plot_data$Region)
  intercepts <- sapply(regions, function(r) {
    nd <- data.frame(disturbedcover = 0,
                    Region = factor(r, levels = regions))
    as.numeric(predict(fit, newdata = nd))
  })

  slope <- coef(fit)["disturbedcover"]

  coef_df <- data.frame(
    region = regions,
    intercept = intercepts,
    slope = rep(slope, length(regions))
  )
  return(coef_df)
}

coef_df<-getCoefDataframe(fit)

pa<-ggplot(plot_data, aes(x = disturbedcover, y = absdev, color = Region)) +
  geom_point(alpha = 0.8, size=0.5) +
  geom_abline(data = coef_df,
              aes(intercept = intercept, slope = slope, color = region),
              size = 0.5) +
  theme_classic()+
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
  xlab("Disturbed forest cover in 2000m radius")+
  ylab("EEOS absolute deviation")+
  ggsci::scale_color_npg()+
  guides(color="none")


pb<-ggplot(plot_data, aes(x = Region, y = absdev, fill = Region, color=Region)) +
  geom_violin() +
  theme_classic()+
  #geom_hline(yintercept=0, linetype=2)+
  ggsci::scale_fill_npg()+ ggsci::scale_color_npg()+
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme(
  axis.title.y = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.y=element_blank(),
  axis.ticks.x=element_blank(),
  axis.line.y=element_blank()
)+
xlab(" \n  ")


cowplot::plot_grid(pa, pb, labels = c('A', 'B'), rel_widths=c(1, 0.5))

ggsave("Figures/Figure_H1.png", height=6, width=6)


coef_signed_df<-getCoefDataframe(fit_signed)

p2a<-ggplot(plot_data, aes(x = disturbedcover, y = dev, color = Region)) +
  geom_point(alpha = 0.8, size=0.5) +
  geom_abline(data = coef_signed_df,
              aes(intercept = intercept, slope = slope, color = region),
              size = 0.5) +
  theme_classic()+
  geom_hline(yintercept=0, linetype=2)+
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  #scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
  xlab("Disturbed forest cover in 2000m radius")+
  ylab("EEOS deviation")+
  ggsci::scale_color_npg()+
  guides(color="none")



p2b<-ggplot(plot_data, aes(x = Region, y = dev, fill = Region, color=Region)) +
  geom_hline(yintercept=0, linetype=2)+
  geom_violin() +
  theme_classic()+
  #geom_hline(yintercept=0, linetype=2)+
  ggsci::scale_fill_npg()+ ggsci::scale_color_npg()+
  #scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme(
  axis.title.y = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.y=element_blank(),
  axis.ticks.x=element_blank(),
  axis.line.y=element_blank()
)+
xlab(" \n  ")


cowplot::plot_grid(p2a, p2b, labels = c('A', 'B'), rel_widths=c(1, 0.5))

ggsave("Figures/Figure_H1_signedsupplement.png", height=6, width=6)
