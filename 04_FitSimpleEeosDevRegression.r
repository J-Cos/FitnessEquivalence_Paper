# --------------------------------------------
# Purpose: Fits simple linear regression between absolute deviation and forest disturbance
# Input Origin Scripts: 03_FitEEOS.py
# Output: figures visualising linear regression findings
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

# alternative with outliers removed
fit_noOutliers <- lm(absdev ~ disturbedcover + Region, data = plot_data[-c(8, 34),] )
summary(fit_noOutliers)
confint(fit_noOutliers)
car::qqPlot(fit_noOutliers)



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
# make main plot (figure 1)
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
  xlab("Disturbed forest cover\n(2000m radius)")+
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
xlab(" \n  \n ")


#create map
# --------------------------------------------
# Purpose: Produces a simple map of the forest plots locations
# Input Origin Scripts: None
# Output: map of forest plots locations
# ---------------------------------------------------

library(tidyverse)
library(terra)
library(sf)
library(rnaturalearth)   # to get South America boundary
library(tidyterra)    # tidyterra provides geom_spatvector for ggplot2
library(ggsci)

# ---------------------------
# 1) Load df
# ---------------------------
df<-read.csv("Data/env.csv") %>% as_tibble %>% filter(Biome=="Amazonia")

# ---------------------------
# 2) Summarise by Region
# ---------------------------
summary_df <- df %>%
  # ensure columns exist and types are okay
  group_by(Region) %>%
  summarise(
    n = n(),
    lon = mean(X, na.rm = TRUE),
    lat = mean(Y, na.rm = TRUE),
    fc2000_mean = mean(fc2000, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(fc2000_mean))

summary_df<-rbind(summary_df, c("Jurua", 30, -67.71, -5.56, 1)) %>%
 mutate(  Region=factor(Region, levels=c("Jurua", "Balbina", "STM", "PGM")),
        n=as.numeric(n),
         lon = as.numeric(lon),
         lat = as.numeric(lat),
         fc2000_mean = as.numeric(fc2000_mean)) 
         # Check
print(summary_df)

# --- new: get all South American countries instead of Brazil ---
south_america_sf <- rnaturalearth::ne_countries(
  continent = "South America", 
  returnclass = "sf"
)
south_america_spv <- terra::vect(south_america_sf)

# ---------------------------
# 3) Load ecoregions
# ---------------------------
eco <- terra::vect("Data/Ecoregions2017/Ecoregions2017.shp")
rainforest<-eco[eco$BIOME_NAME=="Tropical & Subtropical Moist Broadleaf Forests" & eco$REALM=="Neotropic"] %>% crop(south_america_spv)
pts_spv <- vect(summary_df, geom = c("lon", "lat"), crs = "EPSG:4326")

# ---------------------------
# 4) Plot
# ---------------------------
map <- ggplot() +
  geom_spatvector(data = rainforest, fill = "grey", color = "grey", size = 0.2) +
  geom_spatvector(data = south_america_spv, fill = NA , color = "black", linewidth = 0.4) +
  # underlay polygons (drawn first, semi-transparent)
  geom_spatvector(data = pts_spv,
                  aes(alpha = fc2000_mean, size=n) )+
  geom_spatvector(data = pts_spv,
                  aes(size=n/2, color=Region)) +  
    scale_size(range = c(2.5, 5.5))+
  # numeric labels (n) on top
  geom_text(data = summary_df, aes(x = lon, y = lat, label = n),
            color = "black", fontface = "bold", size=2) +
  ggsci::scale_color_npg()+
  # coordinate limits focused on Brazil; tweak to adjust framing
  #coord_sf(xlim = c(-80, -40), ylim = c(-12, 2), expand = FALSE) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    #legend.position = c(0.82, 0.25),
    plot.title = element_text(face = "bold")
  ) +
  guides(size="none", alpha="none", color="none")





cowplot::plot_grid(map, pa, pb, labels = c('A', 'B', 'C'), rel_widths=c(0.5,1, 0.5), ncol=3)

ggsave("Figures/Figure_H1.png", height=4, width=6, bg="white", dpi=1000)




























# make supplementary signed deviation plot

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
  xlab("Disturbed forest cover\n(2000m radius)")+
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
xlab(" \n  \n ")


cowplot::plot_grid(p2a, p2b, labels = c('A', 'B'), rel_widths=c(1, 0.5))

ggsave("Figures/Figure_H1_signedsupplement.png", height=4, width=4.5)
