library(tidyverse)
library(terra)
library(sf)
library(rnaturalearth)   # to get Brazil boundary
library(tidyterra)    # tidyterra provides geom_spatvector for ggplot2


df<-read.csv("Data/env.csv") %>% as_tibble %>% filter(Biome=="Amazonia")


# ---------------------------
# 1) Replace this with your tibble (here I assume it's named `df`)
# ---------------------------
# Example: if your data already in object 'df', skip this block.
# df <- readr::read_csv("your_data.csv")
# Must have columns: Region, X (lon), Y (lat), fc2000

# For demonstration I'll assume you have a tibble called `df`.
# The code below operates on df; if your tibble is named differently, change it.

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

# --- base map: Brazil polygon as SpatVector ---
brazil_sf  <- rnaturalearth::ne_countries(scale = "medium", country = "Brazil", returnclass = "sf")
brazil_spv <- terra::vect(brazil_sf)  # SpatVector in EPSG:4326



pts_spv <- vect(summary_df, geom = c("lon", "lat"), crs = "EPSG:4326")
# --- Plot with tidyterra + ggplot2 ---
p <- ggplot() +
  # base: Brazil
  geom_spatvector(data = brazil_spv, fill = "#757675ff", color = "#999999", size = 0.2) +
  # underlay polygons (drawn first, semi-transparent)
  geom_spatvector(data = pts_spv,
                  aes(alpha = fc2000_mean, size=n) )+
  geom_spatvector(data = pts_spv,
                  aes(size=n/2, color=Region)) +  
    scale_size(range = c(30, 50))+
  # numeric labels (n) on top
  geom_text(data = summary_df, aes(x = lon, y = lat, label = n),
            color = "black", fontface = "bold", size=17) +
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
guides(size="none", alpha="none")

p
  ggsave("Figures/regions_map.png", p, width = 10, height = 10, dpi = 300)
