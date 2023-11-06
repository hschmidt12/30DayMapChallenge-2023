# 30 Day Map Challenge - 2023
# day 6 - asia
# Helen Schmidt

library(tidyverse)
library(sf)
library(elevatr)
library(raster)
library(showtext)
library(ggrepel)

# set working directory
setwd("/Volumes/GoogleDrive/My Drive/SANLab/30DayMapChallenge-2023")

font_add_google("Rock Salt", "salt") 
font_add_google("Oswald", "oswald")
showtext_auto()

# nepal 
nepal <- getData(country = "Nepal", level = 1)

# get elevation using raster
himalayas <- get_elev_raster(nepal, z = 6, clip = "locations",
                             prj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
# convert elevation to data frame
himalayas.df <- as.data.frame(himalayas, xy = TRUE)
colnames(himalayas.df) <- c("x", "y", "elevation") # meters

# 8000+ meter peaks
peaks <- data.frame(long = c(83.823889,84.559722,83.49,86.660833,87.088611,86.933333,88.133331,86.925018),
                    lat = c(28.596111,28.55,28.696667,28.094167,27.889167,27.961667,27.7,27.988236),
                    name = c("  Annapurna", "Manaslu", "Dhaulagiri", "Cho Oyu", "Makalu","     Lhotse","Kanchenjunga","      Mount Everest"),
                    height = c(8091,8163,8167,8201,8463,8516,8586,8849))

# plot
ggplot() +
  geom_polygon(data = nepal,
               aes(x = long, y = lat, group = group),
               linewidth = 0.25, color = "#312f2f", fill = "#3e3c3c") +
  geom_contour(data = himalayas.df, aes(x = x, y = y, z = elevation, color = ..level..), size = 0.4) +
  geom_point(data = peaks,
             aes(x = long, y = lat), size = 1.5, color = "#edeec0", shape = 17) +
  geom_text_repel(data = peaks, aes(x = long, y = lat, label = name, color = height),
                  direction = "y", size = 12, nudge_y = 1, family = "oswald",
                  segment.size = 0.1, segment.linetype = "dashed") +
  theme_void() +
  coord_map() +
  scale_color_gradientn(colors = c("#ab490d","#edeec0"), breaks = c(0,2000,8000)) +
  annotate(geom = "text",
           label = "NEPAL",
           x = -Inf, y = -Inf, hjust = -0.2, vjust = -0.8,
           size = 60, family = "salt", color = "#edeec0") +
  annotate(geom = "text",
           label = "8 0 0 0 m   P e a k s",
           x = -Inf, y = -Inf, hjust = -0.7, vjust = -0.2,
           size = 20, family = "oswald", color = "#ab490d") +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#312f2f", color = NA))

  theme(legend.position = c(0.8, 0.2),
        legend.title = element_blank(),
        legend.key.size = unit(0.5,"line"),
        legend.text = element_text(size = 25),
        plot.background = element_rect(fill = "#312f2f", color = NA))

# save
ggsave(filename = "./maps/day6_asia.png",
       width = 5,
       height = 3,
       units = "in",
       dpi = 600,
       device = "png")

