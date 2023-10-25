# 30 Day Map Challenge - 2023
# day 2 - lines
# Helen Schmidt

# set working directory
setwd("/Volumes/GoogleDrive/My Drive/SANLab/30DayMapChallenge-2023")

# load packages
library(tidyverse)
library(sf)
library(cowplot)
library(RColorBrewer)

# load data
df <- sf::st_read("./data/tfl_lines.json")
df$geometry <- st_cast(df$geometry, to = "MULTILINESTRING")
london <- st_read("./data/London-wards-2018_ESRI/London_Ward.shp")

# create palette
colors <- c("#073B3A", "#1EA896", "#a8f0e6")
palette <- colorRampPalette(colors)
palette <- palette(33)

# plot
ggplot(df) +
  geom_sf(data = london, aes(fill = DISTRICT, color = DISTRICT)) +
  geom_sf(color = "#EFF9F0", size = 1) +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = palette) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#EFF9F0", color = "#EFF9F0"),
        plot.margin=unit(c(-5,0,-4,-6),"cm"))

# save plot
ggsave(filename = "./maps/day2_lines.png",
       width = 5,
       height = 5,
       units = "in",
       dpi = 300,
       device = "png")
