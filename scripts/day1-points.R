# 30 Day Map Challenge - 2023
# day 1 - points
# Helen Schmidt

## SCRIPT SET UP ##
# set working directory
setwd("/Volumes/GoogleDrive/My Drive/SANLab/30DayMapChallenge-2023")
# load packages
library(tidyverse)
library(sf)
library(geomtextpath)
library(maps)
library(showtext)
library(mapview)
# set palette
lava <- c("#1D110B","#922D26","#FF8F33")

## GET DATA ##
# load volcano location data
df <- read.csv("./data/volcanoes.csv")
# get world data to plot country outlines
world <- sf::st_as_sf(maps::map(database = "world", plot = FALSE, fill = TRUE))
# make sure geometries don't get cut in projection
world  <- st_cast(world, 'MULTILINESTRING') %>%
  st_cast('LINESTRING', do_split=TRUE) %>%
  mutate(npts = npts(geom, by_feature = TRUE)) %>%
  st_cast('POLYGON')

## WRANGLING ##
# create title
t = seq(1, 0, length.out = 10) * pi
my.title <- data.frame(x = cos(t),
                       y = sin(t),
                       label = "R i n g   o f   F i r e")
# get sf geometry for lat/long
volcanoes <- st_as_sf(df, coords = c("Longitude", "Latitude"), crs = 4326)
# create pacific ocean shape
ocean <- st_point(x = c(0,0)) %>%
  st_buffer(dist = 6371000) %>%
  st_sfc(crs = "+proj=ortho +lat_0=20 +lon_0=190")

## PLOT ##
# plot
ggplot(volcanoes) +
  geom_sf(data = ocean, fill = lava[1], color = lava[1]) +
  geom_sf(data = world, color = lava[2], fill = lava[1]) +
  geom_sf(size = 0.1, color = lava[3]) +
  coord_sf(crs = "+proj=ortho +lat_0=20 +lon_0=190") +
  geom_textpath(data = my.title, aes(x, y, label = label), 
                size = 16, color = lava[3], vjust = 5.35) +
  theme_void() +
  theme(plot.background = element_rect(fill = lava[1], color = NA))

# save plot
ggsave(filename = "./maps/day1_points.png",
       width = 5,
       height = 5,
       units = "in",
       dpi = 300,
       device = "png")
