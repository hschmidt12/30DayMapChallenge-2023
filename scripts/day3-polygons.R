# 30 Day Map Challenge - 2023
# day 3 - polygons
# Helen Schmidt

# set working directory
setwd("/Volumes/GoogleDrive/My Drive/SANLab/30DayMapChallenge-2023")

# load packages
library(tidyverse)
library(sf)
library(maps)

# get new zealand map
nz <- map_data("nz") |>
  select(lon = long, lat, group, id = region)

# get coastal erosion data
erosion <- st_read("./data/nzlri-erosion-type-and-severity.csv")
# convert geometry to WGS84 coordinates (lat/lon)
# centers <- as.data.frame(st_coordinates(erosion))
# centers.subset <- centers[,1:2]
# proj4 <- "+proj=tmerc"
# lon.lat <- project(centers.subset, proj4, inverse = TRUE)
# lon.lat <- data.frame(lon = lon.lat$x, lat = lon.lat$y)


# plot
ggplot() +
  geom_polygon(data = nz, aes(x = lon, y = lat, group = group)) +
  #geom_polygon(data = lon.lat, aes(lon, y = lat), color = "blue") +
  theme_void() +
  coord_map()

# save plot
ggsave(filename = "./maps/day3_polygons.png",
       width = 5,
       height = 5,
       units = "in",
       dpi = 300,
       device = "png")