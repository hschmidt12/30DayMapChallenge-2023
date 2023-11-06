# 30 Day Map Challenge - 2023
# day 7 - navigation
# Helen Schmidt

# load packages
library(sf)
library(tidyverse)
library(showtext)
library(osmdata)

# x = lon, y = lat
coord <- c(-0.850213, 52.081541,
           -0.637353, 51.989034)

roundabouts <- opq(bbox = coord) |>
  add_osm_feature(key = "junction", value = c("roundabout", "circular")) |>
  add_osm_feature(key = "highway", value = "mini-roundabout") |>
  osmdata_sf()

roads <- opq(bbox = coord) |>
  add_osm_feature(key = "highway", value = c("primary","secondary","tertiary")) |>
  osmdata_sf()
  
ggplot() +
  geom_sf(data = roads$osm_lines, size = 1, color = "red") +
  geom_sf(data = roundabouts$osm_lines, size = 1, color = "blue") 

