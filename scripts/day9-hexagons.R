# 30 Day Map Challenge - 2023
# day 8 - hexagon
# Helen Schmidt

# how much of a hexagon is france really?
# La France est-elle vraiment un hexagone ?

library(tidyverse)
library(sf)
library(raster)
library(showtext)
library(ggforce)

# set working directory
setwd("/Volumes/GoogleDrive/My Drive/SANLab/30DayMapChallenge-2023")

# fonts
font_add_google("Abril Fatface", "abril")
font_add_google("Contrail One", "contrail")
showtext_auto()

# hex
#get width given height
wd_hex <- function(height){
  tri_side <- height/2
  sma_side <- height/4
  width <- 2*sqrt(tri_side^2 - sma_side^2)
  return(width)
}

france <- getData(country = "France", level = 0)
p <- ggplot() +
  # not in hexagon
  geom_polygon(data = france,
               aes(x = long, y = lat, group = group), fill = "#ED2939") +
  # hexagon border
  geom_regon(aes(x0 = 2.5, y0 = 46.5,
                 sides = 6, angle = 499, r = 5.6), 
             alpha = 1, inherit.aes = F, color = "#002654", fill = "#002654") + 
  # in hexagon
  geom_polygon(data = france,
               aes(x = long, y = lat, group = group), fill = NA, color = "white") +
  coord_sf(crs = 2964) +
  theme_void() +
  # add custom legend
  annotate(geom = "label",
           label = "hexagon",
           color = "white", fill = "#002654",
           x = 5.5, y = 40.5, size = 16, 
           label.size = 0.5, family = "contrail") +
  annotate(geom = "label",
           label = "not a hexagon",
           color = "white", fill = "#ED2939",
           x = -1, y = 40.5, size = 16, 
           label.size = 0.5, family = "contrail") +
  labs(subtitle = "is l'hexagone really a hexagon?",
       title = "assessing France's hexagonality") +
  theme(text = element_text(face = "plain"),
        plot.background = element_rect(fill = "#666666", color = NA),
        plot.title = element_text(hjust = 0.5, family = "abril", size = 60, color = "white"),
        plot.subtitle = element_text(hjust = 0.5, face = "italic", family = "contrail", 
                                     size = 40, color = "white", vjust = 0.5))

ggsave(filename = "./maps/day9_hexagons.png",
       plot = p,
       width = 5,
       height = 5,
       units = "in")

