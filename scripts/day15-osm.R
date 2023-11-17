# 30 Day Map Challenge - 2023
# day 15 - open street map
# Helen Schmidt

# load packages
library(sf)
library(tidyverse)
library(showtext)
library(osmdata)

setwd("/Volumes/GoogleDrive/My Drive/SANLab/30DayMapChallenge-2023")

font_add_google("Vast Shadow", "vast") 
font_add_google("Red Hat Display", "redhat") # subtitles
showtext_auto()

# get canary islands 
espana <- getData(country = "spain", level = 1)
canary <- espana |>
  st_as_sf() |>
  filter(NAME_1 %in% "Islas Canarias")

# coord box for canary isalnds
coord <- c(-18.566411, 29.351426, 
           -13.260014, 27.613347)

# [x_min, y_min, x_max, y_max] 
# x_min and y_min are coordinates of the top-left corner of the bounding box. 
# x_max and y_max are coordinates of bottom-right corner of the bounding box.

# get hiking paths 
trails <- opq(bbox = coord) %>%
  add_osm_feature(key = "route", value = "hiking") %>%
  osmdata_sf()

p <- ggplot() +
  geom_sf(data = canary$geometry, color = "#d9cfc1", fill = "#3d0c11", linewidth = 0.1) + 
  geom_sf(data = trails$osm_lines, linewidth = 0.1, color = "#d9cfc1") +
  ggtitle("ISLAS CANARIAS") +
  labs(subtitle = "(canary islands, spain)",
       caption = "HIKING TRAILS") +
  # add island names
  annotate(geom = "text",
           label = "tenerife",
           x = -Inf, y = -Inf, hjust = -3.45, vjust = -12,
           family = "redhat", size = 26, color = "#E07B00", fontface = "italic") +
  annotate(geom = "text",
           label = "fuerteventura",
           x = -Inf, y = -Inf, hjust = -5, vjust = -5,
           family = "redhat", size = 26, color = "#E07B00", fontface = "italic") +
  annotate(geom = "text",
           label = "gran canaria",
           x = -Inf, y = -Inf, hjust = -3.8, vjust = -9,
           family = "redhat", size = 26, color = "#E07B00", fontface = "italic") +
  annotate(geom = "text",
           label = "lanzarote",
           x = -Inf, y = -Inf, hjust = -9.5, vjust = -16.5,
           family = "redhat", size = 26, color = "#E07B00", fontface = "italic") +
  annotate(geom = "text",
           label = "la palma",
           x = -Inf, y = -Inf, hjust = -0.75, vjust = -18,
           family = "redhat", size = 26, color = "#E07B00", fontface = "italic") +
  annotate(geom = "text",
           label = "la gomera",
           x = -Inf, y = -Inf, hjust = -1.68, vjust = -4.5,
           family = "redhat", size = 26, color = "#E07B00", fontface = "italic") +
  annotate(geom = "text",
           label = "el hierro",
           x = -Inf, y = -Inf, hjust = -0.5, vjust = -0.001,
           family = "redhat", size = 26, color = "#E07B00", fontface = "italic") +
  annotate(geom = "text",
           label = "la graciosa",
           x = -Inf, y = -Inf, hjust = -12.75, vjust = -36,
           family = "redhat", size = 16, color = "#E07B00", fontface = "italic") +
  # add themes
  theme_void() +
  theme(plot.title = element_text(family = "vast", hjust = 0.5, 
                                  size = 150, vjust = -3, color = "#E07B00"), 
        plot.subtitle = element_text(family = "redhat", face = "italic", color = "#E07B00",
                                     hjust = 0.5, size = 80, vjust = -17),
        plot.caption = element_text(family = "vast", hjust = 0.5,
                                    size = 130, vjust = 33, color = "#E07B00"),
        plot.background = element_rect(fill = "#036163", color = NA))

ggsave(filename = "./maps/day15_osm.png",
       plot = p,
       width = 7,
       height = 4,
       units = "in",
       dpi = 600,
       device = "png")

