# 30 Day Map Challenge - 2023
# day 4 - bad map
# Helen Schmidt

# "bad" cities in germany (aka spa towns)

# set working directory
setwd("/Volumes/GoogleDrive/My Drive/SANLab/30DayMapChallenge-2023")

# load packages
library(tidyverse)
library(sf)
library(raster)
library(ggrepel)

# palette
background <- "#274c77"
outline <- "#a3cef1"
fill <- "#6096ba"
  
# get germany map for background plot
deutsch <- getData(country = "Germany", level = 1)

# load world city data
# city locations and populations
city <- st_read("./data/worldcities.csv") |>
  dplyr::select(city, lat, lon = lng, country, population)

# select only germany as a country from world city data
germany <- subset(city, country == "Germany")

# select only towns/cities that contain "bad"
bad.city <- germany[grep("bad", germany$city), ]
Bad.city <- germany[grep("Bad", germany$city), ]
bad <- rbind(bad.city, Bad.city)
bad$population <- as.numeric(bad$population)
names(bad)[5] <- "Population"

# add some city labels for plotting
bad$label <- NA
bad$label[bad$city == "Wiesbaden"] <- "Wiesbaden"
bad$label[bad$city == "Baden-Baden"] <- "Baden-Baden"
bad$label[bad$city == "Bad Kissingen"] <- "Bad Kissingen"
bad$label[bad$city == "Bad Driburg"] <- "Bad Driburg"
bad$label[bad$city == "Bad Dürkheim"] <- "Bad Dürkheim"
bad$label[bad$city == "Bad Liebenwerda"] <- "Bad Liebenwerda"

# plot!
ggplot() +
  geom_polygon(data = deutsch,
               aes(x = long, y = lat, group = group),
               color = outline, fill = fill, linewidth = 0.25) +
  geom_point(data = bad, aes(x = as.numeric(lon), y = as.numeric(lat),
                             size = scale(Population), color = rev(scale(Population)))) +
  geom_label_repel(data = bad, aes(x = as.numeric(lon), y = as.numeric(lat),
                                   label = label),
                   box.padding = 1.5, color = "#2E2E3A", fill = "#e7ecef") +
  #scale_color_gradientn(colors = c("#e7ecef","#8b8c89","#2E2E3A")) +
  scale_size_continuous(limits=c(0, 8), breaks=seq(0, 8, by=2)) +
  theme_void() + 
  coord_map() +
  theme(plot.background = element_rect(fill = background, color = background),
        panel.background = element_rect(fill = background, color = background))



  scale_color_gradientn(colors = c("#e7ecef","#8b8c89","#2E2E3A")) + 
  #coord_map() +
  #theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = background, color = background),
        panel.background = element_rect(fill = background, color = background),
        text = element_text(color = outline),
        legend.key.height = unit(0.5, "cm"), 
        legend.key.width = unit(0.1, "cm"))

# save plot
ggsave(filename = "./maps/day4_bad.png",
       width = 5,
       height = 5,
       units = "in",
       dpi = 300,
       device = "png")
