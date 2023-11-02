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
library(showtext)

# palette
fill <- "#274c77"
background <- "#a3cef1"
outline <- "#6096ba"
  
# fonts
font_add_google("Germania One", "germania") 
showtext_auto()
  
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

# add labels for notable spa town per state
bad$label <- NA
# Baden-Württemberg
bad$label[bad$city == "Baden-Baden"] <- "Baden-Baden"
# Bavaria
bad$label[bad$city == "Bad Kissingen"] <- "Bad Kissingen"
# Berlin (nope, city)
# Brandenburg
bad$label[bad$city == "Bad Liebenwerda"] <- "Bad Liebenwerda"
# Bremen (nope, city)
# Hamburg (nope, city)
# Hesse
bad$label[bad$city == "Wiesbaden"] <- "Wiesbaden"
# Lower Saxony
bad$label[bad$city == "Bad Harzburg"] <- "Bad Harzburg"
# Mecklenburg-Vorpommern
bad$label[bad$city == "Bad Doberan"] <- "Bad Doberan"
# North Rhine-Westphalia
bad$label[bad$city == "Bad Driburg"] <- "Bad Driburg"
# Rhineland-Palatinate
bad$label[bad$city == "Bad Dürkheim"] <- "Bad Dürkheim"
# Saarland (none in my list)
# Saxony (none in my list)
# Saxony-Anhalt (none in my list)
# Schleswig-Holstein
bad$label[bad$city == "Bad Schwartau"] <- "Bad Schwartau"
# Thüringen
bad$label[bad$city == "Bad Salzungen"] <- "Bad Salzungen"

# add color variable for plotting
bad$color <- NA
bad$color[is.na(bad$label)] <- "0"
bad$color[!is.na(bad$label)] <- "1"

# add label position
bad$position <- NA
bad$position <- 16 - as.numeric(bad$lon)
bad$position <- as.character(bad$position)

# plot!
p <- ggplot() +
  geom_polygon(data = deutsch,
               aes(x = long, y = lat, group = group),
               color = outline, fill = fill, linewidth = 0.25) +
  geom_point(data = bad, aes(x = as.numeric(lon), y = as.numeric(lat),
                             color = color, size = color)) +
  geom_label_repel(data = bad, aes(x = as.numeric(lon), y = as.numeric(lat),
                                   label = label, color = color),
                  box.padding = 0.1, fill = "#36454f",
                  direction = "x", nudge_x = c(-40,20), point.padding = 0.25,
                  size = 6, label.size = 0.25, fontface = "bold",
                  segment.size = 0.25, segment.linetype = "dashed") +
  scale_color_manual(values = c("#6096ba","#e7ecef")) +
  scale_size_manual(values = c(1,3)) +
  theme_void() + 
  coord_map() +
  labs(title = "Deutsche Badestädte",
       caption = "Spa Cities in Germany") +
  theme(legend.position = "none",
        plot.margin = margin(t = 3, b = 3, unit = "pt"),
        plot.background = element_rect(fill = background, color = background),
        panel.background = element_rect(fill = background, color = background),
        plot.title = element_text(family = "germania", size = 55, hjust = 0.5, color = "#36454f"),
        plot.caption = element_text(family = "germania", size = 40, hjust = 0.5, color = "#36454f"))

# save plot
ggsave(filename = "./maps/day4_bad.png",
       plot = p,
       width = 5,
       height = 5,
       units = "in",
       dpi = 300,
       device = "png")
