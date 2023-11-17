# 30 Day Map Challenge - 2023
# day 17 - flow
# Helen Schmidt

# lava flow hazard zones hawaii big island

library(tidyverse)
library(sf)
library(showtext)
library(maps)
library(patchwork)
library(ggrepel)
library(ggfx)

# set working directory
setwd("/Volumes/GoogleDrive/My Drive/SANLab/30DayMapChallenge-2023")

# fonts
font_add_google("Inika", "inika")
showtext_auto()

# load lava flow hazard zone data
lava <- read_sf("./data/Hawaii_Lava_Flow_Hazard_Zones/Hawaii_Lava_Flow_Hazard_Zones.shp")
# reverse score numeric severity rating so 1 = low hazard, 9 = high hazard
lava$SEVERITY.REV[lava$SEVERITYCO == 1] <- 9
lava$SEVERITY.REV[lava$SEVERITYCO == 2] <- 8
lava$SEVERITY.REV[lava$SEVERITYCO == 3] <- 7
lava$SEVERITY.REV[lava$SEVERITYCO == 4] <- 6
lava$SEVERITY.REV[lava$SEVERITYCO == 5] <- 5
lava$SEVERITY.REV[lava$SEVERITYCO == 6] <- 4
lava$SEVERITY.REV[lava$SEVERITYCO == 7] <- 3
lava$SEVERITY.REV[lava$SEVERITYCO == 8] <- 2
lava$SEVERITY.REV[lava$SEVERITYCO == 9] <- 1

# read census data from 2020
census <- read_sf("./data/2020_Census_Block_Groups/2020_Census_Block_Groups.shp")

# add location for active volcanoes on island (not including dormant/extinct)
volcanoes <- data.frame(volcano = c("Mauna Loa", "Kīlauea", "Hualālai"),
                        x = c(-155.575664364, -155.285165526, -155.866996532),
                        y = c(19.459831494, 19.405998376, 19.688663912))     

# lava flow hazard zones
l <- ggplot() +
  with_shadow(geom_sf(data = lava$geometry, aes(fill = lava$SEVERITY.REV, color = lava$SEVERITY.REV)), 
              colour = "#1b4965", sigma = 3) +
  scale_fill_gradientn(colors = c("#fefae0","#dda15e","#bc6c25"), breaks = c(1,5,9),
                       labels = c("Low", "Medium", "High")) +
  scale_color_gradientn(colors = c("#fefae0","#dda15e","#bc6c25"), breaks = c(1,5,9),
                        labels = c("Low", "Medium", "High")) +
  # add points for mauna kea and mauna loa
  geom_point(data = volcanoes, aes(x = x, y = y), shape = 17, size = 3.5) +
  geom_text_repel(data = volcanoes, aes(x = x, y = y, label = volcano), 
                  size = 10, point.padding = 8, family = "inika") +
  labs(title = "Lava Flow Hazard Severity") +
  coord_sf(xlim = c(-156.0487, -154.8042),
           ylim = c(18.96392, 20.27583)) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 50, family = "inika"),
        plot.background = element_rect(fill = "#62b6cb", color = "#62b6cb"),
        text = element_text(family = "inika"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.height = unit(0.5, "line"),
        legend.key.width = unit(1, "line"),
        legend.text = element_text(size = 24))

# 2020 population
p <- ggplot() +
  with_shadow(geom_sf(data = census$geometry, aes(fill = census$pop20, color = census$pop20)),
              colour = "#1b4965", sigma = 3) +
  scale_fill_gradientn(colors = c("#fefae0","#606c38","#283618"), breaks = c(0,3000,6000)) +
  scale_color_gradientn(colors = c("#fefae0","#606c38","#283618"), breaks = c(0,3000,6000)) +
  # add points for mauna kea and mauna loa
  geom_point(data = volcanoes, aes(x = x, y = y), shape = 17, size = 3.5) +
  geom_text_repel(data = volcanoes, aes(x = x, y = y, label = volcano),
                   size = 10, point.padding = 8, family = "inika") +
  labs(title = "Population in 2020 Census",
       color = "Population",
       fill = "Population") +
  # big island only
  coord_sf(xlim = c(-156.0487, -154.8042),
           ylim = c(18.96392, 20.27583)) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 50, family = "inika"),
        plot.background = element_rect(fill = "#62b6cb", color = "#62b6cb"),
        text = element_text(family = "inika"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.height = unit(0.5, "line"),
        legend.key.width = unit(1, "line"),
        legend.text = element_text(size = 24))

plots <- l + p & patchwork::plot_annotation(theme = theme(plot.background = element_rect(fill = "#62b6cb", color = "#62b6cb")))

# save
ggsave(plot = plots,
       filename = "./maps/day17_flow.png",
       width = 8,
       height = 5, 
       units = "in")
