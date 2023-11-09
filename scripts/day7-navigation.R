# 30 Day Map Challenge - 2023
# day 7 - navigation
# Helen Schmidt

# load packages
library(sf)
library(tidyverse)

# set working directory
setwd("/Volumes/GoogleDrive/My Drive/SANLab/30DayMapChallenge-2023")

# roads 
roads <- read_sf("./data/tl_2019_us_primaryroads/tl_2019_us_primaryroads.shp")

# only roads that are part of primary interstate highway system
interstate <- subset(roads, roads$RTTYP == "I")
# exclude hawaii
interstate <- subset(interstate, interstate$FULLNAME != "I- H-1" &
                       interstate$FULLNAME != "I- H-2" &
                       interstate$FULLNAME != "I- H-3" &
                       interstate$FULLNAME != "I- H-201" &
                       interstate$FULLNAME != "I- H2")

# plot
p <- ggplot() +
  theme_void() +
  geom_sf(data = interstate, color = "white", linewidth = 1.25) +
  coord_sf(expand = FALSE) 

# save
ggsave(filename = "./maps/day7_navigation.png",
       plot = p,
       width = 7.5,
       height = 7.5,
       units = "in",
       dpi = 600,
       device = "png")

# not pictured: a little bit of image magic to combine 
# interstate plot with interstate sign

