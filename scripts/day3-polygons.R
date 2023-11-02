# 30 Day Map Challenge - 2023
# day 3 - polygons
# Helen Schmidt

# set working directory
setwd("/Volumes/GoogleDrive/My Drive/SANLab/30DayMapChallenge-2023")

# load packages
library(tidyverse)
library(sf)
library(MetBrewer)
library(showtext)

# palette
palette <- met.brewer("Hiroshige", n = 15)
background <- "#36454f"
  
# fonts
font_add_google("Macondo", "macondo") 
showtext_auto()

# get new zealand map ()
nz <- read_sf("./data/statsnz-regional-tourism-organisation-areas-2023-clipped-SHP/regional-tourism-organisation-areas-2023-clipped.shp")

# get coastal erosion data
erosion <- read_sf("./data/lris-nzlri-erosion-type-and-severity-SHP/nzlri-erosion-type-and-severity.shp")
# affected land areas only
erosion <- subset(erosion, erosion$ERO1S == "1" | erosion$ERO1S == "2" | erosion$ERO1S == "3" | 
                    erosion$ERO1S == "4" | erosion$ERO1S == "5")

# create new erosion type labels; dominant erosion present
erosion$label <- NA
erosion$label[erosion$ERO1T == "Sh"] <- "sheet"
erosion$label[erosion$ERO1T == "Sc"] <- "scree"
erosion$label[erosion$ERO1T == "Ss"] <- "soil slip"
erosion$label[erosion$ERO1T == "W"] <- "wind"
erosion$label[erosion$ERO1T == "D"] <- "deposition"
erosion$label[erosion$ERO1T == "Da"] <- "debris avalanche"
erosion$label[erosion$ERO1T == "Ef"] <- "earthflow"
erosion$label[erosion$ERO1T == "Es"] <- "earth slip"
erosion$label[erosion$ERO1T == "G"] <- "gully"
erosion$label[erosion$ERO1T == "T"] <- "tunnel gully"
erosion$label[erosion$ERO1T == "Sb"] <- "streambank"
erosion$label[erosion$ERO1T == "Su"] <- "slump"
erosion$label[erosion$ERO1T == "Rs"] <- "riparian slip"
erosion$label[erosion$ERO1T == "R"] <- "rill"
erosion$label[erosion$ERO1T == "Rf"] <- "rockfall"

p <- ggplot() +
  geom_sf(data = nz, fill = "#DBE2E6", color = "#DBE2E6") +
  geom_sf(data = erosion, aes(color = label, fill = label)) + 
  scale_fill_manual(values = palette) +
  scale_color_manual(values = palette) +
  coord_sf() +
  theme_void() +
  labs(title = "New Zealand Land Erosion") +
  theme(# legend
        legend.position = "bottom",
        legend.margin = margin(b = 6, unit = "pt"),
        legend.title = element_blank(),
        legend.key.size = unit(0.5,"line"),
        legend.text = element_text(size = 25),
        # title
        plot.title = element_text(hjust = 0.5, size = 50),
        plot.margin = margin(t = 6, unit = "pt"),
        # background
        plot.background = element_rect(fill = background, color = background),
        panel.background = element_rect(fill = background, color = background),
        # text
        text = element_text(color = "#DBE2E6", family = "macondo"))


# save plot
ggsave(plot = p, 
       filename = "./maps/day3_polygons.png",
       width = 5,
       height = 5,
       units = "in",
       dpi = 300,
       device = "png")
