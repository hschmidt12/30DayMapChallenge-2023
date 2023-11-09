# 30 Day Map Challenge - 2023
# day 8 - africa
# Helen Schmidt

# malaria atlas project
# no. deaths from malaria per 100,000 population during defined year

library(tidyverse)
library(sf)
library(raster)
library(terra)
library(patchwork)
library(showtext)

# set working directory
setwd("/Volumes/GoogleDrive/My Drive/SANLab/30DayMapChallenge-2023")

# fonts
font_add_google("Glory","glory")
showtext_auto()

# africa
africa <- read_sf("./data/Africa_Boundaries-shp/Africa_Boundaries.shp")

# try to do what I did in day6-asia to crop mortality rate raster
# using the africa boundaries shapefile

# malaria data 
df2000 <- raster("./data/202206_Global_Pf_Mortality_Rate_2000/202206_Global_Pf_Mortality_Rate_2000.tif")
df2020 <- raster("./data/202206_Global_Pf_Mortality_Rate_2000/202206_Global_Pf_Mortality_Rate_2020.tif")

# mask malaria data using terra::mask to limit just to africa
# defined by my shapefile for africa
## 2000 ##
df2000 <- terra::mask(df2000, africa)
df2000 <- as.data.frame(df2000, xy = TRUE)
colnames(df2000) <- c("x", "y", "mortality") 
# remove NAs
df2000.noNA <- subset(df2000, !is.na(df2000$mortality))
df2000.noNA$mortality <- as.numeric(df2000.noNA$mortality)
df2000.noNA$mort <- df2000.noNA$mortality*100000

## 2020 ##
df2020 <- terra::mask(df2020, africa)
df2020 <- as.data.frame(df2020, xy = TRUE)
colnames(df2020) <- c("x", "y", "mortality")
# remove NAs
df2020.noNA <- subset(df2020, !is.na(df2020$mortality))
df2020.noNA$mortality <- as.numeric(df2020.noNA$mortality)
df2020.noNA$mort <- df2020.noNA$mortality*100000

# maybe filter to all values above 0 to prevent a lot of zero plots?
# or just plot raster and not shapefile for africa
# also crop plot to just show africa, right now shows big projection

colors <- c("#15616d","#78290f","#ff7d00","#ffecd1")

# plot
p1 <- ggplot() +
  geom_raster(data = df2000.noNA, aes(x = x, y = y, fill = mort)) +
  coord_sf() +
  labs(title = "2000") +
  scale_fill_gradientn(colors = colors) +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 60, color = "#78290f", face = "bold", family = "glory"),
        plot.background = element_rect(fill = "#ffecd1", color = NA))

p2 <- ggplot() +
  geom_raster(data = df2020.noNA, aes(x = x, y = y, fill = mort)) +
  coord_sf() +
  labs(title = "2020") +
  scale_fill_gradientn(colors = colors) +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 60, color = "#78290f", face = "bold", family = "glory"),
        plot.background = element_rect(fill = "#ffecd1", color = NA))
  
plots <- p1 + p2 & patchwork::plot_annotation(title = "Deaths from Malaria",
                                              theme = theme(plot.background = element_rect(fill = "#ffecd1", color = "#ffecd1"),
                                                            plot.title = element_text(size = 100, hjust = 0.5, family = "glory",
                                                                                      color = "#78290f", face = "bold")))

# save plot
ggsave(plot = plots,
       filename = "./maps/day8_africa.png",
       width = 10,
       height = 5, 
       units = "in")

