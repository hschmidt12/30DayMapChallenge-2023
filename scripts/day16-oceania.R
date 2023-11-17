# 30 Day Map Challenge - 2023
# day 16 - oceania
# Helen Schmidt

# islands in oceania, zoom in on elevation of some major ones
# indonesia + melanesia

library(tidyverse)
library(sf)
library(elevatr)
library(raster)
library(showtext)
library(png)
library(grid)

# set working directory
setwd("/Volumes/GoogleDrive/My Drive/SANLab/30DayMapChallenge-2023")

font_add_google("Julius Sans One", "julius") 
font_add_google("Oswald", "oswald")
showtext_auto()

# four independent countries that make up melanesia
# + western new guinea in indonesia, new caledonia, and torres strait islands
fiji <- getData(country = "Fiji", level = 1)
vanuatu <- getData(country = "Vanuatu", level = 1)
solomon <- getData(country = "Solomon Islands", level = 1)
png <- getData(country = "Papua New Guinea", level = 1)
indo <- getData(country = "Indonesia", level = 1)

# get elevation from each country
fiji.elev <- get_elev_raster(fiji, z = 6, clip = "locations",
                             prj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
vanuatu.elev <- get_elev_raster(vanuatu, z = 6, clip = "locations",
                             prj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
solomon.elev <- get_elev_raster(solomon, z = 6, clip = "locations",
                             prj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
png.elev <- get_elev_raster(png, z = 6, clip = "locations",
                             prj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
indo.elev <- get_elev_raster(indo, z = 6, clip = "locations",
                            prj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

# convert elevation to data frame
# fiji
fiji.df <- as.data.frame(fiji.elev, xy = TRUE)
colnames(fiji.df) <- c("x", "y", "elevation") # meters
fiji.df <- subset(fiji.df, elevation >= 0)

# vanuatu
vanuatu.df <- as.data.frame(vanuatu.elev, xy = TRUE)
colnames(vanuatu.df) <- c("x", "y", "elevation") # meters
vanuatu.df <- subset(vanuatu.df, elevation >= 0)

# solomon islands
solomon.df <- as.data.frame(solomon.elev, xy = TRUE)
colnames(solomon.df) <- c("x", "y", "elevation") # meters
solomon.df <- subset(solomon.df, elevation >= 0)

# papua new guinea
png.df <- as.data.frame(png.elev, xy = TRUE)
colnames(png.df) <- c("x", "y", "elevation") # meters
png.df <- subset(png.df, elevation >= 0)

# indonesia (western new guinea)
indo.df <- as.data.frame(indo.elev, xy = TRUE)
colnames(indo.df) <- c("x", "y", "elevation") # meters
indo.df <- subset(indo.df, elevation >= 0)

# mini fiji
f <- ggplot() +
  geom_polygon(data = fiji,
               aes(x = long, y = lat, group = group),
               linewidth = 0.1, color = "#312f2f", fill = "#3e3c3c") +
  geom_contour(data = fiji.df, aes(x = x, y = y, z = elevation, color = ..level..), size = 0.08) +
  scale_color_gradientn(colors = c("#5b9a96","#dce0e5"), breaks = c(0,100,400)) +
  coord_map(xlim = c(177,182),
            ylim = c(-20,-16)) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#0b1b32", color = "#dce0e5"))

ggsave(filename = "./maps/day16_fiji.png",
       width = 3,
       height = 3,
       units = "in",
       device = "png")

# mini vanuatu
v <- ggplot() +
  geom_polygon(data = vanuatu,
               aes(x = long, y = lat, group = group),
               linewidth = 0.1, color = "#312f2f", fill = "#3e3c3c") +
  geom_contour(data = vanuatu.df, aes(x = x, y = y, z = elevation, color = ..level..), size = 0.08) +
  scale_color_gradientn(colors = c("#5b9a96","#dce0e5"), breaks = c(0,100,400)) +
  coord_map(xlim = c(166,171)) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#0b1b32", color = "#dce0e5"))

ggsave(filename = "./maps/day16_vanuatu.png",
       width = 3,
       height = 3,
       units = "in",
       device = "png")

# mini solomon islands
s <- ggplot() +
  geom_polygon(data = solomon,
               aes(x = long, y = lat, group = group),
               linewidth = 0.1, color = "#312f2f", fill = "#3e3c3c") +
  geom_contour(data = solomon.df, aes(x = x, y = y, z = elevation, color = ..level..), size = 0.08) +
  scale_color_gradientn(colors = c("#5b9a96","#dce0e5"), breaks = c(0,100,400)) +
  coord_map(xlim = c(155,168)) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#0b1b32", color = "#dce0e5"))

ggsave(filename = "./maps/day16_solomon.png",
       width = 3,
       height = 3,
       units = "in",
       device = "png")

# load in mini maps I just created
fiji.png <- readPNG("./maps/day16_fiji.png")
vanuatu.png <- readPNG("./maps/day16_vanuatu.png")
solomon.png <- readPNG("./maps/day16_solomon.png")

# big plot w/ zoom subplots
p <- ggplot() +
  # fiji
  geom_polygon(data = fiji,
               aes(x = long, y = lat, group = group),
               linewidth = 0.1, color = "#312f2f", fill = "#3e3c3c") +
  geom_contour(data = fiji.df, aes(x = x, y = y, z = elevation, color = ..level..), size = 0.04) +
  # vanuatu
  geom_polygon(data = vanuatu,
               aes(x = long, y = lat, group = group),
               linewidth = 0.1, color = "#312f2f", fill = "#3e3c3c") +
  geom_contour(data = vanuatu.df, aes(x = x, y = y, z = elevation, color = ..level..), size = 0.04) +
  # solomon islands
  geom_polygon(data = solomon,
               aes(x = long, y = lat, group = group),
               linewidth = 0.1, color = "#312f2f", fill = "#3e3c3c") +
  geom_contour(data = solomon.df, aes(x = x, y = y, z = elevation, color = ..level..), size = 0.04) +
  # papua new guinea
  geom_polygon(data = png,
               aes(x = long, y = lat, group = group),
               linewidth = 0.1, color = "#312f2f", fill = "#3e3c3c") +
  geom_contour(data = png.df, aes(x = x, y = y, z = elevation, color = ..level..), size = 0.04) +
  # indonesia
  geom_polygon(data = indo,
               aes(x = long, y = lat, group = group),
               linewidth = 0.1, color = "#312f2f", fill = "#3e3c3c") +
  geom_contour(data = indo.df, aes(x = x, y = y, z = elevation, color = ..level..), size = 0.04) +
  scale_color_gradientn(colors = c("#5b9a96","#dce0e5"), breaks = c(0,1000,4000)) +
  annotate(geom = "text",
           label = "INDONESIA + MELANESIA",
           x = -Inf, y = -Inf, hjust = -0.1, vjust = -0.8,
           size = 50, family = "julius", color = "#dce0e5") +
  annotate(geom = "text",
           label = "Fiji, Papua New Guinea, Solomon Islands, & Vanuatu",
           x = -Inf, y = -Inf, hjust = -0.2, vjust = -0.3,
           size = 20, family = "julius", color = "#dce0e5") +
  # recenter/crop map using xlim
  coord_map(xlim = c(95,182)) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#0b1b32", color = NA))

ggsave(filename = "./maps/day16_oceania.png",
       width = 7,
       height = 3,
       units = "in",
       dpi = 600,
       device = "png")

