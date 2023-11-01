# 30 Day Map Challenge - 2023
# day 2 - lines
# Helen Schmidt

# set working directory
setwd("/Volumes/GoogleDrive/My Drive/SANLab/30DayMapChallenge-2023")

# load packages
library(tidyverse)
library(sf)
library(showtext)

# get fonts
font_add_google("Cabin", "cabin") # inspired by Edward Johnson's typeface for Underground
showtext_auto()

# load data
df <- sf::st_read("./data/tfl_lines.json")
df$geometry <- st_cast(df$geometry, to = "MULTILINESTRING")
london <- st_read("./data/London-wards-2018_ESRI/London_Ward.shp")
# group polygons into london districts instead of wards
london <- london |>
  group_by(DISTRICT) |>
  summarize(geometry <- st_union(geometry))
# rename geometry vector
st_geometry(london) <- "geometry"

# get tube lines
df$line <- NA
df$line[grep("Overground",df$lines)] <- "Overground"
df$line[grep("Bakerloo",df$lines)] <- "Bakerloo"
df$line[grep("Central",df$lines)] <- "Central"
df$line[grep("Piccadilly",df$lines)] <- "Piccadilly"
df$line[grep("Victoria",df$lines)] <- "Victoria"
df$line[grep("Hammersmith & City",df$lines)] <- "H&C"
df$line[grep("Metropolitan",df$lines)] <- "Metropolitan"
df$line[grep("Jubilee",df$lines)] <- "Jubilee"
df$line[grep("DLR",df$lines)] <- "DLR"
df$line[grep("Northern",df$lines)] <- "Northern"
df$line[grep("Waterloo",df$lines)] <- "Waterloo & City"
df$line[grep("Elizabeth",df$lines)] <- "Elizabeth Line"
df$line[grep("District",df$lines)] <- "District"
df$line[grep("Circle",df$lines)] <- "Circle"

df <- subset(df, !is.na(line))

# create palette
outline <- "#454955"
background <- "#FDFFFC"

# plot!
ggplot() +
  geom_sf(data = london, color = outline, fill = background, linewidth = 0.075) +
  geom_sf(data = df, linewidth = 0.5, aes(color = df$line)) +
  scale_color_manual(values=c("Victoria" = "#0098D4",
                              "Central" = "#E32017",
                              "Bakerloo" = "#B36305",
                              "Circle" = "#FFD300",
                              "District" = "#00782A",
                              "Piccadilly" = "#003688",
                              "Overground" = "#EE7C0E",
                              "H&C" = "#F3A9BB",
                              "Metropolitan" = "#9B0056",
                              "Jubilee" = "#A0A5A9",
                              "DLR" = "#00A4A7",
                              "Northern" = "#000000",
                              "Waterloo & City" = "#95CDBA",
                              "Elizabeth Line" = "#6950a1")) +
  theme_void() +
  labs(title = "L O N D O N    U N D E R G R O U N D",
       caption = "*Elizabeth Line, Overground, & DLR are part of TfL, but not Tube network") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.background = element_rect(fill = background, color = background),
        panel.background = element_rect(fill = background, color = background),
        text = element_text(color = outline, family = "cabin"),
        legend.key.size = unit(10,"point"),
        legend.text = element_text(size = 25),
        plot.title = element_text(hjust = 0.5, size = 40),
        plot.caption = element_text(size = 15, face = "italic", hjust = 0.95))

# save plot
ggsave(filename = "./maps/day2_lines.png",
       width = 5,
       height = 3.5,
       units = "in",
       dpi = 300,
       device = "png")
