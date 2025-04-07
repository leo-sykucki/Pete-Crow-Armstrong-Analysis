getwd()
setwd("C:/Users/lsyku/OneDrive/Documents/Player Analysis/Pete Analysis")


library(plyr)
library(dplyr)
library(devtools)
library(DT)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(gridExtra)
library(janitor)
library(plotly)
library(stringr)
library(tidyr)
library(tidyselect)
library(tidyverse)
library(data.table)
library(reactable)
library(lubridate)
library(ggpubr)

##Load Data

PCA <- read.csv("Pete Crow-Armstrong.csv")
Rook <- read.csv("rookiebymonth.csv")
OF <- read.csv("outfielderbymonth.csv")

## line chart

## grouping by month and combining them

Rook <- Rook %>%
  rename(month = api_game_date_month_text)
OF <- OF %>%
  rename(month = api_game_date_month_text)


rook_avg <- Rook %>%
  group_by(month) %>%
  summarise(batting_avg = mean(ba, na.rm = TRUE),
            slug_avg = mean(slg, na.rm = TRUE),
            obp_avg = mean(obp, na.rm = TRUE))

pca_avg <- Rook %>%
  filter(player_name == "Crow-Armstrong, Pete") %>%
  group_by(month) %>%
  summarise(batting_avg = mean(ba, na.rm = TRUE),
            slug_avg = mean(slg, na.rm = TRUE),
            obp_avg = mean(obp, na.rm = TRUE))

of_avg <- OF %>%
  group_by(month) %>%
  summarise(batting_avg = mean(ba, na.rm = TRUE),
            slug_avg = mean(slg, na.rm = TRUE),
            obp_avg = mean(obp, na.rm = TRUE))


## Combining them

combined_avg <- bind_rows(
  rook_avg %>% mutate(group = "Rookies"),
  of_avg %>% mutate(group = "Outfielders"),
  pca_avg %>% mutate(group = "Pete Crow-Armstrong")) %>%
  mutate(month = factor(month, levels = c("Mar/Apr", "May", "Jun", "Jul", "Aug", "Sep/Oct"))) %>%
  mutate(group = factor(group, levels = c("Pete Crow-Armstrong", "Rookies", "Outfielders")))
  


# Batting Average Visualization
ba_viz <- ggplot(combined_avg, aes(x = month, y = batting_avg, color = group, group = group)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Monthly Batting Averages (2024)",
    x = "Month",
    y = "Batting Average",
    color = "Group"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("blue", "green", "red"))

# Slugging Percentage Visualization
slug_viz <- ggplot(combined_avg, aes(x = month, y = slug_avg, color = group, group = group)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Monthly Slugging Averages (2024)",
    x = "Month",
    y = "Slugging",
    color = "Group"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("blue", "green", "red"))

# On-Base Percentage Visualization
obp_viz <- ggplot(combined_avg, aes(x = month, y = obp_avg, color = group, group = group)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Monthly OBP Averages (2024)",
    x = "Month",
    y = "On Base Percentage",
    color = "Group"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("blue", "green", "red"))

# Arrange the plots with a common legend
line_chart <- ggarrange(ba_viz, slug_viz, obp_viz,
          ncol = 1, nrow = 3,
          common.legend = TRUE,
          legend = "right")

annotate_figure(line_chart,
                bottom = text_grob("**Rookies: 130 career AB surpassed in 2024**Outfielders: min. 100 starts at Oufield position 2024**",
                                   face = "italic", size = 10, hjust = 0.5, color = "gray40"))


## Heatmaps

### Strike zone perameters

left <- -8.5/12
right <- 8.5/12
bottom <- 18.29/12
top <- 44.08/12

## 3x3

width <- (right - left)/3
height <- (top - bottom)/3



# Creates A Color Palette, Reversed (Blue/Less Frequent to Red/More Frequent) With 9 Different Shades, Broken Into 16
heat_colors_interpolated <- colorRampPalette(paletteer::paletteer_d("RColorBrewer::RdBu", n = 9, direction = -1))(10)

# Shows The Color Scale
heat_colors_interpolated %>% scales::show_col()

# Add in Parameters for Strike Zone / Plate
Left <- -8.5/12
Right <- 8.5/12
Bottom <- 18.29/12
Top <- 44.08/12

# This is to Make a 3x3 Strike Zone (Vertical and Horizontal Lines in Zone)
Width <- (Right - Left) / 3
Height <- (Top - Bottom) / 3


### Plot heatmap

heat_colors_interpolated <- colorRampPalette(paletteer::paletteer_d("RColorBrewer::RdBu", n = 9, direction = -1))(16)

# Shows The Color Scale
heat_colors_interpolated %>% scales::show_col()

## Heatmap hit

heatmap_hits <- PCA %>%
  filter(events %in% c("single", "double", "triple", "home_run"))

## Heatmap visual

hits_viz <- ggplot(heatmap_hits, mapping = aes(x=plate_x, y= plate_z)) +
  stat_density2d_filled()  +
  scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill", "color")) +
  # The Box (Bottom, Top, Left, Right)
  geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
  geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
  geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
  geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
  
  # Horizontal Lines (Bottom Inner, Top Inner)
  geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
  geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
  
  # Vertical Lines (Left Inner, Right Inner)
  geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
  geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
  
  # Plate (Bottom, Left Side, Left Diagonal, Right Diagonal, Right Side)  
  geom_segment(x = (Left), y = (0), xend = (Right), yend = (0)) +
  geom_segment(x = (Left), y = (0), xend = (Left), yend = (4.25/12)) +
  geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (8.5/12)) +
  geom_segment(x = (Right), y = (4.25/12), xend = (Right), yend = (0)) +
  geom_segment(x = (0), y = (8.5/12), xend = (Right), yend = (4.25/12)) +
  
  xlim(-3,3) + ylim(0, 5)  + ggtitle(paste("Hits")) +
  theme(
    legend.position = "none",
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", size = 1.5, fill = NA))

## HeatMap groundouts

heatmap_groundouts <- PCA %>%
  filter(description == "hit_into_play") %>%
  filter(events %in% c("field_out", "grounded_into_double_play", "force_out", "sac_fly")) %>%
  filter(str_starts(des, "Pete Crow-Armstrong grounds"))




groundout_viz <- ggplot(heatmap_groundouts, mapping = aes(x=plate_x, y= plate_z)) +
  stat_density2d_filled()  +
  scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill", "color")) +
  # The Box (Bottom, Top, Left, Right)
  geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
  geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
  geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
  geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
  
  # Horizontal Lines (Bottom Inner, Top Inner)
  geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
  geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
  
  # Vertical Lines (Left Inner, Right Inner)
  geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
  geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
  
  # Plate (Bottom, Left Side, Left Diagonal, Right Diagonal, Right Side)  
  geom_segment(x = (Left), y = (0), xend = (Right), yend = (0)) +
  geom_segment(x = (Left), y = (0), xend = (Left), yend = (4.25/12)) +
  geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (8.5/12)) +
  geom_segment(x = (Right), y = (4.25/12), xend = (Right), yend = (0)) +
  geom_segment(x = (0), y = (8.5/12), xend = (Right), yend = (4.25/12)) +
  
  xlim(-3,3) + ylim(0, 5)  + ggtitle(paste("Groundouts")) +
  theme(
    legend.position = "none",
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", size = 1.5, fill = NA))

heatmap_popouts <- PCA %>%
  filter(description == "hit_into_play") %>%
  filter(events %in% c("field_out", "grounded_into_double_play", "force_out", "sac_fly")) %>%
  filter(
    str_starts(des, "Pete Crow-Armstrong flies") |
    str_starts(des, "Pete Crow-Armstrong pops") |
    str_starts(des, "Pete Crow-Armstrong lines") |
    str_starts(des, "Pete Crow-Armstrong out on a sacrifice"))


popouts_viz <- ggplot(heatmap_popouts, mapping = aes(x=plate_x, y= plate_z)) +
  stat_density2d_filled()  +
  scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill", "color")) +
  # The Box (Bottom, Top, Left, Right)
  geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
  geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
  geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
  geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
  
  # Horizontal Lines (Bottom Inner, Top Inner)
  geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
  geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
  
  # Vertical Lines (Left Inner, Right Inner)
  geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
  geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
  
  # Plate (Bottom, Left Side, Left Diagonal, Right Diagonal, Right Side)  
  geom_segment(x = (Left), y = (0), xend = (Right), yend = (0)) +
  geom_segment(x = (Left), y = (0), xend = (Left), yend = (4.25/12)) +
  geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (8.5/12)) +
  geom_segment(x = (Right), y = (4.25/12), xend = (Right), yend = (0)) +
  geom_segment(x = (0), y = (8.5/12), xend = (Right), yend = (4.25/12)) +
  
  xlim(-3,3) + ylim(0, 5)  + ggtitle(paste("Popouts")) +
  theme(
    legend.position = "none",
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", size = 1.5, fill = NA))


heatmap_whiff <- PCA %>%
  filter(description %in% c("swinging_stike", "called_strike", "swinging_strike_blocked"))


whiff_viz <- ggplot(heatmap_whiff, mapping = aes(x=plate_x, y= plate_z)) +
  stat_density2d_filled()  +
  scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill", "color")) +
  # The Box (Bottom, Top, Left, Right)
  geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
  geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
  geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
  geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
  
  # Horizontal Lines (Bottom Inner, Top Inner)
  geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
  geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
  
  # Vertical Lines (Left Inner, Right Inner)
  geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
  geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
  
  # Plate (Bottom, Left Side, Left Diagonal, Right Diagonal, Right Side)  
  geom_segment(x = (Left), y = (0), xend = (Right), yend = (0)) +
  geom_segment(x = (Left), y = (0), xend = (Left), yend = (4.25/12)) +
  geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (8.5/12)) +
  geom_segment(x = (Right), y = (4.25/12), xend = (Right), yend = (0)) +
  geom_segment(x = (0), y = (8.5/12), xend = (Right), yend = (4.25/12)) +
  
  xlim(-3,3) + ylim(0, 5)  + ggtitle(paste("Whiff")) +
  theme(
    legend.position = "none",
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", size = 1.5, fill = NA))
         

ggarrange(hits_viz, groundout_viz, popouts_viz, whiff_viz, ncol = 4, nrow = 1)


## Spraychart

##Spray data

spray_data <- PCA %>%
  filter(!is.na(hc_x), !is.na(hc_y),
         (events %in% c("single", "double", "triple", "home_run"))) %>%
  mutate(events = factor(events, levels = c("single", "double", "triple", "home_run")))


spray_data %>%
  ggplot(aes(x = hc_x, y = -hc_y, color = events)) +
  geom_point(alpha = 0.5) +
  geom_curve(x = 33, xend = 223, y = -100, yend = -100, curvature = -.65) +
  geom_segment(x = 128, xend = 33, y = -208, yend = -100) +
  geom_segment(x = 128, xend = 223, y = -208, yend = -100) +
  geom_curve(x = 83, xend = 173, y = -155, yend = -156, curvature = -.65, linetype = "dotted") +
  coord_fixed() +
  scale_x_continuous(NULL, limits = c(25, 225)) +
  scale_y_continuous(NULL, limits = c(-225, -25)) +
  labs(title = "Spray Chart",
       color = "Events") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~p_throws)


