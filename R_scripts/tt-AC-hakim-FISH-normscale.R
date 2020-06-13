## late tidy tuesday: Animal Crossing ##

# Load packages
library(tidyverse)
library(extrafont)
library(png)
library(grid)
library(cowplot)
library(magick)
library(ggimage)
library(ggtext)

getwd()
setwd("C:/Users/jake/Documents/R/tt-ac")

# Get the Data
#critic <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/critic.tsv')
#user_reviews <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/user_reviews.tsv')
items <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/items.csv')
#villagers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/villagers.csv')

## This is a copy of the plot I already made, but this time using a normal scale instead of a log scale.

#### PLOTTING FISH ###################################################################################################
######################################################################################################################

#had to use distinct() to remove duplicates (that can be caught in different locations)
fish <- items %>% filter(category=="Fish") %>% arrange(desc(sell_value)) %>% distinct(name, .keep_all = TRUE) %>% filter(!(name == "Cruician Carp"))
#transform to log value
#fish$log <- log(fish$sell_value)

# calculating label angles
fish$num <- c(1:nrow(fish))
fish_bars <- nrow(fish)
fish_angle <-  90 - 360 * (fish$num - .5)  /fish_bars

## fish label alignment
fish$hjust <- ifelse(fish_angle < -90, 1, 0)
fish$angle <- ifelse(fish_angle < -90, fish_angle+180, fish_angle)


## ordering for aesthetics
# find order of bugs to use for scale_x, turn into factor
order <- fish$id
order <- c(order)
order <- factor(order, levels=order)

## import PNGs
coelacanth <- readPNG("coelacanth.png")
napoleonfish <- readPNG("napoleonfish.png")
ray <- readPNG("ray.png")
sea_butterfly <- readPNG("seabutterfly.png")
sea_bass <- readPNG("sea-bass.png")
snapping_turtle <- readPNG("snapping-turtle.png")
bellspng <- readPNG("bells.png")
logopng <- readPNG("logo.png")

## FISH PLOT 
fish_plot_norm <- ggplot(fish, aes(y=sell_value, x=id)) + 
  
  #add bars
  geom_bar(position="stack", stat="identity", fill="#29505a") +
  scale_x_discrete(limits=order)+
  
  # add ylim to keep empty circle in middle
 ylim(-18000,18000) +
  
  # remove axes and turn into a polar plot
  theme_void() +
  coord_polar(direction = 1,
             clip = "off") +
  
  
  # add fish labels
  geom_text(data=fish, aes(x=id, y=fish$sell_value+500, label=name, hjust=hjust), 
            color="#000000", family="Calibri", fontface="italic",alpha=0.7, size=3, 
            angle= fish$angle, inherit.aes = FALSE ) +
  
  # change background fill color
  theme(panel.background = element_rect(fill="#c4d9df", color = "white")) +
  
  # add text in center
  annotate(geom = "text",
           x=0,y=-17500,
           hjust=.5, vjust=1.5,
           label="fish",
           size=14, lineheight=.8,
           family="Courier New",
           fontface=2,
           color="#62919c")+
  
  annotate(geom = "text",
           x=0,y=-18000,
           hjust=.5, vjust=4.8,
           label="sorted by price",
           size=6, lineheight=.8,
           family="Calibri",
           color="#04172d")

# color palette from https://www.color-hex.com/color-palette/84296 used for these color values.

ggdraw(fish_plot_norm) + 
  
  #add images
  draw_image(napoleonfish, 
             x = 1, y = 0.96, 
             hjust = 1, vjust = 1, 
             width = 0.2, height = 0.11) +
  draw_image(coelacanth, 
             x = 0.405, y = 1, 
             hjust = 1, vjust = 1, 
             width = 0.2, height = 0.11) +
  draw_image(snapping_turtle, 
             x = 0.99, y = 0.376, 
             hjust = 0.82, vjust = 1, 
             width = 0.2, height = 0.11) +
  draw_image(ray, 
             x = 0.99, y = 0.16, 
             hjust = 1, vjust = 1,
             width = 0.2, height = 0.11) +
  draw_image(sea_bass, 
             x = 0.21, y = 0.9, 
             hjust = 1, vjust = 1, 
             width = 0.2, height = 0.11) +
  draw_image(sea_butterfly, x = 0.22, y = 0.17, 
             hjust = 1, vjust = 1, 
             width = 0.2, height = 0.11) +
  draw_image(logopng, x = 0.624, y = 0.7,
             hjust = 1, vjust = 1,
             width = .25, height = .25) +
  
  #add leader lines
  #napoleonfish
  geom_curve(
    aes(x=.925, y=.86,xend=.837,yend=.748),
    curvature = -.15,
    color = "black",
    inherit.aes = FALSE,
  ) +
  #coelacanth
  geom_curve(
    aes(x=.362, y=.95,xend=.567,yend=.953),
    curvature = -.3,
    color = "black",
    inherit.aes = FALSE,
  )+
  #snapping turtle
  geom_curve(
    aes(x=.916, y=.345,xend=.876,yend=.445),
    curvature = .2,
    color = "black",
    inherit.aes = FALSE,
  )+
  #ray
  geom_curve(
    aes(x=.84, y=.12, xend=.677,yend=.294),
    curvature = -.14,
    color = "black",
    inherit.aes = FALSE,
  )+
  #sea bass
  geom_curve(
    aes(x=.12, y=.802,xend=.252,yend=.616),
    curvature = .19,
    color = "black",
    inherit.aes = FALSE,
  )+
  #sea butterfly
  geom_curve(
    aes(x=.15, y=.11,xend=.323,yend=.238),
    curvature = .1,
    color = "black",
    inherit.aes = FALSE,
  ) +
  
  #add money values
  #napoleonfish
  annotate(geom = "text",
           x=.94,y=.98,
           hjust=1, vjust=1,
           label="10000",
           size=6, lineheight=.8,
           family="Calibri",
           color="#2F4F4F")+
  draw_image(bellspng, x = 0.935, y = 0.984,
             hjust = 1, vjust = 1,
             width = 0.2, height = 0.03) +
  
  #coelacanth
  annotate(geom = "text",
           x=.36,y=.9,
           hjust=1, vjust=1,
           label="15000",
           size=6, lineheight=.8,
           family="Calibri",
           color="#2F4F4F") +
  draw_image(bellspng, x = 0.355, y = 0.902,
             hjust = 1, vjust = 1,
             width = 0.2, height = 0.03) +
  
  #snapping turtle
  annotate(geom = "text",
           x=.96,y=.297,
           hjust=1, vjust=1,
           label="5000",
           size=6, lineheight=.8,
           family="Calibri",
           color="#2F4F4F") +
  draw_image(bellspng, x = 0.972, y = 0.302,
             hjust = 1, vjust = 1,
             width = 0.2, height = 0.03) +
  
  #ray
  annotate(geom = "text",
           x=.926,y=.063,
           hjust=1, vjust=1,
           label="3000",
           size=6, lineheight=.8,
           family="Calibri",
           color="#2F4F4F") +
  draw_image(bellspng, x = 0.936, y = 0.066,
             hjust = 1, vjust = 1,
             width = 0.2, height = 0.03) +
  
  #sea bass
  annotate(geom = "text",
           x=.14,y=.92,
           hjust=1, vjust=1,
           label="400",
           size=6, lineheight=.8,
           family="Calibri",
           color="#2F4F4F") +
  draw_image(bellspng, x = 0.168, y = 0.923,
             hjust = 1, vjust = 1,
             width = 0.2, height = 0.03) +
  
  #sea butterfly
  annotate(geom = "text",
           x=.152,y=.065,
           hjust=1, vjust=1,
           label="1000",
           size=6, lineheight=.8,
           family="Calibri",
           color="#2F4F4F") +
  draw_image(bellspng, x = 0.166, y = 0.071,
             hjust = 1, vjust = 1,
             width = 0.2, height = 0.03) 

#save as PNG
ggsave("Tidy-AC-fish-normscale.jpg", dpi=600)
