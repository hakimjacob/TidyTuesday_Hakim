## late tidy tuesday: Animal Crossing (BUGS)##

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

## I originally wanted to try making plots for all item types
## Eventually settled on creating a polar plot to compare bugs by sell value
## originally made with log scale, but looked bad and misleading

#### PLOTTING BUGS ###################################################################################################
######################################################################################################################

bugs <- items %>% filter(category=="Bugs") %>% arrange(desc(sell_value))

#transform to log value (didn't use this for this version -- it looks way better as a normal scale, much more informative visually)
#bugs$log <- log(bugs$sell_value)

# calculating angle for the labels
bugs$num <- c(1:nrow(bugs))
bug_bars <- nrow(bugs)
bug_angle <-  90 - 360 * (bugs$num - .5)  /bug_bars

# bug label calculations
bugs$hjust <- ifelse(bug_angle < -90, 1, 0)
bugs$angle <- ifelse(bug_angle < -90, bug_angle+180, bug_angle)


## ordering for aesthetics
# find order of bugs to use for scale_x, turn into factor
order <- bugs$id
order <- c(order)
order <- factor(order, levels=order)

# import PNGs
giraffepng <- readPNG("giraffe-stag.png")
grasshopperpng <- readPNG("grasshopper.png")
orchidmantispng <- readPNG("orchid-mantis.png")
walkerpng <- readPNG("walker-cicada.png")
tarantulapng <- readPNG("tarantula.png")
paperkite <- readPNG("paper-kite-butterfly.png")
bellspng <- readPNG("bells.png")
logopng <- readPNG("logo.png")

## BUG PLOT 
bugs_plot_norm <- ggplot(bugs, aes(y=sell_value, x=id)) + 
  
  #add bars
  geom_bar(position="stack", stat="identity", fill="#3B571E") +
  scale_x_discrete(limits=order)+
  
  # add ylim to keep empty circle in middle (had to change this around to make the linear scale work)
  ylim(-18000,15000) +
  
  # remove axes and turn into polar plot
  theme_void() +
  coord_polar(direction = 1,
              clip = "off") +
  
  
  # add bug labels
  geom_text(data=bugs, aes(x=id, y=sell_value+500, label=name, hjust=hjust), 
            color="#000000", family="Calibri", fontface="italic",alpha=0.7, size=3, 
            angle= bugs$angle, inherit.aes = FALSE ) +
  
  # change background fill color
  theme(panel.background = element_rect(fill="#BED2CA", color = "white")) +
  
  # add text in center
  annotate(geom = "text",
           x=0,y=-17500,
           hjust=.5, vjust=1.5,
           label="bugs",
           size=14, lineheight=.8,
           family="Courier New",
           fontface=2,
           color="#9eb6a0")+
  
  annotate(geom = "text",
           x=0,y=-18000,
           hjust=.5, vjust=4.8,
           label="sorted by price",
           size=6, lineheight=.8,
           family="Calibri",
           color="#313124")




# inserting images (had a lot of issues here because of conflict between image and polar plot)
# annotation raster did not work, adding cowplot instead
# honestly, this would take far less time if I just made the bar chart/text and added images and lines
# later in an app like photoshop (or pixlr for a free browser-based editor).

ggdraw(bugs_plot_norm) + 
  
  #add images to plot
  draw_image(tarantulapng, 
             x = 0.99, y = 0.95, 
             hjust = 1, vjust = 1, 
             width = 0.2, height = 0.11) +
  draw_image(giraffepng, 
             x = 0.45, y = 1, 
             hjust = 1, vjust = 1, 
             width = 0.2, height = 0.11) +
  draw_image(orchidmantispng, 
             x = 0.95, y = 0.2, 
             hjust = 1, vjust = 1,
             width = 0.2, height = 0.11) +
  draw_image(grasshopperpng, 
             x = 0.2, y = 0.93, 
             hjust = 1, vjust = 1, 
             width = 0.2, height = 0.11) +
  draw_image(paperkite, 
             x = 0.61, y = 0.15, 
             hjust = 1, vjust = 1, 
             width = 0.2, height = 0.11) +
  draw_image(walkerpng, x = 0.26, y = 0.17, 
             hjust = 1, vjust = 1, 
             width = 0.2, height = 0.11) +
  draw_image(logopng, x = 0.624, y = 0.7,
             hjust = 1, vjust = 1,
             width = .25, height = .25) +
  
  #add leader lines to labels
  #tarantula
  geom_curve(
    aes(x=.89, y=.85,xend=.80,yend=.758),
    curvature = -.15,
    color = "black",
    inherit.aes = FALSE,
  ) +
  #giraffe stag
  geom_curve(
    aes(x=.395, y=.95,xend=.535,yend=.958),
    curvature = -.4,
    color = "black",
    inherit.aes = FALSE,
  )+
  #orchid mantis
  geom_curve(
    aes(x=.87, y=.185, xend=.822,yend=.339),
    curvature = .15,
    color = "black",
    inherit.aes = FALSE,
  )+
  #grasshopper
  geom_curve(
    aes(x=.12, y=.825,xend=.249,yend=.698),
    curvature = .15,
    color = "black",
    inherit.aes = FALSE,
  )+
  #paper kite butterfly
  geom_curve(
    aes(x=.555, y=.11,xend=.67,yend=.165),
    curvature = .2,
    color = "black",
    inherit.aes = FALSE,
  )+
  #walker cicada
  geom_curve(
    aes(x=.18, y=.155,xend=.272,yend=.252),
    curvature = .15,
    color = "black",
    inherit.aes = FALSE,
  ) +
  
  #add money values and bells icon
  #tarantula
  annotate(geom = "text",
           x=.929,y=.97,
           hjust=1, vjust=1,
           label="8000",
           size=6, lineheight=.8,
           family="Calibri",
           color="#2F4F4F")+
  draw_image(bellspng, x = 0.94, y = 0.974,
             hjust = 1, vjust = 1,
             width = 0.2, height = 0.03) +
  
  #giraffe stag
  annotate(geom = "text",
           x=.41,y=.9,
           hjust=1, vjust=1,
           label="12000",
           size=6, lineheight=.8,
           family="Calibri",
           color="#2F4F4F") +
  draw_image(bellspng, x = 0.405, y = 0.904,
             hjust = 1, vjust = 1,
             width = 0.2, height = 0.03) +
  
  #orchid mantis
  annotate(geom = "text",
           x=.895,y=.1,
           hjust=1, vjust=1,
           label="2400",
           size=6, lineheight=.8,
           family="Calibri",
           color="#2F4F4F") +
  draw_image(bellspng, x = 0.908, y = 0.104,
             hjust = 1, vjust = 1,
             width = 0.2, height = 0.03) +
  
  #grasshopper
  annotate(geom = "text",
           x=.13,y=.93,
           hjust=1, vjust=1,
           label="160",
           size=6, lineheight=.8,
           family="Calibri",
           color="#2F4F4F") +
  draw_image(bellspng, x = 0.163, y = 0.934,
             hjust = 1, vjust = 1,
             width = 0.2, height = 0.03) +
  
  #paper kite butterfly
  annotate(geom = "text",
           x=.557,y=.055,
           hjust=1, vjust=1,
           label="1000",
           size=6, lineheight=.8,
           family="Calibri",
           color="#2F4F4F") +
  draw_image(bellspng, x = 0.572, y = 0.059,
             hjust = 1, vjust = 1,
             width = 0.2, height = 0.03) +
  
  #walker cicada
  annotate(geom = "text",
           x=.2,y=.07,
           hjust=1, vjust=1,
           label="400",
           size=6, lineheight=.8,
           family="Calibri",
           color="#2F4F4F") +
  draw_image(bellspng, x = 0.229, y = 0.074,
             hjust = 1, vjust = 1,
             width = 0.2, height = 0.03) 
  
#save image
ggsave('TT-ACbugs-normscale.jpg', dpi = 600)
