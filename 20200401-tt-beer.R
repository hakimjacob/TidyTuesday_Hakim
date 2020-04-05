# beer from TidyTuesday 3/31/20
# Starting work on 4/1/20

# 0. Load libaries
library(tidyverse)
#devtools::install_github("wmurphyrd/fiftystater") #installing package I didn't use
library(fiftystater) # states package I didn't use
#install.packages("usmap") #installing map package
library(usmap)
#install.packages("gganimate")
library(gganimate)
# devtools::install_github("jakelawlor/PNWColors")
library(PNWColors)
#install.packages("extrafont")
library(extrafont) #getting fonts
font_import()
loadfonts(device = "win")
#install.packages("gifski")
library(gifski)#loading gif renderer
#install.packages("png")
library(png)

#set working directory
getwd()
setwd("C:/Users/jake/Documents/R/tt-beer")

# 1. Read in data (I only ended up using one of them this time. Bummer!)
#brewing_materials <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv')
#beer_taxed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_taxed.csv')
#brewer_size <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewer_size.csv')
beer_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')


# 2 cleaning things up a bit
names(state.abb) <- state.name # connect state abb and state name. will use later.
names(state.name) <- state.abb # connect state abb and state name. will use later.

# reorganize dataframe: making "type" into separate columns.
# It took me forever to figure out how to change the spaces to underscore, and convert to lowercase.
beer_states$type <- gsub("\\s", "_", beer_states$type) %>% tolower()

# Now I can safely convert them to column heads.
sbeer <-  beer_states %>% spread(type, barrels)%>%
  mutate(total = on_premises + bottles_and_cans + kegs_and_barrels,
                                                           stotal = total / 1000000)#legend will be easier to read
# test map for 1st year
df2008 <- sbeer %>% filter(year=="2008")
plot_usmap(data = df2008, values = "stotal") +
  scale_fill_gradientn(name = "Canned beer production (millions of barrels)", colors = pnw_palette("Bay", 100))

#trying with a different color palette from Jake Lawlor's lovely PNWColors
pal <- rev(pnw_palette("Mushroom", 100))
plot_usmap(data = df2008, values = "stotal") +
  scale_fill_gradientn(name = "Canned beer production (millions of barrels)", colors = pal)

#Fixing the order of colors, more beer gets darker beer.
pal <- rev(pnw_palette("Mushroom", 100))#light to dark

#Let's do some more dummy maps to get it looking pretty.
plot_usmap(data = df2008, values = "stotal", color = "black") +
  scale_fill_gradientn(name = "Beer production (millions of barrels)", colors = pal, breaks=c(10, 20))+
  theme(plot.background   =element_rect(fill = "#f5f3f3"),
        panel.background = element_blank(),
        plot.title = element_text(size = rel(2.3), 
                                  family = "Bahnschrift",
                                  hjust = 0.5,
                                  color="black"),
        plot.subtitle = element_text(size = rel(1.5), 
                                     family = "Bahnschrift", 
                                     color = "black", 
                                     lineheight = 1.4,
                                     hjust = 0.5),
        legend.position = "bottom",
        legend.justification = "center",
        legend.background = element_rect(fill = "#f5f3f3"),
        legend.text = element_text(size = rel(1),
                                   family = "Bahnschrift",
                                   color="black"),
        legend.title = element_text(size = rel(1.2),
                                    family = "Bahnschrift",
                                    color="black"))+
  labs(title = "Total beer production by state", subtitle = "Year:")
  
#Renderer code
gifski_renderer(file = NULL, loop = TRUE, width = NULL, height = NULL)

file_renderer(dir = ".", prefix = "gganim_plot", overwrite = FALSE)

#The real thing, the final product. 
beermap <- plot_usmap(data = sbeer, values = "stotal") +
  scale_fill_gradientn(name = "Million barrels", colors = pal) +
  theme(plot.background   =element_rect(fill = "#f5f3f3"),
        panel.background = element_blank(),
        plot.title = element_text(size = rel(2.3), 
                                  family = "Bahnschrift",
                                  hjust = 0.5,
                                  color="black"),
        plot.subtitle = element_text(size = rel(1.5), 
                                     family = "Bahnschrift", 
                                     color = "black", 
                                     lineheight = 1.4,
                                     hjust = 0.5),
        legend.position = "bottom",
        legend.justification = "center",
        legend.background = element_rect(fill = "#f5f3f3"),
        legend.text = element_text(size = rel(1),
                                   family = "Bahnschrift",
                                   color="black"),
        legend.title = element_text(size = rel(1.2),
                                    family = "Bahnschrift",
                                    color="black"))+
  labs(title = "Total beer production by state", subtitle = "Year: {current_frame}")+#current_frame pulls the value and uses it as a label, if you're doing transition_manual.
  transition_manual(year)

#Seeing how it looks.
beerani <- animate(beermap)

anim_save("beermap_tt.gif", beerani)
