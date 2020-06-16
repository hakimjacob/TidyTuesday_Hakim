## Tidy Tuesday - Black Firsts

# Load libraries
library(tidyverse)
library(extrafont)
library(ggtext)

# Get the data
firsts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/firsts.csv')

# set working directory
setwd("C:/Users/jake/Documents/R/tt-blm")

female_firsts <- firsts %>%  
  mutate(decade = year - year %% 10) %>% # add decade values
  filter(gender=="Female African American Firsts") %>%
  filter(!(year == 1760)) %>% # Remove Jupiter Hammon (error)
  group_by(decade) %>%
  arrange(category) %>% # arrange categories for bar graph
  mutate(category = recode(category,
                           "Social & Jobs" = "Social\n& Jobs",
                           "Education & Science" = "Education\n& Science")) %>%
  mutate(index = row_number()) %>%
  ungroup()
  

# BAR PLOT
ggplot(female_firsts, aes(x=decade, y=1, group = index, fill=category)) +
  
  geom_bar(stat="identity",
           size=1.1,
           width=10,
           color="#FFF5EE") +
  
  # color and key stuff
  scale_fill_brewer(palette = "Paired",
                    guide = guide_legend(direction = "horizontal", nrow = 2)) +

  # x axis labels
  scale_x_continuous(name = "Decade",
                   breaks = seq(1770, 2010, by = 10),
                   labels = paste0(seq(1770, 2010, by = 10), "s"),
                   expand = c(0, 1)) +
  
 # title
   annotate(geom = "text",
           x=1800,y=20,
           hjust=0.14, vjust=0.5,
           label="Achievements of Black Women in America",
           size=13, lineheight=.8,
           family="Impact",
           color="#2F4F4F") +
  
  #add line and label for 13th amendment
  geom_segment(x=1860, y=0, xend=1860, yend=15, #1865 didn't line up right
             linetype="dotted") +
  
  annotate(geom = "text",
           x=1859,y=5.3,
           hjust=0, vjust=0,
           label="December 18, 1865: \nAdoption of the 13th Amendment",
           size=3.5, lineheight=.8,
           color="#708090",
           angle=90) +
  
  # add line and label for 19th amendment
  geom_segment(x=1916, y=0, xend=1916, yend=15, # 1920 didn't line up right
               linetype="dotted") +
  
  annotate(geom = "text",
           x=1915,y=5.3,
           hjust=0, vjust=0,
           label="August 26, 1920: \nAdoption of the 19th Amendment",
           size=3.5, lineheight=.8,
           color="#708090",
           angle=90) +
  
  labs(caption="Data: Wikipedia | Visualization: @JacobHakim2") +
  
  # theme stuff
  theme(plot.background = element_rect(fill="#FFF5EE", color = "#FFF5EE"),
        panel.background = element_rect(fill="#FFF5EE", color = "#FFF5EE"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(hjust=2, vjust=3,
                                   face="bold",
                                   size=8, angle=50),
        legend.key.size = unit(0.8, "cm"),
        legend.key.width = unit(1,"cm"),
        legend.position = c(0.3, 0.8),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.background = element_rect(fill="#FFF5EE", color = "#FFF5EE"),
        plot.caption = element_text(color="#A9A9A9",
                                    family="Verdana")) +

  # add line and label for achievements
  # Phillis Wheatley
  geom_curve(x = 1770, y = 9, 
             xend = 1769, yend = 1,
             curvature = 0.1,
             size=0.6) +

  annotate(geom="text",
           x = 1765, y = 10.5,
           hjust=0,
           label="Phillis Wheatley",
           size=4, lineheight=.8,
           fontface="bold",
           color="#708090") +
  annotate(geom="text",
           x = 1765, y = 9.85,
           hjust=-0.02,
           label="First African American Woman\nTo Publish a Book, 1773",
           size=2.5, lineheight=.8,
           color="#708090") +

# Dr. Rebecca Davis Lee Crumpler
geom_curve(x = 1870, y = 8.9, 
           xend = 1865.5, yend = 4.5,
           curvature = -0.1,
           size=0.6) +
  
  annotate(geom="text",
           x = 1865, y = 10.5,
           hjust=0,
           label="Dr. Rebecca Davis \nLee Crumpler",
           size=4, lineheight=.8,
           fontface="bold",
           color="#708090") +
  annotate(geom="text",
           x = 1865, y = 9.6,
           hjust=-0.02,
           label="First African American \nWoman to Earn An M.D., 1864",
           size=2.5, lineheight=.8,
           color="#708090") +


# Condoleezza Rice
geom_curve(x = 1955, y = 13, 
           xend = 1995, yend = 11.4,
           curvature = 0.1,
           size=0.6) +
  
  annotate(geom="text",
           x = 1925, y = 14.3,
           hjust=0,
           label="Condoleezza Rice",
           size=4, lineheight=.8,
           fontface="bold",
           color="#708090") +
  annotate(geom="text",
           x = 1925, y = 13.5,
           hjust=-0.02,
           label="First African American Woman \nto Become Secretary of State,\n 2005",
           size=2.5, lineheight=.8,
           color="#708090")

# save as jpg (9.57 x 6.92)
ggsave("female-african-american-achievements.jpg", width=9.57, height=6.92, dpi=600)  
