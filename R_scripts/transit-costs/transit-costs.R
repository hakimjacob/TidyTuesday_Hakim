## Tidy Tuesday - Transit Costs (Tokyo)

# Load libraries
library(tidyverse)
library(extrafont)
library(ggtext)
library(forcats)

# Get the data
transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')

# set working directory
setwd("C:/Users/jake/Documents/R/tidy-tuesday/TidyTuesday_Hakim/R_scripts/transit-costs/")

tokyo <- transit_cost %>% 
  filter(city == "Tokyo") %>%
  mutate(real_cost=as.numeric(real_cost)) %>%
  mutate(trillion=real_cost/1000) %>%
  arrange(-desc(start_year)) %>%
  mutate(line=as.factor(line))
  
  
# POLAR BAR PLOT
ggplot(tokyo, aes(x=fct_rev(fct_reorder(line, start_year)), y=length, fill=trillion)) +
  geom_col(width=0.3) +
  coord_flip() +
  
  # color and key stuff
  scale_fill_gradient(name="Trillions USD",
                      guide=guide_legend(direction="horizontal"),
                      low="#fb6a4a",
                      high="#a50f15",
                      aesthetics="fill") +

  
  labs(title="Railways of Tokyo", caption="Data: Transit Costs Project | Visualization: @JacobHakim2") +
  
  ylab("Length (km)") +
  
  # theme stuff
  theme(plot.background = element_rect(fill="white", color = "white"),
        panel.background = element_rect(fill="white", color = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size=9,
                                   family="Palatino Linotype"),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size=9),
        axis.text.x = element_text(size=8,
                                   family="Palatino Linotype"),
        legend.position = c(0.75, 1),
        legend.text = element_text(size = 8),
        legend.title = element_text(size=8),
        legend.background = element_rect(fill="white", color = "white"),
        plot.title = element_text(color="black",
                                  size=30,
                                  hjust=-1.6,
                                  family="Palatino Linotype"),
        plot.caption = element_text(color="#A9A9A9",
                                    family="Palatino Linotype")) 

  
# save as jpg (9.57 x 6.92)
ggsave("transit-costs-tokyo.jpg", dpi=600)  
