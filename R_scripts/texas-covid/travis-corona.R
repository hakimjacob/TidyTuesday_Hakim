# Covid Travis County

## Visualizing Travis County Coronavirus Data

# load libraries
library(tidyverse)
library(stringr)
library(extrafont)
library(ggtext)
library(RcppRoll)

getwd()
setwd(setwd("C:/Users/jake/Documents/R/bexar-corona"))

my_data <- read.table(file = "clipboard", 
                      sep = "\t", header=TRUE)
# It took a long time to get the dates right.
Travis <- my_data %>% filter(County.Name=="Travis") %>%
  gather("date", "cases", -County.Name, -Population, na.rm = TRUE) %>%
  filter(date!="03-04") %>%
  mutate(Population=as.numeric(Population)) %>%
  mutate(Population=2093502) %>%
  mutate(prev_total=lag(cases)) %>%
  mutate(new_cases=cases-prev_total) %>%
  mutate(perc_total=cases / Population * 100) %>%
  mutate(perc_new=new_cases/Population * 100) %>%
  mutate(total_hundred=round(1000*perc_total)) %>%
  mutate(date=lapply(date, as.character)) %>%
  mutate(date=str_sub(date, 2, 6))

Travis <- Travis %>%
  mutate(date=gsub("\\.", "-", Travis$date))

Travis <- Travis %>%
  mutate(date=as.Date(Travis$date, "%m-%d")) %>%
  mutate(avg=cummean(ifelse(is.na(Travis$new_cases), 0, Travis$new_cases))) %>%
  mutate(week_avg=roll_mean(Travis$new_cases, 5, fill=NA, align="right"))
# Trying some plots.

#new cases with average
ggplot(Travis, aes(x=date, y=new_cases)) +
  geom_col(width=0.7) +
  geom_line(aes(x=date, y=week_avg), color="red", size=2) +
  
  scale_x_date(name = "Date",
               date_breaks = "1 week",
               date_labels = "%m/%d") +
  
  # title
  annotate(geom = "text",
           x=as.Date("02/28", format="%m/%d"),y=300,
           hjust=0, vjust=1,
           label="Rate of new cases of COVID-19 sees\nupward spike in Travis County",
           size=12, lineheight=.8,
           family="Times New Roman",
           fontface="bold",
           color="#2F4F4F") +
  
  annotate(geom = "text",
           x=as.Date("02/28", format="%m/%d"),y=250,
           hjust=0, vjust=1,
           label= "A 5-day average of 111 daily new cases on June 16th",
           size=8, lineheight=.8,
           family="Times New Roman",
           color="#2F4F4F") +
  
  #line for 5 day average
  geom_segment(x=as.Date("03/10", format="%m/%d"),y=0,
               xend=as.Date("03/10", format="%m/%d"),yend=35,
               size=1.2, color="red") +
  
  annotate(geom="text",
           x = as.Date("03/10", format="%m/%d"), y = 40,
           hjust=0.5,
           label="5-day average",
           size=3, lineheight=.8,
           fontface="bold",
           color="red") +
  
  #line and label for 100 and 200
  geom_hline(yintercept=100, linetype="longdash", alpha=0.7, color="#2F4F4F") +
  
  annotate(geom="text",
           x = as.Date("03/04", format="%m/%d"), y = 105,
           hjust=0,
           label="100",
           size=4, lineheight=.8,
           fontface="bold",
           color="#708090") +
  
  geom_hline(yintercept=200, linetype="longdash", alpha=0.7, color="#2F4F4F") +
  
  annotate(geom="text",
           x = as.Date("03/04", format="%m/%d"), y = 205,
           hjust=0,
           label="200",
           size=4, lineheight=.8,
           fontface="bold",
           color="#708090") +
  
  #line for reopen
  geom_vline(xintercept=as.Date("05/01", format="%m/%d"), linetype="dotted", alpha=0.7, color="#2F4F4F") +
  
  annotate(geom="text",
           x = as.Date("04/29", format="%m/%d"), y = 110,
           hjust=0,
           label="May 01:\nPhase One Reopening",
           angle=90,
           size=3, lineheight=.8,
           fontface="bold",
           color="#708090") +
  
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
  
  #add caption (had to use annoying spaces at end to align, idk what happened to my margins)
  labs(caption="Data: Texas Department of State Health Services | Visualization: @JacobHakim2                 ")


# save as jpg (14.2 x 7.44)
ggsave("Travis-corona1-5day.jpg", width=14.2, height=7.44, dpi=600) 


# Second plot: total cases
ggplot(Travis, aes(x=date, y=cases)) +
  geom_line(size=2, color="red") +
  
  scale_x_date(name = "Date",
               date_breaks = "1 week",
               date_labels = "%m/%d") +
  
  # title
  annotate(geom = "text",
           x=as.Date("03/010", format="%m/%d"),y=5000,
           hjust=0, vjust=.65,
           label="More than 4500 confirmed\nCOVID-19 cases in Travis County",
           size=12, lineheight=.8,
           family="Times New Roman",
           fontface="bold",
           color="#2F4F4F") +
  
  annotate(geom = "text",
           x=as.Date("03/010", format="%m/%d"),y=4400,
           hjust=0, vjust=0.5,
           label= "223 per 100,000 confirmed infected",
           size=8, lineheight=.8,
           family="Times New Roman",
           color="#2F4F4F") +
  
  
  
  #line and label for 2000 and 4000
  geom_hline(yintercept=2000, linetype="longdash", alpha=0.7, color="#2F4F4F") +
  
  annotate(geom="text",
           x = as.Date("03/04", format="%m/%d"), y = 2100,
           hjust=0,
           label="2000",
           size=4, lineheight=.8,
           fontface="bold",
           color="#708090") +
  
  geom_hline(yintercept=4000, linetype="longdash", alpha=0.7, color="#2F4F4F") +
  
  annotate(geom="text",
           x = as.Date("03/04", format="%m/%d"), y = 4100,
           hjust=0,
           label="4000",
           size=4, lineheight=.8,
           fontface="bold",
           color="#708090") +
  
  #line for reopen
  geom_vline(xintercept=as.Date("05/01", format="%m/%d"), linetype="dotted", alpha=0.7, color="#2F4F4F") +
  
  annotate(geom="text",
           x = as.Date("04/29", format="%m/%d"), y = 2100,
           hjust=0,
           label="May 01:\nPhase One Reopening",
           angle=90,
           size=3, lineheight=.8,
           fontface="bold",
           color="#708090") +
  
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
  
  #add caption (had to use annoying spaces at end to align, idk what happened to my margins)
  labs(caption="Data: Texas Department of State Health Services | Visualization: @JacobHakim2                   ")

# save as jpg (14.2 x 7.44)
ggsave("Travis-corona2.jpg", width=14.2, height=7.44, dpi=600)



