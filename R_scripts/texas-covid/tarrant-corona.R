## Another plot for Tarrant County

# load libraries
library(tidyverse)
library(stringr)
library(extrafont)
library(ggtext)
library(RcppRoll)

getwd()
setwd("C:/Users/jake/Documents/R/bexar-corona/")

my_data <- read.table(file = "clipboard", 
                      sep = "\t", header=TRUE)

# It took a long time to get the dates right.
tarrant <- my_data %>% filter(County.Name=="Tarrant") %>%
  gather("date", "cases", -County.Name, -Population, na.rm = TRUE) %>%
  filter(date!="03-04") %>%
  mutate(Population=as.numeric(Population)) %>%
  mutate(Population=2143755) %>%
  mutate(prev_total=lag(cases)) %>%
  mutate(new_cases=cases-prev_total) %>%
  mutate(perc_total=cases / Population * 100) %>%
  mutate(perc_new=new_cases/Population * 100) %>%
  mutate(total_hundred=round(1000*perc_total)) %>%
  mutate(date=lapply(date, as.character)) %>%
  mutate(date=str_sub(date, 2, 6))

tarrant <- tarrant %>%
  mutate(date=gsub("\\.", "-", tarrant$date))

tarrant <- tarrant %>% #this part has to be its own step
  mutate(date=as.Date(tarrant$date, "%m-%d")) %>% 
  mutate(avg=cummean(ifelse(is.na(tarrant$new_cases), 0, tarrant$new_cases))) %>%
  mutate(week_avg=roll_mean(tarrant$new_cases, 5, fill=NA,align="right"))
# Trying some plots.

#new cases with average
ggplot(tarrant, aes(x=date, y=new_cases)) +
  geom_col(width=0.7) +
  geom_line(aes(x=date, y=week_avg), color="red", size=2) +
  
  scale_x_date(name = "Date",
               date_breaks = "1 week",
               date_labels = "%m/%d") +
  
  # title
  annotate(geom = "text",
           x=as.Date("03/010", format="%m/%d"),y=600,
           hjust=0, vjust=0.5,
           label="Second wave of new cases of COVID-19 in Tarrant County",
           size=12, lineheight=.8,
           family="Times New Roman",
           fontface="bold",
           color="#2F4F4F") +
  
  annotate(geom = "text",
           x=as.Date("03/010", format="%m/%d"),y=550,
           hjust=0, vjust=0.5,
           label= "A 5-day average of 180 daily new cases on June 14th",
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
  
  #line and label for 100, 200 and 500
  geom_hline(yintercept=100, linetype="longdash", alpha=0.7, color="#2F4F4F") +
  
  annotate(geom="text",
           x = as.Date("03/04", format="%m/%d"), y = 115,
           hjust=0,
           label="100",
           size=4, lineheight=.8,
           fontface="bold",
           color="#708090") +
  
  geom_hline(yintercept=200, linetype="longdash", alpha=0.7, color="#2F4F4F") +
  
  annotate(geom="text",
           x = as.Date("03/04", format="%m/%d"), y = 215,
           hjust=0,
           label="200",
           size=4, lineheight=.8,
           fontface="bold",
           color="#708090") +
  
  geom_hline(yintercept=500, linetype="longdash", alpha=0.7, color="#2F4F4F") +
  
  annotate(geom="text",
           x = as.Date("03/04", format="%m/%d"), y = 515,
           hjust=0,
           label="500",
           size=4, lineheight=.8,
           fontface="bold",
           color="#708090") +
  
  #line for reopen
  geom_vline(xintercept=as.Date("05/01", format="%m/%d"), linetype="dotted", alpha=0.7, color="#2F4F4F") +
  
  annotate(geom="text",
           x = as.Date("04/29", format="%m/%d"), y = 210,
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
ggsave("tarrant-corona1-5day.jpg", width=14.2, height=7.44, dpi=600) 


# Second plot: total cases
ggplot(tarrant, aes(x=date, y=cases)) +
  geom_line(size=2, color="red") +
  
  scale_x_date(name = "Date",
               date_breaks = "1 week",
               date_labels = "%m/%d") +
  
  # title
  annotate(geom = "text",
           x=as.Date("03/010", format="%m/%d"),y=9000,
           hjust=0, vjust=0.5,
           label="Over 7300 confirmed COVID-19\ncases in Tarrant County",
           size=12, lineheight=.8,
           family="Times New Roman",
           fontface="bold",
           color="#2F4F4F") +
  
  annotate(geom = "text",
           x=as.Date("03/010", format="%m/%d"),y=7900,
           hjust=0, vjust=0.5,
           label= "350 per 100,000 confirmed infected",
           size=8, lineheight=.8,
           family="Times New Roman",
           color="#2F4F4F") +
  
  
  
  #line and label for 2000 and 5000
  geom_hline(yintercept=2000, linetype="longdash", alpha=0.7, color="#2F4F4F") +
  
  annotate(geom="text",
           x = as.Date("03/04", format="%m/%d"), y = 2400,
           hjust=0,
           label="2000",
           size=4, lineheight=.8,
           fontface="bold",
           color="#708090") +
  
  geom_hline(yintercept=5000, linetype="longdash", alpha=0.7, color="#2F4F4F") +
  
  annotate(geom="text",
           x = as.Date("03/04", format="%m/%d"), y = 5400,
           hjust=0,
           label="5000",
           size=4, lineheight=.8,
           fontface="bold",
           color="#708090") +
  
  geom_hline(yintercept=10000, linetype="longdash", alpha=0.7, color="#2F4F4F") +
  
  annotate(geom="text",
           x = as.Date("03/04", format="%m/%d"), y = 10400,
           hjust=0,
           label="10000",
           size=4, lineheight=.8,
           fontface="bold",
           color="#708090") +
  
  #line for reopen
  geom_vline(xintercept=as.Date("05/01", format="%m/%d"), linetype="dotted", alpha=0.7, color="#2F4F4F") +
  
  annotate(geom="text",
           x = as.Date("04/29", format="%m/%d"), y = 5100,
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
ggsave("tarrant-corona2.jpg", width = 14.2, height = 7.44, dpi=600)



