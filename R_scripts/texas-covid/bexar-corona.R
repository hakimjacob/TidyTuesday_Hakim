## Visualizing Bexar County Coronavirus Data

# load libraries
library(tidyverse)
library(stringr)
library(extrafont)
library(ggtext)
library(RcppRoll)
library(reshape2)

getwd()
setwd("C:/Users/jake/Documents/R/bexar-corona")

my_data <- read.table(file = "clipboard", 
                      sep = "\t", header=TRUE)
# It took a long time to get the dates right.
bexar <- my_data %>% filter(County.Name=="Bexar") %>%
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

bexar <- bexar %>%
  mutate(date=gsub("\\.", "-", bexar$date))

bexar <- bexar %>%
  mutate(date=as.Date(bexar$date, "%m-%d")) %>%
  mutate(avg=cummean(ifelse(is.na(bexar$new_cases), 0, bexar$new_cases))) %>%
  mutate(week_avg=roll_mean(bexar$new_cases, 5, fill=NA, align="right"))
# Trying some plots.

#new cases with average
ggplot(bexar, aes(x=date, y=new_cases)) +
  geom_col(width=0.7) +
  geom_line(aes(x=date, y=week_avg), color="red", size=2) +
  
  scale_x_date(name = "Date",
               date_breaks = "1 week",
               date_labels = "%m/%d") +
  
  # title
  annotate(geom = "text",
           x=as.Date("03/010", format="%m/%d"),y=300,
           hjust=0, vjust=1,
           label="Rate of new cases of COVID-19 at\nall time high in Bexar County",
           size=12, lineheight=.8,
           family="Times New Roman",
           fontface="bold",
           color="#2F4F4F") +
  
  annotate(geom = "text",
           x=as.Date("03/010", format="%m/%d"),y=250,
           hjust=0, vjust=1,
           label= "A 5-day average of 160 daily new cases on June 14th",
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
ggsave("bexar-corona1-5day.jpg", width=14.2, height=7.44, dpi=600) 


# Second plot: total cases
ggplot(bexar, aes(x=date, y=cases)) +
  geom_line(size=2, color="red") +
  
  scale_x_date(name = "Date",
               date_breaks = "1 week",
               date_labels = "%m/%d") +
  
  # title
  annotate(geom = "text",
           x=as.Date("03/010", format="%m/%d"),y=5000,
           hjust=0, vjust=.65,
           label="More than 4400 confirmed\nCOVID-19 cases in Bexar County",
           size=12, lineheight=.8,
           family="Times New Roman",
           fontface="bold",
           color="#2F4F4F") +
  
  annotate(geom = "text",
           x=as.Date("03/010", format="%m/%d"),y=4400,
           hjust=0, vjust=0.5,
           label= "212 per 100,000 confirmed infected",
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
ggsave("bexar-corona2.jpg", width=14.2, height=7.44, dpi=600)

### FATALATIES AND HOSPITALIZATIONS #################################################################################

fatal <- read.csv("Texas COVID-19 Fatality Count Data by County.csv")
hosp <- read.csv("Texas COVID-19 Hospitalizations by TSA.csv")

# Wrangling fatality data
bexar_fatal <- fatal %>% filter(County.Name=="Bexar") %>%
  gather("date", "deaths", -County.Name, -Population, na.rm = TRUE) %>%
  mutate(deaths=as.numeric(deaths)) %>%
  mutate(Population=as.numeric(Population)) %>%
  mutate(Population=2093502) %>%
  mutate(prev_total=lag(deaths)) %>%
  mutate(new_deaths=deaths-prev_total) %>%
  mutate(perc_total=deaths / Population * 100) %>%
  mutate(perc_new=new_deaths/Population * 100) %>%
  mutate(total_hundred=round(1000*perc_total)) %>%
  mutate(date=lapply(date, as.character)) %>%
  mutate(date=str_sub(date, 12, 16))

bexar_fatal <- bexar_fatal %>%
  mutate(date=gsub("\\.", "-", bexar_fatal$date))

bexar_fatal <- bexar_fatal %>%
  mutate(date=as.Date(bexar_fatal$date, "%m-%d")) %>%
  #mutate(avg=cummean(ifelse(is.na(bexar_fatal$new_cases), 0, bexar_fatal$new_cases))) %>%
  mutate(week_avg=roll_mean(bexar_fatal$new_deaths, 5, fill=NA, align="right"))

#Wrangling hospitalization data
bexar_hosp <- hosp %>% filter(TSA.Name=="San Antonio") %>%
  gather("date", "hosp", -TSA.Name, na.rm = TRUE) %>%
  mutate(hosp=as.numeric(hosp)) %>%
  #mutate(Population=as.numeric(Population)) %>%
  #mutate(Population=2093502) %>%
  mutate(prev_total=lag(hosp)) %>%
  mutate(new_hosp=hosp-prev_total) %>%
  #mutate(perc_total=hosp / Population * 100) %>%
  #mutate(perc_new=new_hosp/Population * 100) %>%
  #mutate(total_hundred=round(1000*perc_total)) %>%
  mutate(date=lapply(date, as.character)) %>%
  mutate(date=str_sub(date, 2, 5))

bexar_hosp <- bexar_hosp %>%
  mutate(date=gsub("\\.", "-", bexar_hosp$date))

bexar_hosp <- bexar_hosp %>%
  mutate(date=as.Date(bexar_hosp$date, "%m-%d")) %>%
  #mutate(avg=cummean(ifelse(is.na(bexar_fatal$new_cases), 0, bexar_fatal$new_cases))) %>%
  mutate(week_avg=roll_mean(bexar_hosp$new_hosp, 5, fill=NA, align="right"))

# Attempting to merge these two
####################################
bexar_both <- merge(bexar_fatal, bexar_hosp, by="date", all.x = TRUE, all.y = TRUE) %>%
  mutate(both_total=new_deaths+(ifelse(is.na(bexar_both$hosp), 0, bexar_both$hosp)))

# use new_deaths and hosp as levels of 1 variable for ggplot
bexar_graph <- melt(bexar_both[,c("date", "new_deaths", "hosp")], id.vars=1)

# assign colors
colors <- c("#8B0000", "#B0C4DE")

#ggplot for new deaths and 5-day avg fatalities in Bexar county
ggplot(bexar_graph, aes(x=date, y=value)) +
  geom_bar(aes(fill=variable), stat="identity", position="stack", width=0.8) +
  
  scale_x_date(name = "Date",
               breaks = seq(as.Date("04/09", format="%m/%d"), as.Date("06/17", format="%m/%d"), by= "3 days"),
               date_labels = "%m/%d",
               limit= c(as.Date("04/09", format="%m/%d"), as.Date("06/17", format="%m/%d"))) +
  
  # title
  annotate(geom = "text",
           x=as.Date("05/04", format="%m/%d"),y=250,
           hjust=0, vjust=1,
           label="Sharp rise in COVID-19 hospitalizations\nin Bexar County",
           size=10, lineheight=.8,
           family="Times New Roman",
           fontface="bold",
           color="#2F4F4F") +
  
  annotate(geom = "text",
           x=as.Date("05/04", format="%m/%d"),y=217,
           hjust=0, vjust=1,
           label= "Marked increase as State continues to reopen",
           size=7, lineheight=.8,
           family="Times New Roman",
           color="#2F4F4F") +
  
  #line for 5 day average
  #geom_segment(x=as.Date("03/10", format="%m/%d"),y=0,
   #            xend=as.Date("03/10", format="%m/%d"),yend=35,
    #           size=1.2, color="red") +
  
  # annotate(geom="text",
  #          x = as.Date("03/10", format="%m/%d"), y = 3,
  #          hjust=0.5,
  #          label="5-day average",
  #          size=3, lineheight=.8,
  #          fontface="bold",
  #          color="red") +
  
  #line and label for 5 and 10
  geom_hline(yintercept=75, linetype="longdash", alpha=0.7, color="#2F4F4F") +
  
  annotate(geom="text",
           x = as.Date("04/11", format="%m/%d"), y = 80,
           hjust=0,
           label="75",
           size=4, lineheight=.8,
           fontface="bold",
           color="#708090") +
  
  geom_hline(yintercept=150, linetype="longdash", alpha=0.7, color="#2F4F4F") +
  
  annotate(geom="text",
           x = as.Date("04/11", format="%m/%d"), y = 155,
           hjust=0,
           label="150",
           size=4, lineheight=.8,
           fontface="bold",
           color="#708090") +
  
  #line for reopen
  geom_vline(xintercept=as.Date("05/01", format="%m/%d"), linetype="dotted", alpha=0.7, color="#2F4F4F") +
  
  annotate(geom="text",
           x = as.Date("04/30", format="%m/%d"), y = 85,
           hjust=0,
           label="May 01:\nPhase One Reopening",
           angle=90,
           size=3, lineheight=.8,
           fontface="bold",
           color="#708090") +
  
  #line for phase 2 reopen
  geom_segment(x=as.Date("05/18", format="%m/%d"),y=0, xend=as.Date("5/18", format="%m/%d"), yend=205, linetype="dotted", alpha=0.7, color="#2F4F4F") +
  
  annotate(geom="text",
           x = as.Date("05/17", format="%m/%d"), y = 85,
           hjust=0,
           label="May 18:\nPhase Two Reopening",
           angle=90,
           size=3, lineheight=.8,
           fontface="bold",
           color="#708090") +
  
  #annotation with curved line to show fatalities
  geom_curve(x=as.Date("06/07 19:00", format="%m/%d", "%h:%m"), y=179, xend=as.Date("06/14", format="%m/%d"), yend=168, curvature =-0.15, alpha=0.7, color="#2F4F4F") +
  
  annotate(geom="text",
           x = as.Date("06/02", format="%m/%d"), y = 180,
           hjust=-0.16,
           label="4 fatalities",
           size=4, lineheight=.8,
           fontface="bold",
           color="#708090") +
  
  #colors
  scale_fill_manual("legend", labels=c("Fatalities", "Ongoing hospitalizations"), values = colors) +
  
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
        legend.position = c(0.13, 0.9),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.background = element_rect(fill="#FFF5EE", color = "#FFF5EE"),
        plot.caption = element_text(color="#A9A9A9",
                                    family="Verdana")) +
  
  #add caption (had to use annoying spaces at end to align, idk what happened to my margins)
  labs(caption="Data: Texas Department of State Health Services | Visualization: @JacobHakim2                 ")

ggsave("bexar-corona3.jpg", width=14.2, height=7.44, dpi=600)
