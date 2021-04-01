rm(list=ls())

#remotes::install_github("hrbrmstr/waffle")

library(waffle)
library(tidyverse)
library(lubridate)
library(extrafont)
library(ragg)

data <- read.csv("mountainlog.csv") %>% 
  mutate(date=as.Date(climbed, format="%d/%m/%Y"),
         year=year(date),
         group=case_when(
           feet>=3000 ~ "Over 3,000ft",
           feet>=2000 ~ "Over 2,000ft",
           TRUE ~ "Below 2,000ft")) %>% 
  arrange(date) %>% 
  group_by(year, group) %>% 
  tally()

agg_tiff("1Mountains.tiff", units="in", width=12, height=3.97, res=500)
ggplot(data, aes(fill = group, values = n)) +
  geom_waffle(colour="LightBlue1", size = .25, n_rows=5, flip = TRUE) +
  facet_wrap(~as.factor(year), nrow=1, strip.position = "bottom")+
  scale_x_discrete() + 
  scale_y_continuous(labels = function(x) x * 5,
                     expand = c(0,0)) +
  scale_fill_manual(values=c("forestgreen", "tan4", "white"), name="")+
  coord_equal() +
  theme_minimal() +
  theme(text=element_text(family="Merriweather"),
        panel.grid = element_blank(), axis.ticks.y = element_line(),
        plot.title=element_text(face="bold", size=rel(2)),
        plot.background = element_rect(fill="LightBlue1", colour="LightBlue1"),
        panel.background=element_rect(fill="LightBlue1", colour="LightBlue1"),
        legend.position="top")+
  labs(title = "A lifetime of ups and downs",
       subtitle = "All of the hills or mountains in the UK that I've ever climbed, sorted by height")
dev.off()

