rm(list=ls())

library(tidyverse)
library(extrafont)
library(ggrepel)
library(ragg)

data <- data.frame(country=c("France", "Italy", "Portugal", "Spain", "Austria", "Belgium", "Denmark",
                             "Finland", "Germany", "Greece", "Ireland", "Netherlands", "Sweden", 
                             "Switzerland", "UK", "Bulgaria", "Croatia", "Georgia",
                             "Hungary", "Moldova", "Romania", "Russia", "Australia", "New Zealand",
                             "Canada", "USA", "Argentina", "Chile", "Uruguay", "South Africa", 
                             "Norway"),
                   cons=c(40.1, 37, 50.2, 15, 27, 22.6, 27.2, 12.5, 23.7, 21.9, 22.1, 20.4, 19.9, 32.4, 
                          19.1, 7.3, 44.5, 23, 24.8, 33.8, 22.7, 6.5, 22.7, 23.2, 13.2, 10.1, 20.8,
                          11.2, 16.6, 7.4, 18.2),
                   prod=c(62.4, 55.7, 63.6, 73.4, 27.9, 1.3, 0, 0, 9.8, 19.1, 0, 0, 0, 11.6, 0.1, 12.9,
                          10.9, 48.4, 24.8, 53.7, 19.6, 3.2, 49.6, 62.7, 1.5, 7.4, 29, 62.8, 20.9,
                          16.6, 0),
                   pop=c(67450, 60550, 10226, 46736, 8955, 12098, 5771, 5532, 83517, 10473, 4882, 17097, 
                         10036, 8591, 67530, 7000, 4130, 3720, 9684, 3540, 19364, 145872, 25203, 4783, 
                         37411, 329065, 44780, 18952, 3461, 58558, 5378)) %>% 
  #Convert litres to bottles
  mutate(cons=cons/0.7, prod=prod/0.7)

agg_tiff("13Correlation.tiff", units="in", width=7, height=7, res=500)
ggplot(data, aes(x=prod, y=cons, label=country))+
  geom_point(aes(size=pop), colour="#7030a0", alpha=0.6, show.legend=FALSE)+
  geom_abline(slope=1, intercept=0, colour="Grey70")+
  scale_x_continuous(name="Wine production\n(bottles per person per year)", limits=c(0,110))+
  scale_y_continuous(name="Wine consumption\n(bottles per person per year)", limits=c(0,110))+
  geom_text_repel(size=2.5, colour="Grey20")+
  coord_equal()+
  theme_classic()+
  theme(text=element_text(family="Roboto"), 
        plot.title=element_text(size=rel(1.8), colour="#7030a0", family="Merriweather"),
        plot.subtitle=element_text(colour="#7030a0", family="Merriweather", size=rel(0.7)),
        plot.caption=element_text(colour="Grey50"))+
  annotate("text", x=60, y=63, angle=45, colour="Grey50", label="Production=Consumption",
           family="Roboto")+
  annotate("text", x=28, y=80, colour="Grey50", label="Drinks more than\nthey produce",
           family="Roboto")+
  annotate("text", x=65, y=15, colour="Grey50", label="Produces more than\nthey drink",
           family="Roboto")+
  labs(title="Getting high on your own (wine) supply",
       subtitle="Annual wine production and wine consumption per capita in major wine consuming and producing nations.\nCountries are sized according to their population",
       caption="Data from Anderson & Pinilla's Annual Database of Global Wine Markets\nPlot by @VictimOfMaths")
dev.off()

