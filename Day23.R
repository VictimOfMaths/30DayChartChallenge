rm(list=ls())

library(tidyverse)
library(viridis)
library(extrafont)
library(ragg)

#Read in data which originally came from the amazing CDC Wonder database
#https://wonder.cdc.gov/ucd-icd10.html
alc <- read.delim("CDCAlcoholxState.txt") %>% 
  mutate(cause="Alcohol") %>% 
  select(Year, State, Age.Adjusted.Rate, cause) %>% 
  mutate(Age.Adjusted.Rate=as.numeric(Age.Adjusted.Rate))
#ICD-10 codes F10, K70, K73, K74, X45, Y15

drg <- read.delim("CDCDrugsxState.txt") %>% 
  mutate(cause="Drugs") %>% 
  select(Year, State, Age.Adjusted.Rate, cause) %>% 
  mutate(Age.Adjusted.Rate=as.numeric(Age.Adjusted.Rate))
#ICD-10 codes F11-16, F18, F19, X40-44, Y10-14

scd <- read.delim("CDCSuicidexState.txt") %>% 
  mutate(cause="Suicide") %>% 
  select(Year, State, Age.Adjusted.Rate, cause) %>% 
  mutate(Age.Adjusted.Rate=as.numeric(Age.Adjusted.Rate))
#ICD-10 codes U03, X60-84, Y87

data <- bind_rows(alc, drg, scd) %>% 
  filter(State!="")

agg_tiff("23Tiles.tiff", units="in", width=9, height=8, res=800)
ggplot(data, aes(x=Year, y=State, fill=Age.Adjusted.Rate))+
  geom_tile()+
  scale_y_discrete(limits=rev, name="")+
  scale_fill_viridis_c(option="rocket", direction=-1, na.value="white", name="Annual deaths per 100,000")+
  facet_wrap(~cause)+
  coord_equal()+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        text=element_text(family="Open Sans"), axis.line.y=element_blank(), 
        plot.title.position = "plot", plot.caption.position = "plot",
        legend.position="top", plot.title=element_text(face="bold", size=rel(1.5)))+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                                barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="Drug deaths in America have overtaken deaths from alcohol and suicide",
       subtitle="Age-standardised rates of 'deaths of despair' in US states between 1999 and 2019\nWhite squares represent years where deaths were too low to reliably calculate an age-standardised rates",
       caption="Data from CDC WONDER database | Plot by @VictimOfMaths")
dev.off()
