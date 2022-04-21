rm(list=ls())

library(tidyverse)
library(extrafont)
library(ragg)
library(curl)
library(readxl)
library(paletteer)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"),
          plot.subtitle=element_text(colour="Grey40", hjust=0, vjust=1),
          plot.caption=element_text(colour="Grey40", hjust=1, vjust=1, size=rel(0.8)),
          axis.text=element_text(colour="Grey40"),
          axis.title=element_text(colour="Grey20"),
          legend.text=element_text(colour="Grey40"),
          legend.title=element_text(colour="Grey20"))
}

#Download ONS alcohol-specific deaths data for the UK
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fhealthandsocialcare%2fcausesofdeath%2fdatasets%2falcoholspecificdeathsintheukmaindataset%2fcurrent/alcoholspecificdeaths2020.xlsx"
temp <- tempfile()
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

UKdata <- read_excel(temp, sheet="Table 1 data") %>% 
  filter(Sex=="Persons") %>% 
  select(c(2,4,5,6)) %>% 
  set_names(c("Area", "Year", "Deaths", "ASMR")) %>% 
  #Fix weird transcription error in the data
  mutate(Area=if_else(Area=="Yorkshire and the Humber", "Yorkshire and The Humber", Area))

#plot UK data by region
ggplot(UKdata, aes(x=Year, y=ASMR))+
  geom_line()+
  scale_y_continuous(limits=c(0,NA))+
  facet_wrap(~Area)+
  theme_custom()

UKnations <- UKdata %>% 
  filter(Area %in% c("England", "Wales", "Scotland", "Northern Ireland"))

#Import US data downloaded from the excellent CDC WONDER database https://wonder.cdc.gov/
USdata <- read.delim("Data/CDC Data/ASD99-20.txt") %>% 
  filter(!is.na(Year)) %>% 
  select(2,6,9) %>% 
  set_names(c("Year", "Deaths", "ASMR")) %>% 
  mutate(Area="USA")

data <- bind_rows(USdata, UKnations)

agg_png("Outputs/ASDPandemicUKUSA.png", units="in", width=9, height=6, res=500)
ggplot(data %>% filter(Year>2000), aes(x=Year, y=ASMR, colour=Area))+
  geom_rect(aes(xmin=2019.1, xmax=2020.5, ymin=0, ymax=30), fill="Grey90", colour=NA)+
  geom_line(show.legend=FALSE)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Age-standardised mortality rate")+
  scale_colour_paletteer_d("rcartocolor::Bold")+
  theme_custom()+
  annotate("text", x=2010, y=24, label="Scotland", colour="#3969AC", family="Lato")+
  annotate("text", x=2007, y=16.5, label="Northern Ireland", colour="#11A579", family="Lato")+
  annotate("text", x=2017, y=14.2, label="Wales", colour="#E73F74", family="Lato")+
  annotate("text", x=2008, y=9.5, label="England", colour="#7F3C8D", family="Lato")+
  annotate("text", x=2012, y=7, label="USA", colour="#F2B701", family="Lato")+
  labs(title="Alcohol deaths have risen in the UK and USA during the pandemic",
       subtitle="Age-standardised rate of deaths from causes that are wholly-attributable to alcohol (e.g. alcoholic liver disease or alcohol poisoning)",
       caption="Data from Office for National Statistics and Center for Disease Control\nPlot by @VictimOfMaths")
  
dev.off()
