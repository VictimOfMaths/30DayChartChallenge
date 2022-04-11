rm(list=ls())

library(tidyverse)
library(extrafont)
library(ggtext)
library(ragg)
library(geofacet)
library(curl)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(size=rel(1)),
          plot.title=element_text(size=rel(2.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Whirly Birdie"),
          plot.subtitle=element_markdown(colour="Grey40", hjust=0, vjust=1, size=rel(1.2)),
          plot.caption=element_text(colour="Grey20", hjust=1, vjust=1, size=rel(1)),
          axis.text=element_text(colour="Grey40"),
          axis.title=element_text(colour="Grey20"),
          legend.text=element_text(colour="Grey40"),
          legend.title=element_text(colour="Grey20"))
}

#Download data from GISAH
url1 <- "https://apps.who.int/gho/athena/data/xmart.csv?target=GHO/SA_0000001400&profile=crosstable&filter=COUNTRY:*;YEAR:2020;YEAR:2019;YEAR:2018;YEAR:2017;YEAR:2016;YEAR:2015;YEAR:2014;YEAR:2013;YEAR:2012;YEAR:2011;YEAR:2010&x-sideaxis=COUNTRY;DATASOURCE;ALCOHOLTYPE&x-topaxis=GHO;YEAR"
temp1 <- tempfile()
temp1 <- curl_download(url=url1, destfile=temp1, quiet=FALSE, mode="wb")

url2 <- "https://apps.who.int/gho/athena/data/GHO/SA_0000001400?filter=COUNTRY:*;YEAR:2009;YEAR:2008;YEAR:2007;YEAR:2006;YEAR:2005;YEAR:2004;YEAR:2003;YEAR:2002;YEAR:2001;YEAR:2000&x-sideaxis=COUNTRY;DATASOURCE;ALCOHOLTYPE&x-topaxis=GHO;YEAR&profile=crosstable&format=csv"
temp2 <- tempfile()
temp2 <- curl_download(url=url2, destfile=temp2, quiet=FALSE, mode="wb")

url3 <- "https://apps.who.int/gho/athena/data/GHO/SA_0000001400?filter=COUNTRY:*;YEAR:1999;YEAR:1998;YEAR:1997;YEAR:1996;YEAR:1995;YEAR:1994;YEAR:1993;YEAR:1992;YEAR:1991;YEAR:1990;YEAR:1989;YEAR:1988;YEAR:1987;YEAR:1986;YEAR:1985;YEAR:1984;YEAR:1983;YEAR:1982;YEAR:1981;YEAR:1980&x-sideaxis=COUNTRY;DATASOURCE;ALCOHOLTYPE&x-topaxis=GHO;YEAR&profile=crosstable&format=csv"
temp3 <- tempfile()
temp3 <- curl_download(url=url3, destfile=temp3, quiet=FALSE, mode="wb")

url4 <- "https://apps.who.int/gho/athena/data/GHO/SA_0000001400?filter=COUNTRY:*;YEAR:1979;YEAR:1978;YEAR:1977;YEAR:1976;YEAR:1975;YEAR:1974;YEAR:1973;YEAR:1972;YEAR:1971;YEAR:1970;YEAR:1969;YEAR:1968;YEAR:1967;YEAR:1966;YEAR:1965;YEAR:1964;YEAR:1963;YEAR:1962;YEAR:1961;YEAR:1960&x-sideaxis=COUNTRY;DATASOURCE;ALCOHOLTYPE&x-topaxis=GHO;YEAR&profile=crosstable&format=csv"
temp4 <- tempfile()
temp4 <- curl_download(url=url4, destfile=temp4, quiet=FALSE, mode="wb")

data1 <- read.csv(temp1, skip=1)
data2 <- read.csv(temp2, skip=1)
data3 <- read.csv(temp3, skip=1)
data4 <- read.csv(temp4, skip=1)

data <- merge(data4, data3,all=T) %>% 
  merge(data2, all=T) %>% 
  merge(data1, all=T) %>%
  select(-Data.Source) %>% 
  gather(year, PCC, c(3:ncol(.))) %>% 
  mutate(year=as.numeric(substr(year, 3, 8)), 
         Country=case_when(
           Country=="United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
           TRUE ~ Country),
         Beverage.Types=case_when(
           Beverage.Types==" All types" ~ "Total",
           Beverage.Types==" Beer" ~ "Beer",
           Beverage.Types==" Spirits" ~ "Spirits",
           Beverage.Types==" Wine" ~ "Wine",
           Beverage.Types==" Other alcoholic beverages" ~ "Other"),
         Beverage.Types=factor(Beverage.Types, 
                               levels=c("Total", "Beer", "Wine", "Spirits", "Other"))) %>% 
  distinct() %>% 
  filter(Country %in% c("France", "Germany", "United Kingdom", "Luxembourg",
                         "Italy", "Denmark", "Sweden", "Finland", "Netherlands", "Ireland", 
                         "Portugal", "Spain", "Belgium", "Czechia", "Austria", "Slovenia",
                         "Poland", "Slovakia", "Hungary", "Croatia", "Latvia", "Lithuania", "Estonia",
                         "Bulgaria", "Romania", "Greece", "Malta", "Cyprus") & !is.na(PCC) &
           year>1960) %>% 
  group_by(year, Country) %>% 
  spread(Beverage.Types, PCC) %>% 
  mutate(Spirits=if_else(is.na(Other), Spirits,
                         Spirits+Other)) %>% 
  select(-"Other") %>% 
  gather(Beverage.Types, PCC, c(3:6)) %>% 
  ungroup() %>% 
  mutate(Country=if_else(Country=="Czechia", "Czech Republic", Country)) %>% 
  group_by(Country) %>% 
  mutate(max=max(PCC[Beverage.Types=="Total"], na.rm=TRUE)) %>% 
  ungroup()

mainplot <- ggplot(data %>% filter(Beverage.Types!="Total"))+
  geom_col(aes(x=year, y=PCC, fill=Beverage.Types), show.legend=FALSE)+
  geom_segment(aes(x=1960.5, xend=1960.5, y=0, yend=max), colour="Black", size=0.1)+
  scale_fill_manual(values=c("#ffc000", "#00b0f0", "#7030a0"))+
  facet_geo(~Country, grid="eu_grid1")+
  theme_custom()+
  coord_polar()+
  theme(axis.text=element_blank(), axis.title=element_blank(), axis.line=element_blank(),
        axis.ticks=element_blank())+
  labs(title="Changes in European alcohol consumption 1961-2019",
       subtitle="Total annual alcohol consumption per person drunk as <span style='color:#ffc000;'>beer</span>, <span style='color:#00b0f0;'>spirits</span> and <span style='color:#7030a0;'>wine</span>.<br><br>",
       caption="Data from WHO GISAH | Plot by @VictimOfMaths")
                                                                                                                  
agg_png("Outputs/GISAHcircles.png", units="in", width=7, height=8, res=600)
mainplot
dev.off()



