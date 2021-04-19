rm(list=ls())

library(curl)
library(magrittr)
library(readxl)
library(zoo)
library(ggstream)
library(extrafont)
library(ggtext)
library(ragg)

temp <- tempfile()
url1 <- "https://apps.who.int/gho/athena/data/GHO/SA_0000001400?filter=COUNTRY:*;YEAR:1979;YEAR:1978;YEAR:1977;YEAR:1976;YEAR:1975;YEAR:1974;YEAR:1973;YEAR:1972;YEAR:1971;YEAR:1970;YEAR:1969;YEAR:1968;YEAR:1967;YEAR:1966;YEAR:1965;YEAR:1964;YEAR:1963;YEAR:1962;YEAR:1961;YEAR:1960&x-sideaxis=COUNTRY;DATASOURCE;ALCOHOLTYPE&x-topaxis=GHO;YEAR&profile=verbose&format=csv"
temp <- curl_download(url=url1, destfile=temp, quiet=FALSE, mode="wb")

data6079 <- read.csv(temp)

url2 <- "https://apps.who.int/gho/athena/data/GHO/SA_0000001400?filter=COUNTRY:*;YEAR:1999;YEAR:1998;YEAR:1997;YEAR:1996;YEAR:1995;YEAR:1994;YEAR:1993;YEAR:1992;YEAR:1991;YEAR:1990;YEAR:1989;YEAR:1988;YEAR:1987;YEAR:1986;YEAR:1985;YEAR:1984;YEAR:1983;YEAR:1982;YEAR:1981;YEAR:1980&x-sideaxis=COUNTRY;DATASOURCE;ALCOHOLTYPE&x-topaxis=GHO;YEAR&profile=verbose&format=csv"
temp <- curl_download(url=url2, destfile=temp, quiet=FALSE, mode="wb")

data8099 <- read.csv(temp)

url3 <- "https://apps.who.int/gho/athena/data/GHO/SA_0000001400?filter=COUNTRY:*;YEAR:2009;YEAR:2008;YEAR:2007;YEAR:2006;YEAR:2005;YEAR:2004;YEAR:2003;YEAR:2002;YEAR:2001;YEAR:2000&x-sideaxis=COUNTRY;DATASOURCE;ALCOHOLTYPE&x-topaxis=GHO;YEAR&profile=verbose&format=csv"
temp <- curl_download(url=url3, destfile=temp, quiet=FALSE, mode="wb")

data0009 <- read.csv(temp)

url4 <- "https://apps.who.int/gho/athena/data/GHO/SA_0000001400?filter=COUNTRY:*;YEAR:2020;YEAR:2019;YEAR:2018;YEAR:2017;YEAR:2016;YEAR:2015;YEAR:2014;YEAR:2013;YEAR:2012;YEAR:2011;YEAR:2010&x-sideaxis=COUNTRY;DATASOURCE;ALCOHOLTYPE&x-topaxis=GHO;YEAR&profile=verbose&format=csv"
temp <- curl_download(url=url4, destfile=temp, quiet=FALSE, mode="wb")

data1018 <- read.csv(temp)

data <- bind_rows(data6079, data8099, data0009, data1018) %>% 
  select(YEAR..CODE., REGION..DISPLAY., COUNTRY..CODE., COUNTRY..DISPLAY., ALCOHOLTYPE..DISPLAY., 
         Numeric)

colnames(data) <- c("Year", "Region", "Code", "Country", "Drink", "PCC")

data %<>% filter(Drink %in% c("Beer", "Wine", "Spirits")) %>% 
  #Mess about with country names to ensure populations match. No value judgment is implied!
  mutate(Country=case_when(
    Code=="CIV" ~ "Côte d'Ivoire",
    Code=="PRK" ~ "Dem. People's Republic of Korea",
    Code=="FSM" ~ "Micronesia",
    Code=="GBR" ~ "United Kingdom",
    TRUE ~ Country))

#Download UN population data
url5 <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_Population/WPP2019_POP_F07_1_POPULATION_BY_AGE_BOTH_SEXES.xlsx"
temp <- curl_download(url=url5, destfile=temp, quiet=FALSE, mode="wb")

pop <- read_excel(temp, sheet="ESTIMATES", range="C18:AC3842", col_names=FALSE) %>% 
  rename(Country=`...1`, Year=`...6`) %>% 
  select(Country, Year, c(10:27)) %>% 
  gather(Age, Pop, c(3:20)) %>%
  group_by(Country, Year) %>% 
  summarise(Pop=sum(as.numeric(Pop))) %>% 
  ungroup()

fullpop <- data.frame(Country=rep(unique(data$Country), each=length(unique(data$Year))),
                      Year=rep(c(min(data$Year):max(data$Year)))) %>% 
  merge(pop, all.x=TRUE) %>% 
  #Remove a few countries with no UN population estimates by age
  filter(!Country %in% c("Andorra", "Cook Islands", "Dominica", "Nauru", "Niue",
                         "Saint Kitts and Nevis", "Tuvalu")) %>% 
  group_by(Country) %>% 
  #Interpolate missing years
  mutate(Pop=na.spline(Pop)*1000) %>% 
  ungroup()

plotdata <- data %>% 
  merge(fullpop) %>% 
  mutate(Alc=Pop*PCC,
         Region2=case_when(
           Region=="Africa" ~ "Africa",
           Country %in% c("Mexico", "Saint Lucia", "Bahamas", "Belize", "Dominican Republic",
                          "Haiti", "Nicaragua", "Honduras", "Jamaica", "El Salvador",
                          "Antigua and Barbuda", "Guatemala", "Trinidad and Tobago", "Panama",
                          "Costa Rica", "Canada", "Cuba", "United States of America", "Barbados",
                          "Saint Vincent and the Grenadines", "Grenada") ~ "North/Central America",
           Region=="Americas" ~ "South America",
           Country %in% c("Egypt", "Djibouti", "Libya", "Morocco", "Sudan", 
                          "Tunisia") ~ "Other Asia/Oceania",
           Region=="Eastern Mediterranean" ~ "Africa",
           Country=="India" ~ "India",
           Country=="China" ~ "China",
           Region %in% c("Western Pacific", "South-East Asia") ~ "Other Asia/Oceania",
           Country %in% c("Portugal", "Spain", "France", "Italy", "Malta", "Slovakia", "Slovenia", 
                          "Croatia", "Bosnia and Herzegovina", "Serbia", "Albania", "North Macedonia",
                          "Montenegro", "Greece", "Cyprus", "Turkey") ~ "Southern Europe",
           Country %in% c("Austria", "Belgium", "Czechia", "Denmark", "Finland", "Germany", "Iceland",
                          "Ireland", "Luxembourg", "Netherlands", "Norway", "Sweden", "Switzerland",
                          "United Kingdom") ~ "Northern/Central Europe",
           Region=="Europe" ~ "Eastern Europe" )) %>% 
  group_by(Region2, Drink, Year) %>% 
  summarise(Alc=sum(Alc)) %>% 
  ungroup() %>% 
  mutate(Region2=factor(Region2, levels=c("China", "India", "Other Asia/Oceania", 
                                          "Northern/Central Europe", "Southern Europe", 
                                          "Eastern Europe", "North/Central America", "South America",
                                          "Africa")))

agg_tiff("19GlobalChange.tiff", units="in", width=11, height=7, res=800)
ggplot(plotdata, aes(x=Year, y=Alc, fill=Drink))+
  geom_stream(show.legend=FALSE)+
  scale_fill_manual(values=c("#ffc000", "#00b0f0", "#7030a0"))+
  facet_wrap(~Region2)+
  theme_classic()+
  theme(axis.line.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(),
        axis.title.y=element_blank(), strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)), text=element_text(family="Roboto"),
        plot.title=element_text(face="bold", size=rel(1.6)),
        plot.subtitle=element_markdown())+
  labs(title="Global drinking has changed a lot in the past 70 years",
       subtitle="Total litres of pure alcohol drunk as <span style='color:#ffc000;'>beer</span>, <span style='color:#00b0f0;'>spirits </span>and <span style='color:#7030a0;'>wine</span> by region",
       caption="Alcohol consumption data from WHO GISAH\nPopulation data from UN\nPlot by @VictimOfMaths")
dev.off()
