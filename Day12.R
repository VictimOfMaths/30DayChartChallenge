rm(list=ls())

library(curl)
library(tidyverse)
library(readxl)
library(paletteer)
library(sf)
library(scales)
library(ragg)
library(gtools)
library(ggridges)
library(patchwork)
library(extrafont)

#Download vaccination data by MSOA
#https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-vaccinations/
vax <- tempfile()
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/04/COVID-19-weekly-announced-vaccinations-8-April-2021.xlsx"
vax <- curl_download(url=url, destfile=vax, quiet=FALSE, mode="wb")

vaxdata <- read_excel(vax, sheet="MSOA", range="F16:O6806", col_names=FALSE) %>% 
  rename(msoa11cd=`...1`, msoa11nm=`...2`, `<50`=`...3`,  `50-54`=`...4`, `55-59`=`...5`, 
         `60-64`=`...6`, `65-69`=`...7`, 
         `70-74`=`...8`, `75-79`=`...9`, `80+`=`...10`) %>% 
  gather(age, vaccinated, c(3:10))

#Download IMD data
temp <- tempfile()
source <- ("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833970/File_1_-_IMD2019_Index_of_Multiple_Deprivation.xlsx")
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

IMD <- read_excel(temp, sheet="IMD2019", range="A2:F32845", col_names=FALSE)[,c(1,2,5,6)]
colnames(IMD) <- c("LSOA11CD", "LSOA11NM", "IMDrank", "IMDdecile")

#Download LSOA to MSOA lookup
temp <- tempfile()
source <- ("https://opendata.arcgis.com/datasets/fe6c55f0924b4734adf1cf7104a0173e_0.csv")
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

lookup <- read.csv(temp) %>% 
  select(LSOA11CD, MSOA11CD, RGN11NM) %>% 
  unique()

#Merge into IMD data
IMD <- merge(IMD, lookup, by="LSOA11CD")

#Bring in population data for LSOAs
temp <- tempfile()
temp2 <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2flowersuperoutputareamidyearpopulationestimatesnationalstatistics%2fmid2019sape22dt13/sape22dt13mid2019lsoabroadagesestimatesunformatted.zip"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

pop <- read_excel(file.path(temp2, "SAPE22DT13-mid-2019-lsoa-Broad_ages-estimates-unformatted.xlsx"),
                  sheet="Mid-2019 Persons", range="A6:G34758", col_names=FALSE)[,c(1,7)]
colnames(pop) <- c("LSOA11CD", "pop")

#Merge into IMD data
IMD <- merge(IMD, pop)

#Calculate IMD rank at MSOA level as weighted average of LSOA level ranks, weight by population
IMD_MSOA <- IMD %>% 
  group_by(MSOA11CD) %>% 
  summarise(IMDrank=weighted.mean(IMDrank, pop), pop=sum(pop)) %>% 
  ungroup() 

pop2 <- read_excel(vax, sheet="Population estimates (NIMS)", range="O16:Y6806", col_names=FALSE) %>% 
  select(-c(2)) %>% 
  rename(msoa11cd=`...1`) %>% 
  gather(age, pop, c(2:10)) %>% 
  mutate(age=case_when(
    age %in% c("...3", "...4") ~ "<50",
    age=="...5" ~ "50-54",
    age=="...6" ~ "55-59",
    age=="...7" ~ "60-64",
    age=="...8" ~ "65-69",
    age=="...9" ~ "70-74",
    age=="...10" ~ "75-79",
    TRUE ~ "80+")) %>% 
  group_by(msoa11cd, age) %>% 
  summarise(pop=sum(pop)) %>% 
  ungroup()

#COMBINE
vaxdata <- merge(vaxdata, pop2) %>% 
  merge(IMD_MSOA %>% select(-pop), by.x="msoa11cd", by.y="MSOA11CD") %>% 
  mutate(vaxprop=vaccinated/pop)

#Add totals
vaxdata <- vaxdata %>% 
  group_by(msoa11cd, msoa11nm, IMDrank) %>% 
  summarise(vaccinated=sum(vaccinated), pop=sum(pop)) %>% 
  mutate(vaxprop=vaccinated/pop, age="Total") %>% 
  ungroup() %>% 
  bind_rows(vaxdata)

#Calculate deprivation gradients within IMD deciles
#Allocate to deciles
vaxdeciles <- vaxdata %>% 
  mutate(decile=quantcut(-IMDrank, 10, labels=FALSE)) %>% 
  group_by(age, decile) %>% 
  mutate(decilemean=sum(vaccinated)/sum(pop)) %>% 
  ungroup() %>% 
  group_by(age) %>% 
  mutate(popmean=sum(vaccinated)/sum(pop)) %>% 
  ungroup()

agg_png("12Strips.png", units="in", width=12, height=8, res=800)
ggplot(vaxdeciles %>% filter(age=="Total"), 
       aes(x=vaxprop, y=as.factor(decile), colour=vaxprop))+
  geom_jitter(shape=21, alpha=0.6, show.legend=FALSE)+
  geom_segment(aes(x=popmean, xend=popmean, y=Inf, yend=-Inf), colour="Grey20")+
  geom_point(aes(x=decilemean, y=as.factor(decile)), colour="Grey20", fill="Cyan", shape=23, size=2)+
  scale_colour_paletteer_c("viridis::magma", direction=-1)+
  scale_x_continuous(name="Proportion of adult population vaccinated",
                     labels=label_percent(accuracy=1))+
  scale_y_discrete(name="Index of Multiple Deprivation", labels=c("1 - least deprived", "2", "3", "4", "5", "6", "7", 
                                                                  "8", "9", "10 - most deprived"))+  
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.6)),
        text=element_text(family="Roboto"))+
  labs(title="COVID vaccination rates are lower in more deprived areas in England",
       subtitle="Number of adults vaccinated by MSOA compared compared to the estimated 80+ population in 2019.",
       caption="Vaccination data from NHS England, Population data from ONS\nPlot by @VictimOfMaths")+
  annotate("text", x=0.7, y=9.9, label="Each circle = 1 MSOA", size=3, family="Roboto")+
  annotate("text", x=0.54, y=6.5, label="Population average", size=3, family="Roboto")+
  annotate("text", x=0.59, y=3.5, label="Decile average", size=3, family="Roboto")+
  geom_segment(aes(x=0.434, y=6.5,  xend=0.5, yend=6.5), colour="Grey20")+
  geom_segment(aes(x=0.56, y=3.55,  xend=0.49, yend=3.95), colour="Grey20")
dev.off()
