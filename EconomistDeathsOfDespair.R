rm(list=ls())

library(curl)
library(readxl)
library(tidyverse)
library(ragg)
library(extrafont)
library(ggtext)
library(grid)

options(scipen=10000)

#Read in England & Wales data from
#https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/the21stcenturymortalityfilesdeathsdataset
ewfile <- tempfile()
ewurl <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/the21stcenturymortalityfilesdeathsdataset/current/21stcenturymortality2019final.xls"
ewfile <- curl_download(url=ewurl, destfile=ewfile, quiet=FALSE, mode="wb")

ewdata <- read_excel(ewfile, sheet="2019", range="A2:E20302") %>% 
  #Allocate causes to code groups
  mutate(code1=substr(ICD10, 1, 1), code2=as.numeric(substr(ICD10,2,3)), 
         code3=as.numeric(substr(ICD10,4,4)),
         Cause=case_when(
           code1=="K" & code2 %in% c(70, 73, 74) ~ "Alcohol",
           code1=="F" & code2==10 ~ "Alcohol",
           code1=="X" & code2==45 ~ "Alcohol", #Difference from the Masters defns as X45 is clearly alcohol-related
           code1=="Y" & code2==15 ~ "Alcohol", #Difference from the Masters defns as Y15 is clearly alcohol-related
           code1=="X" & code2 %in% c(40:44, 85) ~ "Drugs",           
           code1=="Y" & code2 %in% c(10:14) ~ "Drugs",
           code1=="F" & code2 %in% c(11:16, 18, 19) ~ "Drugs", #Including F18 here to align with Scottish data
           code1=="U" & code2==3 ~ "Suicide",
           code1=="X" & code2 %in% c(60:84) ~ "Suicide",
           code1=="Y" & code2 ==87 ~ "Suicide",
           TRUE ~ "Other"),
         Age=case_when(
           Age %in% c("Neonates", "<1", "01-04") ~ "0-4",
           Age %in% c("05-09") ~ "5-9",
           TRUE ~ Age)) %>% 
  #Collapse into cause groups
  group_by(Age, Cause) %>% 
  summarise(Dx=sum(NDTHS)) %>% 
  ungroup()

#Set up framework
ages <- length(unique(ewdata$Age))
causes <- length(unique(ewdata$Cause))
frame <- data.frame(Age=rep(unique(ewdata$Age), times=1, each=causes),
                    Cause=rep(unique(ewdata$Cause), times=ages, each=1))

#Bring together 
ewdata.wide <- ewdata %>% 
  merge(frame, all.y=TRUE) %>% 
  mutate(Dx=replace_na(Dx, 0),
         Country="England & Wales") 

#Repeat for Scottish data
scotfile.2019 <- tempfile()
scoturl.2019 <- "https://www.nrscotland.gov.uk/files//statistics/vital-events-ref-tables/2019/vital-events-19-ref-tabs-6.xlsx"
scotfile.2019 <- curl_download(url=scoturl.2019, destfile=scotfile.2019, quiet=FALSE, mode="wb")

scotdata <- read_excel(scotfile.2019, sheet="6.04", range=c("A9:X1739"), col_names=FALSE) %>% 
  filter(!is.na(`...3`)) %>% 
  rename(ICD10=`...1`, Sex=`...3`) %>% 
  gather(Age, Dx, c(5:24)) %>% 
  fill(ICD10) %>% 
  filter(nchar(ICD10)<=3) %>% 
  #Stupid faff because of *horrible* formatting choices in the data
  mutate(`...4`=as.numeric(`...4`)) %>% 
  arrange(Sex, Age, ICD10, `...4`) %>% 
  distinct(Sex, Age, ICD10, .keep_all=TRUE) %>% 
  select(-c(`...2`, `...4`)) %>% 
  mutate(Dx=as.numeric(if_else(Dx %in% c("-", ".", NA), "0", Dx)),
         Age=case_when(Age=="...5" ~ "0-4", Age=="...6" ~ "0-4",Age=="...7" ~ "5-9",
                       Age=="...8" ~ "10-14", Age=="...9" ~ "15-19", Age=="...10" ~ "20-24",
                       Age=="...11" ~ "25-29", Age=="...12" ~ "30-34", Age=="...13" ~ "35-39",
                       Age=="...14" ~ "40-44", Age=="...15" ~ "45-49", Age=="...16" ~ "50-54",
                       Age=="...17" ~ "55-59", Age=="...18" ~ "60-64", Age=="...19" ~ "65-69",
                       Age=="...20" ~ "70-74", Age=="...21" ~ "75-79", Age=="...22" ~ "80-84",
                       TRUE ~ "85+")) %>% 
  group_by(Age, ICD10) %>% 
  summarise(Dx=sum(Dx)) %>% 
  ungroup() %>% 
  #Remove stuborn codes that remain due to bad formatting in the data
  filter(!ICD10 %in% c("E90", "F99", "G99", "J99", "K93", "L99", "M99", "N99", "Y98")) %>% 
  mutate(code1=substr(ICD10, 1, 1), code2=as.numeric(substr(ICD10,2,3)),
         Cause=case_when(
           code1=="K" & code2 %in% c(70, 73, 74) ~ "Alcohol",
           code1=="F" & code2==10 ~ "Alcohol",
           code1=="X" & code2==45 ~ "Alcohol", #Difference from the Masters defns as X45 is clearly alcohol-related
           code1=="Y" & code2==15 ~ "Alcohol", #Difference from the Masters defns as Y15 is clearly alcohol-related
           code1=="X" & code2 %in% c(40:44, 85) ~ "Drugs",
           code1=="Y" & code2 %in% c(10:14) ~ "Drugs",
           code1=="F" & code2 %in% c(11:16, 18, 19) ~ "Drugs", #Including F18 here to align with Scottish data
           code1=="U" & code2==3 ~ "Suicide",
           code1=="X" & code2 %in% c(60:84) ~ "Suicide",
           code1=="Y" & code2 ==87 ~ "Suicide",
           TRUE ~ "Other")) %>% 
  group_by(Age, Cause) %>% 
  summarise(Dx=sum(Dx)) %>% 
  ungroup() %>% 
  mutate(Country="Scotland")

#Download 2019 mid-year populations from ONS
pops <- tempfile()
popurl <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2019april2019localauthoritydistrictcodes/ukmidyearestimates20192019ladcodes.xls"
pops <- curl_download(url=popurl, destfile=pops, quiet=FALSE, mode="wb")

popdata <- read_excel(pops, sheet="MYE2 - Persons", range="B5:CQ391") %>% 
  filter(Name %in% c("ENGLAND AND WALES", "SCOTLAND")) %>% 
  mutate(Name=if_else(Name=="SCOTLAND", "Scotland", "England & Wales")) %>% 
  gather(Age, pop, c(4:94)) %>% 
  select(Name, Age, pop) %>% 
  mutate(Age=if_else(Age=="90+", 90, as.numeric(Age)),
         Age=case_when(
           Age<5 ~ "0-4", Age<10 ~ "5-9", Age<15 ~ "10-14", Age<20 ~ "15-19", Age<25 ~ "20-24",
           Age<30 ~ "25-29", Age<35 ~ "30-34", Age<40 ~ "35-39", Age<45 ~ "40-44",
           Age<50 ~ "45-49", Age<55 ~ "50-54", Age<60 ~ "55-59", Age<65 ~ "60-64",
           Age<70 ~ "65-69", Age<75 ~ "70-74", Age<80 ~ "75-79", Age<85 ~ "80-84",
           TRUE ~ "85+")) %>% 
  group_by(Name, Age) %>% 
  summarise(pop=sum(pop)) %>% 
  ungroup() %>% 
  set_names("Country", "Age", "pop")
  
#Bring it all together
data <- bind_rows(ewdata.wide, scotdata) %>% 
  merge(popdata) %>% 
  mutate(mx=Dx*100000/pop,
         Age=factor(Age, levels=c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                                  "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                                  "60-64", "65-69", "70-74", "75-79", "80-84", "85+")),
         Cause=factor(Cause, levels=c("Alcohol", "Suicide", "Drugs")))

annotations <- data.frame(Age=c("60-64", "25-29", "10-14"),
                          mx=c(56, 31, 11),
                          lab=c("Alcohol", "Suicide", "Drugs"), 
                          Country=rep("England & Wales", times=3),
                          Cause=c("Alcohol", "Suicide", "Drugs"))

segments <- data.frame(Age=c("60-64", "25-29", "10-14", "25-29", "10-14"),
                       AgeEnd=c("60-64", "25-29", "10-14", "30-34", "15-19"),
                       mx=c(53,28,8,19,1), mxEnd=c(40,19,1,19,1),
                       Country=rep("England & Wales", times=5),
                       Cause=c("Alcohol", "Suicide", "Drugs", "Suicide", "Drugs"))

plot <- ggplot(data %>% filter(Cause!="Other"), 
       aes(x=Age, y=mx, fill=Cause))+
  geom_col(show.legend=FALSE)+
  geom_text(data=annotations, aes(label=lab, colour=Cause), family="PT Sans", fontface="bold")+
  geom_segment(data=segments, aes(xend=AgeEnd, yend=mxEnd, colour=Cause))+
  scale_y_continuous(name="Deaths per 100,000")+
  scale_fill_manual(values=c("#01A2D9", "#014D64", "#E3120B"))+
  scale_colour_manual(values=c("#01A2D9", "#E3120B", "#014D64"))+
  facet_wrap(~Country)+
  labs(title="Scotland has a drug deaths problem",
       subtitle="Deaths from alcohol, suicide and drugs in 2019",
       caption="Source: ONS & NRS")+
  #Economist style theme modified from @leeolney3
  #https://gist.github.com/leeolney3/75415fb8b92cc67673cd2bdb0b3379fd
  theme_minimal()+
  theme(text=element_text(family="PT Sans", color="#0C0C0C"),
        legend.position = "none",
        plot.margin=margin(.75,.6,.5,.6, unit="cm"),
        plot.title.position = "plot",
        panel.grid=element_blank(),
        axis.line.x=element_line(colour="#333333"),
        axis.ticks.x = element_line(color="#333333", size=.5),
        axis.ticks.length.y=unit(.15, "cm"),
        axis.title=element_text(margin=margin(t=5), face="bold", size=13),
        axis.text=element_text(size=7),
        panel.grid.major.y = element_line(colour="#D9D9D9"),
        plot.title=element_text(face="bold", size=20),
        plot.subtitle=element_text(color="black", size=12.5, lineheight = 1.1, margin=margin(b=15)),
        plot.caption.position = "plot",
        plot.caption=element_text(size=11.5, color="#595959",lineheight=1,hjust=0, 
                                  margin=margin(t=13)),
        strip.text=element_text(size=rel(1.2), face="bold"))

#Funky use of {grid} to add Economist's trademark red line/rectangle taken from @_angsar
#https://github.com/bydata/30DayChartChallenge/blob/main/2022/12/theme_day_economist.R

agg_png("Outputs/DeathsOfDespairEWSxAge.png", units="in", width=12, height=6, res=500)
plot
grid.lines(
  x = c(0, 1),
  y = 1,
  gp = gpar(col = "#E3120B", lwd = 2)
)
grid.rect(
  x = 0,
  y = 1,
  width = 0.2, # TODO 10 % of line / image width
  height = 0.05,  # TODO ~2 % of line / image height
  gp = gpar(fill = "#E3120B", col = NA)
)
dev.off()
