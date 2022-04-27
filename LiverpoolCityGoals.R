rm(list=ls())

library(tidyverse)
library(xml2)
library(rvest)
library(lubridate)
library(stringr)
library(extrafont)
library(ragg)
library(ggstream)
library(hrbrthemes)
library(ggtext)

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

temp <- tempfile()

#21/22
url <- "https://lfcstats.co.uk/20212022goalscorers.html"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
data.2122 <- as.data.frame(html_table(temp[1]))[-1,-2] %>% 
  set_names(c("Player", "21/22")) %>% 
  mutate(Player=gsub("Position.*", "", Player),
         Player=stringr::str_trim(Player)) %>% 
  #fix error in Salah's goals
  mutate(`21/22`=if_else(Player=="Mohamed Salah", "22", `21/22`))

#20/21
url <- "https://lfcstats.co.uk/20202021goalscorers.html"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
data.2021 <- as.data.frame(html_table(temp[1]))[-1,] %>% 
  set_names(c("Player", "20/21"))

#19/20
url <- "https://lfcstats.co.uk/20192020goalscorers.html"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
data.1920 <- as.data.frame(html_table(temp[1]))[-1,] %>% 
  set_names(c("Player", "19/20"))

#18/19
url <- "https://lfcstats.co.uk/20182019goalscorers.html"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
data.1819 <- as.data.frame(html_table(temp[1]))[-1,] %>% 
  set_names(c("Player", "18/19"))

#17/18
url <- "https://lfcstats.co.uk/20172018goalscorers.html"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
data.1718 <- as.data.frame(html_table(temp[1]))[-1,] %>% 
  set_names(c("Player", "17/18"))

#16/17
url <- "https://lfcstats.co.uk/20162017goalscorers.html"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
data.1617 <- as.data.frame(html_table(temp[1]))[-1,] %>% 
  set_names(c("Player", "16/17")) %>% 
  mutate(Player=if_else(Player=="Georgino Wijnaldum", "Georginio Wijnaldum", Player))

#15/16
url <- "https://lfcstats.co.uk/20152016oalscorers.html"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
data.1516 <- as.data.frame(html_table(temp[1]))[-1,] %>% 
  set_names(c("Player", "15/16")) 

LFCgoals <- merge(data.2122, data.2021, all=TRUE) %>% 
  merge(data.1920, all=TRUE) %>% 
  merge(data.1819, all=TRUE) %>% 
  merge(data.1718, all=TRUE) %>% 
  merge(data.1617, all=TRUE) %>% 
  merge(data.1516, all=TRUE) %>% 
  gather(Year, Goals, c(2:8)) %>% 
  mutate(Goals=as.numeric(Goals),
         Player=case_when(
           Player=="Sadio Mane" ~ "Sadio Mané",
           Player=="Naby Keita" ~ "Naby Keïta",
           Player=="Martin Skrtel" ~ "Martin Škrtel",
           Player=="Luis Diaz" ~ "Luis Díaz",
           Player=="Kolo Toure" ~ "Kolo Touré",
           Player=="Joel Matip" ~ "Joël Matip",
           Player=="Ibrahima Kounate" ~ "Ibrahima Konaté",
           Player=="Chrisitan Benteke" ~ "Christian Benteke",
           TRUE ~ Player
         ))

#Get DOB data from wikipedia
#21/22
url <- "https://en.wikipedia.org/wiki/2021%E2%80%9322_Liverpool_F.C._season"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
ages.2122 <- as.data.frame(html_table(temp[3])) %>% 
  mutate(DoB=as.Date(substr(Date.Of.Birth, 2,11), format="%Y-%m-%d")) %>% 
  filter(!is.na(DoB)) %>% 
  select(c("Player", "DoB")) %>% 
  mutate(Player=gsub("\\(.*", "", Player),
         Player=stringr::str_trim(Player))

#20/21
url <- "https://en.wikipedia.org/wiki/2020%E2%80%9321_Liverpool_F.C._season"

#Grab html tables
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
ages.2021 <- as.data.frame(html_table(temp[3])) %>% 
  mutate(DoB=as.Date(substr(Date.of.birth, 2,11), format="%Y-%m-%d")) %>% 
  filter(!is.na(DoB)) %>% 
  select(c("Player", "DoB")) %>% 
  mutate(Player=gsub("\\(.*", "", Player),
         Player=stringr::str_trim(Player))

#19/20
url <- "https://en.wikipedia.org/wiki/2019%E2%80%9320_Liverpool_F.C._season"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
ages.1920 <- as.data.frame(html_table(temp[3])) %>% 
  mutate(DoB=as.Date(substr(Date.of.birth, 2,11), format="%Y-%m-%d")) %>% 
  filter(!is.na(DoB)) %>% 
  select(c("Name", "DoB")) %>% 
  set_names(c("Player", "DoB")) %>% 
  mutate(Player=gsub("\\(.*", "", Player),
         Player=stringr::str_trim(Player))

#18/19
url <- "https://en.wikipedia.org/wiki/2018%E2%80%9319_Liverpool_F.C._season"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
ages.1819 <- as.data.frame(html_table(temp[3])) %>% 
  mutate(DoB=as.Date(substr(Date.of.Birth, 2,11), format="%Y-%m-%d")) %>% 
  filter(!is.na(DoB)) %>% 
  select(c("Name", "DoB")) %>% 
  set_names(c("Player", "DoB")) %>% 
  mutate(Player=gsub("\\(.*", "", Player),
         Player=stringr::str_trim(Player))

#17/18
url <- "https://en.wikipedia.org/wiki/2017%E2%80%9318_Liverpool_F.C._season"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
ages.1718 <- as.data.frame(html_table(temp[3])) %>% 
  mutate(DoB=as.Date(substr(Date.of.birth, 2,11), format="%Y-%m-%d")) %>% 
  filter(!is.na(DoB)) %>% 
  select(c("Name", "DoB")) %>% 
  set_names(c("Player", "DoB")) %>% 
  mutate(Player=gsub("\\(.*", "", Player),
         Player=stringr::str_trim(Player))

#16/17
url <- "https://en.wikipedia.org/wiki/2016%E2%80%9317_Liverpool_F.C._season"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
ages.1617 <- as.data.frame(html_table(temp[3])) %>% 
  mutate(DoB=as.Date(substr(Date.of.Birth, 2,11), format="%Y-%m-%d")) %>% 
  filter(!is.na(DoB)) %>% 
  select(c("Name", "DoB")) %>% 
  set_names(c("Player", "DoB")) %>% 
  mutate(Player=gsub("\\(.*", "", Player),
         Player=stringr::str_trim(Player))

#1516
url <- "https://en.wikipedia.org/wiki/2015%E2%80%9316_Liverpool_F.C._season"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
ages.1516 <- as.data.frame(html_table(temp[3])) %>% 
  mutate(DoB=as.Date(substr(Date.of.Birth, 2,11), format="%Y-%m-%d")) %>% 
  filter(!is.na(DoB)) %>% 
  select(c("Name", "DoB")) %>% 
  set_names(c("Player", "DoB")) %>% 
  mutate(Player=gsub("\\(.*", "", Player),
         Player=stringr::str_trim(Player))

ages <- bind_rows(ages.2122, ages.2021, ages.1920, ages.1819, ages.1718, ages.1617, ages.1516) %>% 
  unique() %>% 
  mutate(`21/22`=interval(DoB, as.Date("2021-09-01")) / years(1),
         `20/21`=interval(DoB, as.Date("2020-09-01")) / years(1),
         `19/20`=interval(DoB, as.Date("2019-09-01")) / years(1),
         `18/19`=interval(DoB, as.Date("2018-09-01")) / years(1),
         `17/18`=interval(DoB, as.Date("2017-09-01")) / years(1),
         `16/17`=interval(DoB, as.Date("2016-09-01")) / years(1),
         `15/16`=interval(DoB, as.Date("2015-09-01")) / years(1)) %>% 
  gather(Year, Age, c(3:9)) 

LFCdata <- merge(LFCgoals, ages, all.x=TRUE)

meanage <- LFCdata %>% 
  filter(!is.na(Goals)) %>% 
  group_by(Year) %>% 
  summarise(meanage=weighted.mean(Age, Goals, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(flag="yes")

agg_tiff("Outputs/LiverpoolGoalsLexis.tiff", units="in", width=6, height=9, res=500)
ggplot(LFCdata %>% filter(!is.na(Goals) & Goals>=5))+
  geom_line(aes(x=Year, y=Age, group=Player), 
            colour="Grey80")+
  geom_point(aes(x=Year, y=Age, size=Goals), 
             shape=21, fill="White", colour="White")+
  geom_point(aes(x=Year, y=Age, size=Goals), 
             shape=21, fill="Red", alpha=0.5)+
  geom_line(data=meanage, aes(x=Year, y=meanage, group=flag), colour="Black")+
  coord_equal()+
  theme_custom()
dev.off()

LFCshort <- LFCdata %>% 
  mutate(Player=case_when(
    Player %in% c("Mohamed Salah", "Sadio Mané", "Roberto Firmino", "Philippe Coutinho",
                  "Diogo Jota", "Divock Origi") ~ Player,
    TRUE ~ "Other")) %>% 
  filter(!is.na(Goals)) %>% 
  group_by(Player, Year) %>% 
  summarise(Goals=sum(Goals)) %>% 
  ungroup() %>% 
  mutate(Player=factor(Player, levels=c("Philippe Coutinho", "Roberto Firmino", "Divock Origi",
                                        "Sadio Mané", "Mohamed Salah", "Diogo Jota", "Other")))

ggplot(LFCshort, aes(x=Year, y=Goals, fill=Player, group=Player))+
  geom_stream(bw=0.98, extra_span=0.15)+
  theme_custom()

#Repeat for Manchester City

#Get DOB data from www.worldfootball.net
url <- "https://www.worldfootball.net/teams/manchester-city/10/"

#Grab html tables
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
Cityages <- as.data.frame(html_table(temp[1])) %>% 
  filter(Country=="") %>% 
  mutate(DoB=as.Date(born, format="%d/%m/%Y")) %>% 
  select(c("Player", "DoB")) %>% 
  mutate(`21/22`=interval(DoB, as.Date("2021-09-01")) / years(1),
         `20/21`=interval(DoB, as.Date("2020-09-01")) / years(1),
         `19/20`=interval(DoB, as.Date("2019-09-01")) / years(1),
         `18/19`=interval(DoB, as.Date("2018-09-01")) / years(1),
         `17/18`=interval(DoB, as.Date("2017-09-01")) / years(1),
         `16/17`=interval(DoB, as.Date("2016-09-01")) / years(1)) %>% 
  gather(Year, Age, c(3:8)) 

#Get league goals data from fbref.com
#21/22
url <- "https://fbref.com/en/squads/b8fd03ef/Manchester-City-Stats"

#Grab html table
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
data_2122 <- as.data.frame(html_table(temp[1])) %>% 
  select(c(1,9))%>% 
  slice(-1) %>% 
  set_names(c("Player", "21/22"))

#20/21
url <- "https://fbref.com/en/squads/b8fd03ef/2020-2021/Manchester-City-Stats"

#Grab html table
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
data_2021 <- as.data.frame(html_table(temp[1])) %>% 
  select(c(1,9))%>% 
  slice(-1) %>% 
  set_names(c("Player", "20/21"))

#19/20
url <- "https://fbref.com/en/squads/b8fd03ef/2019-2020/Manchester-City-Stats"

#Grab html table
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
data_1920 <- as.data.frame(html_table(temp[1])) %>% 
  select(c(1,9))%>% 
  slice(-1) %>% 
  set_names(c("Player", "19/20"))

#18/19
url <- "https://fbref.com/en/squads/b8fd03ef/2018-2019/Manchester-City-Stats"

#Grab html table
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
data_1819 <- as.data.frame(html_table(temp[1])) %>% 
  select(c(1,9))%>% 
  slice(-1) %>% 
  set_names(c("Player", "18/19"))

#17/18
url <- "https://fbref.com/en/squads/b8fd03ef/2017-2018/Manchester-City-Stats"

#Grab html table
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
data_1718 <- as.data.frame(html_table(temp[1])) %>% 
  select(c(1,9))%>% 
  slice(-1) %>% 
  set_names(c("Player", "17/18"))

#16/17
url <- "https://fbref.com/en/squads/b8fd03ef/2016-2017/Manchester-City-Stats"

#Grab html table
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
data_1617 <- as.data.frame(html_table(temp[1])) %>% 
  select(c(1,9))%>% 
  slice(-1) %>% 
  set_names(c("Player", "16/17"))

#Bring it all together
Citygoals <- merge(data_2122, data_2021, all=TRUE) %>% 
  merge(data_1920, all=TRUE) %>% 
  merge(data_1819, all=TRUE) %>% 
  merge(data_1718, all=TRUE) %>% 
  merge(data_1617, all=TRUE) %>% 
  gather(Year, Goals, c(2:7)) %>% 
  filter(!Player %in% c("Squad Total", "Opponent Total")) %>% 
  mutate(Goals=as.numeric(Goals)) %>% 
  filter(!is.na(Goals) & Goals>0)

Citydata <- merge(Citygoals, Cityages, all.x=TRUE)

meanageCity <- Citydata %>% 
  filter(!is.na(Goals)) %>% 
  group_by(Year) %>% 
  summarise(meanage=weighted.mean(Age, Goals, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(flag="yes")

agg_tiff("Outputs/CityGoalsLexis.tiff", units="in", width=6, height=9, res=500)
ggplot(Citydata %>% filter(!is.na(Goals) & Goals>=5))+
  geom_line(aes(x=Year, y=Age, group=Player), 
            colour="Grey80")+
  geom_point(aes(x=Year, y=Age, size=Goals), 
             shape=21, fill="White", colour="White")+
  geom_point(aes(x=Year, y=Age, size=Goals), 
             shape=21, fill="SkyBlue", alpha=0.5)+
  geom_line(data=meanageCity, aes(x=Year, y=meanage, group=flag), colour="Black")+
  coord_equal()+
  theme_custom()
dev.off()

combined <- bind_rows(LFCdata %>% mutate(Club="Liverpool"),
                      Citydata %>% mutate(Club="Man City")) %>% 
  mutate(Date=as.Date("2016-09-01")+years(as.numeric(substr(Year,1,2))-16))

highlights <- combined %>% 
  filter(Player=="Mohamed Salah" & Year=="18/19" |
           Player=="Sadio Mané" & Year=="18/19" |
           Player=="Diogo Jota" & Year=="20/21" |
           Player=="Raheem Sterling" & Year=="17/18" |
           Player=="Sergio Agüero" & Year=="19/20")

agg_png("Outputs/LVMCGoalsLexis.png", units="in", width=7.5, height=8, res=800, background="#FFF1E0")
ggplot(combined %>% filter(!is.na(Goals) & Goals>=5 & Year!="15/16"),
       aes(x=Date, y=Age, group=Player, fill=Club, colour=Club))+
  geom_line(aes(x=Date, y=Age, group=Player), colour="Grey85")+
  geom_point(aes(x=Date, y=Age, group=Player, fill=Club, colour=Club, size=Goals), 
             shape=21, fill="#FFF1E0")+
  geom_point(aes(x=Date, y=Age, group=Player, fill=Club, colour=Club,size=Goals), 
             shape=21, alpha=0.7)+ 
  geom_point(data=highlights,
             aes(x=Date, y=Age, group=Player, size=Goals), shape=21, fill=NA, colour="Black", stroke=0.8)+
  scale_x_date(name="Season", breaks=seq.Date(from=as.Date("2016-09-01"), by="years", length.out=6),
               labels=c("16/17", "17/18", "18/19", "19/20", "20/21", "21/22"))+
  scale_y_continuous(name="Player Age")+
  scale_fill_manual(values=c("#e05655", "#99bedd"), guide="none")+
  scale_colour_manual(values=c("#e05655", "#99bedd"), guide="none")+
  theme_custom()+
  theme(plot.background=element_rect(fill="#FFF1E0", colour="#FFF1E0"),
        panel.background=element_rect(fill="#FFF1E0"), text=element_text(family="Roboto"),
        plot.subtitle=element_markdown(), plot.caption = element_text(hjust = 0),
        legend.background=element_rect(fill="#FFF1E0", colour="#FFF1E0"),
        panel.grid.major.y=element_line(colour="Grey91"))+
  labs(title="Man City share the goals around more then Liverpool",
       subtitle="League goals scored per player for **<span style='color:#99bedd;'>Man City</span>** and **<span style='color:#e05655;'>Liverpool</span>** in the Guardiola/Klopp era.<br>Only seasons in which a player scored at least 5 goals are shown.<br><br>",
       caption=" \nSource: data from LFCstats, Wikipedia, worldfootball.net and fbref.com\nPlot by @VictimOfMaths")+
  geom_text(aes(x=as.Date("2020-09-01"), y=32.7, 
                label="Bubbles represent player's goals\nin one season, this is the\nrelentless Sergio Agüero"),
                colour="Grey40")+
  geom_text(aes(x=as.Date("2020-09-01"), y=31), label="Lines conect a player\nacross seasons",
            colour="Grey40")+
  geom_richtext(aes(x=as.Date("2020-03-01"), y=26.5, 
                    label="**<span style='color:#e05655;'>Liverpool</span>** have relied heavily on<br>Salah and Mané for their goals..."),
                fill=NA, colour="Grey40", label.colour=NA, nudge_x=-0.4)+
  geom_richtext(aes(x=as.Date("2017-09-01"), y=24, 
                    label="Raheem Sterling has been<br>a consistent goal threat<br>for **<span style='color:#99bedd;'>Man City</span>**"),
                fill=NA, colour="Grey40", label.colour=NA)+
  geom_text(aes(x=as.Date("2021-09-01"), y=23, 
                    label="...but Diogo Jota has added a\nnew edge to their front line"),
                colour="Grey40")+
  coord_cartesian(clip="off")+
  geom_curve(x = as.Date("2019-11-15"), y = 32.8, xend = as.Date("2019-09-01"), yend = 31.5, 
             color = "Grey40", curvature=0.3, arrow = arrow(length = unit(0.01, "npc")))+
  geom_curve(x = as.Date("2021-03-01"), y = 30.9, xend = as.Date("2021-06-15"), yend = 30.35, 
             color = "Grey40", curvature=-0.3, arrow = arrow(length = unit(0.01, "npc")))+
  geom_curve(x = as.Date("2019-03-01"), y = 26.4, xend = as.Date("2018-10-15"), yend = 26.2, 
             color = "Grey40", curvature=-0.2, arrow = arrow(length = unit(0.01, "npc")))+
  geom_curve(x = as.Date("2017-07-01"), y = 23.4, xend = as.Date("2017-08-01"), yend = 22.9, 
             color = "Grey40", curvature=0.3, arrow = arrow(length = unit(0.01, "npc")))+
  geom_curve(x = as.Date("2021-02-01"), y = 23.4, xend = as.Date("2020-10-01"), yend = 23.8, 
             color = "Grey40", curvature=0.3, arrow = arrow(length = unit(0.01, "npc")))
  
dev.off()
