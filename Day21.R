rm(list=ls())

library(tidyverse)
library(lubridate)
library(scales)
library(extrafont)
library(ragg)

#Enter the data taken from 
#http://www.climbing-records.com/2021/04/alaphilippe-and-roglic-break-huy-record.html
data <- data.frame(year=c(1985, 1986, 1988, 1989, 1991, 1992, 1993, 1994, 1996, 1997, 1998, 1999, 
                          2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 
                          2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021),
                   time=ms(c("03:31", "03:42", "03:47", "03:31", "03:52",
                               "03:15", "03:39", "03:12", "03:03", "03:10",
                               "03:11", "03:36", "03:00", "02:59", "03:20",
                               "02:46", "03:01", "02:51", "02:45", "03:02",
                               "02:46", "02:45", "02:44", "03:00", "02:48",
                               "02:41", "02:49", "02:52", "02:53", "02:48",
                               "02:50", "02:45", "02:40")))

#Is this really the best way to generate a HMS sequence?
lines <- data.frame(mins=rep(c(2,3), each=12), secs=rep(seq(0,55,by=5), times=2)) %>% 
  mutate(time=ms(paste0(mins, ":", secs)))

agg_tiff("21Downwards.tiff", units="in", width=8, height=7, res=500)
ggplot()+
  geom_ribbon(data=data, aes(x=year, ymax=time, ymin=hms("00:02:00")), fill="#f9e609")+
  geom_segment(data=lines, aes(x=1985, xend=2021, y=time, yend=time), colour="#e0c10a")+
  #This feels like a slightly hacky approach to keeping the lines only in the fill, but ¯\_(ツ)_/¯
  geom_ribbon(data=data, aes(x=year, ymin=time, ymax=hms("00:05:00")), fill="White")+
  #Replicate the black bar x-axis legend
  geom_rect(aes(xmin=1985, xmax=2021, ymin=ms("01:55"), ymax=ms("02:00")), fill="black")+
  geom_text(aes(x=seq(1990, 2020, by=5), y=ms("01:56"), label=seq(1990, 2020, by=5)), 
            colour="#f9e609", vjust=0, fontface="bold", family="Open Sans")+
  #Add in the vertical dashed lines
  geom_segment(aes(x=2021, xend=2021, y=ms("01:55"), yend=ms("03:55")))+
  annotate("text", x=2021, y=ms("03:57"), angle=90, label="Alaphilippe/Roglič 2m40s", hjust=0,
           family="Open Sans", size=rel(3.5))+
  geom_segment(aes(x=2014, xend=2014, y=ms("02:00"), yend=ms("03:55")), linetype=2)+
  annotate("text", x=2014, y=ms("03:57"), angle=90, label="Valverde 2m41s", hjust=0,
           family="Open Sans", size=rel(3.5))+
  geom_segment(aes(x=2011, xend=2011, y=ms("02:00"), yend=ms("03:55")), linetype=2)+
  annotate("text", x=2011, y=ms("03:57"), angle=90, label="Gilbert 2m44s", hjust=0,
           family="Open Sans", size=rel(3.5))+
  geom_segment(aes(x=2007, xend=2007, y=ms("02:00"), yend=ms("03:55")), linetype=2)+
  annotate("text", x=2007, y=ms("03:57"), angle=90, label="Rebellin 2m45s", hjust=0,
           family="Open Sans", size=rel(3.5))+
  geom_segment(aes(x=2004, xend=2004, y=ms("02:00"), yend=ms("03:55")), linetype=2)+
  annotate("text", x=2004, y=ms("03:57"), angle=90, label="Rebellin 2m46s", hjust=0,
           family="Open Sans", size=rel(3.5))+
  geom_segment(aes(x=2002, xend=2002, y=ms("02:00"), yend=ms("03:55")), linetype=2)+
  annotate("text", x=2002, y=ms("03:57"), angle=90, label="Frigo/Etxebarria/Boogerd 2m59s", hjust=0,
           family="Open Sans", size=rel(3.5))+
  geom_segment(aes(x=1996, xend=1996, y=ms("02:00"), yend=ms("03:55")), linetype=2)+
  annotate("text", x=1996, y=ms("03:57"), angle=90, label="Armstrong 3m03s", hjust=0,
           family="Open Sans", size=rel(3.5))+
  geom_segment(aes(x=1992, xend=1992, y=ms("02:00"), yend=ms("03:55")), linetype=2)+
  annotate("text", x=1992, y=ms("03:57"), angle=90, label="Furlan 3m15s", hjust=0,
           family="Open Sans", size=rel(3.5))+
  scale_y_time(limits=ms(c("01:55", "05:00")), breaks=ms(c("02:00", "02:20", "02:40",
                                                           "03:00", "03:20", "03:40")),
               labels=c("2min", "2m20s", "2m40s", "3min", "3m20s", "3m40s"), name="")+
  geom_segment(aes(x=1985, xend=1985, y=ms("01:55"), yend=ms("03:55")))+
  annotate("text", x=1985, y=ms("03:57"), angle=90, label="Criquielion 3m31s", hjust=0,
           family="Open Sans")+
  coord_cartesian(clip="off")+
  theme_classic()+
  theme(axis.line=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.x=element_blank(), axis.text.y=element_text(colour="Black"),
        text=element_text(family="Open Sans"), plot.title=element_text(face="bold", size=rel(1.6)))+
  labs(title="Cycling records on the Mur de Huy keep falling",
       subtitle="Fastest ascents of the famous final hill in the annual Flèche Wallone race",
       caption="Data from www.climbing-records.com | Plot by @VictimOfMaths")
dev.off()

