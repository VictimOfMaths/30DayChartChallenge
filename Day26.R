rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(extrafont)
library(ragg)

#Download data from NRS website
temp <- tempfile()
url <- "https://www.nrscotland.gov.uk/files//statistics/drug-related-deaths/2019/drug-related-deaths-19-tabs-figs.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data <- read_excel(temp, sheet="1 - summary", range="A12:F31", col_names=FALSE) %>% 
  rename(year=`...1`, DRD=`...4`, LowerCI=`...5`, UpperCI=`...6`) %>% 
  select(-c(2, 3))

#I'm certain there is a more elegant solution to this, but this works, so ¯\_(ツ)_/¯
n_ribbons <- 100 #This is probably overkill

#Generate the palette
colfunc <- colorRampPalette(c("DarkRed", "white"))
Gradpal <- colfunc(n_ribbons)

#generate the max/min values for each ribbon
for (i in 1:n_ribbons){
  data[paste0("ylo", i)] <- data$DRD-(data$DRD-data$LowerCI)*(i)/n_ribbons
  data[paste0("yhi", i)] <- data$DRD+(data$UpperCI-data$DRD)*(i)/n_ribbons
}

#plot the central ribbon
plot <- ggplot()+
  geom_ribbon(data=data, aes(x=year, ymin=ylo1, ymax=yhi1), fill=Gradpal[1])+
  scale_x_continuous(name="Year")+
  scale_y_continuous(limits=c(0,NA), name="Annual Drug-Related Deaths")

#add in the rest
for (i in 1:(n_ribbons-1)){
  loopdata <- select(data, c("year", paste0("ylo", i), paste0("ylo", i+1), 
                             paste0("yhi", i), paste0("yhi", i+1)))
  colnames(loopdata) <- c("year","ymin1", "ymax1", "ymin2", "ymax2")
  
  plot <- plot+
    geom_ribbon(data=loopdata, aes(x=year, ymin=ymin1, ymax=ymax1), fill=Gradpal[i+1])+
    geom_ribbon(data=loopdata, aes(x=year, ymin=ymin2, ymax=ymax2), fill=Gradpal[i+1])
}

#add the central line (if required) and tidy up
plot <- plot+
  #geom_line(data=data, aes(x=year, y=DRD), colour="Black")+
  theme_classic()+
  theme(text=element_text(family="Lato"), plot.title=element_text(face="bold", size=rel(1.4)))+
  labs(title="Drug-Related Deaths in Scotland have almost quadrupled in the last 20 years",
       subtitle="Centered 5-year rolling average number of Drug-Related Deaths in Scotland with modelled 95% Confidence Intervals",
       caption="Data from National Records of Scotland | Plot by @VictimOfMaths")

agg_png("26Trends.png", units="in", width=9, height=7, res=800)
plot
dev.off()