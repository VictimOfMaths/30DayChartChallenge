rm(list=ls())

library(curl)
library(sf)
library(tidyverse)
library(ggfx)
library(rnaturalearth)
#remotes::install_github("ropensci/rnaturalearthhires")
library(ggtext)
library(ragg)
library(png)

#Download shapefile of Shag distribution from RSPB API
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/datasets/3eed1125f0ee42c0aa7583c52f720f0f_0.zip?outSR=%7B%22latestWkid%22%3A4326%2C%22wkid%22%3A4326%7D"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
shapefile <- st_read(file.path(temp2, name))

#Download shapefile of UK & Ireland from RNaturalEarth
map <- ne_countries(country=c("united kingdom", "ireland", "isle of man"), 
                    scale=10, returnclass="sf")

img <- readPNG("Shag.png")
img <- grid::rasterGrob(img, interpolate=TRUE)

textbox <- data.frame(label="The European Shag is a large seabird which is easily confused with the European Cormorant.
                     They can be distinguished by the Shag's habit of perching with its wings half-open. 
                     Cormorants are also much more common and can be found on inland lakes as well as around the coast.",
                     x=-11.8, y=56.3)

agg_tiff("8Animals.tiff", units="in", width=8, height=10, res=500)
ggplot()+
  geom_sf(data=map, aes(geometry=geometry), fill="#80C18F", colour="darkgreen", size=0.1)+
  with_blur(geom_sf(data=shapefile %>% filter(level_==0.95),
                    aes(geometry=geometry), colour=NA, fill="Red", alpha=0.9),
            sigma=unit(0.7, 'mm'))+
  theme_void()+
  theme(plot.background=element_rect(fill="skyblue", colour=NA),
        panel.background=element_rect(fill="skyblue", colour=NA),
        text=element_text(family="Lora"),
        plot.title=element_text(size=rel(2.2)),
        plot.subtitle=element_markdown(size=rel(1.3)),
        plot.margin = margin(10, 35, 10, 25))+
  labs(title="(Almost) every cormorant is a potential shag",
       subtitle="<span style='color:red;'>Home distribution</span> of the European Shag around the British Isles",
       caption="Data from RSPB | Plot by @VictimOfMaths")+
  annotation_custom(img, ymin=58.3, xmin=-17, ymax=61.4, xmax=-7)+
  geom_textbox(data=textbox, aes(x=x, y=y, label=label), family="Lora", fill="lightskyblue2")
dev.off()
