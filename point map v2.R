library(tidyverse)
library(ggplot2)
library(ggmap)
register_google(key = "AIzaSyAt3EKW6-yQy3ldijCeqnOIosDH3knJBUw")
library(xlsx)
# set up a null map and get the US border
mp<-NULL
mapus<-borders("state",colour = "gray50",fill="white") 
mp<-ggplot() + mapus + xlim(-125,-65)

# import data and extract the latlong information
setwd("C:/Users/Lenovo/Desktop/graph data")

year_list <- c(seq(1938,1943))
for (i in year_list) {
  # read REA and stop cooridnates
  map_site <- read.xlsx(paste0("REA_site/REA site_", i ,".xls"),sheetName= 'Sheet1')
  map_stop <- read.xlsx(paste0("efe_stop/REA efes stops_", i, ".xls"),sheetName= 'Sheet1')

  lat_site <- map_site$latitude
  lon_site <- map_site$longitude
  lat_stop <- map_stop$latitude
  lon_stop <- map_stop$longitude

  map_site$distri <- as.character(map_site$distri)

  map_site$distri[which(map_site$distri==0)] <-"Distributor"
  map_site$distri[which(map_site$distri==1)] <-"Generator"
  map_site$distri[which(map_site$distri==2)] <-"Generator&Distributor"

# Only REA borrowers (distinguished by distributor or generator)
mp2<- mp + 
  geom_point(data= map_site, aes(x=lon_site, y=lat_site, color=distri), size=2.5, alpha=0.7, position = "jitter")+
  labs(x = "",
       y = "",
       title = paste0("Location of REA Borrowers ", i),
       subtitle= "Distributor v.s. Generator")+
  theme(plot.title = element_text(hjust = 0.5, size=24))+
  theme(plot.subtitle = element_text(hjust = 0.5, size=18))+
  scale_color_manual(values=c("#E69F00", "#56B4E9", "#999999"))+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=14))+
  theme(legend.justification=c(1,0), legend.position=c(1,0))+
  theme(panel.background = element_rect(fill = "white", colour = "white"))

ggsave(paste0("REA_loc_distri",i,".png"), plot = mp2, width = 9.6, height = 6.4, units = 'in', dpi = 600)


# REA borrowers + EFE stop
mp3 <- mp +
  geom_point(data= map_site, aes(x=lon_site, y=lat_site), color="red", size=2.5, alpha=0.7, shape=20)+
  geom_point(data= map_stop, aes(x=lon_stop, y=lat_stop), color="blue", size=4, alpha=0.7, shape=24)+
  labs(x = "",
       y = "",
       title = paste0("Location of REA Borrowers and EFE Stop ", i),
       caption = "Note: Red circle denotes REA sites, while blue traiangle denotes EFE stop")+
  theme(plot.title = element_text(hjust = 0.5, size=24))+
  theme(plot.caption = element_text(hjust = 0, size=10))+
  theme(panel.background = element_rect(fill = "white", colour = "white"))

ggsave(paste0("site_stop",i,".png"), plot = mp3, width = 9.6, height = 6.4, units = 'in', dpi = 600)

}
