library(tidyverse)

library(ggplot2)
library(ggmap)
register_google(key = "Your API Key") # Get From Google Cloud Console, you can find details in the "Google API.txt"

# set up a null map and get the US border
mp<-NULL
mapus<-borders("state",colour = "gray50",fill="white") 
mp<-ggplot() + mapus + xlim(-125,-65)

# import data and extract the latlong information
setwd("C:/Users/Lenovo/Desktop")
map1941 <- read_csv("1941_mapdata.csv")
latlon <- map1941 %>% 
  select(system, latitude, longitude, st_code) %>%
  filter(st_code!="AK")
lat <- latlon$latitude
lon <- latlon$longitude

mp2<- mp + 
      geom_point(aes(x=lon,y=lat),size=3, alpha=0.6, color="darkorange")+
      labs(x = "",
       y = "",
       title = "Location of REA Borrowers 1941")+
      theme(legend.position = "none")+
      theme(plot.title = element_text(hjust = 0.5))
mp2
ggsave("REA_loc.png", plot = mp2, dpi = 600)