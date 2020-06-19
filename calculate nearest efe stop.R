library(xlsx)
library(raster)
library(spatstat)
library(maptools)
library(dismo) 
library(tidyverse)
library(rgdal)

setwd("C:/Users/Lenovo/Desktop/graph data")
site <- read.xlsx(paste0("REA_site/REA site allyear.xls"),sheetName= 'Sheet1')

year_list <- c(seq(1938,1943))
for (i in year_list) {
  # read REA and stop cooridnates
  stop <- read.xlsx(paste0("efe_stop/REA efes stops_",i,".xls"),sheetName= 'Sheet1')
  
  # NAD 1983 is the often used geographic system for us. 
  # epsg:26915 is the often used projection system for us. 
  # See https://epsg.io/26915 for more information
  site.cord <- SpatialPoints(cbind(site$longitude, site$lat), proj4string = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"))
  site.UTM <- spTransform(site.cord, CRS("+init=epsg:26915"))
  
  stop.cord <- SpatialPoints(cbind(stop$longitude, stop$lat), proj4string = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"))
  stop.UTM <- spTransform(stop.cord, CRS("+init=epsg:26915"))
  
  # Note: this converts spatial data to polar coordinate system
  X<- as.ppp(site.UTM)
  Y<- as.ppp(stop.UTM)
  
  dist <- nncross(X,Y)
  
  dist$which <- stop$stop_id[dist$which]
  dist$system <- site$system
  dist$min_stop_city <- stop$city[dist$which]
  dist$min_stop_county <- stop$county[dist$which]
  dist$min_stop_state <- stop$state[dist$which]
  
  dist <- dist %>% 
    rename(min_stop_dist = dist) %>%
    rename(min_stop_id= which) %>%
    mutate(min_stop_dist = (min_stop_dist/1000)*0.621371192) # convert distance to miles
  write.xlsx(dist, paste0("site_stop_mindist",i,".xlsx"), row.names = FALSE)
}