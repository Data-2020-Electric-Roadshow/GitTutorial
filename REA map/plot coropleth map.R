library(sp)
library(RColorBrewer)
library(tidyverse)

library(rgdal)
library(ggplot2)
library(dplyr)

setwd("C:/Users/Lenovo/Desktop")

# import data and aggregate on state level
map1941 <- read_csv("1941_mapdata.csv")

allo_all <- map1941 %>%
            filter(state!="Alaska") %>%
            select(system, latitude, longitude, allo_all, state, st_code)

allo_mean <- aggregate(allo_all~st_code,allo_all,mean)

# import shape file and convert it to data frame
state_map <- readOGR("shapefile/cb_2018_us_state_500k.shp")
state_map@data$id = rownames(state_map@data)
state_map.f <- fortify(state_map)
state_map.df = left_join(state_map.f, state_map@data, by="id")

mp_us <- state_map.df %>%
          filter(between(long,-125,0)&between(lat,24,50)&STUSPS!="AK")

# Merge with information of state-level mean allotment          
plot_data <- right_join(mp_us, allo_mean, by=c('STUSPS' = 'st_code')) %>%
              filter(!is.na(long))

# Plot Map                
map <- ggplot(data = plot_data, aes(x = long, y = lat, label = NAME
                                    , group = group))
mp2 <- map + 
  geom_polygon(aes(fill = allo_all), color = 'grey', size = 0.1) +
  coord_fixed(1.3)+
  scale_fill_gradient(high = "#f03b20", low = "#ffeda0", guide = "colorbar")+
  theme(legend.justification=c(0,0), legend.position=c(0,0))+
  labs(x = "",
       y = "",
       title = "Mean Allotment Distribution By State")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill=guide_legend(title="Mean Allotment($)"))
mp2
