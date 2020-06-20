library(sp)
library(RColorBrewer)
library(tidyverse)

library(rgdal)
library(ggplot2)
library(dplyr)

setwd("C:/Users/Lenovo/Desktop/graph data")

# import data and aggregate on state level
agg_state <- read_csv("state_data.csv")

# import shape file and convert it to data frame
state_map <- readOGR("shapefile of US/cb_2018_us_state_500k.shp")
state_map@data$id = rownames(state_map@data)
state_map.f <- fortify(state_map)
state_map.df = left_join(state_map.f, state_map@data, by="id")

mp_us <- state_map.df %>%
  filter(STUSPS!="AK") %>%
  filter(STUSPS!="HI") %>%
  filter(STUSPS!="AS") %>%
  filter(STUSPS!="GU") %>%
  filter(STUSPS!="MP") %>%
  filter(STUSPS!="PR") %>%
  filter(STUSPS!="VI")
# Plot Map 
year_list <- c(seq(1941,1943))
for (i in year_list) {
tempf<- agg_state %>% filter(year==i)
# Merge with information of state-level mean allotment          
plot_data <- left_join(mp_us, tempf, by=c('STUSPS' = 'st_code')) %>%
  filter(!is.na(long))

map <- ggplot(data = plot_data, aes(x = long, y = lat, label = NAME
                                    , group = group))
mp2 <- map + 
  geom_polygon(aes(fill = allo_perfarm), color = 'grey', size = 0.1) +
  coord_fixed(1.3)+
  scale_fill_gradient(low = "#ffffb2", high = "#bd0026", guide = "colorbar")+
  labs(x = "",
       y = "",
       title = paste0("REA Loan Distribution (Per Farm) ",i),
       subtitle= "By state")+
  theme(plot.title = element_text(hjust = 0.5, size=24))+
  theme(plot.subtitle = element_text(hjust = 0.5, size=18))+
  guides(fill=guide_legend(title="Loans Per Farm ($)"))+
  theme(legend.justification=c(1,0.05), legend.position=c(1,0.05))+
  theme(panel.background = element_rect(fill = "white", colour = "white"))

ggsave(paste0("Loan_distri_PerFarm ", i, ".png"), plot = mp2, width = 9.6, height = 6.4, units = 'in', dpi = 600)

mp4 <- map + 
  geom_polygon(aes(fill = allo_perrurpop), color = 'grey', size = 0.1) +
  coord_fixed(1.3)+
  scale_fill_gradient(low = "#f2f0f7", high = "#54278f", guide = "colorbar")+
  labs(x = "",
       y = "",
       title = paste0("Loan Distribution (Per Rural Farmer)", i),
       subtitle= "By state")+
  theme(plot.title = element_text(hjust = 0.5, size=24))+
  theme(plot.subtitle = element_text(hjust = 0.5, size=18))+
  guides(fill=guide_legend(title="Loans Per \n Rural Farmer ($)"))+
  theme(legend.justification=c(1,0.05), legend.position=c(1,0.05))+
  theme(panel.background = element_rect(fill = "white", colour = "white"))

ggsave(paste0("Loan_distri_PerrurfarmPop ",i, ".png"), plot = mp4, width = 9.6, height = 6.4, units = 'in', dpi = 600)
}

