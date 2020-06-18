library(tidyverse)
library(ggplot2)

setwd("C:/Users/Lenovo/Desktop/graph data")
system_3843 <- read_csv("var_3843.csv")

grey_theme <- theme(axis.text.x = element_text(colour = "grey20", size = 12, hjust = 0.5, vjust = 0.5),
                    axis.text.y = element_text(colour = "grey20", size = 12),
                    text = element_text(size = 14))

## Miles Energized
me_by_year <- system_3843 %>%
  group_by(year) %>%
  filter(!is.na(op_me)) %>%
  summarize(mean_me= mean(op_me))
                       
g1 <-  system_3843 %>% 
  filter(!is.na(op_me)) %>%
  ggplot(aes(y=op_me, x= factor(year)))
g2 <- g1 +
  geom_violin() +
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="red", fill="red") +
  stat_summary(fun=mean, geom="line", aes(group=1), linetype = "dashed", color="blue", size=0.8)+
  geom_text(data = me_by_year, aes(label = round(mean_me, 1), y= mean_me+1), position = position_dodge(width = 1),
            vjust = -0.5, size = 4)+ 
  coord_cartesian(ylim = c(0, 1200))+
  labs(x = "Year",
       y = "Miles Energized",
       title = "Miles Energized by REA Borrowers")+
  theme_classic()
g2 <- g2+ theme(plot.title = element_text(hjust = 0.5, size=18))+
  grey_theme

ggsave("Miles Energized.png", plot = g2, width = 9.6, height = 6.4, units = 'in', dpi = 600)


## Consumer per mile (Consumer Connected)
cpm_by_year <- system_3843 %>%
  group_by(year) %>%
  filter(!is.na(op_cpm)) %>%
  summarize(mean_cpm= mean(op_cpm))

g3 <-  system_3843 %>% 
  filter(!is.na(op_cpm)) %>%
  ggplot(aes(y=op_cpm, x= factor(year)))
g4 <- g3 +
  geom_violin() +
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="red", fill="red") +
  stat_summary(fun=mean, geom="line", aes(group=1), linetype = "dashed", color="blue", size=0.8)+
  geom_text(data = cpm_by_year, aes(label = round(mean_cpm, 1), y= mean_cpm+1))+ 
  coord_cartesian(ylim = c(1, 6))+
  labs(x = "Year",
       y = "Customer Per Mile",
       title = "Customer connected by REA Borrowers (Per Mile)")+
  theme_classic()
g4 <- g4+ theme(plot.title = element_text(hjust = 0.5, size=18))+
  grey_theme

ggsave("Customer Per Mile.png", plot = g4, width = 9.6, height = 6.4, units = 'in', dpi = 600)


## KWH per residual customer (Energy Consumption)
kwh_pr_by_year <- system_3843 %>%
  group_by(year) %>%
  filter(!is.na(op_kwh_pr)) %>%
  summarize(mean_kwh_pr= mean(op_kwh_pr))

g5 <-  system_3843 %>% 
  filter(!is.na(op_kwh_pr)) %>%
  ggplot(aes(y=op_kwh_pr, x= factor(year)))
g6 <- g5 +
  geom_violin() +
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="red", fill="red") +
  stat_summary(fun=mean, geom="line", aes(group=1), linetype = "dashed", color="blue", size=0.8)+
  geom_text(data = kwh_pr_by_year, aes(label = round(mean_kwh_pr, 1), y= mean_kwh_pr+1), position = position_dodge(width = 1),
            vjust = -0.5, size = 4)+ 
  coord_cartesian(ylim = c(20, 160))+
  labs(x = "Year",
       y = "Energy Consumed Per Residual Customer (KWH)",
       title = "Energy Consumption Per Residual Customer")+
  theme_classic()
g6 <- g6+ theme(plot.title = element_text(hjust = 0.5, size=18))+
  grey_theme

ggsave("Energy Consumption Per Customer.png", plot = g6, width = 9.6, height = 6.4, units = 'in', dpi = 600)

## Operation Revenue Per Mile (Profitablity of REA Program)
rpm_by_year <- system_3843 %>%
  group_by(year) %>%
  filter(!is.na(op_rpm)) %>%
  summarize(mean_rpm= mean(op_rpm))

g7 <-  system_3843 %>% 
  filter(!is.na(op_rpm)) %>%
  ggplot(aes(y=op_rpm, x= factor(year)))
g8 <- g7 +
  geom_violin() +
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="red", fill="red") +
  stat_summary(fun=mean, geom="line", aes(group=1), linetype = "dashed", color="blue", size=0.8)+
  geom_text(data = rpm_by_year, aes(label = round(mean_rpm, 1), y= mean_rpm+1), position = position_dodge(width = 1),
            vjust = -0.5, size = 4)+ 
  coord_cartesian(ylim = c(2, 25))+
  labs(x = "Year",
       y = "Revenue Per Mile ($)",
       title = "Operation Revenue Per Mile")+
  theme_classic()
g8 <- g8+ theme(plot.title = element_text(hjust = 0.5, size=18))+
  grey_theme

ggsave("Operation Revenue Per Mile.png", plot = g8, width = 9.6, height = 6.4, units = 'in', dpi = 600)