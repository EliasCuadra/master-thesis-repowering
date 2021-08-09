########################################################################
#Analysis of rotor diameter, hub heights and electricity yield per area#
########################################################################

Sys.setenv(LANG = "en")
pacman::p_load(data.table, tidyverse, magrittr, leaflet, htmltools,
               htmlwidgets)

#import data
read.csv("results_of_distance_estimation")

#remove WT's that have distance greater than 1000 m equal to zero 
wts_rlp_filtered <- filter(wts_rlp_distances, nearest < 1000 & nearest != 0)

#omit NA values 
wts_rlp_filtered <- na.omit(wts_rlp_filtered)

#create colum with mwh and area
wts_rlp_filtered <- wts_rlp_filtered %>%
  mutate(menge_mwh = round(menge_kwh/1000)) %>% 
  mutate(area_m2 = round(nearest^2)) %>% 
  mutate(area_km2 = area_m2/1000000) %>% 
  mutate(kwh_m2 = menge_kwh/area_m2) %>% 
  mutate(d = round(nearest/rotor_m))

#filter those with electricity yield smaller than 10 Mwh and small distance 
wts_rlp_filtered <- filter(wts_rlp_filtered, menge_mwh > 20)
wts_rlp_filtered <- filter(wts_rlp_filtered, nearest > 30)

#filter out with electricity yield per m2 above 100 kwh/m2 
wts_rlp_filtered <- filter(wts_rlp_filtered, kwh_m2 < 100)

#change date formate
attach(wts_rlp_filtered)
wts_rlp_filtered$inbetriebnahme <- as.Date(wts_rlp_filtered$inbetriebnahme, "%Y-%m-%d")


#create linear model
lm_e_yield_per_area <- lm(wts_rlp_filtered$kwh_m2 ~ wts_rlp_filtered$inbetriebnahme)
summary(lm_e_yield_per_area)
plot(lm_e_yield_per_area)


#calculate average distance in relation to rotor diameter
mean(wts_rlp_filtered$d)
#4.983264


#calculate mean distance again and area consumption with average
mean(wts_rlp_filtered$nearest)
#~417.3347m that means 174168.3 m2 or 0.1741683 km2 per WT
#area of RLP 19847
#total are consumption with 1702 WT's is about 296434447m2 and 296.4344km2
#that is 1.493598 % of RLP
#with a total electricity yield of 6,782 TWh that means 22.87858kWh/m? 
#constant power output of 2.61022 W/m2
#wind speed with 6.307671


#calculate total mean electricity yield per m2
mean(wts_rlp_filtered$kwh_m2)
#~23.3kwh/m2
#~2.6 W/m2
x <- 298.0769
x^(1/3)
#wind speed of 6.679995 m/s

#calculate area consumption of 22 TWh with ~23.3 kWh/m2
#22*(1e+09kwh/23.3 kwh/m2) = 943422522m2 = 943.4225km2 = 4.753477 % = 3.168985 times the current area

#calculate area consumption of 20 TWh with mean of WT's built after 2010
wts_rlp_filtered_2010 <- filter(wts_rlp_filtered, inbetriebnahme > "2010-01-01")
mean(wts_rlp_filtered_2010$nearest)
#474.8368
mean(wts_rlp_filtered_2010$d)
#4.671875
mean(wts_rlp_filtered_2010$area_m2)
#243284.6
mean(wts_rlp_filtered_2010$area_km2)
#0.2432846
mean(wts_rlp_filtered_2010$kwh_m2)
#27.41782 kWh/m2
#3.128103 W/m2


#calculate area consumption of 22 TWh with ~24.7 kWh/m2
#this equals 802397857m? = 802.3979km? = 4.042918 % = 2.695279 times the current area


p_e_yield_per_area <- ggplot() +
  geom_point(data = wts_rlp_filtered, aes(x=inbetriebnahme, y=kwh_m2), size = 0.4, colour = "#0051fd") +
  geom_smooth(data = wts_rlp_filtered, aes(x=inbetriebnahme, y=kwh_m2, colour = "2019"), method=lm, se=TRUE, fullrange = TRUE, size = 0.5)  +
  theme_light() +
  ylim(0,80) +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  xlab("Commissioning date") +
  ylab("Electricity yield per area [kWh/m²a]") +
  theme( axis.text=element_text(size=11),
         axis.title=element_text(size=12),
         plot.title = element_text(size=14),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 27.4, linetype = 'dashed', size = 0.25) +
  annotate(geom="text",x=as.Date("2025-01-01"),
           y=25,label="Mean of ~ 27.4 kWh/m²a after 2010", size = 2.5) +
  geom_hline(yintercept = 33.5, linetype = 'dashed', size = 0.25) +
  annotate(geom="text",x=as.Date("1995-01-01"),
           y=35.5,label="Predicted mean of  ~ 33.5 kWh/m²a in 2021", size = 2.5) +
  geom_hline(yintercept = 40.5, linetype = 'dashed', size = 0.25) +
  annotate(geom="text",x=as.Date("1995-01-01"),
           y=42.5,label="Predicted mean of ~ 40.5 kWh/m²a in 2030", size = 2.5) +
  annotate(geom="text",x=as.Date("2025-01-01"),
           y=5,label= "R² = ~ 11 % \n p-value << 0.001") +
  scale_colour_manual(name = "Joined data from MaStR and amprion", values="#0051fd")

#plot
p_e_yield_per_area +  theme(legend.position = c(0.7,0.9))

#save plot
ggsave("e_yield_per_m2_over_commissioning_date.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)


#the model assumes an average of around 33.5 kWh/m2a in 2021
#3.822019 W/m2
y <- 365.3846
y^(1/3)
#wind speed of 7.149079 m/s
##calculate area consumption of 22 TWh
#This requires an area of 656716418 m2 = 656.7164 km2 = 3.308895 % = 2.20593 times as much WT's

#if average is 40.5 kwh/m2a in 2030
#4.62065 W/m2
#wind speed required of 
z <- 442.3077
z^(1/3)
#7.619179 m/s
#area consumtion with demand of 22 TWh
# 543209877 m2 = 543.2099 km2 = 2.736987 % = 1.824658 times as much