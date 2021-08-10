########################################################################
#Analysis of rotor diameter, hub heights and electricity yield per area#
########################################################################

Sys.setenv(LANG = "en")
pacman::p_load(data.table, tidyverse, magrittr, leaflet, htmltools,
               htmlwidgets)

#import data
wts_rlp_distances <- read.csv(
  "result_of_distance_estimation/wts_rlp_distances.csv")

#change date format
wts_rlp_distances$inbetriebnahme <- as.Date(
  wts_rlp_distances$inbetriebnahme, "%Y-%m-%d")

############################
#Analysis of rotor diameter#
############################
wts_rlp_rotor <- filter(wts_rlp_distances, rotor_m != "NA")
wts_rlp_rotor <- filter(wts_rlp_rotor, rotor_m < 200)
sum(is.na(wts_rlp_rotor$rotor_m))
mean(wts_rlp_rotor$rotor_m)
#86.54686 m

#linear model of rotor diameter over commissioning
lm_rotor <- lm(wts_rlp_rotor$rotor_m ~ wts_rlp_rotor$inbetriebnahme)
summary(lm_rotor)
plot(lm_rotor)

#plot rotor diameter over commissioning date
p_rotor <- ggplot() +
  geom_point(data = wts_rlp_rotor, aes(x=inbetriebnahme, y=rotor_m), 
             size = 0.4, colour = "#4500fe") +
  geom_smooth(data = wts_rlp_rotor, 
              aes(x=inbetriebnahme, y=rotor_m, colour = "2019"), 
              method=lm, se=TRUE, fullrange = TRUE, size = 0.5)  +
  theme_light() +
  ylim(25,170) +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  xlab("Commissioning date") +
  ylab("Rotor diameter [m]") +
  theme( axis.text=element_text(size=11),
         axis.title=element_text(size=12),
         plot.title = element_text(size=14),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  annotate(geom="text",x=as.Date("2025-01-01"),
           y=50,label= "R² = 68.7 % \n p-value << 0.001") +
  scale_colour_manual(name = "Data from MaStR", 
                      values="#4500fe")

#plot
p_rotor +  theme(legend.position = c(0.3,0.9))

#save plot
ggsave("results_of_analysis/rotor_diameter.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)

########################
#Analysis of hub height#
########################
wts_rlp_nabe <- filter(wts_rlp_distances, nabe_m != "NA")
wts_rlp_nabe <- filter(wts_rlp_nabe, nabe_m < 200)
sum(is.na(wts_rlp_nabe$nabe_m))
mean(wts_rlp_nabe$nabe_m)
#110.3489 m

#linear model
lm_nabe <- lm(wts_rlp_nabe$nabe_m ~ wts_rlp_nabe$inbetriebnahme)
summary(lm_nabe)
plot(lm_nabe)

#plot hub height over commissioning date
p_nabe <- ggplot() +
  geom_point(data = wts_rlp_nabe, aes(x=inbetriebnahme, y=nabe_m), 
             size = 0.4, colour = "#0051fd") +
  geom_smooth(data = wts_rlp_nabe, 
              aes(x=inbetriebnahme, y=nabe_m, colour = "2019"), 
              method=lm, se=TRUE, fullrange = TRUE, size = 0.5)  +
  theme_light() +
  ylim(25,220) +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  xlab("Commissioning date") +
  ylab("Hub height [m]") +
  theme( axis.text=element_text(size=11),
         axis.title=element_text(size=12),
         plot.title = element_text(size=14),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  annotate(geom="text",x=as.Date("2025-01-01"),
           y=50,label= "R² = 77.9 % \n p-value << 0.001") +
  scale_colour_manual(name = "Data from MaStR", 
                      values="#0051fd")

#plot
p_nabe  +  theme(legend.position = c(0.3,0.9))

#save plot
ggsave("results_of_analysis/hub_height.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)

########################################
#Analysis of electricity yield per area#
########################################

#remove WT's that have distance greater than 1000 m equal to zero 
wts_rlp_filtered <- filter(wts_rlp_distances, nearest < 1000 & nearest != 0)

#omit NA values 
sum(is.na(wts_rlp_filtered$menge_mwh))
wts_rlp_filtered <- na.omit(wts_rlp_filtered)

#filter those with electricity yield smaller than 10 Mwh and small distance 
wts_rlp_filtered <- filter(wts_rlp_filtered, menge_mwh > 20)
wts_rlp_filtered <- filter(wts_rlp_filtered, nearest > 30)

#filter out with electricity yield per m2 above 100 kwh/m2 
wts_rlp_filtered <- filter(wts_rlp_filtered, kwh_m2 < 100)

#create linear model
lm_e_yield_per_area <- lm(
  wts_rlp_filtered$kwh_m2 ~ wts_rlp_filtered$inbetriebnahme)
summary(lm_e_yield_per_area)
plot(lm_e_yield_per_area)

#calculate average distance in relation to rotor diameter
mean(wts_rlp_filtered$d)
#4.983264

#calculate mean distance again and area consumption with average
mean(wts_rlp_filtered$nearest)
#~417.3347m that means 174168.3 m2 or 0.1741683 km2 per WT
#area of RLP 19847
#total are consumption with 1702 WT's is about 296434447 m2 and 296.4344 km2
#that is 1.493598 % of RLP
#with a total electricity yield of 6,782 TWh that means 22.87858 kWh/m2
#constant power output of 2.61022 W/m2
#calculate wind speed with third root of electricity yield per area divided 
#by 0.016 * 1.3 * 0.5 = 0.0104
#2.61022/0.0104 = 250.9827
#250.9827^(1/3)
#wind speed with 6.307671 m/s required


#calculate actual total mean electricity yield per m2 in data
mean(wts_rlp_filtered$kwh_m2)
#~23.3kwh/m2 close to the theoretical value of 22.87 kWh/m2 from above
#Calculate average power per area with 23.3(1000/8765)
#~2.6583 W/m2
#2.6583/0.0104 = 255.6058
#255.6058^(1/3) = 6.346343 m/s wind speed required
#calculate area consumption of 22 TWh with ~23.3 kWh/m2
#22*(1e+09kwh/23.3 kwh/m2) = 943422522m2 = 943.4225km2 = 4.753477 % 
# = 3.168985 times the current area

#calculate area consumption with mean of WT's built after 2010
wts_rlp_filtered_2010 <- filter(wts_rlp_filtered, inbetriebnahme > "2010-01-01")
mean(wts_rlp_filtered_2010$nearest)
#474.8368 m
mean(wts_rlp_filtered_2010$d)
#4.671875 times d
mean(wts_rlp_filtered_2010$area_m2)
#243284.6 m2
mean(wts_rlp_filtered_2010$area_km2)
#0.2432846 km2
mean(wts_rlp_filtered_2010$kwh_m2)
#27.41782 kWh/m2
#3.128103 W/m2
#3.128103/0.0104 = 300.7791
#300.7791^(1/3) = 6.70012 m/s wind speed required
#calculate area consumption of 22 TWh with ~24.7 kWh/m2
#this equals 802397857m? = 802.3979km? = 4.042918 % 
# = 2.695279 times the current area


#plot electricity yield per area over commissioning date
p_e_yield_per_area <- ggplot() +
  geom_point(data = wts_rlp_filtered, aes(x=inbetriebnahme, y=kwh_m2), 
             size = 0.4, colour = "#00A2ff") +
  geom_smooth(data = wts_rlp_filtered, 
              aes(x=inbetriebnahme, y=kwh_m2, colour = "2019"), 
              method=lm, se=TRUE, fullrange = TRUE, size = 0.5)  +
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
           y=35.5,label="Predicted mean of  ~ 33.5 kWh/m²a in 2021", 
           size = 2.5) +
  geom_hline(yintercept = 40.5, linetype = 'dashed', size = 0.25) +
  annotate(geom="text",x=as.Date("1995-01-01"),
           y=42.5,label="Predicted mean of ~ 40.5 kWh/m²a in 2030",
           size = 2.5) +
  annotate(geom="text",x=as.Date("2025-01-01"),
           y=5,label= "R² = ~ 11 % \n p-value << 0.001") +
  scale_colour_manual(name = "Joined data from MaStR and amprion", 
                      values="#00A2ff")

#plot
p_e_yield_per_area +  theme(legend.position = c(0.7,0.9))

#save plot
ggsave("results_of_analysis/e_yield_per_m2_over_commissioning_date.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)


#the model assumes an average of around 33.5 kWh/m2a in 2021
#3.822019 W/m2
#3.822019/0.0104 = 367.5018
#367.5018^(1/3) = 7.16286 m/s wind speed required
#wind speed of 7.16286 m/s
##calculate area consumption of 22 TWh
#This requires an area of 656716418 m2 = 656.7164 km2 = 3.308895 % 
#= 2.20593 times as much WT's

#if average is 40.5 kwh/m2a in 2030
#4.62065 W/m2
#4.62065/0.0104 = 444.2933
#444.2933^(1/3) = 7.630563 m/s wind speed required
#area consumtion with demand of 22 TWh
# 543209877 m2 = 543.2099 km2 = 2.736987 % = 1.824658 times as much


