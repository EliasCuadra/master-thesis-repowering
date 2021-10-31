########################################################################
#Analysis of rotor diameter, hub heights and electricity yield per area#
########################################################################

Sys.setenv(LANG = "en")
pacman::p_load(data.table, tidyverse, magrittr, leaflet, htmltools,
               htmlwidgets, gridExtra)

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
             size = 0.4, colour = "#fde600") +
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
                      values="#fde600") +
  theme(legend.position = c(0.3,0.9))


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
             size = 0.4, colour = "#fc5a03") +
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
                      values="#fc5a03") +
  theme(legend.position = c(0.3,0.9))

p_nabe

#plot both in one
grid.arrange(p_rotor, p_nabe, ncol=1)

#save plot
ggsave("results_of_analysis/rotor_diameter_hub_height.png",
       plot = grid.arrange(p_rotor, p_nabe, ncol=1),
       dpi = 900,
       width = 7,
       height = 4)

par(mfrow=c(1,1))
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
wts_rlp_filtered <- filter(wts_rlp_filtered, kwh_m2 < 500)

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
#~417.3347m that means 169,744 m2 or 0.17 km2 per WT
#area of RLP 19847
#total are consumption with 1702 WT's is  289.34 km2 this is about 289,340,000 m2
#that is 1.457 % of RLP
#with a total electricity yield of 6,782 TWh that means 23,439 kWh/m2
#constant power output of 2.675 W/m2
#calculate wind speed with third root of electricity yield per area divided 
#by 0.016 * 1.3 * 0.5 = 0.0104
#2.675/0.0104 = 257.2115
#250.9827^(1/3)
#wind speed with 6.359605 m/s required


#calculate actual total mean electricity yield per m2 in data
mean(wts_rlp_filtered$kwh_m2)
#~26.17383 kwh/m2 
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
#467.9847 m
mean(wts_rlp_filtered_2010$d)
#4.60781 times d
mean(wts_rlp_filtered_2010$area_m2)
#238527.4 m2
mean(wts_rlp_filtered_2010$area_km2)
#0.2385161 km2
mean(wts_rlp_filtered_2010$kwh_m2)
#30.46168 kWh/m2
#3.477361 W/m2


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
#3.822019/0.0104 = 367.5018 (5d)
#3.822019/0.0436 = 87.66099 (3d)
#367.5018^(1/3) = 7.16286 m/s wind speed required (5d)
#87.66099^(1/3) = 4.442241 m/s wind speed required (3d)
#wind speed of 7.16286 m/s
##calculate area consumption of 22 TWh
#This requires an area of 656716418 m2 = 656.7164 km2 = 3.308895 % 
#= 2.20593 times as much WT's

#if average is 40.5 kwh/m2a in 2030
#4.62065 W/m2
#4.62065/0.0104 = 444.2933 (5d)
#4.62065/0.0436 = 105.9782 (3d)
#444.2933^(1/3) = 7.630563 m/s wind speed required (5d)
#105.9782^(1/3) = 4.732299 m/s wind speed required (3d)
#area consumption with demand of 22 TWh
# 543209877 m2 = 543.2099 km2 = 2.736987 % = 1.824658 times as much

#if average is 63.5 kWh/m2a in 2030
#7.2 W/m2
#7.2/0.0104 = 692.3077 (5d)
#7.2/0.0436 = 165.1376
#692.3077^(1/3) = 8.846396 m/s (5d)
#165.1376^(1/3) = 5.486331 m/s (3d)
#area of 346.456 km2 = 1.74 %












