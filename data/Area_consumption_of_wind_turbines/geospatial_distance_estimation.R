########################################################################
#Calculation  and visualization of area consumption                    #
########################################################################

Sys.setenv(LANG = "en")
pacman::p_load(data.table, tidyverse, magrittr, leaflet, sp, raster, htmltools,
               htmlwidgets, sf, spatstat, rgeos)

#import data
MaStR_amprion <- read.csv("MaStR_amprion2019.csv")
attach(MaStR_amprion)

#change lat and long to numeric
MaStR_amprion$l_wgs84 <- gsub(",",".", l_wgs84)  
MaStR_amprion$b_wgs84 <- gsub(",",".", b_wgs84)
MaStR_amprion$b_wgs84 <- as.numeric(MaStR_amprion$b_wgs84)
MaStR_amprion$l_wgs84 <- as.numeric(MaStR_amprion$l_wgs84)

#load boundaries of RLP
border_sf  <- st_read("Borders_RLP_shape/Landesgrenze_RLP.shp")
border_sp <- readOGR("Borders_RLP_shape/Landesgrenze_RLP.shp")

#create coordinate columns and check CRS
class(MaStR_amprion)
coordinates(MaStR_amprion) <- ~ l_wgs84 + b_wgs84
crs(MaStR_amprion)

#create crs
WGS84 <- CRS("+proj=longlat +datum=WGS84 +no_defs")
crs(MaStR_amprion) <- WGS84
crs(MaStR_amprion)

#clip those inside RLP 
wts_rlp <- MaStR_amprion[border_sp,]
wts_rlp_df <- as.data.frame(wts_rlp)


#convert point layer to in sf format
wts_rlp_sf <- st_as_sf(wts_rlp)
crs(wts_sf)
class(wts_sf)

#transform point layer of wts to flat
wts_rlp_flat <- st_transform(wts_rlp_sf, crs = 6345)
plot(wts_rlp_flat)

#transform border polygon of RLP to flat
border_flat <- st_transform(border_sf, crs = 6345)

##############################
#PPA - Point Pattern Analysis#
##############################

#create ppp formate of WT's
wts_rlp_ppp  <- as.ppp(wts_rlp_flat)
plot(wts_rlp_ppp)

#create owin format
border_owin <- as.owin(border_flat)

#create window of points with borders of rlp and plot
Window(wts_rlp_ppp) <- border_owin
plot(wts_rlp_ppp, cols=rgb(0,0,0,.2), pch=20)

#calculate average nearest neighbor 
mean(nndist(wts_rlp_ppp, k=1))
#~549m -----> seems reasonable

#calculate nearest neighbor
nearest <- nndist(wts_rlp_ppp, k=1)

wts_rlp_distances <- data.frame(wts_rlp_df, nearest)
wts_rlp_distances$nearest <- round(wts_rlp_distances$nearest)


#plot with leaflet
leaflet(data = wts_rlp_distances) %>% 
  addTiles() %>% 
  addPolygons(data = border_sp,
              color = "#5DADE2",
              weight = 2,
              opacity = 0.6,
              fillColor = "#5DADE200",
              highlight = highlightOptions(weight = 7,
                                           color = "#5DADE2",
                                           fillColor = "#5DADE2",
                                           fillOpacity = 0.3,
                                           bringToFront = TRUE),
              label = "Rheinland-Pfalz",
              group = "Rheinland-Pfalz") %>%
  addMarkers(lng = wts_rlp$l_wgs84,
             lat = wts_rlp$b_wgs84,
             clusterOptions = markerClusterOptions(disableClusteringAtZoom = 10),
             popup = ~paste("<h3> Daten der Windkraftanlage</h3>",
                            "<b>Distance to next WT:</b>", nearest, "<br>"))

#create colum with mwh and area
wts_rlp_distances <- wts_rlp_distances %>%
  mutate(menge_mwh = round(menge_kwh/1000)) %>% 
  mutate(area_m2 = round(nearest^2)) %>% 
  mutate(area_km2 = round(area_m2/1000000, digits = 3)) %>% 
  mutate(kwh_m2 = round(menge_kwh/area_m2, digits = 3)) %>% 
  mutate(d = round(nearest/rotor_m))

#clean environment
rm(border_sp, border_flat, border_owin, border_sf, wts_rlp_ppp, wts_rlp_flat, 
   wts_rlp_sf, nearest, MaStR_amprion, WGS84, wts_rlp_df, wts_rlp)

#write csv file 
write.csv(wts_rlp_distances, "wts_rlp_distances.csv")

#remove WT's that have distance greater than 1000 m equal to zero 
wts_rlp_filtered <- filter(wts_rlp_distances, nearest < 1000 & nearest != 0)

#omit NA values 
wts_rlp_filtered <- na.omit(wts_rlp_filtered)


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





