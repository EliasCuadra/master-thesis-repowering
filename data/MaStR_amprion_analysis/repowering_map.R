##########################################
#Create interactive repowering map of RLP#
##########################################
Sys.setenv(LANG = "en")
pacman::p_load(rio, data.table, tidyverse, tidyr, purrr, magrittr, compare, 
               ggplot2, leaflet, sp, raster, rgdal, htmltools, htmlwidgets,
               tmaptools, readr)
#############
#Import data#
#############
wts_rlp <- read.csv(
  "result_of_distance_estimation/wts_rlp_distances.csv")
wts_rlp <- wts_rlp[,-1]
sum(is.na(wts_rlp$menge_mwh))
#change date format
wts_rlp$inbetriebnahme <- as.Date(
  wts_rlp$inbetriebnahme, "%Y-%m-%d")
#find WTs built before 2006
commissioning_before2006 <- subset(wts_rlp, 
                                   inbetriebnahme < "2005-12-31")
############################################################
#create column for predicted electricity yield and increase#
############################################################
repowering_potential <- commissioning_before2006 %>% 
  mutate(flh = round(menge_kwh/leistung, digits = 0)) %>% 
  mutate(newyield = 12500) %>%
  mutate(increase = round(newyield/menge_mwh*100, digits = 0))
#############################
#load borders as shape files#
#############################
land <- readOGR("Borders_RLP_shape/Landesgrenze_RLP.shp")
landkreise <- readOGR("Borders_RLP_shape/Landkreise_RLP.shp")
gemeinden <- readOGR("Borders_RLP_shape/Verbandsgemeinde_RLP.shp")



#########################
#Create map with leaflet#
#########################
attach(repowering_potential)
m <- leaflet(data = repowering_potential) %>%
  addTiles() %>%
  
  addPolygons(data = land,
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
  
  addPolygons(data = landkreise,
              color = "#000fff",
              weight = 2,
              opacity = 0.6,
              fillColor = "#000fff00",
              highlight = highlightOptions(weight = 7,
                                           color = "#000fff",
                                           fillColor = "#000fff",
                                           fillOpacity = 0.3,
                                           bringToFront = TRUE),
              label = landkreise$ldkreis,
              group = "Landkreise") %>%
  
  addPolygons(data = gemeinden,
              color = "#D93F0D",
              weight = 1,
              opacity = 0.6,
              fillColor = "#D93F0D00",
              highlight = highlightOptions(weight = 7,
                                           color = "#D93F0D",
                                           fillColor = "#D93F0D",
                                           fillOpacity = 0.3,
                                           bringToFront = TRUE),
              label = gemeinden$vgname,
              group = "Verbandsgemeinden") %>% 
  addLayersControl(overlayGroups = c("Rheinland-Pfalz", "Landkreise", "Verbandsgemeinden"),
                   options = layersControlOptions(collapsed = FALSE)) %>% 
  addMarkers(lng = repowering_potential$l_wgs84,
             lat = repowering_potential$b_wgs84,
             clusterOptions = markerClusterOptions(disableClusteringAtZoom = 10),
             popup = ~paste("<h3> Daten der Windkraftanlage</h3>",
                            "<b>Ort:</b>", ort,"<br>",
                            "<b>PLZ:</b>", plz,"<br>",
                            "<b>EEG-Nr.:</b>", eeg_nr,"<br>",
                            "<b>Rotordurchmesser [m]:</b>", rotor_m, "<br>",
                            "<b>Nabenhöhe [m]:</b>", nabe_m, "<br>",
                            "<b>Inbetriebnahmedatum:</b>", inbetriebnahme, "<br>",
                            "<b>Leistung [kW]:</b>", leistung, "<br>",
                            "<b>Stromertrag 2019 [MWh]:</b>", menge_mwh, "<br>",
                            "<b>Volllaststunden 2019 [h]:</b>", flh, "<br>",
                            "<b style ='color: red'>Prognose Stromertrag [MWh]:</b>", newyield, "<br>",
                            "<b style ='color: red'>Prognose Ertragssteigerung [%]:</b>", increase, "<br>",
                            collapse = NULL),
             label = ~as.character(ort)) 
m
 
saveWidget(m, file="repowering.html")
###########################################
#create map without shapes for faster load#
###########################################
#clustered Map#
leaflet(data = msb19_before2005) %>% 
  addTiles() %>% 
  addCircleMarkers(lng = lon_wgs84,
                   lat = lat_wgs84,
                   color = "#F40707",
                   weight = 3,
                   radius = 5,
                   label = vg_name,
                   clusterOptions = markerClusterOptions(disableClusteringAtZoom = 9),
                   popup = ~paste("<h3> Daten der Windkraftanlage</h3>",
                                  "<b>Landkreis (LK):</b>",          lk_name, "<br>",
                                  "<b>Verbandsgemeinde:</b>", vg_name, "<br>",
                                  "<b>EEG-Nr.:</b>", eeg_nr,"<br>",
                                  "<b>Leistung [kW]:</b>", leistung_s, "<br>",
                                  "<b>Nabenh?he [m]:</b>", nabe, "<br>",
                                  "<b>Rotordurchmesser [m]:</b>", rotor, "<br>",
                                  "<b>Stromertrag 2019 [MWh]:</b>", Ertrag2019_MWh, "<br>",
                                  "<b>Volllaststunden im LK 2019 [h]:</b>", round(menge_kwh/leistung_s), "<br>",
                                  "<b style ='color: red'>Prognose Volllaststunden [h]:</b>", lk_volllast, "<br>",
                                  "<b style ='color: red'>Prognose Stromertrag nach <br>Repowering [MWh]:</b>", ErtragRepowert, "<br>",
                                  "<b style ='color: red'>Prognose Ertragssteigerung [%]:</b>", Ertragssteigerung, "<br>",
                                  "<b style ='color: red'>Emissionsminderung nach Repowering bei Bundesstrommix 2019 [t/a]:</b>", round(Emissionsminderung), "<br>"
                   ),)                     
##########################
#Map with multiple Layers#
##########################

m <- leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(lng = msb19_before2001$lon_wgs84,
                   lat = msb19_before2001$lat_wgs84,
                   color = "red",
                   weight = 3,
                   radius = 5,
                   label = vg_name,
                   group = "vor 2001") %>% 
  addCircleMarkers(lng = msb19_2001_2005$lon_wgs84,
                   lat = msb19_2001_2005$lat_wgs84,
                   color = "blue",
                   weight = 3,
                   radius = 5,
                   label = vg_name,
                   group = "zwischen 2001 - 2005") %>% 
  addLayersControl(overlayGroups = c("vor 2001","zwischen 2001 - 2005"),
                   options = layersControlOptions(collapsed = FALSE))
m

##########################################                

m <- leaflet() %>% 
  addTiles() %>%
  addPolygons(data = landkreise,
              color = "red",
              weight = 2,
              opacity = 0.6,
              fillColor = "red",
              fillOpacity = 0.2,
              highlight = highlightOptions(stroke = 4, 
                                           weight = 7,
                                           color = "#000000",
                                           fillOpacity = 0.7,
                                           bringToFront = TRUE),
              label = "Hello World")
m

?addMarkers


###################################
#find Wind turbines outside of RLP#
###################################

ausreisser <- msb19_before2005 %>% 
  filter(msb19_before2005$eeg_nr == "E31238010000020170000009043119003" |
           msb19_before2005$eeg_nr == "E30254010000000000005230797401810" |
           msb19_before2005$eeg_nr == "E30254010000000000005230797401809" |
           msb19_before2005$eeg_nr == "E30254010000000000005230797405750" |
           msb19_before2005$eeg_nr == "E30254010000000000005230797401812" |
           msb19_before2005$eeg_nr == "E30254010000000000005230797401811" |
           msb19_before2005$eeg_nr == "E31238010000010170000001432618844" |
           msb19_before2005$eeg_nr == "E31238010000010170000001432618846"|
           msb19_before2005$eeg_nr == "E31238010000000000000045704818817")


ausreisser[2,1] <- "Morbach 54497 Germany"

unique_ads <- unique(ausreisser$adress)
unique_ads
############################
#geocoding with geocode_OSM#
############################
geocodes <- geocode_OSM(unique_ads)
geo <- geocodes[,c(1,2,3)]
leaflet(data = geo) %>% 
  addTiles %>% 
  addMarkers(lng = geo$lon,
             lat = geo$lat,
             label = geo$query)



##################################
#combine data frame with adresses#
##################################
geo <- geo %>% 
  rename(adress = query)
ausreisser <- ausreisser[,-c(28:29)]
ausreisser_coords <- left_join(ausreisser, geo, by = "adress")
ausreisser_coords <- ausreisser_coords %>% 
  rename(lat_wgs84 = lat) %>% 
  rename(lon_wgs84 = lon)

ausreisser_coords$lat_wgs84 <- jitter(ausreisser_coords$lat_wgs84, factor = 2)
ausreisser_coords$lon_wgs84 <- jitter(ausreisser_coords$lon_wgs84, factor = 2)


########################################################################
#take the wind turbines with wrong coordinates out of original data set#
########################################################################

msb19_before2005_sub <- subset(msb19_before2005, eeg_nr != "E31238010000020170000009043119003" &
                                 msb19_before2005$eeg_nr != "E30254010000000000005230797401810" &
                                 msb19_before2005$eeg_nr != "E30254010000000000005230797401809" &
                                 msb19_before2005$eeg_nr != "E30254010000000000005230797405750" &
                                 msb19_before2005$eeg_nr != "E30254010000000000005230797401812" &
                                 msb19_before2005$eeg_nr != "E30254010000000000005230797401811" &
                                 msb19_before2005$eeg_nr != "E31238010000010170000001432618844" &
                                 msb19_before2005$eeg_nr != "E31238010000010170000001432618846" &
                                 msb19_before2005$eeg_nr != "E31238010000000000000045704818817")



msb19_before2005_coords <- rbind(msb19_before2005_sub, ausreisser_coords)


##########
#Test Map#






