########################################################################
#Calculation  and visualization of area consumption                    #
########################################################################

Sys.setenv(LANG = "en")
pacman::p_load(data.table, tidyverse, magrittr, leaflet, sp, raster, htmltools,
               htmlwidgets, sf, spatstat, rgeos, rgdal, DiagrammeR)

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


#create flow chart of distance estimation
grViz(diagram = "digraph flowchart {
  node [fontname = arial, shape = oval, fixedsize = FALSE]
  tab1 [label = '@@1', fontsize=30]
  tab2 [label = '@@2', fontsize=30]
  tab3 [label = '@@3', fontsize=30]
  tab4 [label = '@@4', fontsize=30]
  tab5 [label = '@@5', fontsize=30]
  tab6 [label = '@@6', fontsize=30]
  tab7 [label = '@@7', fontsize=30]
  tab8 [label = '@@8', fontsize=30]
  tab9 [label = '@@9', fontsize=30]
  
  
  tab1 -> tab2 -> tab3 -> tab4 -> tab5 -> tab6 -> tab7 -> tab8 -> tab9;
  
}
  
  [1]: 'Import of merged data set of Amprion and MaStR'
  [2]: 'Adjust format' 
  [3]: 'Load border of RLP as shape files'
  [4]: 'Unify CRS'
  [5]: 'Transform to flat projection'
  [6]: 'Create ppp format of the WT data set'
  [7]: 'Clip those WTs inside RLP'
  [8]: 'Calculate nearest neighbor with nndist() function and add collumn to data frame'
  [9]: 'Check for reliability with Leaflet and create wts_rlp_distances.csv'
  
  ")

