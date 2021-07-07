###################################################################################################
#Fusion of the data from the Marktstammdatenregister and the master and movement data from Amprion#
###################################################################################################
#packages
Sys.setenv(LANG = "en")
pacman::p_load(rio, data.table, tidyverse, tidyr, purrr, magrittr, compare,
               rgdal, raster, rgeos
               )

#Import data

MaStR_final <- read.csv("Marktstammdaten/MaStR_final.csv", na.strings=c("","NA"))
amprion2019 <- read.csv("Amprion/amprion_2019_processed.csv", na.strings=c("","NA"))


################
#Merge datasets#
################

#make equal columnn names 
names(MaStR_final)[2] <- "eeg_nr"
names(amprion2019)[9] <- "eeg_nr"


#merge
msb19 <- merge(data.frame(MaStR_final), data.frame(amprion2019), by = "eeg_nr", all = TRUE)


#rename collumns
MSB19_rename <- msb19 %>% 
  rename(typ = Typenbezeichnung)%>%
  rename(leistung_m = Leistung)%>%
  rename(nabe = Nabenhoehe.der.Windenergieanlage)%>%
  rename(rotor = Rotordurchmesser.der.Windenergieanlage)%>%
  rename(leistung_s = leistung)%>%
  rename(indatum_m = Inbetriebnahmedatum.der.Einheit) %>%
  rename(indatum_s = inbetriebnahme) %>%
  rename(gem_m = Gemeindeschluessel) %>%
  rename(gem_s = gem) %>%
  rename(hersteller = Hersteller.der.Windenergieanlage) %>%
  rename(b_wgs84 = Koordinate..Breitengrad..WGS84.) %>%
  rename(l_wgs84 = Koordinate..Laengengrad..WGS84.) 

#filter out Na for EEG Nr. and remove duplicates
sum(is.na(MSB19_rename$eeg_nr))
attach(MSB19_rename)
MSB19_complete <- MSB19_rename %>% 
  subset(select = -c(2,12)) %>%
  filter(!is.na(eeg_nr)) %>% 
  distinct(eeg_nr, .keep_all = TRUE)

#count Na in coordinates
sum(is.na(l_wgs84))

#write csv
write.csv(MSB19_complete,"MaStR_Amprion_join/MaStR_Amprion_join.csv")


#Create table with georeference

MSB19_geo <- subset(MSB19_complete, !is.na(l_wgs84))
write.csv(MSB19_geo,"MaStR_Amprion_join/MaStR_Amprion_join_with_georeferences.csv")

























