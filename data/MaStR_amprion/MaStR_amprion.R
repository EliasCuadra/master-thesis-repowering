#################################################################################
#Fusion of the data from the MaStR and the master and movement data from amprion#
#################################################################################

#packages
Sys.setenv(LANG = "en")
pacman::p_load(rio, data.table, tidyverse, tidyr, purrr, magrittr, compare,
               rgdal, raster, rgeos
               )


#import data
MaStR <- read.csv("MaStR.csv", sep = ";", na.strings=c("","NA"))
amprion2019 <- read.csv("amprion_2019_processed_and_without_outliers.csv", na.strings=c("","NA"))
amprion2019 <- amprion2019[,c(2:8)]

#choose variables
MaStR <- MaStR[,c(1, 5, 7, 16, 17, 18, 21, 22, 23, 24, 43)]
MaStR <- MaStR[,c(2:6,8,9,11)]

#change column names
names(MaStR)[1] <- "leistung_m"
names(MaStR)[2] <- "inbetrieb_m"
names(MaStR)[3] <- "gem_m"
names(MaStR)[4] <- "b_wgs84"
names(MaStR)[5] <- "l_wgs84"
names(MaStR)[6] <- "nabe_m"
names(MaStR)[7] <- "rotor_m"
names(MaStR)[8] <- "eeg_nr"

#check for Na's in coordinates
sum(is.na(MaStR$b_wgs84))
sum(is.na(MaStR$l_wgs84))
sum(is.na(MaStR$eeg_nr))
MaStR_without_na <- filter(MaStR, ! is.na(MaStR$eeg_nr) & ! is.na(MaStR$b_wgs84))


#################
#Merge data sets#
#################

#rename system key column
names(amprion2019)[7] <- "eeg_nr"


#merge data sets
MaStR_amprion2019 <- merge(data.frame(MaStR_without_na), data.frame(amprion2019), by = "eeg_nr", all = TRUE)


#filter out NAs coordinates and remove duplicates
sum(is.na(MaStR_amprion2019$eeg_nr))
sum(is.na(MaStR_amprion2019$b_wgs84))
MaStR_amprion2019_without_na <- filter(MaStR_amprion2019, ! is.na(MaStR_amprion2019$b_wgs84))

sum(duplicated(MaStR_amprion2019_without_na$eeg_nr))
MaStR_amprion2019_without_dup <- distinct(MaStR_amprion2019_without_na$eeg_nr)


#write csv
write.csv(MaStR_amprion2019_without_dup,"MaStR_amprion2019.csv")



























