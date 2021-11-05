##########################################################
#Merging of the data from the MaStR and from amprion 2019#
##########################################################

#packages
Sys.setenv(LANG = "en")
pacman::p_load(rio, data.table, tidyverse, tidyr, purrr, magrittr, compare,
               rgdal, raster, rgeos, DiagrammeR)


#import data
MaStR <- read.csv("MaStR.csv", sep = ";", na.strings=c("","NA"))
amprion2019 <- read.csv("amprion_2019_processed_and_without_outliers.csv", 
                        na.strings=c("","NA"))

#choose variables
amprion2019 <- amprion2019[,c(2:8)]
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

names(amprion2019)[6] <- "eeg_nr"

#check for Na's in coordinates
sum(is.na(MaStR$b_wgs84))
sum(is.na(MaStR$l_wgs84))
sum(is.na(MaStR$eeg_nr))
MaStR_without_na <- filter(MaStR, ! is.na(MaStR$eeg_nr) & 
                             ! is.na(MaStR$b_wgs84))

#merge data sets
MaStR_amprion2019 <- merge(data.frame(MaStR_without_na), 
                           data.frame(amprion2019), 
                           by = "eeg_nr", 
                           all = TRUE)


#filter out NAs coordinates and remove duplicates
sum(is.na(MaStR_amprion2019$eeg_nr))
sum(is.na(MaStR_amprion2019$b_wgs84))
MaStR_amprion2019_without_na <- filter(MaStR_amprion2019, ! 
                                         is.na(MaStR_amprion2019$b_wgs84))

sum(duplicated(MaStR_amprion2019_without_na$eeg_nr))
MaStR_amprion2019_without_dup <- distinct(MaStR_amprion2019_without_na)


#write csv
write.csv(MaStR_amprion2019_without_dup,"MaStR_amprion2019.csv")

#create flow chart
grViz(diagram = "digraph flowchart {
  node [fontname = arial, shape = oval, fixedsize = FALSE]
  tab1 [label = '@@1', fontsize=30]
  tab2 [label = '@@2', fontsize=30]
  tab3 [label = '@@3', fontsize=30]
  tab4 [label = '@@4', fontsize=30]
  tab5 [label = '@@5', fontsize=30]
  tab6 [label = '@@6', fontsize=30]
  tab7 [label = '@@7', fontsize=30]

  
  tab1 -> tab2 -> tab3 -> tab4 -> tab5 -> tab6 -> tab7
}
  
  [1]: 'Import data'
  [2]: 'Drop unnecessary variables'
  [3]: 'Rename columns' 
  [4]: 'Check for NAs and delet rows with NA values'
  [5]: 'Merge data sets by EEG system key'
  [6]: 'Filter out NAs again and delet duplicates'
  [7]: 'Write CSV file as MaStR_amprion2019.csv'
  ")
























