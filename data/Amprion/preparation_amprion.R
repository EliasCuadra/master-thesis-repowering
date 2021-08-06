########################################################################
#Preparation of the master and movement data of the transmission system# 
#operator Amprion from 2015 - 2019                                     #
########################################################################

#packages
Sys.setenv(LANG = "en")
pacman::p_load(rio, data.table, tidyverse, magrittr, compare, DiagrammeR)

#Import data and format columns
amprion_2015 <- read.csv("EEG_StammBew_2015_Amprion-EAtlas.csv")
amprion_2016 <- read.csv("EEG_StammBew_2016_Amprion-EAtlas.csv")
amprion_2017 <- read.csv("EEG_StammBew_2017_Amprion-EAtlas.csv")
amprion_2018 <- read.csv("EEG_StammBew_2018_Amprion-EAtlas.csv")
amprion_2019 <- read.csv("EEG-StammBew_2019_Amprion-EAtlas.csv")
names(amprion_2019)[1] <- "gem"
names(amprion_2018)[1] <- "gem"
names(amprion_2017)[1] <- "gem"

#calculate and plot trend of total WT's
year <- c(2012:2019)
wts <- c(1207, 1322, 1420, 1478, 1532, 1624, 1675, 1702)
year_wts <- data.frame(year, wts)

lm_year_wts <- lm(year_wts$wts ~ year_wts$year)
summary(lm_year_wts)


pyear_wts <- ggplot() +
  geom_point(data = year_wts, 
             aes(x= year, y=wts), 
             size = 2, colour = "#8600fd") +
  geom_smooth(data = year_wts, 
              aes(x=year, y=wts), 
              method=lm, se=TRUE, fullrange = TRUE, 
              size = 0.5, colour = "#8600fd")  +
  theme_light() +
  ylim(0, 3000) +
  xlim(2010, 2030) +
  xlab("Year") +
  ylab("Number of WT's") +
  theme( axis.text=element_text(size=11),
         axis.title=element_text(size=12),
         plot.title = element_text(size=14),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") 


#print plot
pyear_wts + theme(legend.position = c(0.25,0.9))


#save plot
ggsave("results_of_analysis/year_wts.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)

#Variable selection
selection_2015 <- amprion_2015[,c(1:3,8,10,17,27)]
selection_2016 <- amprion_2016[,c(1:3,8,10,17,27)]
selection_2017 <- amprion_2017[,c(1:3,8,10,17,27)]
selection_2018 <- amprion_2018[,c(1:3,8,10,17,27)]
selection_2019 <- amprion_2019[,c(1:3,7,9,16,26)]

#formatting commissioning date
selection_2015$inbetriebnahme <- as.Date(selection_2015$inbetriebnahme, 
                                         "%d/%m/%Y")
selection_2016$inbetriebnahme <- as.Date(selection_2016$inbetriebnahme, 
                                         "%d/%m/%Y")
selection_2017$inbetriebnahme <- as.Date(selection_2017$inbetriebnahme, 
                                         "%d/%m/%Y")
selection_2018$inbetriebnahme <- as.Date(selection_2018$inbetriebnahme, 
                                         "%d/%m/%Y")
selection_2019$inbetriebnahme <- as.Date(selection_2019$inbetriebnahme, 
                                         "%d/%m/%Y")

#find outliers
outliers_2015 <- filter(selection_2015, leistung < 100 | leistung > 4500 | 
                          inbetriebnahme > "2015-02-15")
outliers_2016 <- filter(selection_2016, leistung < 100 | leistung > 4500 | 
                          inbetriebnahme > "2016-02-15")
outliers_2017 <- filter(selection_2017, leistung < 100 | leistung > 4500 | 
                          inbetriebnahme > "2017-02-15")
outliers_2018 <- filter(selection_2018, leistung < 100 | leistung > 4500 | 
                          inbetriebnahme > "2018-02-15")
outliers_2019 <- filter(selection_2019, leistung < 100 | leistung > 4500 | 
                          inbetriebnahme > "2019-02-15")

#create df without outliers
selection_2015_without_outliers <- setdiff(selection_2015, outliers_2015)
selection_2016_without_outliers <- setdiff(selection_2016, outliers_2016)
selection_2017_without_outliers <- setdiff(selection_2017, outliers_2017)
selection_2018_without_outliers <- setdiff(selection_2018, outliers_2018)
selection_2019_without_outliers <- setdiff(selection_2019, outliers_2019)

#delete some
rm(selection_2015, selection_2016, selection_2017, selection_2018,
   selection_2019)

#add electricity yiel in MWh and full load hours 
selection_2015_without_outliers <- selection_2015_without_outliers %>% 
  mutate(flh = menge_kwh/leistung) %>%
  mutate(menge_mwh = round(menge_kwh/1000))
selection_2016_without_outliers <- selection_2016_without_outliers %>% 
  mutate(flh = menge_kwh/leistung) %>%
  mutate(menge_mwh = round(menge_kwh/1000))
selection_2017_without_outliers <- selection_2017_without_outliers %>% 
  mutate(flh = menge_kwh/leistung) %>%
  mutate(menge_mwh = round(menge_kwh/1000))
selection_2018_without_outliers <- selection_2018_without_outliers %>% 
  mutate(flh = menge_kwh/leistung) %>%
  mutate(menge_mwh = round(menge_kwh/1000))
selection_2019_without_outliers <- selection_2019_without_outliers %>% 
  mutate(flh = menge_kwh/leistung) %>%
  mutate(menge_mwh = round(menge_kwh/1000))

#write files
write.csv(
  selection_2015_without_outliers, 
  "results_of_preparation/amprion_2015_processed.csv")
write.csv(
  selection_2016_without_outliers, 
  "results_of_preparation/amprion_2016_processed.csv")
write.csv(
  selection_2017_without_outliers, 
  "results_of_preparation/amprion_2017_processed.csv")
write.csv(
  selection_2018_without_outliers, 
  "results_of_preparation/amprion_2018_processed.csv")
write.csv(
  selection_2019_without_outliers, 
  "results_of_preparation/amprion_2019_processed.csv")

#find different commissioning dates
commissioning_before2001 <- subset(selection_2019_without_outliers, 
                                   inbetriebnahme < "2000-12-31")
sum(commissioning_before2001$menge_mwh)
commissioning_before2005 <- subset(selection_2019_without_outliers, 
                                   inbetriebnahme < "2005-12-31")
commissioning_2001_2005 <- subset(commissioning_before2005, 
                                  inbetriebnahme > "2000-12-31")
sum(commissioning_2001_2005$menge_mwh)

#write csv
write.csv(commissioning_before2001,
          "results_of_preparation/commissioning_before_2001.csv")
write.csv(commissioning_2001_2005,
          "results_of_preparation/commissioning_2001_2005.csv")
write.csv(commissioning_before2005,
          "results_of_preparation/commissioning_before_2005.csv")

#create flow chart of data preparation
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
  tab10 [label = '@@10', fontsize=30]
  
  
  tab1 -> tab2 -> tab3 -> tab4 -> tab5 -> tab6 -> tab7;
  tab1 -> tab8;
  tab5 -> tab9;
  tab6 -> tab10;
}
  
  [1]: 'Import data from 2015 to 2019'
  [2]: 'Drop unnecessary variables' 
  [3]: 'Adjust variable formats and names'
  [4]: 'Detect outliers and remove from data'
  [5]: 'Add columns for full load hours and electricity yield in MWh'
  [6]: 'Filter only data from 2019 \\nfor different commissioning dates'
  [7]: 'Write files as: \\n commissioning_before_2005.csv \\n commissioning_before_2001.csv \\n commissioning_2001_2005.csv'
  [8]: 'Calculate and plot \\ntotal number of WTs over time'
  [9]: 'Write files as: \\namprion_2015_processed.csv, \\namprion_2016_processed.csv, \\namprion_2017_processed.csv, \\namprion_2018_processed.csv, \\namprion_2019_processed.csv'
  [10]: 'Calculate sum of electricity yield \\nfor different commissioning dates'
  
  ")
