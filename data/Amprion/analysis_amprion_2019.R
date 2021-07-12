##############################################################################
#Analysis the electricity yield over time in  Rheinland-Palatinate until 2030#
##############################################################################


#packages#
Sys.setenv(LANG = "en")
pacman::p_load(rio, data.table, tidyverse, tidyr, purrr, magrittr, compare, 
               ggplot2, leaflet, sp, raster, rgdal, ggmap, tmaptools, pdftools,
               qpdf, bookdown, kableExtra, here, usethis)

#import data#
amprion <- read.csv("amprion_2019_processed.csv")

#change format to date#
amprion$inbetriebnahme <- as.Date(amprion$inbetriebnahme, "%Y-%m-%d")


##########################################################
#Linear model of electricity yield over comissioning date#
##########################################################
lm_electricity_yield_rlp <- lm(amprion$menge_mwh ~ amprion$inbetriebnahme)
summary(lm_electricity_yield_rlp)
plot(lm_electricity_yield_rlp)


################################
#Do not use scientific notation#
################################
options(scipen=5)


###################################
#Plot the values with linear trend#
###################################
pelectricity_yield_rlp <- ggplot(amprion, aes(x=inbetriebnahme, y=menge_mwh)) +
  geom_point(size = 0.6) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm, se=TRUE, fullrange = TRUE, aes(color ="red") )  +
  xlab("Comissioning Date") +
  ylab("Electricity yield [MWh]") +
  ggtitle("Electricity yield of WT's in Rhineland-Palatinate in 2019 \nover the comissioning date") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=13),
         plot.title = element_text(size=16),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 7500, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1992-01-01"),
           y=6500,label="~ 7500 MWh") +
  annotate(geom="text",x=as.Date("2028-01-01"),
           y=4000,label="R? = 64.9 %") +
  annotate(geom="text",x=as.Date("2028-01-01"),
           y=400,label="p-value << 0.001") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pelectricity_yield_rlp)
####################
#save plot as image#
####################
ggsave("results/electricity_yield_2019_rlp_over_comissioning_date.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)


############################################################################################
#Calculate linear model with electricity yield over rated capacity for Rhineland-Palatinate#
############################################################################################
lm_electricity2 <- lm(amprion$menge_mwh ~ amprion$leistung)
summary(lm_electricity2)
plot(lm_electricity2) #outliers spotted here in the first place
###################################
#Plot the values with linear trend#
###################################
pelectricity2 <- ggplot(amprion, aes(x=leistung, y=menge_mwh)) +
  geom_point(size = 0.6) +
  theme_light() +
  geom_smooth(method=lm, se=TRUE, fullrange = TRUE, aes(color ="red") )  +
  scale_x_continuous(limits = c(0,8000)) +
  xlab("Rated capacity [kW]") +
  ylab("Electricity yield [MWh]") +
  ggtitle("Electricity yield of WT's in Rhineland-Palatinate in 2019 \nover the rated capacity") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=13),
         plot.title = element_text(size=16),
         legend.position = c(0.70, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 7330, linetype = 'dashed') +
  annotate(geom="text",x = 750 ,
           y=8000,label="~ 7330 MWh (3.4 MW)") +
  annotate(geom="text",x = 7000,
           y=2500,label="R² = 79.6 %") +
  annotate(geom="text",x = 7000,
           y=400,label="p-value << 0.001") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pelectricity2)
####################
#save plot as image#
####################
ggsave("results/electricity_yield_2019_over_rated_capacity.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)


###############################################################################
#Calculate linear model with rated capacity over time for Rhineland-Palatinate#
###############################################################################
lm_rated_capacity <- lm(amprion$leistung ~ amprion$inbetriebnahme)
summary(lm_rated_capacity)
plot(lm_rated_capacity)
###################################
#Plot the values with linear trend#
###################################
prated_capacity <- ggplot(amprion, aes(x=inbetriebnahme, y=leistung)) +
  geom_point(size = 0.6) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm, se=TRUE, fullrange = TRUE, aes(color ="red") )  +
  xlab("Comissioning date") +
  ylab("Rated Capacity [kW]") +
  ggtitle("Rated capacity of WT's in Rhineland-Palatinate in  2019 \nover the comissioning date") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=13),
         plot.title = element_text(size=16),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 3400, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=4000,label="~ 3400 kW") +
  annotate(geom="text",x=as.Date("2028-01-01"),
           y=2000,label="R? = 60 %") +
  annotate(geom="text",x=as.Date("2028-01-01"),
           y=400,label="p-value << 0.001") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(prated_capacity)
####################
#save plot as image#
####################
ggsave("results/rated_capacity_over_comissioning_date.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)


################################################################################
#Calculate linear model with full load hours over time for Rhineland-Palatinate#
################################################################################
lm_full_load_hours <- lm(amprion$flh ~ amprion$inbetriebnahme)
summary(lm_full_load_hours)
plot(lm_full_load_hours)
###################################
#Plot the values with linear trend#
###################################
pfull_load_hours <- ggplot(amprion, aes(x=inbetriebnahme, y=flh)) +
  geom_point(size = 0.6) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm, se=TRUE, fullrange = TRUE, aes(color ="red") )  +
  xlab("Comissioning date") +
  ylab("Full load hours [h]") +
  ggtitle("Full load hours of WT's in Rhineland-Palatinate in 2019 \nover the comissioning date") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=13),
         plot.title = element_text(size=16),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 2375, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=2500,label="~ 2375 h") +
  annotate(geom="text",x=as.Date("2028-01-01"),
           y=2000,label="R? = 48.3 %") +
  annotate(geom="text",x=as.Date("2028-01-01"),
           y=400,label="p-value << 0.001") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pfull_load_hours)
####################
#save plot as image#
####################
ggsave("results/full_load_hours_over_comissioning_date.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)






###################################################################################################
#calculating linear models for each county to find prediction electricity yield or full load hours#
###################################################################################################
###########
#Ahrweiler#
###########
Ahrweiler <- subset(sb19.0_LK, lk_nr == 7131)
Ahrweiler_without_outliers <- subset(sb19_without_outliers, lk_nr ==7131)
##########################
#Sum of electricity yield#
##########################
sum(Ahrweiler$menge_kwh/10^6)
##############################################################################
#Count plants that are built before 2001 and between 2001 and 2005 and sum up# 
#the electricity yield                                                       #
##############################################################################
Ahrweiler_before2000 <- subset(Ahrweiler, inbetriebnahme < "2000-12-31")
sum(Ahrweiler_before2000$menge_kwh/10^6)
Ahrweiler_before2005 <- subset(Ahrweiler, inbetriebnahme < "2005-12-31")
Ahrweiler_2001_2005 <- subset(Ahrweiler_before2005, inbetriebnahme > "2000-12-31")
sum(Ahrweiler_2001_2005$menge_kwh/10^6)
##############
#linear model#
##############
lm_Ahrweiler <- lm(Ahrweiler_without_outliers$menge_mwh ~ Ahrweiler_without_outliers$inbetriebnahme)
summary(lm_Ahrweiler)
######
#Plot#
######
pAhrweiler <- ggplot(Ahrweiler_without_outliers, aes(x=inbetriebnahme, y=menge_mwh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Stromertrag [MWh]") +
  ggtitle("Stromertrag 2019 nach dem Inbetriebnahmedatum im \n Landkreis Ahrweiler") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 5500, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=6000,label="~ 5500") +
  annotate(geom="text",x=as.Date("2028-01-01"),
           y=2500,label="R² = 86.1 %",fontface="bold") +
  annotate(geom="text",x=as.Date("2028-01-01"),
           y=400,label="p-value = 0.003",fontface="bold") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pAhrweiler)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Ahrweiler.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
#############################################################################
#Compare the model with the model of full load hours to see if more suitable#
#############################################################################
lm_Ahrweiler2 <- lm(Ahrweiler_without_outliers$flh ~ Ahrweiler_without_outliers$inbetriebnahme)
summary(lm_Ahrweiler2)
######
#Plot#
######
pAhrweiler2 <- ggplot(Ahrweiler_without_outliers, aes(x=inbetriebnahme, y=flh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Volllaststunden [h]") +
  ggtitle("Volllaststunden 2019 nach dem Inbetriebnahmedatum im \nLandkreis Ahrweiler") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 2120, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=2300,label="~ 2120") +
  annotate(geom="text",x=as.Date("2028-01-01"),
           y=1500,label="R² = 37.2 %",fontface="bold") +
  annotate(geom="text",x=as.Date("2028-01-01"),
           y=400,label="p-value = 0.15",fontface="bold") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pAhrweiler2)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Ahrweiler2.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
################
#delete objects#
################
rm(Ahrweiler, lm_Ahrweiler, pAhrweiler, Ahrweiler_before2000, Ahrweiler_before2005, Ahrweiler_2001_2005, Ahrweiler_without_outliers, lm_Ahrweiler2, pAhrweiler2)
#############################################################################
##############
#Altenkirchen#
##############
Altenkirchen <- subset(sb19.0_LK, lk_nr == 7132)
Altenkirchen_without_outliers <- subset(sb19_without_outliers, lk_nr ==7132)
##########################
#Sum of electricity yield#
##########################
sum(Altenkirchen$menge_mwh)
##############################################################################
#Count plants that are built before 2001 and between 2001 and 2005 and sum up# 
#the electricity yield                                                       #
##############################################################################
Altenkirchen_before2001 <- subset(Altenkirchen, inbetriebnahme < "2000-12-31")
sum(Altenkirchen_before2001$menge_mwh)
Altenkirchen_before2005 <- subset(Altenkirchen, inbetriebnahme < "2005-12-31")
Altenkirchen_2001_2005 <- subset(Altenkirchen_before2005, inbetriebnahme > "2000-12-31")
sum(Altenkirchen_2001_2005$menge_mwh)
##############
#linear model#
##############
attach(Altenkirchen_without_outliers)
lm_Altenkirchen <- lm(menge_mwh ~ inbetriebnahme)
summary(lm_Altenkirchen)
######
#Plot#
######
pAltenkirchen <- ggplot(Altenkirchen_without_outliers, aes(x=inbetriebnahme, y=menge_mwh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Stromertrag [MWh]") +
  ggtitle("Stromertrag 2019 nach dem Inbetriebnahmedatum im \nLandkreis Altenkirchen") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 6000, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=5500,label="~ 6000") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pAltenkirchen)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Altenkirchen.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
#############################################################################
#Compare the model with the model of full load hours to see if more suitable#
#############################################################################
lm_Altenkirchen2 <- lm(flh ~ inbetriebnahme)
summary(lm_Altenkirchen2)
######
#Plot#
######
pAltenkirchen2 <- ggplot(Altenkirchen_without_outliers, aes(x=inbetriebnahme, y=flh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Volllaststunden [h]") +
  ggtitle("Volllaststunden 2019 nach dem Inbetriebnahmedatum im \nLandkreis Altenkirchen") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 2120, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=2300,label="~ 2120") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pAltenkirchen2)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Altenkirchen2.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
################
#delete objects#
################
rm(Altenkirchen, lm_Altenkirchen, pAltenkirchen, Altenkirchen_before2001, Altenkirchen_before2005, Altenkirchen_2001_2005, Altenkirchen_without_outliers, lm_Altenkirchen2, pAltenkirchen2)
#############################################################################
##############
#Alzey-Worms#
##############
Alzey_Worms <- subset(sb19.0_LK, lk_nr == 7331)
Alzey_Worms_without_outliers <- subset(sb19_without_outliers, lk_nr ==7331)
##########################
#Sum of electricity yield#
##########################
sum(Alzey_Worms$menge_mwh)
##############################################################################
#Count plants that are built before 2001 and between 2001 and 2005 and sum up# 
#the electricity yield                                                       #
##############################################################################
Alzey_Worms_before2001 <- subset(Alzey_Worms_without_outliers, inbetriebnahme < "2000-12-31")
sum(Alzey_Worms_before2001$menge_mwh)
Alzey_Worms_before2005 <- subset(Alzey_Worms_without_outliers, inbetriebnahme < "2005-12-31")
Alzey_Worms_2001_2005 <- subset(Alzey_Worms_before2005, inbetriebnahme > "2000-12-31")
sum(Alzey_Worms_2001_2005$menge_mwh)
##############
#linear model#
##############
attach(Alzey_Worms_without_outliers)
lm_Alzey_Worms <- lm(menge_mwh ~ inbetriebnahme)
summary(lm_Alzey_Worms)
######
#Plot#
######
pAlzey_Worms <- ggplot(Alzey_Worms_without_outliers, aes(x=inbetriebnahme, y=menge_mwh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Stromertrag [MWh]") +
  ggtitle("Stromertrag 2019 nach dem Inbetriebnahmedatum im \nLandkreis Alzey_Worms") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 7000, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=5500,label="~ 7000") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pAlzey_Worms)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Alzey_Worms.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
#############################################################################
#Compare the model with the model of full load hours to see if more suitable#
#############################################################################
lm_Alzey_Worms2 <- lm(flh ~ inbetriebnahme)
summary(lm_Alzey_Worms2)
######
#Plot#
######
pAlzey_Worms2 <- ggplot(Alzey_Worms_without_outliers, aes(x=inbetriebnahme, y=flh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Volllaststunden [h]") +
  ggtitle("Volllaststunden 2019 nach dem Inbetriebnahmedatum im \nLandkreis Alzey_Worms") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 2150, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=2300,label="~ 2150") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pAlzey_Worms2)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Alzey_Worms2.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
################
#delete objects#
################
rm(Alzey_Worms, lm_Alzey_Worms, pAlzey_Worms, Alzey_Worms_before2001, Alzey_Worms_before2005, Alzey_Worms_2001_2005, Alzey_Worms_without_outliers, lm_Alzey_Worms2, pAlzey_Worms2)
#############################################################################
##############
#Bad_Dürkheim#
##############
Bad_Dürkheim <- subset(sb19.0_LK, lk_nr == 7332)
Bad_Dürkheim_without_outliers <- subset(sb19_without_outliers, lk_nr ==7332)
##########################
#Sum of electricity yield#
##########################
sum(Bad_Dürkheim$menge_mwh)
##############################################################################
#Count plants that are built before 2001 and between 2001 and 2005 and sum up# 
#the electricity yield                                                       #
##############################################################################
Bad_Dürkheim_before2001 <- subset(Bad_Dürkheim_without_outliers, inbetriebnahme < "2000-12-31")
sum(Bad_Dürkheim_before2001$menge_mwh)
Bad_Dürkheim_before2005 <- subset(Bad_Dürkheim_without_outliers, inbetriebnahme < "2005-12-31")
Bad_Dürkheim_2001_2005 <- subset(Bad_Dürkheim_before2005, inbetriebnahme > "2000-12-31")
sum(Bad_Dürkheim_2001_2005$menge_mwh)
##############
#linear model#
##############
attach(Bad_Dürkheim_without_outliers)
lm_Bad_Dürkheim <- lm(menge_mwh ~ inbetriebnahme)
summary(lm_Bad_Dürkheim)
######
#Plot#
######
pBad_Dürkheim <- ggplot(Bad_Dürkheim_without_outliers, aes(x=inbetriebnahme, y=menge_mwh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Stromertrag [MWh]") +
  ggtitle("Stromertrag 2019 nach dem Inbetriebnahmedatum im \nLandkreis Bad_Dürkheim") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 6500, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=5500,label="~ 6500") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pBad_Dürkheim)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Bad_Dürkheim.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
#############################################################################
#Compare the model with the model of full load hours to see if more suitable#
#############################################################################
lm_Bad_Dürkheim2 <- lm(flh ~ inbetriebnahme)
summary(lm_Bad_Dürkheim2)
######
#Plot#
######
pBad_Dürkheim2 <- ggplot(Bad_Dürkheim_without_outliers, aes(x=inbetriebnahme, y=flh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Volllaststunden [h]") +
  ggtitle("Volllaststunden 2019 nach dem Inbetriebnahmedatum im \nLandkreis Bad_Dürkheim") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 2075, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=2300,label="~ 2075") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pBad_Dürkheim2)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Bad_Dürkheim2.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
################
#delete objects#
################
rm(Bad_Dürkheim, lm_Bad_Dürkheim, pBad_Dürkheim, Bad_Dürkheim_before2001, Bad_Dürkheim_before2005, Bad_Dürkheim_2001_2005, Bad_Dürkheim_without_outliers, lm_Bad_Dürkheim2, pBad_Dürkheim2)
#############################################################################
###############
#Bad_Kreuznach#
###############
Bad_Kreuznach <- subset(sb19.0_LK, lk_nr == 7133)
Bad_Kreuznach_without_outliers <- subset(sb19_without_outliers, lk_nr ==7133)
##########################
#Sum of electricity yield#
##########################
sum(Bad_Kreuznach$menge_mwh)
##############################################################################
#Count plants that are built before 2001 and between 2001 and 2005 and sum up# 
#the electricity yield                                                       #
##############################################################################
Bad_Kreuznach_before2001 <- subset(Bad_Kreuznach_without_outliers, inbetriebnahme < "2000-12-31")
sum(Bad_Kreuznach_before2001$menge_mwh)
Bad_Kreuznach_before2005 <- subset(Bad_Kreuznach_without_outliers, inbetriebnahme < "2005-12-31")
Bad_Kreuznach_2001_2005 <- subset(Bad_Kreuznach_before2005, inbetriebnahme > "2000-12-31")
sum(Bad_Kreuznach_2001_2005$menge_mwh)
##############
#linear model#
##############
attach(Bad_Kreuznach_without_outliers)
lm_Bad_Kreuznach <- lm(menge_mwh ~ inbetriebnahme)
summary(lm_Bad_Kreuznach)
######
#Plot#
######
pBad_Kreuznach <- ggplot(Bad_Kreuznach_without_outliers, aes(x=inbetriebnahme, y=menge_mwh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Stromertrag [MWh]") +
  ggtitle("Stromertrag 2019 nach dem Inbetriebnahmedatum im \nLandkreis Bad_Kreuznach") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 8000, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=5500,label="~ 8000") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pBad_Kreuznach)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Bad_Kreuznach.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
#############################################################################
#Compare the model with the model of full load hours to see if more suitable#
#############################################################################
lm_Bad_Kreuznach2 <- lm(flh ~ inbetriebnahme)
summary(lm_Bad_Kreuznach2)
######
#Plot#
######
pBad_Kreuznach2 <- ggplot(Bad_Kreuznach_without_outliers, aes(x=inbetriebnahme, y=flh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Volllaststunden [h]") +
  ggtitle("Volllaststunden 2019 nach dem Inbetriebnahmedatum im \nLandkreis Bad_Kreuznach") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 2600, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=2300,label="~ 2600") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pBad_Kreuznach2)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Bad_Kreuznach2.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
################
#delete objects#
################
rm(Bad_Kreuznach, lm_Bad_Kreuznach, pBad_Kreuznach, Bad_Kreuznach_before2001, Bad_Kreuznach_before2005, Bad_Kreuznach_2001_2005, Bad_Kreuznach_without_outliers, lm_Bad_Kreuznach2, pBad_Kreuznach2)
#############################################################################
#####################
#Bernkastel_Wittlich#
#####################
Bernkastel_Wittlich <- subset(sb19.0_LK, lk_nr == 7231)
Bernkastel_Wittlich_without_outliers <- subset(sb19_without_outliers, lk_nr ==7231)
##########################
#Sum of electricity yield#
##########################
sum(Bernkastel_Wittlich_without_outliers$menge_mwh)
##############################################################################
#Count plants that are built before 2001 and between 2001 and 2005 and sum up# 
#the electricity yield                                                       #
##############################################################################
Bernkastel_Wittlich_before2001 <- subset(Bernkastel_Wittlich_without_outliers, inbetriebnahme < "2000-12-31")
sum(Bernkastel_Wittlich_before2001$menge_mwh)
Bernkastel_Wittlich_before2005 <- subset(Bernkastel_Wittlich_without_outliers, inbetriebnahme < "2005-12-31")
Bernkastel_Wittlich_2001_2005 <- subset(Bernkastel_Wittlich_before2005, inbetriebnahme > "2000-12-31")
sum(Bernkastel_Wittlich_2001_2005$menge_mwh)
##############
#linear model#
##############
attach(Bernkastel_Wittlich_without_outliers)
lm_Bernkastel_Wittlich <- lm(menge_mwh ~ inbetriebnahme)
summary(lm_Bernkastel_Wittlich)
######
#Plot#
######
pBernkastel_Wittlich <- ggplot(Bernkastel_Wittlich_without_outliers, aes(x=inbetriebnahme, y=menge_mwh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Stromertrag [MWh]") +
  ggtitle("Stromertrag 2019 nach dem Inbetriebnahmedatum im \nLandkreis Bernkastel_Wittlich") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 8000, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=5500,label="~ 8000") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pBernkastel_Wittlich)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Bernkastel_Wittlich.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
#############################################################################
#Compare the model with the model of full load hours to see if more suitable#
#############################################################################
lm_Bernkastel_Wittlich2 <- lm(flh ~ inbetriebnahme)
summary(lm_Bernkastel_Wittlich2)
######
#Plot#
######
pBernkastel_Wittlich2 <- ggplot(Bernkastel_Wittlich_without_outliers, aes(x=inbetriebnahme, y=flh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Volllaststunden [h]") +
  ggtitle("Volllaststunden 2019 nach dem Inbetriebnahmedatum im \nLandkreis Bernkastel_Wittlich") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 2550, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=2300,label="~ 2550") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pBernkastel_Wittlich2)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Bernkastel_Wittlich2.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
################
#delete objects#
################
rm(Bernkastel_Wittlich, lm_Bernkastel_Wittlich, pBernkastel_Wittlich, Bernkastel_Wittlich_before2001, Bernkastel_Wittlich_before2005, Bernkastel_Wittlich_2001_2005, Bernkastel_Wittlich_without_outliers, lm_Bernkastel_Wittlich2, pBernkastel_Wittlich2)
#############################################################################
############
#Birkenfeld#
############
Birkenfeld <- subset(sb19.0_LK, lk_nr == 7134)
Birkenfeld_without_outliers <- subset(sb19_without_outliers, lk_nr ==7134)
##########################
#Sum of electricity yield#
##########################
sum(Birkenfeld_without_outliers$menge_mwh)
##############################################################################
#Count plants that are built before 2001 and between 2001 and 2005 and sum up# 
#the electricity yield                                                       #
##############################################################################
Birkenfeld_before2001 <- subset(Birkenfeld_without_outliers, inbetriebnahme < "2000-12-31")
Birkenfeld_before2001$menge_mwh)
Birkenfeld_before2005 <- subset(Birkenfeld_without_outliers, inbetriebnahme < "2005-12-31")
Birkenfeld_2001_2005 <- subset(Birkenfeld_before2005, inbetriebnahme > "2000-12-31")
sum(Birkenfeld_2001_2005$menge_mwh)
##############
#linear model#
##############
attach(Birkenfeld_without_outliers)
lm_Birkenfeld <- lm(menge_mwh ~ inbetriebnahme)
summary(lm_Birkenfeld)
######
#Plot#
######
pBirkenfeld <- ggplot(Birkenfeld_without_outliers, aes(x=inbetriebnahme, y=menge_mwh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Stromertrag [MWh]") +
  ggtitle("Stromertrag 2019 nach dem Inbetriebnahmedatum im \nLandkreis Birkenfeld") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 6200, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=5500,label="~ 6200") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pBirkenfeld)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Birkenfeld.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
#############################################################################
#Compare the model with the model of full load hours to see if more suitable#
#############################################################################
lm_Birkenfeld2 <- lm(flh ~ inbetriebnahme)
summary(lm_Birkenfeld2)
######
#Plot#
######
pBirkenfeld2 <- ggplot(Birkenfeld_without_outliers, aes(x=inbetriebnahme, y=flh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Volllaststunden [h]") +
  ggtitle("Volllaststunden 2019 nach dem Inbetriebnahmedatum im \nLandkreis Birkenfeld") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 1850, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=1850,label="~ 2550") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pBirkenfeld2)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Birkenfeld2.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
################
#delete objects#
################
rm(Birkenfeld, lm_Birkenfeld, pBirkenfeld, Birkenfeld_before2001, Birkenfeld_before2005, Birkenfeld_2001_2005, Birkenfeld_without_outliers, lm_Birkenfeld2, pBirkenfeld2)
#############################################################################
#############
#Cochem_Zell#
#############
Cochem_Zell <- subset(sb19.0_LK, lk_nr == 7135)
Cochem_Zell_without_outliers <- subset(sb19_without_outliers, lk_nr ==7135)
##########################
#Sum of electricity yield#
##########################
sum(Cochem_Zell_without_outliers$menge_mwh)
##############################################################################
#Count plants that are built before 2001 and between 2001 and 2005 and sum up# 
#the electricity yield                                                       #
##############################################################################
Cochem_Zell_before2001 <- subset(Cochem_Zell_without_outliers, inbetriebnahme < "2000-12-31")
sum(Cochem_Zell_before2001$menge_mwh)
Cochem_Zell_before2005 <- subset(Cochem_Zell_without_outliers, inbetriebnahme < "2005-12-31")
Cochem_Zell_2001_2005 <- subset(Cochem_Zell_before2005, inbetriebnahme > "2000-12-31")
sum(Cochem_Zell_2001_2005$menge_mwh)
##############
#linear model#
##############
attach(Cochem_Zell_without_outliers)
lm_Cochem_Zell <- lm(menge_mwh ~ inbetriebnahme)
summary(lm_Cochem_Zell)
######
#Plot#
######
pCochem_Zell <- ggplot(Cochem_Zell_without_outliers, aes(x=inbetriebnahme, y=menge_mwh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Stromertrag [MWh]") +
  ggtitle("Stromertrag 2019 nach dem Inbetriebnahmedatum im \nLandkreis Cochem_Zell") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 6200, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=5500,label="~ 6200") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pCochem_Zell)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Cochem_Zell.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
#############################################################################
#Compare the model with the model of full load hours to see if more suitable#
#############################################################################
lm_Cochem_Zell2 <- lm(flh ~ inbetriebnahme)
summary(lm_Cochem_Zell2)
######
#Plot#
######
pCochem_Zell2 <- ggplot(Cochem_Zell_without_outliers, aes(x=inbetriebnahme, y=flh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Volllaststunden [h]") +
  ggtitle("Volllaststunden 2019 nach dem Inbetriebnahmedatum im \nLandkreis Cochem_Zell") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 2275, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=1850,label="~ 2275") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pCochem_Zell2)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Cochem_Zell2.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
################
#delete objects#
################
rm(Cochem_Zell, lm_Cochem_Zell, pCochem_Zell, Cochem_Zell_before2001, Cochem_Zell_before2005, Cochem_Zell_2001_2005, Cochem_Zell_without_outliers, lm_Cochem_Zell2, pCochem_Zell2)
#############################################################################
##################
#Donnersbergkreis#
##################
Donnersbergkreis <- subset(sb19.0_LK, lk_nr == 7333)
Donnersbergkreis_without_outliers <- subset(sb19_without_outliers, lk_nr ==7333)
##########################
#Sum of electricity yield#
##########################
sum(Donnersbergkreis_without_outliers$menge_mwh)
##############################################################################
#Count plants that are built before 2001 and between 2001 and 2005 and sum up# 
#the electricity yield                                                       #
##############################################################################
Donnersbergkreis_before2001 <- subset(Donnersbergkreis_without_outliers, inbetriebnahme < "2000-12-31")
sum(Donnersbergkreis_before2001$menge_mwh)
Donnersbergkreis_before2005 <- subset(Donnersbergkreis_without_outliers, inbetriebnahme < "2005-12-31")
Donnersbergkreis_2001_2005 <- subset(Donnersbergkreis_before2005, inbetriebnahme > "2000-12-31")
sum(Donnersbergkreis_2001_2005$menge_mwh)
##############
#linear model#
##############
attach(Donnersbergkreis_without_outliers)
lm_Donnersbergkreis <- lm(menge_mwh ~ inbetriebnahme)
summary(lm_Donnersbergkreis)
######
#Plot#
######
pDonnersbergkreis <- ggplot(Donnersbergkreis_without_outliers, aes(x=inbetriebnahme, y=menge_mwh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Stromertrag [MWh]") +
  ggtitle("Stromertrag 2019 nach dem Inbetriebnahmedatum im \nLandkreis Donnersbergkreis") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 8150, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=6000,label="~ 8150") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pDonnersbergkreis)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Donnersbergkreis.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
#############################################################################
#Compare the model with the model of full load hours to see if more suitable#
#############################################################################
lm_Donnersbergkreis2 <- lm(flh ~ inbetriebnahme)
summary(lm_Donnersbergkreis2)
######
#Plot#
######
pDonnersbergkreis2 <- ggplot(Donnersbergkreis_without_outliers, aes(x=inbetriebnahme, y=flh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Volllaststunden [h]") +
  ggtitle("Volllaststunden 2019 nach dem Inbetriebnahmedatum im \nLandkreis Donnersbergkreis") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 2475, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=2200,label="~ 2475") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pDonnersbergkreis2)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Donnersbergkreis2.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
################
#delete objects#
################
rm(Donnersbergkreis, lm_Donnersbergkreis, pDonnersbergkreis, Donnersbergkreis_before2001, Donnersbergkreis_before2005, Donnersbergkreis_2001_2005, Donnersbergkreis_without_outliers, lm_Donnersbergkreis2, pDonnersbergkreis2)
#############################################################################
############
#Eifelkreis#
############
Eifelkreis <- subset(sb19.0_LK, lk_nr == 7232)
Eifelkreis_without_outliers <- subset(sb19_without_outliers, lk_nr == 7232)
##########################
#Sum of electricity yield#
##########################
sum(Eifelkreis_without_outliers$menge_mwh)
##############################################################################
#Count plants that are built before 2001 and between 2001 and 2005 and sum up# 
#the electricity yield                                                       #
##############################################################################
Eifelkreis_before2001 <- subset(Eifelkreis_without_outliers, inbetriebnahme < "2000-12-31")
sum(Eifelkreis_before2001$menge_mwh)
Eifelkreis_before2005 <- subset(Eifelkreis_without_outliers, inbetriebnahme < "2005-12-31")
Eifelkreis_2001_2005 <- subset(Eifelkreis_before2005, inbetriebnahme > "2000-12-31")
sum(Eifelkreis_2001_2005$menge_mwh)
##############
#linear model#
##############
attach(Eifelkreis_without_outliers)
lm_Eifelkreis <- lm(menge_mwh ~ inbetriebnahme)
summary(lm_Eifelkreis)
######
#Plot#
######
pEifelkreis <- ggplot(Eifelkreis_without_outliers, aes(x=inbetriebnahme, y=menge_mwh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Stromertrag [MWh]") +
  ggtitle("Stromertrag 2019 nach dem Inbetriebnahmedatum im \nLandkreis Eifelkreis") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 5700, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=6000,label="~ 5700") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pEifelkreis)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Eifelkreis.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
#############################################################################
#Compare the model with the model of full load hours to see if more suitable#
#############################################################################
lm_Eifelkreis2 <- lm(flh ~ inbetriebnahme)
summary(lm_Eifelkreis2)
######
#Plot#
######
pEifelkreis2 <- ggplot(Eifelkreis_without_outliers, aes(x=inbetriebnahme, y=flh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Volllaststunden [h]") +
  ggtitle("Volllaststunden 2019 nach dem Inbetriebnahmedatum im \nLandkreis Eifelkreis") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 1875, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=2200,label="~ 1875") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pEifelkreis2)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Eifelkreis2.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
################
#delete objects#
################
rm(Eifelkreis, lm_Eifelkreis, pEifelkreis, Eifelkreis_before2001, Eifelkreis_before2005, Eifelkreis_2001_2005, Eifelkreis_without_outliers, lm_Eifelkreis2, pEifelkreis2)
#############################################################################
#############
#Germersheim#
#############
Germersheim <- subset(sb19.0_LK, lk_nr == 7334)
Germersheim_without_outliers <- subset(sb19_without_outliers, lk_nr == 7334)
##########################
#Sum of electricity yield#
##########################
sum(Germersheim_without_outliers$menge_mwh)
##############################################################################
#Count plants that are built before 2001 and between 2001 and 2005 and sum up# 
#the electricity yield                                                       #
##############################################################################
Germersheim_before2001 <- subset(Germersheim_without_outliers, inbetriebnahme < "2000-12-31")
sum(Germersheim_before2001$menge_mwh)
Germersheim_before2005 <- subset(Germersheim_without_outliers, inbetriebnahme < "2005-12-31")
Germersheim_2001_2005 <- subset(Germersheim_before2005, inbetriebnahme > "2000-12-31")
sum(Germersheim_2001_2005$menge_mwh)
##############
#linear model#
##############
attach(Germersheim_without_outliers)
lm_Germersheim <- lm(menge_mwh ~ inbetriebnahme)
summary(lm_Germersheim)
######
#Plot#
######
pGermersheim <- ggplot(Germersheim_without_outliers, aes(x=inbetriebnahme, y=menge_mwh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Stromertrag [MWh]") +
  ggtitle("Stromertrag 2019 nach dem Inbetriebnahmedatum im \nLandkreis Germersheim") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 8150, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=6000,label="~ 8150") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pGermersheim)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Germersheim.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
#############################################################################
#Compare the model with the model of full load hours to see if more suitable#
#############################################################################
lm_Germersheim2 <- lm(flh ~ inbetriebnahme)
summary(lm_Germersheim2)
######
#Plot#
######
pGermersheim2 <- ggplot(Germersheim_without_outliers, aes(x=inbetriebnahme, y=flh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Volllaststunden [h]") +
  ggtitle("Volllaststunden 2019 nach dem Inbetriebnahmedatum im \nLandkreis Germersheim") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 2425, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=2200,label="~ 2425") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pGermersheim2)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Germersheim2.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
################
#delete objects#
################
rm(Germersheim, lm_Germersheim, pGermersheim, Germersheim_before2001, Germersheim_before2005, Germersheim_2001_2005, Germersheim_without_outliers, lm_Germersheim2, pGermersheim2)
#############################################################################


################
#Kaiserslautern#
################
Kaiserslautern <- subset(sb19.0_LK, lk_nr == 7335)
Kaiserslautern_without_outliers <- subset(sb19_without_outliers, lk_nr == 7335)
##########################
#Sum of electricity yield#
##########################
sum(Kaiserslautern_without_outliers$menge_mwh)
##############################################################################
#Count plants that are built before 2001 and between 2001 and 2005 and sum up# 
#the electricity yield                                                       #
##############################################################################
Kaiserslautern_before2001 <- subset(Kaiserslautern_without_outliers, inbetriebnahme < "2000-12-31")
sum(Kaiserslautern_before2001$menge_mwh)
Kaiserslautern_before2005 <- subset(Kaiserslautern_without_outliers, inbetriebnahme < "2005-12-31")
Kaiserslautern_2001_2005 <- subset(Kaiserslautern_before2005, inbetriebnahme > "2000-12-31")
sum(Kaiserslautern_2001_2005$menge_mwh)
##############
#linear model#
##############
attach(Kaiserslautern_without_outliers)
lm_Kaiserslautern <- lm(menge_mwh ~ inbetriebnahme)
summary(lm_Kaiserslautern)
######
#Plot#
######
pKaiserslautern <- ggplot(Kaiserslautern_without_outliers, aes(x=inbetriebnahme, y=menge_mwh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Stromertrag [MWh]") +
  ggtitle("Stromertrag 2019 nach dem Inbetriebnahmedatum im \nLandkreis Kaiserslautern") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 7550, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=6000,label="~ 7550") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pKaiserslautern)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Kaiserslautern.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
#############################################################################
#Compare the model with the model of full load hours to see if more suitable#
#############################################################################
lm_Kaiserslautern2 <- lm(flh ~ inbetriebnahme)
summary(lm_Kaiserslautern2)
######
#Plot#
######
pKaiserslautern2 <- ggplot(Kaiserslautern_without_outliers, aes(x=inbetriebnahme, y=flh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Volllaststunden [h]") +
  ggtitle("Volllaststunden 2019 nach dem Inbetriebnahmedatum im \nLandkreis Kaiserslautern") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 2375, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=2200,label="~ 2375") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pKaiserslautern2)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Kaiserslautern2.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
################
#delete objects#
################
rm(Kaiserslautern, lm_Kaiserslautern, pKaiserslautern, Kaiserslautern_before2001, Kaiserslautern_before2005, Kaiserslautern_2001_2005, Kaiserslautern_without_outliers, lm_Kaiserslautern2, pKaiserslautern2)
#############################################################################


#######
#Kusel#
#######
Kusel <- subset(sb19.0_LK, lk_nr == 7336)
Kusel_without_outliers <- subset(sb19_without_outliers, lk_nr == 7336)
##########################
#Sum of electricity yield#
##########################
sum(Kusel_without_outliers$menge_mwh)
##############################################################################
#Count plants that are built before 2001 and between 2001 and 2005 and sum up# 
#the electricity yield                                                       #
##############################################################################
Kusel_before2001 <- subset(Kusel_without_outliers, inbetriebnahme < "2000-12-31")
sum(Kusel_before2001$menge_mwh)
Kusel_before2005 <- subset(Kusel_without_outliers, inbetriebnahme < "2005-12-31")
Kusel_2001_2005 <- subset(Kusel_before2005, inbetriebnahme > "2000-12-31")
sum(Kusel_2001_2005$menge_mwh)
##############
#linear model#
##############
attach(Kusel_without_outliers)
lm_Kusel <- lm(menge_mwh ~ inbetriebnahme)
summary(lm_Kusel)
######
#Plot#
######
pKusel <- ggplot(Kusel_without_outliers, aes(x=inbetriebnahme, y=menge_mwh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Stromertrag [MWh]") +
  ggtitle("Stromertrag 2019 nach dem Inbetriebnahmedatum im \nLandkreis Kusel") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 8900, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=6000,label="~ 8900") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pKusel)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Kusel.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
#############################################################################
#Compare the model with the model of full load hours to see if more suitable#
#############################################################################
lm_Kusel2 <- lm(flh ~ inbetriebnahme)
summary(lm_Kusel2)
######
#Plot#
######
pKusel2 <- ggplot(Kusel_without_outliers, aes(x=inbetriebnahme, y=flh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Volllaststunden [h]") +
  ggtitle("Volllaststunden 2019 nach dem Inbetriebnahmedatum im \nLandkreis Kusel") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 2700, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=2200,label="~ 2700") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pKusel2)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Kusel2.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
################
#delete objects#
################
rm(Kusel, lm_Kusel, pKusel, Kusel_before2001, Kusel_before2005, Kusel_2001_2005, Kusel_without_outliers, lm_Kusel2, pKusel2)
#############################################################################


#######
#Mainz#
#######
Mainz <- subset(sb19.0_LK, lk_nr == 7315)
Mainz_without_outliers <- subset(sb19_without_outliers, lk_nr == 7315)
##########################
#Sum of electricity yield#
##########################
sum(Mainz_without_outliers$menge_mwh)
##############################################################################
#Count plants that are built before 2001 and between 2001 and 2005 and sum up# 
#the electricity yield                                                       #
##############################################################################
Mainz_before2001 <- subset(Mainz_without_outliers, inbetriebnahme < "2000-12-31")
sum(Mainz_before2001$menge_mwh)
Mainz_before2005 <- subset(Mainz_without_outliers, inbetriebnahme < "2005-12-31")
Mainz_2001_2005 <- subset(Mainz_before2005, inbetriebnahme > "2000-12-31")
sum(Mainz_2001_2005$menge_mwh)
##############
#linear model#
##############
attach(Mainz_without_outliers)
lm_Mainz <- lm(menge_mwh ~ inbetriebnahme)
summary(lm_Mainz)
######
#Plot#
######
pMainz <- ggplot(Mainz_without_outliers, aes(x=inbetriebnahme, y=menge_mwh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Stromertrag [MWh]") +
  ggtitle("Stromertrag 2019 nach dem Inbetriebnahmedatum im \nLandkreis Mainz") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 7900, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=6000,label="~ 7900") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pMainz)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Mainz.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
#############################################################################
#Compare the model with the model of full load hours to see if more suitable#
#############################################################################
lm_Mainz2 <- lm(flh ~ inbetriebnahme)
summary(lm_Mainz2)
######
#Plot#
######
pMainz2 <- ggplot(Mainz_without_outliers, aes(x=inbetriebnahme, y=flh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Volllaststunden [h]") +
  ggtitle("Volllaststunden 2019 nach dem Inbetriebnahmedatum im \nLandkreis Mainz") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 2510, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=2200,label="~ 2510") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pMainz2)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Mainz2.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
################
#delete objects#
################
rm(Mainz, lm_Mainz, pMainz, Mainz_before2001, Mainz_before2005, Mainz_2001_2005, Mainz_without_outliers, lm_Mainz2, pMainz2)
#############################################################################


##############
#Mainz_Bingen#
##############
Mainz_Bingen <- subset(sb19.0_LK, lk_nr == 7339)
Mainz_Bingen_without_outliers <- subset(sb19_without_outliers, lk_nr == 7339)
##########################
#Sum of electricity yield#
##########################
sum(Mainz_Bingen_without_outliers$menge_mwh)
##############################################################################
#Count plants that are built before 2001 and between 2001 and 2005 and sum up# 
#the electricity yield                                                       #
##############################################################################
Mainz_Bingen_before2001 <- subset(Mainz_Bingen_without_outliers, inbetriebnahme < "2000-12-31")
sum(Mainz_Bingen_before2001$menge_mwh)
Mainz_Bingen_before2005 <- subset(Mainz_Bingen_without_outliers, inbetriebnahme < "2005-12-31")
Mainz_Bingen_2001_2005 <- subset(Mainz_Bingen_before2005, inbetriebnahme > "2000-12-31")
sum(Mainz_Bingen_2001_2005$menge_mwh)
##############
#linear model#
##############
attach(Mainz_Bingen_without_outliers)
lm_Mainz_Bingen <- lm(menge_mwh ~ inbetriebnahme)
summary(lm_Mainz_Bingen)
######
#Plot#
######
pMainz_Bingen <- ggplot(Mainz_Bingen_without_outliers, aes(x=inbetriebnahme, y=menge_mwh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Stromertrag [MWh]") +
  ggtitle("Stromertrag 2019 nach dem Inbetriebnahmedatum im \nLandkreis Mainz_Bingen") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 6800, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=6000,label="~ 6800") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pMainz_Bingen)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Mainz_Bingen.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
#############################################################################
#Compare the model with the model of full load hours to see if more suitable#
#############################################################################
lm_Mainz_Bingen2 <- lm(flh ~ inbetriebnahme)
summary(lm_Mainz_Bingen2)
######
#Plot#
######
pMainz_Bingen2 <- ggplot(Mainz_Bingen_without_outliers, aes(x=inbetriebnahme, y=flh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Volllaststunden [h]") +
  ggtitle("Volllaststunden 2019 nach dem Inbetriebnahmedatum im \nLandkreis Mainz_Bingen") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 2300, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=2200,label="~ 2300") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pMainz_Bingen2)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Mainz_Bingen2.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
################
#delete objects#
################
rm(Mainz_Bingen, lm_Mainz_Bingen, pMainz_Bingen, Mainz_Bingen_before2001, Mainz_Bingen_before2005, Mainz_Bingen_2001_2005, Mainz_Bingen_without_outliers, lm_Mainz_Bingen2, pMainz_Bingen2)
#############################################################################


###############
#Mayen_Koblenz#
###############
Mayen_Koblenz <- subset(sb19.0_LK, lk_nr == 7137)
Mayen_Koblenz_without_outliers <- subset(sb19_without_outliers, lk_nr == 7137)
##########################
#Sum of electricity yield#
##########################
sum(Mayen_Koblenz_without_outliers$menge_mwh)
##############################################################################
#Count plants that are built before 2001 and between 2001 and 2005 and sum up# 
#the electricity yield                                                       #
##############################################################################
Mayen_Koblenz_before2001 <- subset(Mayen_Koblenz_without_outliers, inbetriebnahme < "2000-12-31")
sum(Mayen_Koblenz_before2001$menge_mwh)
Mayen_Koblenz_before2005 <- subset(Mayen_Koblenz_without_outliers, inbetriebnahme < "2005-12-31")
Mayen_Koblenz_2001_2005 <- subset(Mayen_Koblenz_before2005, inbetriebnahme > "2000-12-31")
sum(Mayen_Koblenz_2001_2005$menge_mwh)
##############
#linear model#
##############
attach(Mayen_Koblenz_without_outliers)
lm_Mayen_Koblenz <- lm(menge_mwh ~ inbetriebnahme)
summary(lm_Mayen_Koblenz)
######
#Plot#
######
pMayen_Koblenz <- ggplot(Mayen_Koblenz_without_outliers, aes(x=inbetriebnahme, y=menge_mwh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Stromertrag [MWh]") +
  ggtitle("Stromertrag 2019 nach dem Inbetriebnahmedatum im \nLandkreis Mayen_Koblenz") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 7150, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=6000,label="~ 7150") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pMayen_Koblenz)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Mayen_Koblenz.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
#############################################################################
#Compare the model with the model of full load hours to see if more suitable#
#############################################################################
lm_Mayen_Koblenz2 <- lm(flh ~ inbetriebnahme)
summary(lm_Mayen_Koblenz2)
######
#Plot#
######
pMayen_Koblenz2 <- ggplot(Mayen_Koblenz_without_outliers, aes(x=inbetriebnahme, y=flh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Volllaststunden [h]") +
  ggtitle("Volllaststunden 2019 nach dem Inbetriebnahmedatum im \nLandkreis Mayen_Koblenz") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 2275, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=2200,label="~ 2275") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pMayen_Koblenz2)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Mayen_Koblenz2.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
################
#delete objects#
################
rm(Mayen_Koblenz, lm_Mayen_Koblenz, pMayen_Koblenz, Mayen_Koblenz_before2001, Mayen_Koblenz_before2005, Mayen_Koblenz_2001_2005, Mayen_Koblenz_without_outliers, lm_Mayen_Koblenz2, pMayen_Koblenz2)
#############################################################################


#########
#Neuwied#
#########
Neuwied <- subset(sb19.0_LK, lk_nr == 7138)
Neuwied_without_outliers <- subset(sb19_without_outliers, lk_nr == 7138)
################no wind turbines


###########
#Pirmasens#
###########
Pirmasens <- subset(sb19.0_LK, lk_nr == 7317)
Pirmasens_without_outliers <- subset(sb19_without_outliers, lk_nr == 7317)
##########################
#Sum of electricity yield#
##########################
sum(Pirmasens_without_outliers$menge_mwh)
################one wind turbine


################
#Rhein_Hunsrück#
################
Rhein_Hunsrück <- subset(sb19.0_LK, lk_nr == 7140)
Rhein_Hunsrück_without_outliers <- subset(sb19_without_outliers, lk_nr == 7140)
##########################
#Sum of electricity yield#
##########################
sum(Rhein_Hunsrück_without_outliers$menge_mwh)
##############################################################################
#Count plants that are built before 2001 and between 2001 and 2005 and sum up# 
#the electricity yield                                                       #
##############################################################################
Rhein_Hunsrück_before2001 <- subset(Rhein_Hunsrück_without_outliers, inbetriebnahme < "2000-12-31")
sum(Rhein_Hunsrück_before2001$menge_mwh)
Rhein_Hunsrück_before2005 <- subset(Rhein_Hunsrück_without_outliers, inbetriebnahme < "2005-12-31")
Rhein_Hunsrück_2001_2005 <- subset(Rhein_Hunsrück_before2005, inbetriebnahme > "2000-12-31")
sum(Rhein_Hunsrück_2001_2005$menge_mwh)
##############
#linear model#
##############
attach(Rhein_Hunsrück_without_outliers)
lm_Rhein_Hunsrück <- lm(menge_mwh ~ inbetriebnahme)
summary(lm_Rhein_Hunsrück)
######
#Plot#
######
pRhein_Hunsrück <- ggplot(Rhein_Hunsrück_without_outliers, aes(x=inbetriebnahme, y=menge_mwh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Stromertrag [MWh]") +
  ggtitle("Stromertrag 2019 nach dem Inbetriebnahmedatum im \nLandkreis Rhein_Hunsrück") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 7800, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=6000,label="~ 7800") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pRhein_Hunsrück)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Rhein_Hunsrück.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
#############################################################################
#Compare the model with the model of full load hours to see if more suitable#
#############################################################################
lm_Rhein_Hunsrück2 <- lm(flh ~ inbetriebnahme)
summary(lm_Rhein_Hunsrück2)
######
#Plot#
######
pRhein_Hunsrück2 <- ggplot(Rhein_Hunsrück_without_outliers, aes(x=inbetriebnahme, y=flh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Volllaststunden [h]") +
  ggtitle("Volllaststunden 2019 nach dem Inbetriebnahmedatum im \nLandkreis Rhein_Hunsrück") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 2475, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=2200,label="~ 2475") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pRhein_Hunsrück2)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Rhein_Hunsrück2.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
################
#delete objects#
################
rm(Rhein_Hunsrück, lm_Rhein_Hunsrück, pRhein_Hunsrück, Rhein_Hunsrück_before2001, Rhein_Hunsrück_before2005, Rhein_Hunsrück_2001_2005, Rhein_Hunsrück_without_outliers, lm_Rhein_Hunsrück2, pRhein_Hunsrück2)
#############################################################################


############
#Rhein_Lahn#
############
Rhein_Lahn <- subset(sb19.0_LK, lk_nr == 7141)
Rhein_Lahn_without_outliers <- subset(sb19_without_outliers, lk_nr == 7141)
##########################
#Sum of electricity yield#
##########################
sum(Rhein_Lahn_without_outliers$menge_mwh)
##############################################################################
#Count plants that are built before 2001 and between 2001 and 2005 and sum up# 
#the electricity yield                                                       #
##############################################################################
Rhein_Lahn_before2001 <- subset(Rhein_Lahn_without_outliers, inbetriebnahme < "2000-12-31")
sum(Rhein_Lahn_before2001$menge_mwh)
Rhein_Lahn_before2005 <- subset(Rhein_Lahn_without_outliers, inbetriebnahme < "2005-12-31")
Rhein_Lahn_2001_2005 <- subset(Rhein_Lahn_before2005, inbetriebnahme > "2000-12-31")
sum(Rhein_Lahn_2001_2005$menge_mwh)
##############
#linear model#
##############
attach(Rhein_Lahn_without_outliers)
lm_Rhein_Lahn <- lm(menge_mwh ~ inbetriebnahme)
summary(lm_Rhein_Lahn)
######
#Plot#
######
pRhein_Lahn <- ggplot(Rhein_Lahn_without_outliers, aes(x=inbetriebnahme, y=menge_mwh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Stromertrag [MWh]") +
  ggtitle("Stromertrag 2019 nach dem Inbetriebnahmedatum im \nLandkreis Rhein_Lahn") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 9600, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=6000,label="~ 9600") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pRhein_Lahn)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Rhein_Lahn.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
#############################################################################
#Compare the model with the model of full load hours to see if more suitable#
#############################################################################
lm_Rhein_Lahn2 <- lm(flh ~ inbetriebnahme)
summary(lm_Rhein_Lahn2)
######
#Plot#
######
pRhein_Lahn2 <- ggplot(Rhein_Lahn_without_outliers, aes(x=inbetriebnahme, y=flh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Volllaststunden [h]") +
  ggtitle("Volllaststunden 2019 nach dem Inbetriebnahmedatum im \nLandkreis Rhein_Lahn") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 3700, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=2200,label="~ 3700") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pRhein_Lahn2)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Rhein_Lahn2.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
################
#delete objects#
################
rm(Rhein_Lahn, lm_Rhein_Lahn, pRhein_Lahn, Rhein_Lahn_before2001, Rhein_Lahn_before2005, Rhein_Lahn_2001_2005, Rhein_Lahn_without_outliers, lm_Rhein_Lahn2, pRhein_Lahn2)
#############################################################################


#############
#Rhein_Pfalz#
#############
Rhein_Pfalz <- subset(sb19.0_LK, lk_nr == 7338)
Rhein_Pfalz_without_outliers <- subset(sb19_without_outliers, lk_nr == 7338)
##########################
#Sum of electricity yield#
##########################
sum(Rhein_Pfalz_without_outliers$menge_mwh)
##############################################################################
#Count plants that are built before 2001 and between 2001 and 2005 and sum up# 
#the electricity yield                                                       #
##############################################################################
Rhein_Pfalz_before2001 <- subset(Rhein_Pfalz_without_outliers, inbetriebnahme < "2000-12-31")
sum(Rhein_Pfalz_before2001$menge_mwh)
Rhein_Pfalz_before2005 <- subset(Rhein_Pfalz_without_outliers, inbetriebnahme < "2005-12-31")
Rhein_Pfalz_2001_2005 <- subset(Rhein_Pfalz_before2005, inbetriebnahme > "2000-12-31")
sum(Rhein_Pfalz_2001_2005$menge_mwh)
##############
#linear model#
##############
attach(Rhein_Pfalz_without_outliers)
lm_Rhein_Pfalz <- lm(menge_mwh ~ inbetriebnahme)
summary(lm_Rhein_Pfalz)
######
#Plot#
######
pRhein_Pfalz <- ggplot(Rhein_Pfalz_without_outliers, aes(x=inbetriebnahme, y=menge_mwh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Stromertrag [MWh]") +
  ggtitle("Stromertrag 2019 nach dem Inbetriebnahmedatum im \nLandkreis Rhein_Pfalz") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 4250, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=6000,label="~ 4250") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pRhein_Pfalz)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Rhein_Pfalz.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
#############################################################################
#Compare the model with the model of full load hours to see if more suitable#
#############################################################################
lm_Rhein_Pfalz2 <- lm(flh ~ inbetriebnahme)
summary(lm_Rhein_Pfalz2)
######
#Plot#
######
pRhein_Pfalz2 <- ggplot(Rhein_Pfalz_without_outliers, aes(x=inbetriebnahme, y=flh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Volllaststunden [h]") +
  ggtitle("Volllaststunden 2019 nach dem Inbetriebnahmedatum im \nLandkreis Rhein_Pfalz") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 1635, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=2200,label="~ 3700") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pRhein_Pfalz2)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Rhein_Pfalz2.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
################
#delete objects#
################
rm(Rhein_Pfalz, lm_Rhein_Pfalz, pRhein_Pfalz, Rhein_Pfalz_before2001, Rhein_Pfalz_before2005, Rhein_Pfalz_2001_2005, Rhein_Pfalz_without_outliers, lm_Rhein_Pfalz2, pRhein_Pfalz2)
#############################################################################


#####################
#Südliche_Weinstraße#
#####################
Südliche_Weinstraße <- subset(sb19.0_LK, lk_nr == 7337)
Südliche_Weinstraße_without_outliers <- subset(sb19_without_outliers, lk_nr == 7337)
##########################
#Sum of electricity yield#
##########################
sum(Südliche_Weinstraße_without_outliers$menge_mwh)
##############################################################################
#Count plants that are built before 2001 and between 2001 and 2005 and sum up# 
#the electricity yield                                                       #
##############################################################################
Südliche_Weinstraße_before2001 <- subset(Südliche_Weinstraße_without_outliers, inbetriebnahme < "2000-12-31")
sum(Südliche_Weinstraße_before2001$menge_mwh)
Südliche_Weinstraße_before2005 <- subset(Südliche_Weinstraße_without_outliers, inbetriebnahme < "2005-12-31")
Südliche_Weinstraße_2001_2005 <- subset(Südliche_Weinstraße_before2005, inbetriebnahme > "2000-12-31")
sum(Südliche_Weinstraße_2001_2005$menge_mwh)
##############
#linear model#
##############
attach(Südliche_Weinstraße_without_outliers)
lm_Südliche_Weinstraße <- lm(menge_mwh ~ inbetriebnahme)
summary(lm_Südliche_Weinstraße)
######
#Plot#
######
pSüdliche_Weinstraße <- ggplot(Südliche_Weinstraße_without_outliers, aes(x=inbetriebnahme, y=menge_mwh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Stromertrag [MWh]") +
  ggtitle("Stromertrag 2019 nach dem Inbetriebnahmedatum im \nLandkreis Südliche_Weinstraße") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 8450, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=6000,label="~ 8450") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pSüdliche_Weinstraße)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Südliche_Weinstraße.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
#############################################################################
#Compare the model with the model of full load hours to see if more suitable#
#############################################################################
lm_Südliche_Weinstraße2 <- lm(flh ~ inbetriebnahme)
summary(lm_Südliche_Weinstraße2)
######
#Plot#
######
pSüdliche_Weinstraße2 <- ggplot(Südliche_Weinstraße_without_outliers, aes(x=inbetriebnahme, y=flh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Volllaststunden [h]") +
  ggtitle("Volllaststunden 2019 nach dem Inbetriebnahmedatum im \nLandkreis Südliche_Weinstraße") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 2515, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=2200,label="~ 3700") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pSüdliche_Weinstraße2)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Südliche_Weinstraße2.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
################
#delete objects#
################
rm(Südliche_Weinstraße, lm_Südliche_Weinstraße, pSüdliche_Weinstraße, Südliche_Weinstraße_before2001, Südliche_Weinstraße_before2005, Südliche_Weinstraße_2001_2005, Südliche_Weinstraße_without_outliers, lm_Südliche_Weinstraße2, pSüdliche_Weinstraße2)
#############################################################################


##############
#Südwestpfalz#
##############
Südwestpfalz <- subset(sb19.0_LK, lk_nr == 7340)
Südwestpfalz_without_outliers <- subset(sb19_without_outliers, lk_nr == 7340)
##########################
#Sum of electricity yield#
##########################
sum(Südwestpfalz_without_outliers$menge_mwh)
##############################################################################
#Count plants that are built before 2001 and between 2001 and 2005 and sum up# 
#the electricity yield                                                       #
##############################################################################
Südwestpfalz_before2001 <- subset(Südwestpfalz_without_outliers, inbetriebnahme < "2000-12-31")
sum(Südwestpfalz_before2001$menge_mwh)
Südwestpfalz_before2005 <- subset(Südwestpfalz_without_outliers, inbetriebnahme < "2005-12-31")
Südwestpfalz_2001_2005 <- subset(Südwestpfalz_before2005, inbetriebnahme > "2000-12-31")
sum(Südwestpfalz_2001_2005$menge_mwh)
##############
#linear model#
##############
attach(Südwestpfalz_without_outliers)
lm_Südwestpfalz <- lm(menge_mwh ~ inbetriebnahme)
summary(lm_Südwestpfalz)
######
#Plot#
######
pSüdwestpfalz <- ggplot(Südwestpfalz_without_outliers, aes(x=inbetriebnahme, y=menge_mwh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Stromertrag [MWh]") +
  ggtitle("Stromertrag 2019 nach dem Inbetriebnahmedatum im \nLandkreis Südwestpfalz") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 8225, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=6000,label="~ 8225") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pSüdwestpfalz)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Südwestpfalz.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
#############################################################################
#Compare the model with the model of full load hours to see if more suitable#
#############################################################################
lm_Südwestpfalz2 <- lm(flh ~ inbetriebnahme)
summary(lm_Südwestpfalz2)
######
#Plot#
######
pSüdwestpfalz2 <- ggplot(Südwestpfalz_without_outliers, aes(x=inbetriebnahme, y=flh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Volllaststunden [h]") +
  ggtitle("Volllaststunden 2019 nach dem Inbetriebnahmedatum im \nLandkreis Südwestpfalz") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 2475, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=2200,label="~ 2475") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pSüdwestpfalz2)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Südwestpfalz2.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
################
#delete objects#
################
rm(Südwestpfalz, lm_Südwestpfalz, pSüdwestpfalz, Südwestpfalz_before2001, Südwestpfalz_before2005, Südwestpfalz_2001_2005, Südwestpfalz_without_outliers, lm_Südwestpfalz2, pSüdwestpfalz2)
#############################################################################


################
#Trier_Saarburg#
################
Trier_Saarburg <- subset(sb19.0_LK, lk_nr == 7235)
Trier_Saarburg_without_outliers <- subset(sb19_without_outliers, lk_nr == 7235)
##########################
#Sum of electricity yield#
##########################
sum(Trier_Saarburg_without_outliers$menge_mwh)
##############################################################################
#Count plants that are built before 2001 and between 2001 and 2005 and sum up# 
#the electricity yield                                                       #
##############################################################################
Trier_Saarburg_before2001 <- subset(Trier_Saarburg_without_outliers, inbetriebnahme < "2000-12-31")
sum(Trier_Saarburg_before2001$menge_mwh)
Trier_Saarburg_before2005 <- subset(Trier_Saarburg_without_outliers, inbetriebnahme < "2005-12-31")
Trier_Saarburg_2001_2005 <- subset(Trier_Saarburg_before2005, inbetriebnahme > "2000-12-31")
sum(Trier_Saarburg$menge_mwh)
##############
#linear model#
##############
attach(Trier_Saarburg_without_outliers)
lm_Trier_Saarburg <- lm(menge_mwh ~ inbetriebnahme)
summary(lm_Trier_Saarburg)
######
#Plot#
######
pTrier_Saarburg <- ggplot(Trier_Saarburg_without_outliers, aes(x=inbetriebnahme, y=menge_mwh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Stromertrag [MWh]") +
  ggtitle("Stromertrag 2019 nach dem Inbetriebnahmedatum im \nLandkreis Trier_Saarburg") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 7000, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=6000,label="~ 7000") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pTrier_Saarburg)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Trier_Saarburg.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
#############################################################################
#Compare the model with the model of full load hours to see if more suitable#
#############################################################################
lm_Trier_Saarburg2 <- lm(flh ~ inbetriebnahme)
summary(lm_Trier_Saarburg2)
######
#Plot#
######
pTrier_Saarburg2 <- ggplot(Trier_Saarburg_without_outliers, aes(x=inbetriebnahme, y=flh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Volllaststunden [h]") +
  ggtitle("Volllaststunden 2019 nach dem Inbetriebnahmedatum im \nLandkreis Trier_Saarburg") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 2675, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=2200,label="~ 2675") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pTrier_Saarburg2)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Trier_Saarburg2.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
################
#delete objects#
################
rm(Trier_Saarburg, lm_Trier_Saarburg, pTrier_Saarburg, Trier_Saarburg_before2001, Trier_Saarburg_before2005, Trier_Saarburg_2001_2005, Trier_Saarburg_without_outliers, lm_Trier_Saarburg2, pTrier_Saarburg2)
#############################################################################


#############
#Vulkaneifel#
#############
Vulkaneifel <- subset(sb19.0_LK, lk_nr == 7233)
Vulkaneifel_without_outliers <- subset(sb19_without_outliers, lk_nr == 7233)
##########################
#Sum of electricity yield#
##########################
sum(Vulkaneifel_without_outliers$menge_mwh)
##############################################################################
#Count plants that are built before 2001 and between 2001 and 2005 and sum up# 
#the electricity yield                                                       #
##############################################################################
Vulkaneifel_before2001 <- subset(Vulkaneifel_without_outliers, inbetriebnahme < "2000-12-31")
sum(Vulkaneifel_before2001$menge_mwh)
Vulkaneifel_before2005 <- subset(Vulkaneifel_without_outliers, inbetriebnahme < "2005-12-31")
Vulkaneifel_2001_2005 <- subset(Vulkaneifel_before2005, inbetriebnahme > "2000-12-31")
sum(Vulkaneifel_2001_2005$menge_mwh)
##############
#linear model#
##############
attach(Vulkaneifel_without_outliers)
lm_Vulkaneifel <- lm(menge_mwh ~ inbetriebnahme)
summary(lm_Vulkaneifel)
######
#Plot#
######
pVulkaneifel <- ggplot(Vulkaneifel_without_outliers, aes(x=inbetriebnahme, y=menge_mwh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Stromertrag [MWh]") +
  ggtitle("Stromertrag 2019 nach dem Inbetriebnahmedatum im \nLandkreis Vulkaneifel") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 7800, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=6000,label="~ 7800") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pVulkaneifel)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Vulkaneifel.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
#############################################################################
#Compare the model with the model of full load hours to see if more suitable#
#############################################################################
lm_Vulkaneifel2 <- lm(flh ~ inbetriebnahme)
summary(lm_Vulkaneifel2)
######
#Plot#
######
pVulkaneifel2 <- ggplot(Vulkaneifel_without_outliers, aes(x=inbetriebnahme, y=flh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Volllaststunden [h]") +
  ggtitle("Volllaststunden 2019 nach dem Inbetriebnahmedatum im \nLandkreis Vulkaneifel") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 2825, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=2200,label="~ 2825") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pVulkaneifel2)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Vulkaneifel2.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
################
#delete objects#
################
rm(Vulkaneifel, lm_Vulkaneifel, pVulkaneifel, Vulkaneifel_before2001, Vulkaneifel_before2005, Vulkaneifel_2001_2005, Vulkaneifel_without_outliers, lm_Vulkaneifel2, pVulkaneifel2)
#############################################################################


############
#Westerwald#
############
Westerwald <- subset(sb19.0_LK, lk_nr == 7143)
Westerwald_without_outliers <- subset(sb19_without_outliers, lk_nr == 7143)
##########################
#Sum of electricity yield#
##########################
sum(Westerwald_without_outliers$menge_mwh)
##############################################################################
#Count plants that are built before 2001 and between 2001 and 2005 and sum up# 
#the electricity yield                                                       #
##############################################################################
Westerwald_before2001 <- subset(Westerwald_without_outliers, inbetriebnahme < "2000-12-31")
sum(Westerwald_before2001$menge_mwh)
Westerwald_before2005 <- subset(Westerwald_without_outliers, inbetriebnahme < "2005-12-31")
Westerwald_2001_2005 <- subset(Westerwald_before2005, inbetriebnahme > "2000-12-31")
sum(Westerwald_2001_2005$menge_mwh)
##############
#linear model#
##############
attach(Westerwald_without_outliers)
lm_Westerwald <- lm(menge_mwh ~ inbetriebnahme)
summary(lm_Westerwald)
######
#Plot#
######
pWesterwald <- ggplot(Westerwald_without_outliers, aes(x=inbetriebnahme, y=menge_mwh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Stromertrag [MWh]") +
  ggtitle("Stromertrag 2019 nach dem Inbetriebnahmedatum im \nLandkreis Westerwald") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 6650, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=6000,label="~ 6650") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pWesterwald)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Westerwald.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
#############################################################################
#Compare the model with the model of full load hours to see if more suitable#
#############################################################################
lm_Westerwald2 <- lm(flh ~ inbetriebnahme)
summary(lm_Westerwald2)
######
#Plot#
######
pWesterwald2 <- ggplot(Westerwald_without_outliers, aes(x=inbetriebnahme, y=flh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Volllaststunden [h]") +
  ggtitle("Volllaststunden 2019 nach dem Inbetriebnahmedatum im \nLandkreis Westerwald") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 2350, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=2200,label="~ 2350") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pWesterwald2)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Westerwald2.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
################
#delete objects#
################
rm(Westerwald, lm_Westerwald, pWesterwald, Westerwald_before2001, Westerwald_before2005, Westerwald_2001_2005, Westerwald_without_outliers, lm_Westerwald2, pWesterwald2)
#############################################################################


#######
#Worms#
#######
Worms <- subset(sb19.0_LK, lk_nr == 7319)
Worms_without_outliers <- subset(sb19_without_outliers, lk_nr == 7319)
##########################
#Sum of electricity yield#
##########################
sum(Worms_without_outliers$menge_mwh)
##############################################################################
#Count plants that are built before 2001 and between 2001 and 2005 and sum up# 
#the electricity yield                                                       #
##############################################################################
Worms_before2001 <- subset(Worms_without_outliers, inbetriebnahme < "2000-12-31")
sum(Worms_before2001$menge_mwh)
Worms_before2005 <- subset(Worms_without_outliers, inbetriebnahme < "2005-12-31")
Worms_2001_2005 <- subset(Worms_before2005, inbetriebnahme > "2000-12-31")
sum(Worms_2001_2005$menge_mwh)
##############
#linear model#
##############
attach(Worms_without_outliers)
lm_Worms <- lm(menge_mwh ~ inbetriebnahme)
summary(lm_Worms)
######
#Plot#
######
pWorms <- ggplot(Worms_without_outliers, aes(x=inbetriebnahme, y=menge_mwh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Stromertrag [MWh]") +
  ggtitle("Stromertrag 2019 nach dem Inbetriebnahmedatum im \nLandkreis Worms") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 6690, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=6000,label="~ 6690") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pWorms)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Worms.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
#############################################################################
#Compare the model with the model of full load hours to see if more suitable#
#############################################################################
lm_Worms2 <- lm(flh ~ inbetriebnahme)
summary(lm_Worms2)
######
#Plot#
######
pWorms2 <- ggplot(Worms_without_outliers, aes(x=inbetriebnahme, y=flh)) +
  geom_point(size = 1) +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE, fullrange = TRUE) +
  xlab("Inbetriebnahmedatum") +
  ylab("Volllaststunden [h]") +
  ggtitle("Volllaststunden 2019 nach dem Inbetriebnahmedatum im \nLandkreis Worms") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         plot.title = element_text(size=16, face = "bold"),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 2300, linetype = 'dashed') +
  annotate(geom="text",x=as.Date("1991-01-01"),
           y=2200,label="~ 2300") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend")
print(pWorms2)
####################
#save plot as image#
####################
ggsave("C:/Users/elias.cuadra/Desktop/Final Files/images/sb_LK/Worms2.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)
################
#delete objects#
################
rm(Worms, lm_Worms, pWorms, Worms_before2001, Worms_before2005, Worms_2001_2005, Worms_without_outliers, lm_Worms2, pWorms2)
#############################################################################


#############
#Zweibrücken#
#############
Zweibrücken <- subset(sb19.0_LK, lk_nr == 7320)
Zweibrücken_without_outliers <- subset(sb19_without_outliers, lk_nr == 7320)
##########################
#Sum of electricity yield#
##########################
sum(Zweibrücken$menge_mwh)


#########################################################
#Check municipal communities for Windenergy power plants#
#########################################################
X <- subset(sb19.0_LK_VG, vg_nr == 732000)
sum(X$menge_mwh)
X_before2001 <- subset(X, inbetriebnahme < "2000-12-31")
sum(X_before2001$menge_mwh)
X_before2005 <- subset(X, inbetriebnahme < "2005-12-31")
X_2001_2005 <- subset(X_before2005, inbetriebnahme > "2000-12-31")
sum(X_2001_2005$menge_mwh)