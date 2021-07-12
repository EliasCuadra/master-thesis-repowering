##########################################################################################################
#Preparation of the master and movement data of the transmission system operator Amprion from 2017 - 2019#
##########################################################################################################

#packages#
Sys.setenv(LANG = "en")
pacman::p_load(rio, data.table, tidyverse, tidyr, purrr, magrittr, compare, 
               ggplot2, DiagrammeR)


################################
#Import data and format columns#
################################
amprion_2017 <- read.csv("EEG_StammBew_2017_Amprion-EAtlas.csv")
amprion_2018 <- read.csv("EEG_StammBew_2018_Amprion-EAtlas.csv")
amprion_2019 <- read.csv("EEG-StammBew_2019_Amprion-EAtlas.csv")
names(amprion_2019)[1] <- "gem"
names(amprion_2018)[1] <- "gem"
names(amprion_2017)[1] <- "gem"


####################
#Variable selection#
####################
selection_2017 <- amprion_2017[,c(1:3,8,10,17,27)]
selection_2018 <- amprion_2018[,c(1:3,8,10,17,27)]
selection_2019 <- amprion_2019[,c(1:3,7,9,16,26)]


###############################
#formatting commissioning date#
###############################
selection_2017$inbetriebnahme <- as.Date(selection_2017$inbetriebnahme, "%d/%m/%Y")
selection_2018$inbetriebnahme <- as.Date(selection_2018$inbetriebnahme, "%d/%m/%Y")
selection_2019$inbetriebnahme <- as.Date(selection_2019$inbetriebnahme, "%d/%m/%Y")


###############
#find outliers#
###############
outliers_2017 <- filter(selection_2017, leistung < 100 | leistung > 4500 | inbetriebnahme > "2017-02-15")
outliers_2018 <- filter(selection_2018, leistung < 100 | leistung > 4500 | inbetriebnahme > "2018-02-15")
outliers_2019 <- filter(selection_2019, leistung < 100 | leistung > 4500 | inbetriebnahme > "2019-02-15")


############################
#create df without outliers#
############################
selection_2017_without_outliers <- setdiff(selection_2017, outliers_2017)
selection_2018_without_outliers <- setdiff(selection_2018, outliers_2018)
selection_2019_without_outliers <- setdiff(selection_2019, outliers_2019)


#############
#write files#
#############
#write.csv(selection_2017_without_outliers, "results_of_preparation/amprion_2017_proccessed_and_without_outliers.csv")
#write.csv(selection_2018_without_outliers, "results_of_preparation/amprion_2018_proccessed_and_without_outliers.csv")
#write.csv(selection_2019_without_outliers, "results_of_preparation/amprion_2019_proccessed_and_without_outliers.csv")


################################################################################
#creating groups of counties and calculate full load hours and change dimension#
################################################################################
selection_2019_counties <- selection_2019_without_outliers %>%
  mutate(lk_nr = sub("\\D*(\\d{4}).*", "\\1", selection_2019_without_outliers$gem)) %>%
  mutate(lk_name = case_when(lk_nr == 7131 ~ 'Ahrweiler',
                             lk_nr == 7132 ~ 'Altenkirchen',
                             lk_nr == 7133 ~ 'Bad Kreuznach',
                             lk_nr == 7134 ~ 'Birkenfeld',
                             lk_nr == 7135 ~ 'Cochem-Zell',
                             lk_nr == 7137 ~ 'Mayen-Koblenz',
                             lk_nr == 7138 ~ 'Neuwied',
                             lk_nr == 7140 ~ 'Rhein-Hundsr?ck',
                             lk_nr == 7141 ~ 'Rhein-Lahn',
                             lk_nr == 7143 ~ 'Westerwald',
                             lk_nr == 7211 ~ 'Trier',
                             lk_nr == 7231 ~ 'Bernkastel-Wittlich',
                             lk_nr == 7232 ~ 'Eifelkreis Bitburg-Pr?m',
                             lk_nr == 7233 ~ 'Vulkaneifel',
                             lk_nr == 7235 ~ 'Trier-Saarburg',
                             lk_nr == 7311 ~ 'Frankenthal',
                             lk_nr == 7312 ~ 'Kaiserslautern',
                             lk_nr == 7313 ~ 'Landau',
                             lk_nr == 7314 ~ 'Ludwigshafen',
                             lk_nr == 7315 ~ 'Mainz',
                             lk_nr == 7316 ~ 'Neustadt',
                             lk_nr == 7317 ~ 'Pirmasens',
                             lk_nr == 7318 ~ 'Speyer',
                             lk_nr == 7319 ~ 'Worms',
                             lk_nr == 7320 ~ 'Zweibr?cken',
                             lk_nr == 7331 ~ 'Alzey-Worms',
                             lk_nr == 7332 ~ 'Bad D?rkheim',
                             lk_nr == 7333 ~ 'Donnersbergkreis',
                             lk_nr == 7334 ~ 'Germersheim',
                             lk_nr == 7335 ~ 'Kaiserslautern',
                             lk_nr == 7336 ~ 'Kusel',
                             lk_nr == 7337 ~ 'S?dliche Weinstra?e',
                             lk_nr == 7338 ~ 'Rhein-Pfalz',
                             lk_nr == 7339 ~ 'Mainz-Bingen',
                             lk_nr == 7340 ~ 'S?dwestpfalz')) %>%
  mutate(flh = menge_kwh/leistung) %>%
  mutate(menge_mwh = round(menge_kwh/1000))


##################
#Formatting lk_nr#
##################
selection_2019_counties$lk_nr <- as.numeric(selection_2019_counties$lk_nr)

##############################################
#Create groups of the association communities#
##############################################
selection_2019_counties_communities <- selection_2019_counties %>%
  mutate(vg_nr = sub("\\D*(\\d{6}).*", "\\1", selection_2019_counties$gem))%>%
  mutate(vg_name = case_when(vg_nr == 713100 ~ 'vfr Ahrweiler',
                             vg_nr == 713101 ~ 'Adenau',
                             vg_nr == 713102 ~ 'Altenahr',
                             vg_nr == 713103 ~ 'Bad Breisig',
                             vg_nr == 713104 ~ 'Brohltal',
                             vg_nr == 713102 ~ 'Altenahr',
                             vg_nr == 713210 ~ 'Altenkirchen Flammersfeld',
                             vg_nr == 713203 ~ 'Daaden-Herdorf',
                             vg_nr == 713206 ~ 'Hamm (Siegen)',
                             vg_nr == 713207 ~ 'Kirchen (Siegen)',
                             vg_nr == 713208 ~ 'Wissen',
                             vg_nr == 713209 ~ 'Betzdorf-Gebhardshain',
                             vg_nr == 713300 ~ 'vfr Bad Kreuznach',
                             vg_nr == 713309 ~ 'Kirner Land',
                             vg_nr == 713301 ~ 'Bad Kreuznach',
                             vg_nr == 713311 ~ 'Langenlonsheim-Stromberg',
                             vg_nr == 713310 ~ 'Nahe Glan',
                             vg_nr == 713306 ~ 'R?desheim',
                             vg_nr == 713400 ~ 'vfr Birkenfeld',
                             vg_nr == 713401 ~ 'Baumholder',
                             vg_nr == 713402 ~ 'Birkefeld',
                             vg_nr == 713405 ~ 'Herstein-Rhaunen',
                             vg_nr == 713501 ~ 'Cochem',
                             vg_nr == 713502 ~ 'Kaisersesch',
                             vg_nr == 713503 ~ 'Ulmen',
                             vg_nr == 713505 ~ 'Zell (Mosel)',
                             vg_nr == 713700 ~ 'vfr Mayen-Koblenz',
                             vg_nr == 713701 ~ 'Pellenz',
                             vg_nr == 713702 ~ 'Maifeld',
                             vg_nr == 713703 ~ 'Vordereifel',
                             vg_nr == 713704 ~ 'Mendig',
                             vg_nr == 713707 ~ 'Vallendar',
                             vg_nr == 713708 ~ 'Wei?enthurm',
                             vg_nr == 713709 ~ 'Rhein-Mosel',
                             vg_nr == 713800 ~ 'vfr Neuwied',
                             vg_nr == 713801 ~ 'Asbach',
                             vg_nr == 713802 ~ 'Bad H?nningen',
                             vg_nr == 713803 ~ 'Dierdorf',
                             vg_nr == 713804 ~ 'Linz am Rhein',
                             vg_nr == 713805 ~ 'Puderbach',
                             vg_nr == 713807 ~ 'Unkel',
                             vg_nr == 713809 ~ 'Rengsdorf-Waldbreitbach',
                             vg_nr == 714000 ~ 'vfr Rhein-Hundsr?ck',
                             vg_nr == 714009 ~ 'Hundsr?ck-Mittelrhein',
                             vg_nr == 714003 ~ 'Kastellaun',
                             vg_nr == 714004 ~ 'Kirchberg (Hunsr?ck)',
                             vg_nr == 714008 ~ 'Simmern-Rheinb?llen',
                             vg_nr == 714100 ~ 'vfr Rhein-Lahn',
                             vg_nr == 714103 ~ 'Diez',
                             vg_nr == 714110 ~ 'Bad Ems-Nassau',
                             vg_nr == 714107 ~ 'Nast?tten',
                             vg_nr == 714109 ~ 'Loreley',
                             vg_nr == 714111 ~ 'Aar-Einrich',
                             vg_nr == 714301 ~ 'Bad Marienberg',
                             vg_nr == 714302 ~ 'Hachenburg',
                             vg_nr == 714303 ~ 'H?r-Grenzhausen',
                             vg_nr == 714304 ~ 'Montabaur',
                             vg_nr == 714305 ~ 'Ransbach-Baumbach',
                             vg_nr == 714306 ~ 'Rennerod',
                             vg_nr == 714307 ~ 'Selters (Westerwald)',
                             vg_nr == 714308 ~ 'Wallmerod',
                             vg_nr == 714309 ~ 'Westerburg',
                             vg_nr == 714310 ~ 'Wirges',
                             vg_nr == 721100 ~ 'vfr Trier',
                             vg_nr == 723100 ~ 'vfr Bernkastel-Wittlich',
                             vg_nr == 723101 ~ 'Bernkastel-Kues',
                             vg_nr == 723106 ~ 'Thalfang am Erbeskopf',
                             vg_nr == 723108 ~ 'Wittlich-Land',
                             vg_nr == 723109 ~ 'Traben-Trarbach',
                             vg_nr == 723200 ~ 'vfr Eifelkreis Bitburg-Pr?m',
                             vg_nr == 723201 ~ 'Arzfeld',
                             vg_nr == 723205 ~ 'S?deifel',
                             vg_nr == 723206 ~ 'Pr?m',
                             vg_nr == 723207 ~ 'Speicher',
                             vg_nr == 723208 ~ 'Bitburger Land',
                             vg_nr == 723301 ~ 'Daun',
                             vg_nr == 723304 ~ 'Kelberg',
                             vg_nr == 723306 ~ 'Geroldstein',
                             vg_nr == 723501 ~ 'Hermeskeil',
                             vg_nr == 723503 ~ 'Konz',
                             vg_nr == 723508 ~ 'Saarburg-Kell',
                             vg_nr == 723504 ~ 'Ruwer',
                             vg_nr == 723506 ~ 'Schweich',
                             vg_nr == 723507 ~ 'Trier-Land',
                             vg_nr == 733100 ~ 'Alzey',
                             vg_nr == 733101 ~ 'Alzey-Land',
                             vg_nr == 733102 ~ 'Eich',
                             vg_nr == 733103 ~ 'Monsheim',
                             vg_nr == 733105 ~ 'W?llstein',
                             vg_nr == 733106 ~ 'W?rrstadt',
                             vg_nr == 733107 ~ 'Wonnegau',
                             vg_nr == 733200 ~ 'vfr Bad D?rkheim',
                             vg_nr == 733201 ~ 'Deidesheim',
                             vg_nr == 733202 ~ 'Freinsheim',
                             vg_nr == 733205 ~ 'Lambrecht',
                             vg_nr == 733207 ~ 'Leiningerland',
                             vg_nr == 733206 ~ 'Wachenheim',
                             vg_nr == 733307 ~ 'Nordpf?lzer Land',
                             vg_nr == 733304 ~ 'Kirchheimbolanden',
                             vg_nr == 733302 ~ 'Eisenberg',
                             vg_nr == 733303 ~ 'G?llheim',
                             vg_nr == 733306 ~ 'Winnweiler',
                             vg_nr == 733400 ~ 'vfr Germersheim',
                             vg_nr == 733401 ~ 'Bellheim',
                             vg_nr == 733402 ~ 'Hagenbach',
                             vg_nr == 733403 ~ 'Jockgrim',
                             vg_nr == 733404 ~ 'Kandel',
                             vg_nr == 733405 ~ 'Lingenfeld',
                             vg_nr == 733406 ~ 'R?lzheim',
                             vg_nr == 733501 ~ 'Bruchm?hlbach-Miesau',
                             vg_nr == 733502 ~ 'Enkenbach-Alsenborn',
                             vg_nr == 733511 ~ 'Landstuhl',
                             vg_nr == 733508 ~ 'Ramstein-Miesenbach',
                             vg_nr == 733509 ~ 'Weilerbach',
                             vg_nr == 733510 ~ 'Otterbach-Otterberg',
                             vg_nr == 733608 ~ 'Lauterecken-Wolfstein',
                             vg_nr == 733610 ~ 'Kusel-Altenglan',
                             vg_nr == 733609 ~ 'Oberes Glantal',
                             vg_nr == 733701 ~ 'Annweiler am Triefels',
                             vg_nr == 733702 ~ 'Bad Bergzabern',
                             vg_nr == 733703 ~ 'Edenkoben',
                             vg_nr == 733704 ~ 'Herxheim',
                             vg_nr == 733705 ~ 'Landau-Land',
                             vg_nr == 733706 ~ 'Maikammer',
                             vg_nr == 733707 ~ 'Offenbach an der Queich',
                             vg_nr == 733800 ~ 'vfr Rhein-Pfalz',
                             vg_nr == 733801 ~ 'Dannstadt-Schauernheim',
                             vg_nr == 733804 ~ 'Maxdorf',
                             vg_nr == 733806 ~ 'Lambsheim-He?heim',
                             vg_nr == 733807 ~ 'R?merberg-Dudenhofen',
                             vg_nr == 733808 ~ 'Rheinauen',
                             vg_nr == 733900 ~ 'vfr Mainz-Bingen',
                             vg_nr == 733901 ~ 'Rhein-Nahe',
                             vg_nr == 733902 ~ 'Bodenheim',
                             vg_nr == 733903 ~ 'Gau-Algesheim',
                             vg_nr == 733906 ~ 'Nieder-Olm',
                             vg_nr == 733907 ~ 'Rhein-Selz',
                             vg_nr == 733908 ~ 'Sprendlingen-Gensingen',
                             vg_nr == 734001 ~ 'Dahner Felsenland',
                             vg_nr == 734002 ~ 'Hauenstein',
                             vg_nr == 734003 ~ 'Pirmasens Land',
                             vg_nr == 734004 ~ 'Rodalben',
                             vg_nr == 734006 ~ 'Waldfischbach-Burgalben',
                             vg_nr == 734009 ~ 'Thaleischweiler-Wallhalben',
                             vg_nr == 734008 ~ 'Zweibr?cken-Land',
                             vg_nr == 731700 ~ 'Pirmasens',
                             vg_nr == 731500 ~ 'Mainz',
                             vg_nr == 731900 ~ 'Worms',
                             vg_nr == 732000 ~ 'Zweibr?cken'))


#######################################################################################
#add electricity yiel in MWh and full load hours in the selection_without_outlier df's#
#######################################################################################
selection_2017_without_outliers <- selection_2017_without_outliers %>% 
  mutate(flh = menge_kwh/leistung) %>%
  mutate(menge_mwh = round(menge_kwh/1000))
selection_2018_without_outliers <- selection_2018_without_outliers %>% 
  mutate(flh = menge_kwh/leistung) %>%
  mutate(menge_mwh = round(menge_kwh/1000))
selection_2019_without_outliers <- selection_2019_without_outliers %>% 
  mutate(flh = menge_kwh/leistung) %>%
  mutate(menge_mwh = round(menge_kwh/1000))


#######################################################################################################################
#count plants for different comissioning periods for Rhineland-Palatinate and their electricity yield and create files#
#######################################################################################################################
comissioning_before2001 <- subset(selection_without_outliers, inbetriebnahme < "2000-12-31")
sum(comissioning_before2001$menge_kwh/10^6)
comissioning_before2005 <- subset(selection_without_outliers, inbetriebnahme < "2005-12-31")
comissioning_2001_2005 <- subset(comissioning_before2005, inbetriebnahme > "2000-12-31")
sum(comissioning_2001_2005$menge_kwh/10^6)

#write.csv(comissioning_before2001,"results_of_preparation/commissioning_before2001.csv")
#write.csv(comissioning_2001_2005,"results_of_preparation/commissioning_2001_2005.csv")
#write.csv(comissioning_before2005,"results_of_preparation/commissioning_before_2005.csv")


#######################################
#create flow chart of data preparation#
#######################################
#grViz(diagram = "digraph flowchart {
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
  tab3 -> tab2;
  tab6 -> tab2;
  tab2 -> tab6;
}
  
  [1]: 'Import data from 2017, 2018 and 2019'
  [2]: 'Adjust variable formats and names'
  [3]: 'Drop unnecessary variables' 
  [4]: 'Detect outliers and remove from data'
  [5]: 'Write files as: \\n amprion_2017_proccessed_and_without_outliers.csv \\n amprion_2018_proccessed_and_without_outliers.csv \\n amprion_2019_proccessed_and_without_outliers.csv'
  [6]: 'Group data of 2019 by counties and municipalities \\n with the community key'
  [7]: 'Add columns for full load hours and electricity yield in MWh'
  [8]: 'Filter for different commissioning dates'
  [9]: 'Write files as: \\n commissioning_before_2005.csv, \\n commissioning_before2001.csv \\n commissioning_2001_2005.csv'
  
  ")




################################################################################





##################################
#Statistical analysis of the data#
##################################


###########################################################################
#Linear and polynomial models of electricity yield over commissioning date#
###########################################################################
#2017
lm_electricity_yield_2017 <- lm(selection_2017_without_outliers$menge_mwh ~ selection_2017_without_outliers$inbetriebnahme)
summary(lm_electricity_yield_2017)
plot(lm_electricity_yield_2017)
#2018
lm_electricity_yield_2018 <- lm(selection_2018_without_outliers$menge_mwh ~ selection_2018_without_outliers$inbetriebnahme)
summary(lm_electricity_yield_2018)
plot(lm_electricity_yield_2018)
#2019
lm_electricity_yield_2019 <- lm(selection_2019_without_outliers$menge_mwh ~ selection_2019_without_outliers$inbetriebnahme)
summary(lm_electricity_yield_2019)
par(mfrow = c(2, 2))
plot(lm_electricity_yield_2019)
#check polynomial model for 2019
pm_electricity_yield_2019 <- lm(selection_2019_without_outliers$menge_mwh ~ poly(selection_2019_without_outliers$inbetriebnahme, 3))
summary(pm_electricity_yield_2019)
plot(pm_electricity_yield_2019)
#2018
pm_electricity_yield_2018 <- lm(selection_2018_without_outliers$menge_mwh ~ poly(selection_2018_without_outliers$inbetriebnahme, 3))
summary(pm_electricity_yield_2018)
plot(pm_electricity_yield_2018)
#2018
pm_electricity_yield_2017 <- lm(selection_2017_without_outliers$menge_mwh ~ poly(selection_2017_without_outliers$inbetriebnahme, 3))
summary(pm_electricity_yield_2017)
plot(pm_electricity_yield_2017)



###################################################################
#Plot the values with linear and polynomial trends for 2017 - 2019#
###################################################################
pelectricity_yield_2019_poly <- ggplot() +
  geom_point(data = selection_2017_without_outliers, aes(x=inbetriebnahme, y=menge_mwh), size = 0.4, colour = "#03a1fc") +
  geom_point(data = selection_2018_without_outliers, aes(x=inbetriebnahme, y=menge_mwh), size = 0.4, colour = "#fc5a03") +
  geom_point(data = selection_2019_without_outliers, aes(x=inbetriebnahme, y=menge_mwh), size = 0.4, colour = "#94fc03") +
  geom_smooth(data = selection_2017_without_outliers, aes(x=inbetriebnahme, y=menge_mwh, colour = "2017"), method=lm, se=TRUE, fullrange = TRUE, size = 0.5)  +
  geom_smooth(data = selection_2018_without_outliers, aes(x=inbetriebnahme, y=menge_mwh, colour = "2018"), method=lm, se=TRUE, fullrange = TRUE, size = 0.5)  +
  geom_smooth(data = selection_2019_without_outliers, aes(x=inbetriebnahme, y=menge_mwh, colour = "2019"), method=lm, se=TRUE, fullrange = TRUE, size = 0.5)  +
  geom_smooth(data = selection_2017_without_outliers, aes(x=inbetriebnahme, y=menge_mwh, colour = "2017"), method= "lm", formula = y ~ poly(x, 3), fullrange = TRUE, size = 0.5)  +
  geom_smooth(data = selection_2018_without_outliers, aes(x=inbetriebnahme, y=menge_mwh, colour = "2018"), method= "lm", formula = y ~ poly(x, 3), fullrange = TRUE, size = 0.5)  +
  geom_smooth(data = selection_2019_without_outliers, aes(x=inbetriebnahme, y=menge_mwh, colour = "2019"), method= "lm", formula = y ~ poly(x, 3), fullrange = TRUE, size = 0.5)  +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  ylim(-1000, 20000) +
  xlab("Comissioning Date") +
  ylab("Electricity yield per WT and year [MWh]") +
  theme( axis.text=element_text(size=11),
         axis.title=element_text(size=12),
         plot.title = element_text(size=14),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 7400, linetype = 'dashed', size = 0.25) +
  annotate(geom="text",x=as.Date("1997-01-01"),
           y=6500,label="mean of ~ 7,400 MWh/a in 2021 with linear trends", size = 2.5) +
  geom_hline(yintercept = 10000, linetype = 'dashed', size = 0.25) +
  annotate(geom="text",x=as.Date("2000-01-01"),
           y=11000,label="mean of ~ 10,000 MWh/a in 2030 with linear trends", size = 2.5) +
  geom_hline(yintercept = 15000, linetype = 'dashed', size = 0.25) +
  annotate(geom="text",x=as.Date("2003-01-01"),
           y=14200, label="> 15,000 MWh/a in 2030 with polynomial models", size = 2.5) +
  annotate(geom="text",x=as.Date("2026-01-01"),
           y=2000,label= "All adj. R² (3rd poly) ~ 72 %", size = 2.5) +
  annotate(geom="text",x=as.Date("2026-01-01"),
           y=3500,label= "All R² (linear) ~ 68 - 70 %", size = 2.5) +
  annotate(geom="text",x=as.Date("2026-01-01"),
           y=400,label="p-values all << 0.001", size = 2.5) +
  scale_colour_manual(name = "Year", values=c("#03a1fc", "#fc5a03", "#94fc03")) 



############
#print plot#
############
pelectricity_yield_2019_poly +  theme(legend.position = c(0.25,0.9))


####################
#save plot as image#
####################
ggsave("results_of_analysis/electricity_yield_2017-2019_rlp_over_comissioning_date2.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)




###############################################################################



####################################################
#linear models with electricity over rated capacity#
####################################################
lm_e_over_rated_capacity_2019 <- lm(selection_2019_without_outliers$menge_mwh ~ selection_2019_without_outliers$leistung)
summary(lm_e_over_rated_capacity_2019)
plot(lm_e_over_rated_capacity_2019)
##########################################################################
#Plot electricity yield over rated capacity with linear trend 2017 - 2019#
##########################################################################
pe_over_ratedcapacity_2019 <- ggplot() +
  geom_point(data = selection_2017_without_outliers, aes(x=leistung, y=menge_mwh), size = 0.4, colour = "#03a1fc") +
  geom_point(data = selection_2018_without_outliers, aes(x=leistung, y=menge_mwh), size = 0.4, colour = "#fc5a03") +
  geom_point(data = selection_2019_without_outliers, aes(x=leistung, y=menge_mwh), size = 0.4, colour = "#94fc03") +
  geom_smooth(data = selection_2017_without_outliers, aes(x=leistung, y=menge_mwh, colour = "2017"), method=lm, se=TRUE, fullrange = TRUE)  +
  geom_smooth(data = selection_2018_without_outliers, aes(x=leistung, y=menge_mwh, colour = "2018"), method=lm, se=TRUE, fullrange = TRUE)  +
  geom_smooth(data = selection_2019_without_outliers, aes(x=leistung, y=menge_mwh, colour = "2019"), method=lm, se=TRUE, fullrange = TRUE)  +
  theme_light() +
  ylim(-1000, 12000) +
  xlim(0,5000) +
  xlab("Rated capacity (kW)") +
  ylab("Electricity yield [MWh]") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=13),
         plot.title = element_text(size=16),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 7400, linetype = 'dashed', size = 0.25) +
  geom_vline(xintercept = 3500, linetype = 'dashed', size = 0.25) +
  annotate(geom="text",x=1000,
           y=7000,label="7,400 MWh/a in 2021 from plot above") +
  annotate(geom="text",x=4000,
           y=2000,label="min 3.5 MW") +
  annotate(geom="text",x=4500,
           y=11500,label= "R² = ~ 79 %") +
  scale_colour_manual(name = "Year", values=c("#03a1fc", "#fc5a03", "#94fc03")) 



############
#print plot#
############
pe_over_ratedcapacity_2019 +  theme(legend.position = c(0.25,0.9))


####################
#save plot as image#
####################
ggsave("results_of_analysis/electricity_rated_capacity.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)



###############################################################################



############################################################
#linear models with rated capacity over commissioning date #
############################################################
lm_rated_capacity_over_time_2019 <- lm(selection_2019_without_outliers$leistung ~ selection_2019_without_outliers$inbetriebnahme)
summary(lm_rated_capacity_over_time_2019)
plot(lm_rated_capacity_over_time_2019)
###########################################################################
#Plot rated capacity over commissioning date with linear trend 2017 - 2019#
###########################################################################
prated_capacity_over_commission <- ggplot() +
  geom_point(data = selection_2019_without_outliers, aes(x=inbetriebnahme, y=leistung), size = 0.4, colour = "#94fc03") +
  geom_smooth(data = selection_2019_without_outliers, aes(x=inbetriebnahme, y=leistung, colour = "2019"), method=lm, se=TRUE, fullrange = TRUE, size = 0.5)  +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  ylim(-1000, 6000) +
  xlab("Comissioning Date") +
  ylab("Rated capacity [kW]") +
  theme( axis.text=element_text(size=11),
         axis.title=element_text(size=12),
         plot.title = element_text(size=14),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 3500, linetype = 'dashed', size = 0.25) +
  annotate(geom="text",x=as.Date("1997-01-01"),
           y=3300,label="mean of ~ 3,500 kW in 2021", size = 2.5) +
  geom_hline(yintercept = 4500, linetype = 'dashed', size = 0.25) +
  annotate(geom="text",x=as.Date("1997-01-01"),
           y=4700,label="Predicted mean of ~ 4,500 kW in 2030", size = 2.5) +
  annotate(geom="text",x=as.Date("2025-01-01"),
           y=0,label= "R² = ~ 70 %") +
  scale_colour_manual(name = "Year", values="#94fc03") 



############
#print plot#
############
prated_capacity_over_commission +  theme(legend.position = c(0.25,0.9))


####################
#save plot as image#
####################
ggsave("results_of_analysis/rated_capacity_over_commissioning.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)




###############################################################################



#############################################################################
#linear models with full load hours  over the commissioning date 2017 - 2019#
#############################################################################
#2017
lm_flh_2017 <- lm(selection_2017_without_outliers$flh ~ selection_2017_without_outliers$inbetriebnahme)
summary(lm_flh_2017)
plot(lm_flh_2017)
#2018
lm_flh_2018 <- lm(selection_2018_without_outliers$flh ~ selection_2018_without_outliers$inbetriebnahme)
summary(lm_flh_2018)
plot(lm_flh_2018)
#2019
lm_flh_2019 <- lm(selection_2019_without_outliers$flh ~ selection_2019_without_outliers$inbetriebnahme)
summary(lm_flh_2019)
plot(lm_flh_2019)

#########################################################
#plot full load hours over commissioning date with trend#
#########################################################
pflh <- ggplot() +
  geom_point(data = selection_2017_without_outliers, aes(x=inbetriebnahme, y=flh), size = 0.4, colour = "#03a1fc") +
  geom_point(data = selection_2018_without_outliers, aes(x=inbetriebnahme, y=flh), size = 0.4, colour = "#fc5a03") +
  geom_point(data = selection_2019_without_outliers, aes(x=inbetriebnahme, y=flh), size = 0.4, colour = "#94fc03") +
  geom_smooth(data = selection_2017_without_outliers, aes(x=inbetriebnahme, y=flh, colour = "2017"), method=lm, se=TRUE, fullrange = TRUE, size = 0.5)  +
  geom_smooth(data = selection_2018_without_outliers, aes(x=inbetriebnahme, y=flh, colour = "2018"), method=lm, se=TRUE, fullrange = TRUE, size = 0.5)  +
  geom_smooth(data = selection_2019_without_outliers, aes(x=inbetriebnahme, y=flh, colour = "2019"), method=lm, se=TRUE, fullrange = TRUE, size = 0.5)  +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  ylim(0, 4000) +
  xlab("Comissioning Date") +
  ylab("Electricity yield per WT and year [MWh]") +
  theme( axis.text=element_text(size=11),
         axis.title=element_text(size=12),
         plot.title = element_text(size=14),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 7400, linetype = 'dashed', size = 0.25) +
  annotate(geom="text",x=as.Date("1997-01-01"),
           y=6500,label="mean of ~ 7,400 MWh/a in 2021 with linear trends", size = 2.5) +
  geom_hline(yintercept = 10000, linetype = 'dashed', size = 0.25) +
  annotate(geom="text",x=as.Date("2000-01-01"),
           y=11000,label="mean of ~ 10,000 MWh/a in 2030 with linear trends", size = 2.5) +
  geom_hline(yintercept = 15000, linetype = 'dashed', size = 0.25) +
  annotate(geom="text",x=as.Date("2003-01-01"),
           y=14200, label="> 15,000 MWh/a in 2030 with polynomial models", size = 2.5) +
  annotate(geom="text",x=as.Date("2026-01-01"),
           y=2000,label= "All adj. R² (3rd poly) ~ 72 %", size = 2.5) +
  annotate(geom="text",x=as.Date("2026-01-01"),
           y=3500,label= "All R² (linear) ~ 68 - 70 %", size = 2.5) +
  annotate(geom="text",x=as.Date("2026-01-01"),
           y=400,label="p-values all << 0.001", size = 2.5) +
  scale_colour_manual(name = "Year", values=c("#03a1fc", "#fc5a03", "#94fc03")) 



############
#print plot#
############
pflh +  theme(legend.position = c(0.25,0.9))


####################
#save plot as image#
####################
ggsave("results_of_analysis/electricity_yield_2017-2019_rlp_over_comissioning_date2.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)






