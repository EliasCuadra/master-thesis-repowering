###################################################################################################
#Preparation of the master and movement data of the transmission system operator Amprion from 2019#
###################################################################################################

#packages#
Sys.setenv(LANG = "en")
pacman::p_load(rio, data.table, tidyverse, tidyr, purrr, magrittr, compare, 
               ggplot2)


#################################
#Import data and format callumns#
#################################
amprion_2019 <- read.csv("EEG-StammBew_2019_Amprion-EAtlas.csv")
names(amprion_2019)[1] <- "gem"


####################
#Variable selection#
####################
selection <- amprion_2019[,c(1:3,7,9,14:16,26)]
attach(selection)


###############################
#formatting commissioning date#
###############################
selection$inbetriebnahme <- as.Date(selection$inbetriebnahme, "%d/%m/%Y")


################################################################################
#creating groups of counties and calculate full load hours and change dimension#
################################################################################
selection_counties <- selection %>%
  mutate(lk_nr = sub("\\D*(\\d{4}).*", "\\1", selection$gem)) %>%
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
selection_counties$lk_nr <- as.numeric(selection_counties$lk_nr)


##############################################
#Create groups of the association communities#
##############################################
selection_counties_communities <- selection_counties %>%
  mutate(vg_nr = sub("\\D*(\\d{6}).*", "\\1", selection_counties$gem))%>%
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



##################################################################################
#Calculate linear model with electricity yield over time for Rhineland-Palatinate#
##################################################################################
###############
#Find outliers#
###############
small_values <- subset(selection_counties_communities, leistung < 100)
big_values <- subset(selection_counties_communities, leistung > 4500)
small_electricity_yield <- subset(selection_counties_communities, menge_mwh < 500)


#########################################
#Create subset without outliers and file#
#########################################
selection_built_2019 <- subset(selection_counties_communities, inbetriebnahme > "2019-02-15")
selection_without_outliers <- selection_counties_communities[-c(327, 329, 1025, 1026, 1030, 1033, 1530, 1592, 1620, 1623, 1409, 1591, 1662:1664, 1667:1677, 1679, 1683, 1684, 1688:1695, 1701),]

write.csv(selection_without_outliers,"results_of_preparation/amprion_2019_processed_and_without_outliers.csv")


#######################################################################################################################
#count plants for different comissioning periods for Rhineland-Palatinate and their electricity yield and create files#
#######################################################################################################################
comissioning_before2001 <- subset(selection_without_outliers, inbetriebnahme < "2000-12-31")
sum(comissioning_before2001$menge_kwh/10^6)
comissioning_before2005 <- subset(selection_without_outliers, inbetriebnahme < "2005-12-31")
comissioning_2001_2005 <- subset(comissioning_before2005, inbetriebnahme > "2000-12-31")
sum(comissioning_2001_2005$menge_kwh/10^6)

write.csv(comissioning_before2001,"results_of_preparation/comissioning_before2001.csv")
write.csv(comissioning_2001_2005,"results_of_preparation/comissioning_2001_2005.csv")
write.csv(comissioning_before2005,"results_of_preparation/comissioning_before_2005.csv")































