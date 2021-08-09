##################################
#Statistical analysis of the data#
##################################

#packages#
Sys.setenv(LANG = "en")
pacman::p_load(rio, data.table, tidyverse, tidyr, purrr, magrittr, compare, 
               ggplot2, DiagrammeR)

#Import data and format columns
amprion_2015 <- read.csv("results_of_preparation/amprion_2015_processed.csv")
amprion_2016 <- read.csv("results_of_preparation/amprion_2016_processed.csv")
amprion_2017 <- read.csv("results_of_preparation/amprion_2017_processed.csv")
amprion_2018 <- read.csv("results_of_preparation/amprion_2018_processed.csv")
amprion_2019 <- read.csv("results_of_preparation/amprion_2019_processed.csv")

#formatting commissioning date
amprion_2015$inbetriebnahme <- as.Date(amprion_2015$inbetriebnahme, "%Y-%m-%d")
amprion_2016$inbetriebnahme <- as.Date(amprion_2016$inbetriebnahme, "%Y-%m-%d")
amprion_2017$inbetriebnahme <- as.Date(amprion_2017$inbetriebnahme, "%Y-%m-%d")
amprion_2018$inbetriebnahme <- as.Date(amprion_2018$inbetriebnahme, "%Y-%m-%d")
amprion_2019$inbetriebnahme <- as.Date(amprion_2019$inbetriebnahme, "%Y-%m-%d")

#Linear model of electricity yield over commissioning date 2017 - 2019
#2017
lm_electricity_yield_2017 <- lm(
  amprion_2017$menge_mwh ~ amprion_2017$inbetriebnahme)
summary(lm_electricity_yield_2017)
plot(lm_electricity_yield_2017)

#2018
lm_electricity_yield_2018 <- lm(
  amprion_2018$menge_mwh ~ amprion_2018$inbetriebnahme)
summary(lm_electricity_yield_2018)
plot(lm_electricity_yield_2018)

#2019
lm_electricity_yield_2019 <- lm(
  amprion_2019$menge_mwh ~ amprion_2019$inbetriebnahme)
summary(lm_electricity_yield_2019)
par(mfrow = c(2, 2))
plot(lm_electricity_yield_2019)

#check polynomial model
#2017
pm_electricity_yield_2017 <- lm(
  amprion_2017$menge_mwh ~ 
    poly(amprion_2017$inbetriebnahme, 3))
summary(pm_electricity_yield_2017)
plot(pm_electricity_yield_2017)

#2018
pm_electricity_yield_2018 <- lm(
  amprion_2018$menge_mwh ~ 
    poly(amprion_2018$inbetriebnahme, 3))
summary(pm_electricity_yield_2018)
plot(pm_electricity_yield_2018)

#2019
pm_electricity_yield_2019 <- lm(
  amprion_2019$menge_mwh ~ 
    poly(amprion_2019$inbetriebnahme, 3))
summary(pm_electricity_yield_2019)
plot(pm_electricity_yield_2019)

#Plot the values with linear and polynomial trends for 2017 - 2019
pelectricity_yield_2019_poly <- ggplot() +
  geom_point(data = amprion_2017, aes(x=inbetriebnahme, y=menge_mwh), 
             size = 0.4, colour = "#03a1fc") +
  geom_point(data = amprion_2018, aes(x=inbetriebnahme, y=menge_mwh), 
             size = 0.4, colour = "#fc5a03") +
  geom_point(data = amprion_2019, aes(x=inbetriebnahme, y=menge_mwh), 
             size = 0.4, colour = "#94fc03") +
  geom_smooth(data = amprion_2017, 
              aes(x=inbetriebnahme, y=menge_mwh, colour = "2017"), 
              method=lm, se=TRUE, fullrange = TRUE, size = 0.5)  +
  geom_smooth(data = amprion_2018, 
              aes(x=inbetriebnahme, y=menge_mwh, colour = "2018"), 
              method=lm, se=TRUE, fullrange = TRUE, size = 0.5)  +
  geom_smooth(data = amprion_2019, 
              aes(x=inbetriebnahme, y=menge_mwh, colour = "2019"), 
              method=lm, se=TRUE, fullrange = TRUE, size = 0.5)  +
  geom_smooth(data = amprion_2017, 
              aes(x=inbetriebnahme, y=menge_mwh, colour = "2017"), 
              method= "lm", formula = y ~ poly(x, 3), 
              fullrange = TRUE, size = 0.5)  +
  geom_smooth(data = amprion_2018, 
              aes(x=inbetriebnahme, y=menge_mwh, colour = "2018"), 
              method= "lm", formula = y ~ poly(x, 3), 
              fullrange = TRUE, size = 0.5)  +
  geom_smooth(data = amprion_2019, 
              aes(x=inbetriebnahme, y=menge_mwh, colour = "2019"), 
              method= "lm", formula = y ~ poly(x, 3), 
              fullrange = TRUE, size = 0.5)  +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  ylim(-1000, 20000) +
  xlab("Commissioning Date") +
  ylab("Electricity yield per WT and year [MWh]") +
  theme( axis.text=element_text(size=11),
         axis.title=element_text(size=12),
         plot.title = element_text(size=14),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 7400, linetype = 'dashed', size = 0.25) +
  annotate(geom="text",x=as.Date("1997-01-01"),
           y=6500,label="mean of ~ 7,400 MWh/a in 2021 with linear trends",
           size = 2.5) +
  geom_hline(yintercept = 10000, linetype = 'dashed', size = 0.25) +
  annotate(geom="text",x=as.Date("2000-01-01"),
           y=11000,label="mean of ~ 10,000 MWh/a in 2030 with linear trends", 
           size = 2.5) +
  geom_hline(yintercept = 15000, linetype = 'dashed', size = 0.25) +
  annotate(geom="text",x=as.Date("2003-01-01"),
           y=14200, label="> 15,000 MWh/a in 2030 with polynomial models", 
           size = 2.5) +
  annotate(geom="text",x=as.Date("2026-01-01"),
           y=2000,label= "All adj. R² (3rd poly) ~ 72 %", size = 2.5) +
  annotate(geom="text",x=as.Date("2026-01-01"),
           y=3500,label= "All R² (linear) ~ 68 - 70 %", size = 2.5) +
  annotate(geom="text",x=as.Date("2026-01-01"),
           y=400,label="p-values all << 0.001", size = 2.5) +
  scale_colour_manual(name = "Year", values=c("#03a1fc", "#fc5a03", "#94fc03")) 

#print plot
pelectricity_yield_2019_poly +  theme(legend.position = c(0.25,0.9))

#save plot as image
ggsave("results_of_analysis/electricity_yield_2017-2019.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)

#linear models with electricity over rated capacity 2019
lm_e_over_rated_capacity_2019 <- lm(
  amprion_2019$menge_mwh ~ amprion_2019$leistung)
summary(lm_e_over_rated_capacity_2019)
plot(lm_e_over_rated_capacity_2019)

#Plot electricity yield over rated capacity with linear trend 2017 - 2019
pe_over_ratedcapacity_2019 <- ggplot() +
  geom_point(data = amprion_2017, aes(x=leistung, y=menge_mwh), 
             size = 0.4, colour = "#03a1fc") +
  geom_point(data = amprion_2018, aes(x=leistung, y=menge_mwh), 
             size = 0.4, colour = "#fc5a03") +
  geom_point(data = amprion_2019, aes(x=leistung, y=menge_mwh), 
             size = 0.4, colour = "#94fc03") +
  geom_smooth(data = amprion_2017, 
              aes(x=leistung, y=menge_mwh, colour = "2017"), 
              method=lm, se=TRUE, fullrange = TRUE)  +
  geom_smooth(data = amprion_2018, 
              aes(x=leistung, y=menge_mwh, colour = "2018"), 
              method=lm, se=TRUE, fullrange = TRUE)  +
  geom_smooth(data = amprion_2019, 
              aes(x=leistung, y=menge_mwh, colour = "2019"), 
              method=lm, se=TRUE, fullrange = TRUE)  +
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

#print plot
pe_over_ratedcapacity_2019 +  theme(legend.position = c(0.25,0.9))

#save plot as image
ggsave("results_of_analysis/electricity_rated_capacity.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)

#linear models with rated capacity over commissioning date
lm_rated_capacity_over_time_2019 <- lm(
  amprion_2019$leistung ~ amprion_2019$inbetriebnahme)
summary(lm_rated_capacity_over_time_2019)
plot(lm_rated_capacity_over_time_2019)

#Plot rated capacity over commissioning date with linear trend 2017 - 2019
prated_capacity_over_commission <- ggplot() +
  geom_point(data = amprion_2019, 
             aes(x=inbetriebnahme, y=leistung), 
             size = 0.4, colour = "#94fc03") +
  geom_smooth(data = amprion_2019, 
              aes(x=inbetriebnahme, y=leistung, colour = "2019"), 
              method=lm, se=TRUE, fullrange = TRUE, size = 0.5)  +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  ylim(-1000, 6000) +
  xlab("Commissioning Date") +
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

#print plot
prated_capacity_over_commission +  theme(legend.position = c(0.25,0.9))

#save plot as image
ggsave("results_of_analysis/rated_capacity_over_commissioning.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)

#linear models with full load hours  over the commissioning date 2015 - 2019
#2015
lm_flh_2015 <- lm(
  amprion_2015$flh ~ amprion_2015$inbetriebnahme)
summary(lm_flh_2015)
plot(lm_flh_2015)

#2016
lm_flh_2016 <- lm(
  amprion_2016$flh ~ amprion_2016$inbetriebnahme)
summary(lm_flh_2016)
plot(lm_flh_2016)

#2017
lm_flh_2017 <- lm(
  amprion_2017$flh ~ amprion_2017$inbetriebnahme)
summary(lm_flh_2017)
plot(lm_flh_2017)

#2018
lm_flh_2018 <- lm(
  amprion_2018$flh ~ amprion_2018$inbetriebnahme)
summary(lm_flh_2018)
plot(lm_flh_2018)

#2019
lm_flh_2019 <- lm(
  amprion_2019$flh ~ amprion_2019$inbetriebnahme)
summary(lm_flh_2019)
plot(lm_flh_2019)

#plot full load hours over commissioning date with trend
pflh <- ggplot() +
  geom_point(data = amprion_2015, aes(x=inbetriebnahme, y=flh), 
             size = 0.4, colour = "#fd00e2") +
  geom_point(data = amprion_2016, aes(x=inbetriebnahme, y=flh), 
             size = 0.4, colour = "#fde600") +
  geom_point(data = amprion_2017, aes(x=inbetriebnahme, y=flh), 
             size = 0.4, colour = "#03a1fc") +
  geom_point(data = amprion_2018, aes(x=inbetriebnahme, y=flh), 
             size = 0.4, colour = "#fc5a03") +
  geom_point(data = amprion_2019, aes(x=inbetriebnahme, y=flh), 
             size = 0.4, colour = "#94fc03") +
  geom_smooth(data = amprion_2015, 
              aes(x=inbetriebnahme, y=flh, colour = "2015"), 
              method=lm, se=TRUE, fullrange = TRUE, size = 0.5)  +
  geom_smooth(data = amprion_2016, 
              aes(x=inbetriebnahme, y=flh, colour = "2016"), 
              method=lm, se=TRUE, fullrange = TRUE, size = 0.5)  +
  geom_smooth(data = amprion_2017, 
              aes(x=inbetriebnahme, y=flh, colour = "2017"), 
              method=lm, se=TRUE, fullrange = TRUE, size = 0.5)  +
  geom_smooth(data = amprion_2018, 
              aes(x=inbetriebnahme, y=flh, colour = "2018"),
              method=lm, se=TRUE, fullrange = TRUE, size = 0.5)  +
  geom_smooth(data = amprion_2019, 
              aes(x=inbetriebnahme, y=flh, colour = "2019"), 
              method=lm, se=TRUE, fullrange = TRUE, size = 0.5)  +
  theme_light() +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  ylim(0, 4000) +
  xlab("Commissioning Date") +
  ylab("Full load hours [h/a]") +
  theme( axis.text=element_text(size=11),
         axis.title=element_text(size=12),
         plot.title = element_text(size=14),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  geom_hline(yintercept = 2300, linetype = 'dashed', size = 0.25) +
  annotate(geom="text",x=as.Date("1997-01-01"),
           y=2200,label="mean of ~ 2,300 h/a in 2021 with linear trends", 
           size = 2.5) +
  geom_hline(yintercept = 2800, linetype = 'dashed', size = 0.25) +
  annotate(geom="text",x=as.Date("2000-01-01"),
           y=2900,label="mean of ~ 2,800 h/a in 2030 with linear trends", 
           size = 2.5) +
  geom_hline(yintercept = 15000, linetype = 'dashed', size = 0.25) +
  annotate(geom="text",x=as.Date("2003-01-01"),
           y=14200, label="> 15,000 MWh/a in 2030 with polynomial models", 
           size = 2.5) +
  annotate(geom="text",x=as.Date("2026-01-01"),
           y=800,label= "All R²  ~ 46 - 51 %", size = 2.5) +
  annotate(geom="text",x=as.Date("2026-01-01"),
           y=400,label="p-values all << 0.001", size = 2.5) +
  scale_colour_manual(name = "Year", 
                      values=c("#fd00e2", "#fde600", 
                               "#03a1fc", "#fc5a03", "#94fc03")) 

#print plot
pflh +  theme(legend.position = c(0.35,0.93))

#save plot as image
ggsave("results_of_analysis/flh.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)








