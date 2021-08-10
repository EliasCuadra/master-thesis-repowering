#check sgd-nord data

#packages#
Sys.setenv(LANG = "en")
pacman::p_load(rio, data.table, tidyverse, magrittr)

#import data
sgd_nord <- read.csv("sgd_nord.csv")
attach(sgd_nord)

#change date format
sgd_nord$datum <- as.Date(
  sgd_nord$datum, "%Y/%m/%d")

#filter date 0001-01-01
sgd_nord_filtered <- filter(sgd_nord, datum != "0001-01-01")

#check mean rotor diameter
mean(sgd_nord$rotor)
#86.33528 m

#plot rotor diameter over commissioning date
p_rotor <- ggplot() +
  geom_point(data = sgd_nord, aes(x=datum, y=rotor), 
             size = 0.4, colour = "#0051fd") +
  geom_smooth(data = sgd_nord, 
              aes(x=datum, y=rotor, colour = "2019"), 
              method=lm, se=TRUE, fullrange = TRUE, size = 0.5)  +
  theme_light() +
  ylim(25,170) +
  scale_x_date(limits = as.Date(c("1990-01-01","2030-12-31"))) +
  xlab("Commissioning date") +
  ylab("Rotor diameter [m]") +
  theme( axis.text=element_text(size=11),
         axis.title=element_text(size=12),
         plot.title = element_text(size=14),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  annotate(geom="text",x=as.Date("2025-01-01"),
           y=5,label= "R² = ~ 11 % \n p-value << 0.001") +
  scale_colour_manual(name = "Data from SGD north", 
                      values="#0051fd")

#plot
p_rotor +  theme(legend.position = c(0.7,0.9))

#save plot
ggsave("rotor_diameter.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)

#linear model
lm_rotor <- lm(sgd_nord_filtered$rotor ~ sgd_nord_filtered$datum)
summary(lm_rotor)
plot(lm_rotor)

















