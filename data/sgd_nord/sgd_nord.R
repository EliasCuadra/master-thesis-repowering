#check sgd-nord data

#packages#
Sys.setenv(LANG = "en")
pacman::p_load(rio, data.table, tidyverse, tidyr, purrr, magrittr, compare, 
               ggplot2)

#import data

sgd_nord <- read.csv("sgd_nord.csv")
