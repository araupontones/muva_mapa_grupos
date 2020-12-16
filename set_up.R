library(pacman)


p_load(rio, sf, tidyverse, ggrepel, extrafont, cowplot)


#loadfonts(device = 'win')
dir_shapes = "C:/mapa-risco-covid-mocambique/data/2.clean/districts.rds"
dir_shapes_risk = "C:/Users/andre/Dropbox/Andres/03.Dashboards/11.MozambiqueRisk/data/Moz_shape"
dir_data="datos"
dir_charts = 'charts'


#loadfonts(dev = 'win')

purple = "#8B42B9"
orange = "#F77333"
aqua = "#5CD6C7"

