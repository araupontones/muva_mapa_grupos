source("set_up.R")


#Data from grupos

data_groups = import(file.path(dir_data, "BaseDadoUSAID.xlsx")) %>%
  group_by(Province, District) %>%
  summarise(grupos = sum(`#  grupos`),.groups = 'drop') %>%
  mutate(prop =  grupos / min(grupos))


#Shapefile globe
globe = maps::map("world", fill=TRUE, plot=FALSE) %>%
  sf::st_as_sf() 

#shapefile
provinces = import(file.path(dir_data, "provinces.rds")) %>%
  sf::st_as_sf() %>%
  mutate(linea = ADM1_PT == "Maputo")


distritos = import(file.path(dir_data, "districts.rds")) %>%
  sf::st_as_sf() %>%
  left_join(data_groups) %>%
  mutate(linea = Province == "Maputo")




#Define map --------------------------------------------------------------------

province_names = unique(data_groups$Province)


crear_mapas  = function(p){
  
  min_size = 12
  
  if(p == "Maputo"){
    padding = 2.1
    range = c(min_size * 1 , min_size* 1.83)
    xmin = 31.2
    xmax = 33.5
    ymin = -27
    ymax = -24
    nudge_x = c(-1,.7)
    nudge_y = c(-.2,.3)
    
  }  else if(p == "Sofala"){
    padding = 2.5
    range = c(min_size * 2.5)
    xmin = 33
    xmax = 36.3
    ymin = -21.5
    ymax = -17 
    nudge_x = c(.1)
    nudge_y = c(-.2)

  } else {
    scale = 1.83
    padding = 2.1
    range = c(min_size * 3.08 , min_size* 4)
    xmin = 35
    xmax = 39.3
    ymin = -19
    ymax = -15 
    nudge_x = c(.5,-.7)
    nudge_y = c(-.3,.3)
    
    

   }

  
  #shapefile
  provinces = import(file.path(dir_data, "provinces.rds")) %>%
    sf::st_as_sf() %>%
    mutate(linea = ADM1_PT == p)
  
  #distritos
  distritos = import(file.path(dir_data, "districts.rds")) %>%
    sf::st_as_sf() %>%
    left_join(data_groups) %>%
    mutate(linea = Province == p)
  
  
  map =  ggplot(data = globe) +
     geom_sf(color = NA) +

     #limits to center province
     xlim(xmin,xmax)+
     ylim(ymin,ymax) +
     #border distritos ------------------------------------------------------------
   geom_sf(data = distritos,
           aes(color = linea)
   ) +
     #provincias -----------------------------------------------------------

   geom_sf(data = provinces,
           aes(color = linea),
           size =1,
           alpha = .5,
           fill = NA) +
     geom_point(
       data = subset(distritos, grupos > 0),
       aes(
         x = lon,
         y = lat,
         size = grupos),
       shape = 21,
       alpha = .7,
       color = aqua,
       stroke = 2,
       fill = orange
     ) +
     geom_text(
       data = subset(distritos, grupos > 0),
       aes(
         x = lon,
         y = lat,
         label = grupos),
       color = 'white',
       size = 7,
       family= "Roboto"
     ) +
     geom_text_repel(data =subset(distritos, grupos > 0),
                     aes(y = lat,
                         x = lon,
                         label = District),
                     nudge_x = nudge_x,
                     nudge_y = nudge_y,
                     fontface ='bold',
                     size = 5,
                     point.padding = padding,
                     direction = "both",
                     segment.size = 1,
                     segment.colour = aqua,
                     family = "Open Sans Light",
                     color = "black",
                     alpha = 1
     ) +

     labs(title = str_to_upper(p)) +
     scale_size_continuous(range = c(min_size, min_size* scale)) +
     scale_color_manual(values = c("white", purple)) +


     theme(

       panel.background = element_rect("#8AB4F8"),
       panel.grid = element_blank(),
       legend.position = 'none',
       axis.text = element_blank(),
       axis.title = element_blank(),
       axis.ticks = element_blank(),
       plot.title = element_text(family = "Open Sans Light", size = 24, hjust = .5),
       plot.caption = element_text(family = "Open Sans Light", size = 8, hjust = 1)
     )



   return(map)
  
  
  }
  
  
mapas = map(province_names, crear_mapas)
names(mapas)<- province_names

mapas["Maputo"]
