source("set_up.R")


df_usaid =import(file.path(dir_data, "Copy of BaseDadoUSAID21_01.xlsx")) %>%
  filter(!is.na(Comunidade)) %>%
  mutate(Comunidade = str_replace(Comunidade, "Chiconela\\, Vladimir Lenine e Avoz da FRELIMO", "Chiconela"),
         Comunidade = str_remove(Comunidade, "(?=\\/).*"))




#Shapefile globe
globe = maps::map("world", fill=TRUE, plot=FALSE) %>%
  sf::st_as_sf() 

#count of groups by province
data_districts = df_usaid %>%
  group_by(Province, District) %>%
  summarise(grupos = sum(`#  grupos`),.groups = 'drop') %>%
  mutate(prop =  grupos / min(grupos))


#count groups by comunidade
data_comunidades = df_usaid %>%
  group_by(
    Province, District, Comunidade
  ) %>%
  summarise(grupos = sum((as.numeric(`#  grupos`))),
            .groups = 'drop') 




###
###--Define parameters for each province ----



define_parameters = function(p){
  
  min_size = 12
  
  print(p)
  
  if(p == "Maputo"){
    
    padding = 2.1
    range = c(min_size * 1 , min_size* 1.5)
    xmin = 31.2
    xmax = 33.5
    ymin = -27
    ymax = -24
    nudge_x = c(-1,.7)
    nudge_y = c(-.2,.3)
    #nudge text on map
    nudge_text_x = 0
    nudge_text_y = 0
    nudge_bars = .4
    hjust_bars = 0
    org = "FH e CARE"
    
  }  else if(p == "Sofala"){
    
    padding = 2.5
    range = c(min_size * 1.6)
    xmin = 33
    xmax = 36.3
    ymin = -21.5
    ymax = -17
    nudge_x = c(.1)
    nudge_y = c(-.2)
    nudge_text_x = 0
    nudge_text_y = 0
    nudge_bars = .1
    hjust_bars = 0
    org = "Comussanas"
    
  } else if(p == "Zambezia") {
    list(
    scale = c(1.9,2.2),
    padding = 2.6,
    range = c(min_size * 1 , min_size* 1.9),
    xmin = 35,
    xmax = 39.3,
    ymin = -19.25,
    ymax = -14.25,
    nudge_x = c(.5,-.8),
    nudge_y = c(-.3,.3),
    nudge_text_x = 0,
    nudge_text_y = 0,
    nudge_bars = -1,
    hjust_bars = 0,
    org = "Nweti"
    )
    
    
  } else if(p == "Gaza") {
    list(
    scale = c(1.9,2.2, 1.99, 2.2),
    padding = 2.5,
    range = c(10, 20),
    xmin = 31.2,
    xmax = 34.8,
    ymin = -25.7,
    ymax = -21.5,
    #chokwe, Chongone, Xai Xai, Limpopo
    nudge_x = c(-1,0,0,-1),
    nudge_y = c(0,0,0,0),
    
    nudge_text_x = c(0,1,.15,0),
    nudge_text_y = c(0,0,-.1,.1),
    
    hjust_bars = 1,
    nudge_bars = 0,
    org = "Nweti"
    )
    
  }
  
  
  
}



provinces = unique(df_usaid$Province)


parameters = map(provinces,define_parameters)

##Crear map
crear_mapas  = function(p){
  
  
  
  # define data for this province ---------------------------------------
  #shapefile
  provinces = import(file.path(dir_data, "provinces.rds")) %>%
    sf::st_as_sf() %>%
    mutate(linea = ADM1_PT == p)
  
  #distritos
  distritos = import(file.path(dir_data, "districts.rds")) %>%
    sf::st_as_sf() %>%
    left_join(data_districts) %>%
    mutate(linea = Province == p)
  
  
  #create map ----------------------------------------------------------------
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
    #   #points ----------------------------------------------------------------
  geom_point(
    data = subset(distritos, grupos > 0) %>% filter(Province == p),
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
    #   #number of groups ------------------------------------------------------
  geom_text(
    data = subset(distritos, grupos > 0) %>% filter(Province == p),
    aes(
      x = lon,
      y = lat,
      label = grupos),
    color = 'white',
    size = 7,
    family= "Open Sans Light",
    nudge_x = nudge_text_x,
    nudge_y = nudge_text_y,
    angle = 0
  ) +
    #   
    #   #label of district on the map -----------------------------------------
  geom_text_repel(data =subset(distritos, grupos > 0) %>% filter(Province == p),
                  aes(y = lat,
                      x = lon,
                      label = District
                  ),
                  nudge_x = nudge_x,
                  nudge_y = nudge_y,
                  #fontface ='bold',
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
    scale_size_continuous(range = range) +
    scale_color_manual(values = c("white", purple)) +
    
    
    theme(
      
      panel.background = element_rect("#8AB4F8"),
      panel.grid = element_blank(),
      legend.position = 'none',
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(family = "Open Sans Light", size = 20, hjust = .5),
      plot.caption = element_text(family = "Open Sans Light", size = 8, hjust = 1)
    )
  
  
  #return(map)
  # 
  # ##bars ------------------------------------------------------------------------
  data_chart = data_comunidades %>%
    filter(Province == p) %>%
    filter(grupos > 0)
  
  print(p)
  
  plot = ggplot(data= data_chart,
                aes(x = grupos,
                    y = reorder(Comunidade, grupos),
                    fill = District)
                
  ) +
    geom_col(width = .8) +
    geom_text(aes(label = grupos),
              hjust= hjust_bars,
              nudge_x = nudge_bars,
              size = 5,
              family = "Open Sans Light",
              color = purple
              
    )+
    facet_wrap(~District,
               scales = 'free_y') +
    # #Colors ----------------------------------------------------------------------
  scale_fill_manual(
    values = c(aqua, orange, aqua, orange)
  ) +
    labs(title = "NÚMERO DE GRUPOS POR COMUNIDADE",
         caption = paste0(org,","," 2020")
    )+
    theme(
      panel.background = element_blank(),
      panel.grid.minor.x = element_line(linetype = "dotted", colour = purple),
      
      #axis
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_text(hjust = 1,size = 14, family = "Open Sans Light"),
      axis.text.x = element_blank(),
      
      #legend
      legend.position = 'none',
      
      #strips
      strip.background = element_blank(),
      strip.text = element_text(hjust = 0, size = 16, family = "Open Sans Light"),
      
      #title and caption
      plot.title = element_text(family = "Open Sans Light", size = 20, hjust = .5),
      plot.caption = element_text(family = "Open Sans Light", size = 12, hjust = 1)
      
    )
  
  grid = plot_grid(map, plot, rel_widths = c(.5,1), rel_heights = c(1,1))
  #
  # #return(grid)
  #
  png = paste0(p,".png")
  filename = file.path(dir_charts, png)
  
  ggsave(filename, grid,
         width = 30,
         height = 12,
         units = "cm")
  
  
  print("success")
}


province_names = unique(df_usaid$Province)
map(province_names, crear_mapas)


