source("set_up.R")


#Data from grupos

data_groups = import(file.path(dir_data, "BaseDadoUSAID.xlsx")) %>%
  group_by(Province, District) %>%
  summarise(grupos = sum(`#  grupos`),.groups = 'drop') %>%
  mutate(prop =  grupos / min(grupos))


data_raw_comunidades = import(file.path(dir_data, "BaseDadoUSAID.xlsx")) %>%
  group_by(
    Province, District, Comunidade
  ) %>%
  summarise(grupos = sum((as.numeric(`#  grupos`))),
            .groups = 'drop') 


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
    range = c(min_size * 1 , min_size* 1.5)
    xmin = 31.2
    xmax = 33.5
    ymin = -27
    ymax = -24
    nudge_x = c(-1,.7)
    nudge_y = c(-.2,.3)
    nudge_bars = .4
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
    nudge_bars = .1
    org = "Comussanas"
    
  } else {
    scale = c(1.9,2.2)
    padding = 2.6
    range = c(min_size * 1 , min_size* 1.9)
    xmin = 35
    xmax = 39.3
    ymin = -19.25
    ymax = -14.25
    nudge_x = c(.5,-.8)
    nudge_y = c(-.3,.3)
    nudge_bars = -1
    org = "Nweti"
    
    
    
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
      family= "Open Sans Light",
    ) +
    geom_text_repel(data =subset(distritos, grupos > 0),
                    aes(y = lat,
                        x = lon,
                        label = District),
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
  
  
  ##bars ------------------------------------------------------------------------
  data_chart = data_raw_comunidades %>%
    filter(Province == p) %>%
    filter(grupos > 0)
  
  
  plot = ggplot(data= data_chart,
                aes(x = grupos,
                    y = reorder(Comunidade, grupos),
                    fill = District)
                
  ) +
    geom_col(width = .8) +
    geom_text(aes(label = grupos),
              hjust= 0,
              nudge_x = nudge_bars,
              size = 5,
              family = "Open Sans Light",
              color = purple
              
    )+
    facet_wrap(~District,
               scales = 'free_y') +
    #Colors ----------------------------------------------------------------------
  scale_fill_manual(
    values = c(aqua, orange)
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
  
  #return(grid)
  
  png = paste0(p,".png")
  filename = file.path(dir_charts, png)
  
  ggsave(filename, grid,
         width = 30,
         height = 12,
         units = "cm")
  
  
}

map(province_names, crear_mapas)
