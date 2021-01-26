source("set_up.R")


df_usaid =import(file.path(dir_data_clean, "usaid_clean.rds"))




#Shapefile globe
globe = maps::map("world", fill=TRUE, plot=FALSE) %>%
  sf::st_as_sf() 

#count of groups by province
data_groups = df_usaid %>%
  group_by(Province, District) %>%
  summarise(grupos = sum(`#  grupos`),.groups = 'drop') %>%
  mutate(prop =  grupos / min(grupos))


#count groups by comunidade
data_raw_comunidades = df_usaid %>%
  group_by(
    Province, District, Comunidade
  ) %>%
  summarise(grupos = sum((as.numeric(`#  grupos`))),
            .groups = 'drop') 




###
###-----------------------------------------------------------------------------


##Crear map
crear_mapas  = function(p){
  
  min_size = 12
  
  axis_text_size = 14
  strip_text_size = 16
  title_text_size = 20
  caption_text_size = 12
  text_bars_size = 5
  
  height_print = 12
  width_print = 30
  
  province_border_size = 1
  border_distritos_size = .5
  repel_text_size = 5
  grupos_text_size = 7
  alpha_points = .7
  grid_color = purple
  
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
    bars_text_color = purple
    
    
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
    bars_text_color = purple
    
   
    org = "Comussanas"
   

  } else if(p == "Zambezia") {
    scale = c(1.9,2.2)
    padding = 2.6
    range = c(min_size * 1 , min_size* 1.9)
    xmin = 35
    xmax = 39.3
    ymin = -19.25
    ymax = -14.25
    nudge_x = c(.5,-.8)
    nudge_y = c(-.3,.3)
    nudge_text_x = 0
    nudge_text_y = 0
    nudge_bars = -1
    hjust_bars = 0
    bars_text_color = "white"
   
    org = "Nweti"
   



  } else {

    scale = c(1.9,2.2, 1.99, 2.2)
    padding = 7
    range = c(40, 100)
    xmin = 31.2
    xmax = 34.8
    ymin = -25.7
    ymax = -21.5
    
    #chokwe, Chongone, Xai Xai, Limpopo
    nudge_x = c(-1,0,0,-1)
    nudge_y = c(0,0,0,0)

    nudge_text_x = c(0,0,.15,0)
    nudge_text_y = c(0,0,-.1,.1)
    
    province_border_size = 3
    border_distritos_size = 2

    hjust_bars = 1
    nudge_bars = 0
    
    width_print = 120
    height_print = 50
    
    axis_text_size = 28
    caption_text_size = 50
    text_bars_size = 10
    strip_text_size = 60
    title_text_size = 80
    bars_text_color = "white"
    
    repel_text_size = 20
    grupos_text_size = 18
    alpha_points = .5
    grid_color = 'white'
   
    org = "Nweti"


  }

  # define data for this province ---------------------------------------
  #shapefile
  provinces = import(file.path(dir_data, "provinces.rds")) %>%
    sf::st_as_sf() %>%
    mutate(linea = ADM1_PT == p)

  #distritos
  distritos = import(file.path(dir_data, "districts.rds")) %>%
    sf::st_as_sf() %>%
    left_join(data_groups) %>%
    mutate(linea = Province == p)


  #create map ----------------------------------------------------------------
  map =  ggplot(data = globe) +
    geom_sf(color = NA) +

    #limits to center province
    xlim(xmin,xmax)+
    ylim(ymin,ymax) +
    #border distritos ------------------------------------------------------------
  geom_sf(data = distritos,
          aes(color = linea),
          size = border_distritos_size
   ) +
    #provincias -----------------------------------------------------------

  geom_sf(data = provinces,
          aes(color = linea),
          size =province_border_size,
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
      alpha = alpha_points,
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
      size = grupos_text_size,
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
                    size = repel_text_size,

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
      plot.title = element_text(family = "Open Sans Light", size = title_text_size, hjust = .5),
      plot.caption = element_text(family = "Open Sans Light", size = 8, hjust = 1)
    )


  #return(map)
  # 
  # ##bars ------------------------------------------------------------------------
  data_chart = data_raw_comunidades %>%
    filter(Province == p) %>%
    filter(grupos > 0) %>%
    group_by(District) %>%
    mutate(District = as.factor(District),
           Comunidade = reorder_within(Comunidade, grupos, District))
    

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
            size = text_bars_size,
            family = "Open Sans Light",
            color = bars_text_color

  )+
  facet_wrap(~District,
             scales = 'free_y') +
    scale_y_reordered() +
  # #Colors ----------------------------------------------------------------------
  scale_fill_manual(
    values = c(aqua, orange, aqua, orange)
  ) +
    labs(title = "NÚMERO DE GRUPOS POR COMUNIDADE",
         caption = paste0(org,","," 2020")
         )+
    theme(
      panel.background = element_blank(),
      panel.grid.minor.x = element_line(linetype = "dotted", colour = grid_color),

      #axis
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_text(hjust = 1,size = axis_text_size, family = "Open Sans Light"),
      axis.text.x = element_blank(),

      #legend
      legend.position = 'none',

      #strips
      strip.background = element_blank(),
      strip.text = element_text(hjust = 0, size = strip_text_size, family = "Open Sans Light"),

      #title and caption
      plot.title = element_text(family = "Open Sans Light", size = title_text_size, hjust = .5),
      plot.caption = element_text(family = "Open Sans Light", size = caption_text_size, hjust = 1)

    )

  grid = plot_grid(map, plot, rel_widths = c(.5,1), rel_heights = c(1,1))
  #
  # #return(grid)
  #
  png = paste0(p,".png")
  pdf = paste0(p,".pdf")
  jpeg = paste0(p,".jpeg")
  filename = file.path(dir_charts, jpeg)

  ggsave(filename, grid,
         width = width_print,
         height = height_print,
         units = "cm")


  print("success")
}


province_names = unique(df_usaid$Province)
map(province_names, crear_mapas)


