source("set_up.R")
source("style.R")

#sapefile



data_raw = import(file.path(dir_data, "BaseDadoUSAID.xlsx")) %>%
  group_by(
    Province, District, Comunidade
  ) %>%
  summarise(grupos = sum((as.numeric(`#  grupos`))),
            .groups = 'drop') 
  

province_names = sort(unique(data_raw$Province))


crear_bars = function(p){
  
  data_chart = data_raw %>%
    filter(Province == p)
  
  
  plot = ggplot(data= data_chart,
                aes(x = grupos,
                    y = reorder(Comunidade, grupos),
                    fill = District)
                
  ) +
    geom_col(width = .8) +
    geom_text(aes(label = grupos),
              hjust= 0,
              nudge_x = 1,
              size = 6,
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
         caption = "MUVA & USAID 2020") +
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
      strip.text = element_text(hjust = 0, size = 20, family = "Open Sans Light"),
      
      #title and caption
      plot.title = element_text(family = "Open Sans Light", size = 22, hjust = .5),
      plot.caption = element_text(family = "Open Sans Light", size = 12, hjust = 1)
      
    )
  
  
  return(plot)
  
  
  
}


#crear barras
barras = map(province_names, crear_bars)
names(barras)<- province_names


