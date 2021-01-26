
source("set_up.R")


#read shapefile ---------------------------------------------------------------
shape = import(file.path(dir_data, "districts.rds")) %>%
 sf::st_as_sf()


provinces = import(file.path(dir_data, "provinces.rds")) %>%
  sf::st_as_sf()

#esport list of districts ----------------------------------------------------
shape %>%
  as.data.frame() %>%
  select(Province_ID, Province, District_ID, District) %>%
  arrange(Province, District) %>%
   export(file.path(dir_data, "lista_distritos.xlsx"))

list.files(dir_data)


#test polygons
ggplot(data = provinces) +
  geom_sf()



