source("set_up.R")


#clean raw data
df_usaid =import(file.path(dir_data_raw, "BaseDadoUSAID25_01.xlsx")) %>%
  filter(!is.na(Comunidade)) %>%
  mutate(Comunidade = str_replace(Comunidade, "Chiconela\\, Vladimir Lenine e Avoz da FRELIMO", "Chiconela"),
         Comunidade = str_remove(Comunidade, "(?=\\/).*")) %>%
  #Clean comunidades in Gaza
  mutate(Comunidade = case_when(str_detect(Comunidade, "Hokwe" ) & Province == "Gaza"~ "Hokwe",
                                str_detect(Comunidade, "Lionde") & Province == "Gaza" ~ "Lionde",
                                str_detect(Comunidade, "[cC]ocamissava") & Province == "Gaza" ~ "Cocamissava",
                                str_detect(Comunidade, "Fidel Castro") & Province == "Gaza" ~ "Fidel Castro",
                                str_detect(Comunidade, "[cC]idade|xai xai") & Province == "Gaza"~ "Cidade",
                                str_detect(Comunidade, "1 bairro|1 Bairro|1o bairro") & Province == "Gaza" ~ "Bairro 1",
                                str_detect(Comunidade, "2 bairo|2 [bB]airro|2o [bB]airro|2bairro|B. 2") & Province == "Gaza" ~ "Bairro 2",
                                str_detect(Comunidade, "3 bairo|3 [bB]airro|3o [bB]airro") & Province == "Gaza"~ "Bairro 3",
                                str_detect(Comunidade, "4 bairo|4 [bB]airro|4o [bB]airro") & Province == "Gaza" ~ "Bairro 4",
                                str_detect(Comunidade, "5 bairo|5 [bB]airro|5o [bB]airro|5 [bB]airrro") & Province == "Gaza" ~ "Bairro 5",
                                str_detect(Comunidade, "9 bairro") & Province == "Gaza" ~ "Bairro 9",
                                str_detect(Comunidade, "24") & Province == "Gaza" ~ "Bairro 24",
                               
                                str_detect(Comunidade, "11") & Province == "Gaza"~ "Bairro 11",
                                str_detect(Comunidade, "Bango") & Province == "Gaza" ~ "Bango",
                                str_detect(Comunidade, "Chongoene") & Province == "Gaza" ~ "Chongoene",
                                str_detect(Comunidade, "Bungane") & Province == "Gaza" ~ "Bungane",
                                str_detect(Comunidade, "Nhancutse") & Province == "Gaza" ~ "Nhancutse",
                                str_detect(Comunidade, "Nhocoene") & Province == "Gaza"~ "Nhocoene",
                                str_detect(Comunidade, "Siaia") & Province == "Gaza"~ "Siaia",
                                str_detect(Comunidade, "B. [0-9] de ") & Province == "Gaza"~ gsub("B. [0-9] de ", "", Comunidade, perl = T),
                                str_detect(Comunidade, "B. [0-9] ") & Province == "Gaza"~ gsub("B. [0-9] ", "", Comunidade, perl = T),
                                
                                T ~ Comunidade)
         )





sort(unique(df_usaid$Comunidade))

export(df_usaid, file.path(dir_data_clean, "usaid_clean.rds"))


