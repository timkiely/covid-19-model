
library(tidyverse)
library(rvest)
library(sf)

ma_towns <- read_sf('data/ma_towns_2.geojson') %>% 
  select(city_town, geometry)

the_link_4_22 <- "https://www.boston.com/news/coronavirus/2020/04/22/massachusetts-cities-towns-coronavirus-cases-numbers"
the_link_4_29 <- "https://www.boston.com/news/coronavirus/2020/04/29/latest-massachusetts-cities-towns-covid-19-cases-numbers"

the_page_4_22 <- read_html(the_link_4_22)
the_page_4_29 <- read_html(the_link_4_29)

infection_rates_by_town_22 <- 
  html_table(the_page_4_22, header = T)[[1]] %>% 
  as_tibble() %>% 
  rename("Count 4/22" = Count, "Rate 4/22" = Rate)

infection_rates_by_town_29 <- 
  html_table(the_page_4_29, header = T)[[1]] %>% 
  as_tibble() %>% 
  rename("Count 4/29" = Count, "Rate 4/29" = Rate)

infection_rate_df <- 
  infection_rates_by_town_22 %>% 
  full_join(infection_rates_by_town_29, by = "City/Town")

infection_rate_sf <- 
  ma_towns %>% 
  inner_join(infection_rate_df, by = c("city_town"="City/Town")) %>% 
  mutate_at( vars(-c(city_town,geometry)), as.numeric)


library(mapview)

infection_rate_sf %>% 
  mutate(change_in_count = `Count 4/29`/`Count 4/22`-1
         , increase_in_count = `Count 4/29`-`Count 4/22`
         , change_in_rate = `Rate 4/29`/`Rate 4/22`-1) %>% 
  mapview(., zcol = "increase_in_count", cex = "increase_in_count"
          , label = paste0(.$city_town, " ",.$increase_in_count))  


infection_rate_sf %>% 
  st_set_geometry(NULL) %>% 
  select(city_town, contains("Count")) %>% 
  gather(Date, Value,-city_town) %>% 
  mutate(Date = lubridate::mdy(paste0(str_remove(Date,"Count "),"/20")))


library(CGPfunctions)

custom_colors <- 
  infection_rate_sf %>% 
  st_set_geometry(NULL) %>% 
  select(city_town, contains("Rate")) %>% 
  mutate(difference = `Rate 4/29` - `Rate 4/22`) %>%
  mutate(trend = case_when(
    city_town %in% c("Topsfield","Newburyport"
                     ,"Danvers","Beverly","Georgetown"
                     , "Amherst","Williamstown","Boston") ~ "green",
    TRUE ~ "gray"
  )
  ) %>%
  select(city_town, trend) %>%
  tibble::deframe()


infection_rate_sf %>% 
  mutate(custome_colors = custom_colors) %>% 
  st_set_geometry(NULL) %>% 
  filter(custom_colors=="green") %>% 
  select(city_town, contains("Rate")) %>% 
  gather(Date, Value,-city_town) %>% 
  newggslopegraph(dataframe = .,
                  Times = Date
                  , Measurement = Value
                  , Grouping = city_town
                  , LineColor = custom_colors
                  , Title = "Infection Rate"
                  , Caption = NULL
  )


infection_rate_sf %>% 
  mutate(custome_colors = custom_colors) %>% 
  st_set_geometry(NULL) %>% 
  filter(custom_colors=="green", city_town!="Boston") %>% 
  select(city_town, contains("Count")) %>% 
  gather(Date, Value,-city_town) %>% 
  newggslopegraph(dataframe = .,
                  Times = Date
                  , Measurement = Value
                  , Grouping = city_town
                  , LineColor = custom_colors
                  , Title = "Infection Counts"
                  , Caption = NULL
  )


