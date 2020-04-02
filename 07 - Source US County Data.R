


library(tidyverse)
library(httr)

PATH <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
county_data <- read_csv(url(PATH))

county_data %>% 
  filter(state == "Massachusetts") %>% 
  ggplot()+
  aes(x = date, y = cases, color = county)+
  geom_line()
plotly::ggplotly()

library(tigris)
library(sf)
library(tmap)
options(tigris_use_cache = TRUE)
MA <- counties("MA", cb = TRUE)
MA_sf <- st_as_sf(MA) %>% st_transform(crs = 4326)

MA_sf %>% st_set_geometry(NULL) %>%  count(NAME)

plot(MA_sf$geometry)

tmap::ttm()
tmap::qtm(MA_sf, text = "NAME")







