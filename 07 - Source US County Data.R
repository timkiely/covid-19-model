


library(tidyverse)
library(tidyquant)
library(lubridate)
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

MA_DATA <- 
  county_data %>% 
  filter(state == "Massachusetts") %>% 
  left_join(MA_sf, by = c("county" = "NAME")) %>% 
  group_by(county) %>% 
  filter(date==max(date)) %>% 
  st_as_sf()

tmap::qtm(MA_DATA, fill = "deaths", text = "county")



# Looking at Georgia ----
# Georgia started re-opening as of 4/24/2020

county_data %>% 
  filter(state=="Georgia") %>% 
  group_by(state, date) %>% 
  summarise(cases = sum(cases), deaths = sum(deaths)) %>% 
  ggplot()+
  aes(x = date, y = cases)+
  geom_line()+
  geom_vline(xintercept = ymd("2020 04 24"))


# New Cases in GA
county_data %>% 
  filter(state=="Georgia") %>% 
  group_by(state, date) %>% 
  summarise(cases = sum(cases, na.rm = T), deaths = sum(deaths, na.rm = T)) %>% 
  mutate(new_cases = c(NA, diff(cases))) %>% 
  ggplot()+
  aes(x = date, y = new_cases)+
  geom_col()+
  geom_vline(xintercept = ymd("2020 04 24"))+
  geom_smooth(group = 1, se = F)+
  theme_tq()+
  labs(y = "New Cases"
       , x = NULL
       , title = "New Cases in GA Post Reopening"
       , subtitle = Sys.Date())




GA <- counties("GA", cb = TRUE)
GA_sf <- st_as_sf(GA) %>% st_transform(crs = 4326)

GA_DATA <- 
  county_data %>% 
  filter(state == "Georgia") %>% 
  left_join(GA_sf, by = c("county" = "NAME")) %>% 
  group_by(county) %>% 
  filter(date==max(date)) %>% 
  st_as_sf()

tmap::qtm(GA_DATA, fill = "cases"
          #, text = "county"
          )

GA_DATA_after_reopen <- 
  county_data %>% 
  filter(state == "Georgia") %>% 
  left_join(GA_sf, by = c("county" = "NAME")) %>% 
  filter(!is.na(fips)) %>% 
  filter(date>=ymd("2020 04 24")) %>% 
  group_by(county) %>% 
  filter(date==max(date)|date==min(date)) %>% 
  arrange(county, date) %>% 
  select(county, date, cases, geometry) %>% 
  mutate(change_since_reopen = cases-lag(cases,1)) %>% 
  filter(change_since_reopen>0) %>% 
  filter(date==max(date)) %>% 
  st_as_sf()

tmap::qtm(GA_DATA_after_reopen
          , fill = "change_since_reopen"
          #, text = "county"
)



library(mapview)
mapview::mapview(GA_DATA_after_reopen, zcol = "change_since_reopen")


