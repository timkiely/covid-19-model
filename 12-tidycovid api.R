

# remotes::install_github("joachim-gassen/tidycovid19")
library(tidycovid19)
library(tidyverse)
library(ggrepel)
library(gghighlight)
library(zoo)
library(tidyquant)


merged <- download_merged_data(silent = F, cached = TRUE)

goog_trends <- tidycovid19::download_google_cmr_data(type = "us_county", cached = TRUE)
goog_trends %>% glimpse

jhu_data <- tidycovid19::download_jhu_csse_covid19_data(type = "us_county", cached = T)


goog_trends %>% 
  filter(state=="Arizona") %>% 
  filter(county=="Maricopa County") %>% 
  gather(var, value, -state, -county, -date,-timestamp) %>% 
  ggplot()+
  aes(x = date, y = value, color = var) %>% 
  geom_line()+
  facet_wrap(~county)


goog_trends %>% 
  filter(state=="Massachusetts") %>% 
  gather(var, value, -state, -county, -date,-timestamp) %>% 
  filter(var%in%c("residential","workplaces")) %>%
  group_by(state, county, var) %>% 
  mutate(value = rollmean(value, 7, na.pad = T)) %>% 
  ggplot()+
  aes(x = date, y = value, color = var) %>% 
  geom_line()+
  facet_wrap(~county)


goog_trends_state <- tidycovid19::download_google_cmr_data(type = "country_region", cached = F)



GA_mobility <- 
  goog_trends_state %>% 
  filter(region=="Georgia") %>% 
  mutate_at(vars(retail_recreation:residential), function(x) rollmean(x, 7, na.pad = T)) %>% 
  gather(Var, Value, -c(iso3c:date, timestamp)) %>% 
  filter(Var%in%c("residential","retail_recreation")) %>%
  filter(date>ymd("2020 04 01")) %>% 
  ggplot()+
  aes(x = date, y = Value, color = Var) %>% 
  geom_line()+
  geom_vline(xintercept = ymd("2020 04 24"))+
  theme_tq()+
  scale_color_tq()+
  scale_x_date(limits = c(ymd("2020 04 01"), ymd("2020 05 10")))+
  labs(title = "Google mobility data"
       , subtitle = "1 = baseline from January"
       , y = "percent change since baseline"
       , color = NULL)



goog_trends %>% 
  filter(state%in%c("New York", "Georgia","Arizona","California","Oregon")) %>%
  #filter(state=="Arizona") %>%
  gather(var, value, -state, -county, -date,-timestamp) %>% 
  filter(var%in%c("residential")) %>%
  group_by(state, var, date) %>% 
  summarise(value = mean(value, na.rm = T)) %>% 
  group_by(state, var) %>% 
  mutate(value = rollmean(value, 7, na.pad = T)) %>% 
  ggplot()+
  aes(x = date, y = value, color = state) %>% 
  geom_line()+
  geom_vline(xintercept = lubridate::ymd("2020 04 24"))
  


goog_trends %>% 
  filter(state%in%c("New York", "Georgia","Arizona","California","Oregon")) %>%
  #filter(state=="Arizona") %>%
  gather(var, value, -state, -county, -date,-timestamp) %>% 
  filter(var%in%c("retail_recreation")) %>%
  group_by(state, var, date) %>% 
  summarise(value = mean(value, na.rm = T)) %>% 
  group_by(state, var) %>% 
  mutate(value = rollmean(value, 7, na.pad = T)) %>% 
  ggplot()+
  aes(x = date, y = value, color = state) %>% 
  geom_line()+
  geom_vline(xintercept = lubridate::ymd("2020 04 24"))




apple_data <- tidycovid19::download_apple_mtr_data(type = "country_region", cached = T)

apple_data_city <- tidycovid19::download_apple_mtr_data(type = "country_city", cached = T)


apple_data %>% 
  filter(iso3c=="USA") %>% 
  filter(region%in%c("Illinois"
                     , "Iowa"
                     , "Michigan"
                     , "Minnesota"
                     , "Wisconsin"
                     , "Missouri"
                     , "Maryland"
                     , "Pennsylvania"
                     , "Alabama"
                     , "Florida"
                     , "Georgia"
                     , "Mississippi"
                     , "Tennessee"
                     , "Texas"
                     , "Arizona"
                     , "Colorado"
                     , "New Mexico"
                     , "Utah"
                     , "California"
                     , "Idaho"
                     , "Washington"
                     )) %>% 
  ggplot()+
  aes(x = date, y = driving)+
  geom_line()+
  geom_hline(yintercept = 100, color = "red")+
  facet_wrap(~region)+
  theme_tq()+
  theme(panel.grid = element_blank())+
  labs(x = NULL
       , y = "Percent relative to January 13th, 2020"
       , subtitle = "Apple's Mobility Trend Reports"
       , title = "Requests for Driving Directions from Apple Maps Relative to January")

apple_data_city %>% 
  filter(iso3c=="USA") %>% 
  ggplot()+
  aes(x = date, y = driving)+
  geom_line()+
  geom_hline(yintercept = 100, color = "red")+
  facet_wrap(~city)+
  theme_tq()+
  theme(panel.grid = element_blank())+
  labs(x = NULL
       , y = "Percent relative to January 13th, 2020"
       , subtitle = "Apple's Mobility Trend Reports"
       , title = "Requests for Driving Directions from Apple Maps Relative to January")



