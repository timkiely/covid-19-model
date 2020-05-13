

# testing and general case/hosp/death data from COVID TRACKING PROJECT
# https://covidtracking.com/api
# browseURL("https://covidtracking.com/")


# QUESTION: WHAT DOES US TEST POSITIVE RATE NEED TO BE?


library(tidyverse)
library(jsonlite)
library(lubridate)
library(tidyquant)
library(patchwork)


# US TESTING DATA
covid_tracking_data <- fromJSON("https://covidtracking.com/api/v1/states/daily.json") %>% as_tibble() %>% 
  select(date, state, positive, negative, pending, total, hospitalizedCurrently, hospitalizedCumulative
         , inIcuCurrently, inIcuCumulative, recovered, death, posNeg) %>% 
  mutate(date = ymd(date))

covid_tracking_data %>% glimpse()


# DEATHS AND HOSPITALIATIONS ALL US ----

new_hospitalizations <- 
  covid_tracking_data %>% 
  mutate(ny = ifelse(state=="NY", "NY","Other")) %>%
  group_by(ny, date) %>% 
  arrange(state, date) %>% 
  summarise(deaths = sum(death, na.rm = T)
            , hospitalizations = sum(hospitalizedCumulative, na.rm = T)) %>% 
  mutate(new_deaths = c(NA,diff(deaths))
         , new_hospitalizations = c(NA, diff(hospitalizations))) %>% 
  mutate(trailing_7_day_new_hospitalizations = TTR::runSum(new_hospitalizations, n = 7)) %>%  
  mutate(trailing_7_day_new_deaths = TTR::runSum(new_deaths, n = 7)) %>%
  select(
    ny
    , date
    , "New Deaths" = new_deaths
    , "New Hospitalizations" = new_hospitalizations
    , "Trailing 7 day hospitalizations" = trailing_7_day_new_hospitalizations
    , "Trailing 7 day deaths" = trailing_7_day_new_deaths 
  ) %>% 
  gather(Var, Value, -date,-ny) %>% 
  filter(date>ymd("2020 03 20")) %>% 
  ungroup() %>% 
  ggplot()+
  aes(x = date, y = Value, group = ny, fill = ny)+
  geom_col(position = "dodge")+
  geom_vline(xintercept = ymd("2020 04 24"), color = palette_dark()[1], size = 1.5)+
  facet_wrap(~Var, ncol = 1, scales = "free_y")+
  theme_tq()+
  tidyquant::scale_fill_tq()+
  theme(plot.title.position = "plot"
        , legend.position = "top")+
  labs(y = NULL
       , x = NULL
       , fill = NULL
       , title = "NY numbers improving while rest of US plateaus / worsens"
       , subtitle = "Blue line is Friday, May 24th"
       , caption = Sys.Date())

new_by_state <- 
  covid_tracking_data %>% 
  group_by(state) %>% 
  arrange(state, date) %>% 
  mutate(new_hospitalizations = c(NA, diff(hospitalizedCumulative))) %>% 
  filter(date>ymd("2020 03 20")) %>% 
  filter(date>ymd("2020 05 06")) %>% 
  group_by(state) %>% 
  summarise(recent_hopitalizaitons = sum(new_hospitalizations, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(desc(recent_hopitalizaitons)) %>% 
  select(state, recent_hopitalizaitons) %>% 
  filter(state!="NY") %>% 
  mutate(percent = recent_hopitalizaitons/sum(recent_hopitalizaitons)) %>% 
  mutate(cumulative = cumsum(percent)) %>% 
  mutate(state = fct_lump(state, 8, w = recent_hopitalizaitons)) %>% 
  group_by(state) %>% 
  summarise(recent_hopitalizaitons=sum(recent_hopitalizaitons)) %>%
  arrange(recent_hopitalizaitons) %>% 
  mutate(state = fct_inorder(state)) %>% 
  ggplot()+
  aes(x = state, y = recent_hopitalizaitons)+
  geom_col()+
  theme_tq()+
  scale_fill_tq()+
  scale_y_continuous(labels = scales::comma)+
  coord_flip()+
  labs(x = NULL
       , y = "Recent Hospitalizations"
       , title = "80% of non-NY new hospitalizations 5/6-5/10 were in 8 states"
  )


new_hospitalizations+new_by_state

jpeg(paste0('img/new-hospitalizations-all-us',Sys.Date(),'.jpeg')
     , width = 480*4
     , height = 480*2
     , res = 100
)
new_hospitalizations+new_by_state
dev.off()
