

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

# GEORGIA SINCE REOPENING 
GA_since_reopening <- 
  covid_tracking_data %>% 
  group_by(state) %>% 
  arrange(state, date) %>% 
  mutate(test_per_day = c(NA, diff(positive+negative))
         , positive_rate = positive/test_per_day
         , new_hospitalizations = c(NA, diff(hospitalizedCumulative))
         , weekday = lubridate::wday(date, label = T)) %>% 
  filter(state=="GA") %>%  
  mutate(trailing_7_day_new_hospitalizations = TTR::runSum(new_hospitalizations, n = 7)) %>%  
  select(date, new_hospitalizations, trailing_7_day_new_hospitalizations, weekday) %>% # tail(20) 
  filter(date>ymd("2020 03 20")) %>% 
  ggplot()+
  aes(x = date, y = new_hospitalizations, fill = weekday)+
  geom_col()+
  geom_vline(xintercept = ymd("2020 04 24"))+
  geom_line(aes(y = trailing_7_day_new_hospitalizations, group =1 ), color = "red", show.legend = F)+
  scale_y_continuous(labels = scales::comma)+
  theme_tq()+
  theme(plot.title.position = "plot"
        , legend.position = "right")+
  labs(y = "New Hospitalizations"
       , x = NULL
       , title = "New hospitalizations in GA have \nincreaseddecreased since reopening on Friday, April 24th"
       , subtitle = "Red line = trailing 7 day admissions"
       , caption = Sys.Date())


GA_since_reopening

jpeg(paste0('img/GA-since-reopening',Sys.Date(),'.jpeg')
     , width = 480*2
     , height = 480*2
     , res = 200
)
GA_since_reopening
dev.off()


# COMPARE GA TO OTHER STATES

all_state_index_value <- 
  covid_tracking_data %>% 
  group_by(state) %>% 
  arrange(state, date) %>% 
  mutate(new_hospitalizations = c(NA, diff(hospitalizedCumulative))) %>%
  select(state, date, new_hospitalizations) %>% 
  filter(!is.na(new_hospitalizations)) %>% 
  mutate(trailing_7_new_hospitalizations_index = RcppRoll::roll_sumr(new_hospitalizations, 7, fill = NA, na.rm = T)) %>% 
  filter(date==ymd("2020 04 24")) %>%
  select(state, trailing_7_new_hospitalizations_index) %>% 
  filter(trailing_7_new_hospitalizations_index>0)  %>% 
  filter(state!="AR")


other_states_indexed <- 
  covid_tracking_data %>% 
  inner_join(all_state_index_value, by = 'state') %>% 
  group_by(state) %>% 
  arrange(state, date) %>% 
  mutate(new_hospitalizations = c(NA, diff(hospitalizedCumulative))) %>% 
  mutate(trailing_7_new_hospitalizations = RcppRoll::roll_sumr(new_hospitalizations, 7, fill = NA, na.rm = T)) %>% 
  mutate(trailing_7_indexed = trailing_7_new_hospitalizations/trailing_7_new_hospitalizations_index) %>% 
  filter(date>=ymd("2020 04 24")) %>%
  select(state, date, trailing_7_indexed, trailing_7_new_hospitalizations, trailing_7_new_hospitalizations_index) %>% 
  mutate(label = ifelse(date==max(date), state, NA)) %>% 
  mutate(now_index = ifelse(date==max(date), trailing_7_indexed, NA)) %>% 
  filter(any(now_index>1)) %>% 
  filter(any(now_index<2)) %>% 
  filter(!state%in%c("ID","RI","KY","SC")) %>% 
  ggplot()+
  aes(x = date, y = trailing_7_indexed, color = state)+
  #geom_line()+
  geom_smooth(se = F)+
  geom_vline(xintercept = ymd("2020 04 24"))+
  geom_hline(yintercept = 1, color = "black", size = 2)+
  ggrepel::geom_label_repel(aes(label = label), show.legend = F)+
  scale_y_continuous(labels = scales::comma)+
  scale_color_tq()+
  theme_tq()+
  theme(plot.title.position = "plot"
        , legend.position = "none")+
  labs(y = "Trailing 7 day new hospitalizations\nindexed to 2020 04 24"
       , x = NULL
       , title = "2) However, Georgia's uptick in new hospitalizations \nis not extradorinary for this period compared to other states"
       , subtitle = "Trailing 7 day new hospitalizations indexed to Friday May 24th. Select states shown"
       , caption = Sys.Date())

library(patchwork)


GA_since_reopening

jpeg(paste0('img/GA-indexed-since-reopening',Sys.Date(),'.jpeg')
     , width = 480*4
     , height = 480*2
     , res = 100
)
GA_since_reopening+other_states_indexed
dev.off()



# HOSPITALIZATIONS AND DEATHS -----


covid_tracking_data %>% 
  group_by(state) %>% 
  arrange(state, date) %>% 
  mutate(test_per_day = c(NA, diff(positive+negative))
         , positive_rate = positive/test_per_day
         , new_hospitalizations = c(NA, diff(hospitalizedCumulative))
         , weekday = lubridate::wday(date, label = T)) %>% 
  filter(state=="GA") %>%  
  mutate(trailing_7_day_new_hospitalizations = TTR::runSum(new_hospitalizations, n = 7)) %>%  
  select(date, new_hospitalizations, trailing_7_day_new_hospitalizations
         , death) %>%
  mutate(new_deaths = c(NA, diff(death))) %>% 
  mutate(trailing_7_day_new_deaths = TTR::runSum(new_deaths, n = 7)) %>%  
  gather(Var, Value, -date,-state) %>% 
  filter(date>ymd("2020 03 20")) %>% 
  
  ggplot()+
  aes(x = date, y = Value, color = Var)+
  geom_line()+
  geom_vline(xintercept = ymd("2020 04 24"))+
  facet_wrap(~Var, ncol = 1, scales = "free_y")+
  theme_tq()+
  theme(plot.title.position = "plot"
        , legend.position = "right")+
  labs(y = "New Hospitalizations"
       , x = NULL
       , title = "Since reopening on Friday, April 24th"
       , caption = Sys.Date())



# GA HOSPITALIZATIONS ----

ga_new_hospitalizations <- 
  covid_tracking_data %>% 
  filter(state=="GA") %>%  
  group_by(state) %>% 
  arrange(state, date) %>% 
  mutate(new_hospitalizations = c(NA, diff(hospitalizedCumulative))) %>% 
  mutate(trailing_7_day_new_hospitalizations = TTR::runSum(new_hospitalizations, n = 7)) %>%  
  select(date, new_hospitalizations, trailing_7_day_new_hospitalizations) %>%
  filter(date>ymd("2020 04 01")) %>% 
  ggplot()+
  aes(x = date, y = trailing_7_day_new_hospitalizations)+
  geom_col()+
  geom_vline(xintercept = ymd("2020 04 24"))+
  theme_tq()+
  scale_x_date(limits = c(ymd("2020 04 01"), ymd("2020 05 10")))+
  theme(plot.title.position = "plot"
        , legend.position = "right")+
  labs(y = "New Hospitalizations"
       , x = NULL
       , title = "GA since reopening on Friday, April 24th"
       , caption = Sys.Date())


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

  