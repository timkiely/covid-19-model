


library(tidyverse)
library(tidyquant)


# from: https://github.com/jakobzhao/virus
cases_data <- read_csv("http://hgis.uw.edu/virus/assets/virus.csv")


# names by country
china_names <- 
  cases_data %>% 
  select(anhui:zhejiang) %>% 
  names()

us_names <- 
  cases_data %>% 
  select(us:texas,florida:`georgia usa`) %>% 
  names()

mexico_names <- 
  cases_data %>% 
  select(mexico) %>% 
  names()

uk_names <- 
  cases_data %>% 
  select(uk) %>% 
  names()

canada_names <- 
  cases_data %>% 
  select(canada, ontario:quebec) %>% 
  names()

  
south_korea_names <- 
  cases_data %>% 
  select(`south korea`) %>% 
  names()

singapore_names <- cases_data %>% select(singapore) %>% names()
japan_names <- cases_data %>% select(japan) %>% names()
vietnam_names <- cases_data %>% select(vietnam) %>% names()
france_names <- cases_data %>% select(france) %>% names()
australia_names <- cases_data %>% select(australia) %>% names()
germany_names <- cases_data %>% select(germany) %>% names()
russia_names <- cases_data %>% select(russia) %>% names()
italy_names <- cases_data %>% select(italy) %>% names()
iran_names <- cases_data %>% select(iran) %>% names()
israel_names <- cases_data %>% select(israel) %>% names()



# The first sequel represents the number of confirmed cases, 
# the second sequel represents suspected cases, the third sequel 
# represents cured cases, the fourth sequel represents death cases.
processed <-
  cases_data %>% 
  gather(area, cases, -datetime) %>% 
  separate(cases, into = c("Confirmed","Suspected","Cured","Deaths"), sep = "-", extra = "warn", remove = F) %>% 
  mutate_at(vars(Confirmed:Deaths), function(x)ifelse(is.na(x),0,x)) %>% 
  mutate_at(vars(Confirmed:Deaths), as.numeric) %>% 
  mutate(Active = Confirmed+Suspected-Cured-Deaths) %>% 
  mutate(area = str_remove_all(area, "\\\\xa0")) %>% 
  mutate(first_reported = case_when(!is.na(cases)&is.na(lag(cases,1)) ~ 1
                                    , TRUE ~ NA_real_
                                    )) %>% 
  group_by(area) %>%
  mutate(first_reported = case_when(sum(first_reported, na.rm = T)==0&datetime==min(datetime) ~ 1
                                    , TRUE ~ first_reported
                                    )
         ) %>% 
  fill(first_reported, .direction = "down") %>% 
  mutate(count_report_days = case_when(first_reported  == 1~1, TRUE ~ 0)) %>% 
  mutate(days_since_reported = cumsum(count_report_days)) %>% 
  ungroup() %>% 
  mutate(Country = case_when(
    area %in% china_names ~ "China"
    , area %in% us_names ~ "US"
    , area %in% mexico_names ~ "Mexico"
    , area %in% uk_names ~ "UK"
    , area %in% canada_names ~ "canada"
    , area %in% south_korea_names ~ "south_korea"
    , area %in% singapore_names ~ "singapore"
    , area %in% japan_names ~ "japan"
    , area %in% vietnam_names ~ "vietnam"
    , area %in% france_names ~ "france"
    , area %in% australia_names ~ "australia"
    , area %in% germany_names ~ "germany"
    , area %in% russia_names ~ "russia"
    , area %in% italy_names ~ "italy"
    , area %in% iran_names ~ "iran"
    , area %in% israel_names ~ "israel"
    
    , TRUE ~ "Other"
  ))


# US data 
us_cases <- processed %>% filter(area =="us") %>% filter(!is.na(first_reported))



# New York State Cases
processed %>% 
  ungroup() %>% 
  filter(area=="new york")  %>% 
  filter(datetime == max(datetime)) %>% glimpse
  

NYC_reports <- 
  tribble(~days_since_reported, ~Confirmed
          , 1, 0
          , 4, 43 # Wednesday 3/11
          , 5, 100 # Thursday 3/12
          , 6, 170 # Friday 3/13
          , 7, 213 # Saturday 3/14
          ) %>% 
  mutate(area = "NYC", Country = "US")


# Most advanced cases - China
NYC_vs_China_cases <-
  processed %>% 
  filter(Country=="China", area!="hubei") %>% 
  ggplot()+
  aes(x = days_since_reported, y = Confirmed, group = area, label = area, color = Country)+
  geom_line(color = 2) + 
  geom_line(data = NYC_reports, color = "black", size = 2)+
  theme_tq()+
  scale_color_tq()+
  theme(legend.position = "none")+
  labs(title = "NYC Covid-19 cases vs. Chinese provinces"
       , subtitle = "Each line represents a Chinese province. Excludes Hubei (Wuhan). \n Black line is NYC"
       , y = "Count of Confirmed Cases"
       , x = "Days since outbreak first reported"
       , caption = "Source: http://hgis.uw.edu/virus/assets/virus.csv\n NYC data collected by hand")


jpeg('img/covid-nyc-china-20200314.jpeg'
     , width = 480*2
     , height = 480*2
     , res = 200
     )
NYC_vs_China_cases
dev.off()

plotly::ggplotly()

# Days since reported
processed %>% 
  filter(Country!="Other") %>% 
  ggplot()+
  aes(x = days_since_reported, y = Active, group = area, label = area, color = Country)+
  geom_line()
plotly::ggplotly()

# US
processed %>% 
  filter(Country=="US") %>% 
  ggplot()+
  aes(x = days_since_reported, y = Confirmed, color = area)+
  geom_line()

plotly::ggplotly()


# Western Countries
processed %>% 
  filter(Country%in% c("US","canada","france","australia","germany","israel")) %>% 
  ggplot()+
  aes(x = days_since_reported, y = Active, color = area)+
  geom_line()
plotly::ggplotly()


# NYC
processed %>% 
  filter(area %in% "new york") %>% 
  ggplot()+
  aes(x = datetime, y = Active, color = area)+
  geom_line()
plotly::ggplotly()


# MODELING
library(modelr)

not_na_processed <- processed %>% 
  mutate(Country = as.factor(Country)) %>% 
  filter(!is.na(days_since_reported)) 

not_na_western_only <- not_na_processed %>% 
  filter(Country%in% c("US","canada","france","australia","germany","israel"))
  
not_na_us_states <- not_na_processed %>% 
  filter(area%in% us_names) %>% 
  filter(area!='us')

not_na_japan <- not_na_processed %>% 
  filter(area=='japan')


model_all <- loess(Active~days_since_reported, data = not_na_processed
                   , control = loess.control(surface = "direct")
                   )  
model_western <- loess(Active~days_since_reported, data = not_na_western_only
                       , control = loess.control(surface = "direct")
                       )  
model_us_states <- loess(Active~days_since_reported, data = not_na_us_states
                         , control = loess.control(surface = "direct")
                         )  
model_japan <- loess(Active~days_since_reported
                     , data = not_na_japan, control = loess.control(surface = "direct")
                     )  

nyc_prediction_plot <- 
  data_grid(not_na_processed, area, days_since_reported) %>% 
  add_predictions(model_all, var = "world model") %>%
  add_predictions(model_western, var = "western model") %>% 
  add_predictions(model_us_states, var = "us model") %>% 
  add_predictions(model_japan, var = "japan model") %>% 
  filter(area=="new york") %>% 
  gather(model, prediction, -area, -days_since_reported) %>% 
  ggplot()+
  aes(days_since_reported, y = prediction, color = model)+
  geom_line(size = 2)+
  theme_tq()+
  scale_color_tq()+
  labs(title = "New York City Active Case Scenarios"
       , y = "Predicted Active Cases"
       , x = "Days since first reported cases")
  







