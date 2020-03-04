Modeling Scenarios for Covid-19
================

``` r
library(tidyverse)
library(tidyquant)

theme_set(theme_tq())
```

# from: <https://github.com/jakobzhao/virus>

``` r
# updated every ~4 hours
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

mexico_names <- cases_data %>% select(mexico) %>% names()
uk_names <- cases_data %>% select(uk) %>% names()
canada_names <- cases_data %>% select(canada, ontario:quebec) %>% names()
south_korea_names <- cases_data %>% select(`south korea`) %>% names()
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
```

# Days since reported

``` r
processed %>% 
  filter(Country!="Other") %>% 
  ggplot()+
  aes(x = days_since_reported, y = Active, group = area, label = area, color = Country)+
  geom_line()
```

![](README_files/figure-gfm/days%20since%20reported-1.png)<!-- -->

# US

``` r
processed %>% 
  filter(Country=="US") %>% 
  ggplot()+
  aes(x = days_since_reported, y = Confirmed, color = area)+
  geom_line()
```

![](README_files/figure-gfm/US-1.png)<!-- -->

# Western Countries

``` r
processed %>% 
  filter(Country%in% c("US","canada","france","australia","germany","israel")) %>% 
  ggplot()+
  aes(x = days_since_reported, y = Active, color = area)+
  geom_line()
```

![](README_files/figure-gfm/Western%20Countries-1.png)<!-- -->

# NYC

``` r
processed %>% 
  filter(area %in% "new york") %>% 
  ggplot()+
  aes(x = datetime, y = Active, color = area)+
  geom_line()
```

![](README_files/figure-gfm/NYC-1.png)<!-- -->

# MODELING

``` r
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
```

``` r
actual_nyc <- 
  not_na_processed %>% 
  filter(area=="new york") %>% 
  filter(days_since_reported>0) %>% 
  select(days_since_reported, "Actual Active" = Active) %>% 
  mutate(model = "Actual Active")

max_days <- not_na_processed %>% 
  filter(area=="new york") %>% 
  summarise(days = max(days_since_reported)) %>% 
  pull(days)

not_na_processed %>% 
  data_grid(area, days_since_reported) %>% 
  filter(area=="new york") %>% 
  add_predictions(model_all, var = "world model") %>%
  add_predictions(model_western, var = "western model") %>% 
  add_predictions(model_us_states, var = "us model") %>% 
  add_predictions(model_japan, var = "japan model") %>% 
  gather(model, prediction, -area, -days_since_reported) %>% 
  ggplot()+
  aes(days_since_reported, y = prediction, color = model)+
  geom_line(size = 1)+
  geom_line(data = actual_nyc, aes(y = `Actual Active`), linetype = 2, size = 3)+
  geom_vline(xintercept = max_days, color = "black", size = 2)+
  theme_tq()+
  scale_color_tq()+
  labs(title = "New York City Active Case Scenarios"
       , subtitle = "black line denotes NYC actual days since reported"
       , y = "Predicted Active Cases"
       , x = "Days since first reported cases")
```

![](README_files/figure-gfm/NYC%20predictions-1.png)<!-- -->
