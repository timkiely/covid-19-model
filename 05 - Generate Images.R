


library(tidyverse)
library(tidyquant)

# 0.0 DATA ----
# from: https://github.com/jakobzhao/virus
source("01 - Download Latest Case Data.R")

cases_data <- suppressMessages(read_csv(latest_file))

source("04 - Process Case Data.R")

# US data 
us_cases <- processed %>% filter(area =="us") %>% filter(!is.na(first_reported))

source("00 - Recording NYC Cases.R")


# 1.0 NYC vs. China ----
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


NYC_vs_China_cases

jpeg(paste0('img/covid-nyc-china-',Sys.Date(),'.jpeg')
     , width = 480*2
     , height = 480*2
     , res = 200
)
NYC_vs_China_cases
dev.off()


# 2.0 US STATES ----
library(ggrepel)
US_States <- 
  processed %>% 
  filter(area%in%us_state_names) %>% 
  group_by(area) %>% 
  filter(Confirmed>20) %>% 
  mutate(days_since_reported = 1:n()) %>% 
  mutate(label = ifelse(days_since_reported == max(days_since_reported) & Confirmed>100
                        ,area, NA_character_)) %>% 
  ggplot()+
  aes(x = days_since_reported, y = Confirmed, color = area)+
  geom_line(size = 2)+
  ggrepel::geom_label_repel(aes(label = label), na.rm = T)+
  theme_tq()+
  theme(legend.position = "none")+
  scale_color_tq(theme = "light")+
  labs(x = "Days since reaching 20 cases"
       , y = "Confirmed Cases")



jpeg(paste0('img/covid-US-States-',Sys.Date(),'.jpeg')
     , width = 480*2
     , height = 480*2
     , res = 200
)
US_States
dev.off()



# 3.0 NYC VS. ITALY LOG ----
source("00 - Recording NYC Cases.R")

nyc_china_italy_log <- 
  processed %>% 
  filter(Country%in%c("China","italy"), area!="hubei") %>%  
  bind_rows(NYC_reports %>% mutate(Country = "NYC")) %>% 
  group_by(area) %>% 
  filter(Confirmed>20) %>% 
  mutate(days_since_reported = 1:n()) %>% 
  ggplot()+
  aes(x = days_since_reported, y = Confirmed, group = area, color = Country)+
  geom_line(size = 1.5, alpha = 0.6) + 
  theme_tq()+
  scale_color_tq()+
  theme(legend.position = "bottom")+
  scale_y_log10()+
  labs(title = "NYC Covid-19 cases currently tracking closer to Italy than Chinese provinces"
       , y = "Log Scale Count of Confirmed Cases"
       , x = "Days since reaching 20 reported cases"
       , caption = "Source: http://hgis.uw.edu/virus/assets/virus.csv\n NYC data collected by hand")

jpeg(paste0('img/covid-NYC-italy-china-log ',Sys.Date(),'.jpeg')
     , width = 480*2
     , height = 480*2
     , res = 200
)
nyc_china_italy_log
dev.off()


# 4.0 NYC VS. ITALY ----
nyc_china_italy <- 
  processed %>% 
  filter(Country%in%c("China","italy"), area!="hubei") %>%  
  bind_rows(NYC_reports %>% mutate(Country = "NYC")) %>% 
  arrange(desc(Country)) %>% 
  group_by(area) %>% 
  filter(Confirmed>20) %>% 
  mutate(days_since_reported = 1:n()) %>% 
  filter(days_since_reported<20) %>% 
  ggplot()+
  aes(x = days_since_reported, y = Confirmed, group = area, color = Country)+
  geom_line(size = 1.5, alpha = 0.5) + 
  theme_tq()+
  scale_color_tq()+
  theme(legend.position = "bottom")+
  labs(title = "NYC Covid-19 cases days 1-20"
       , y = "(Actual) Count of Confirmed Cases"
       , x = "Days since reaching 20 reported cases"
       , caption = "Source: http://hgis.uw.edu/virus/assets/virus.csv\n NYC data collected by hand")

jpeg(paste0('img/covid-NYC-italy-china',Sys.Date(),'.jpeg')
     , width = 480*2
     , height = 480*2
     , res = 200
)
nyc_china_italy
dev.off()



# 5.0 Dualplot NYC vs. Italy ----

plot_row <- plot_grid(nyc_china_italy+labs(title = NULL, caption = NULL)
                      , nyc_china_italy_log+labs(title = NULL, caption = NULL)
                      , scale = 1)

# now add the title
title <- ggdraw() + 
  draw_label(
    "NYC Cases Tracking Close to Italy Case",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )



library(cowplot)
jpeg(paste0('img/covid-NYC-italy-china-dualplot',Sys.Date(),'.jpeg')
     , width = 480*2
     , height = 480*2
     , res = 200
)
plot_grid(
  title, plot_row,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)
dev.off()






# 6.0 NYC vs. western countires ----



nyc_western_countries <- 
  processed %>% 
  filter(Country%in%c("italy","spain","UK","germany","japan","US")) %>%  
  bind_rows(NYC_reports %>% mutate(Country = "NYC")) %>% 
  arrange(desc(Country)) %>% 
  group_by(area) %>% 
  filter(Confirmed>20) %>% 
  mutate(days_since_reported = 1:n()) %>% 
  mutate(label = ifelse(days_since_reported == max(days_since_reported), area, NA_character_)) %>% 
  ggplot()+
  aes(x = days_since_reported, y = Confirmed, group = area, color = Country)+
  geom_line(size = 1.5, alpha = 1) + 
  ggrepel::geom_label_repel(aes(label = label), na.rm = T)+
  theme_tq()+
  scale_color_tq()+
  theme(legend.position = "none")+
  labs(title = "NYC Covid-19 cases days 1-20"
       , y = "(Actual) Count of Confirmed Cases"
       , x = "Days since reaching 20 reported cases"
       , caption = "Source: http://hgis.uw.edu/virus/assets/virus.csv\n NYC data collected by hand")

jpeg(paste0('img/covid-NYC-western_countries-',Sys.Date(),'.jpeg')
     , width = 480*2
     , height = 480*2
     , res = 200
)
nyc_western_countries
dev.off()


# 7.0 NYC vs. italy spain countires ----


nyc_vs_hubei <- 
  processed %>% 
  filter(area=="hubei") %>%  
  bind_rows(NYC_reports %>% mutate(area = "NYC")) %>% 
  arrange(desc(Country)) %>% 
  group_by(area) %>% 
  filter(Confirmed>20) %>% 
  mutate(days_since_reported = 1:n()) %>% 
  filter(days_since_reported<20) %>%
  select(area, days_since_reported, Confirmed, Deaths) %>% 
  gather(Var, Value, -area, -days_since_reported) %>% 
  mutate(label = ifelse(days_since_reported == max(days_since_reported), area, NA_character_)) %>% 
  ggplot()+
  aes(x = days_since_reported, y = Value, group = area, color = area)+
  geom_line(size = 1.5, alpha = 1) + 
  ggrepel::geom_label_repel(aes(label = label), na.rm = T, nudge_y = 20)+
  theme_tq()+
  scale_color_tq()+
  theme(legend.position = "none")+
  facet_wrap(~Var, ncol = 1, scales = "free_y")+
  labs(title = "Infection Counts Days 1-20 NYC and Hubei (Wuhan)"
       , y = "Count of Cases"
       , x = "Days since reaching 20 reported cases"
       , caption = "Source: http://hgis.uw.edu/virus/assets/virus.csv\n NYC data collected by hand")



jpeg(paste0('img/covid-NYC-NYC-vs-hubei',Sys.Date(),'.jpeg')
     , width = 480*4
     , height = 480*2
     , res = 200
)
nyc_vs_hubei
dev.off()



# 8.0 China Resurgence ----
chinese_provinces <- 
  processed %>% 
  filter(Country=="China", area!="hubei") %>% 
  ggplot()+
  aes(x = days_since_reported, y = Active, group = area, label = area, color = Country)+
  geom_line()+
  theme_tq()+
  scale_color_tq()+
  theme(legend.position = "none", plot.title.position = "plot")+
  labs(x = "Days Since First Reported"
       , y = "Active Cases"
       , title = "China Provinces Ex-Wuhan")

jpeg(paste0('img/chinese-provinces-',Sys.Date(),'.jpeg')
     , width = 480*2
     , height = 480*2
     , res = 200
)
chinese_provinces
dev.off()



# 9.0 Passing Wuhan ----

all_china_cases <- 
  processed %>% 
  filter(Country =="China") %>% 
  group_by(Country, datetime) %>% 
  summarise_at(vars(Confirmed:Active), sum, na.rm = T) %>% 
  ungroup()

passing_wuhan <- 
  processed %>% 
  filter(area %in% c("italy","us","spain","france","iran","south korea","uk")) %>% 
  bind_rows(NYC_reports %>% mutate(area = "NYC") %>% mutate(Active = Confirmed)) %>% 
  bind_rows(all_china_cases %>% mutate(area = "China")) %>% 
  filter(Active>20) %>% 
  group_by(area) %>% 
  mutate(days_since_reported=1:n()) %>% 
  mutate(label = ifelse(days_since_reported==max(days_since_reported) & 
                          Active>2000, area, NA_character_)) %>% 
  ggplot()+
  aes(x = days_since_reported, y = Active, group = area, label = area, color = area)+
  geom_line()+
  
  geom_label_repel(aes(label = label), nudge_x = 0.5, segment.alpha = 0.5)+
  ggthemes::scale_color_economist()+
  theme_tq()+
  theme(plot.title.position = "plot", legend.position = "none")+
  labs(title = paste0("Active Cases ",format(Sys.Date(), '%Y-%m-%d'))
       , x = "days since reacing 20 active cases")


jpeg(paste0('img/us-passing-wuhan-',Sys.Date(),'.jpeg')
     , width = 480*2
     , height = 480*2
     , res = 200
)
passing_wuhan
dev.off()


# 10.0 Deaths lagging active cases ----
processed %>% 
  select(datetime, area, Confirmed, Active, Deaths) %>% 
  group_by(area) %>% 
  filter(some(Confirmed, function(x)x>2000)) %>% 
  filter(Confirmed>2000) %>% 
  mutate(days_since_100 = 1:n()) %>% 
  ggplot()+
  aes(x = days_since_100, y = Confirmed, group = area)+
  geom_line()
  

ccf_data <- 
  processed %>% 
  filter(area!="hubei") %>% 
  select(datetime, area, Confirmed, Active, Deaths) %>% 
  group_by(area) %>% 
  filter(some(Confirmed, function(x)x>2000)) %>% 
  filter(Confirmed>2000) %>% 
  mutate(days_since_100 = 1:n()) %>% 
  group_by(days_since_100) %>% 
  summarise(Confirmed = sum(Confirmed)
            , Deaths = sum(Deaths)) %>% 
  mutate(Confirmed_diff = Confirmed-lag(Confirmed,1))


ccf_vals_diff <- ccf(diff(ccf_data$Confirmed), ccf_data$Deaths, lag.max = 25)
ccf_vals <- ccf(ccf_data$Confirmed, ccf_data$Deaths, lag.max = 25)

lags_data <- 
  tibble(lags = as.numeric(ccf_vals_diff$lag)
       , change_in_confirmed = as.numeric(ccf_vals_diff$acf)
       , confirmed = as.numeric(ccf_vals$acf)) %>% 
  filter(lags<(-5))

change_lag <- lags_data %>% filter(change_in_confirmed==max(change_in_confirmed)) %>% head(1) %>% pull(lags)
confirmed_lag <- lags_data %>% filter(confirmed==max(confirmed)) %>% head(1) %>% pull(lags)


summary(lm(Deaths ~ Confirmed + days_since_100, data = ccf_data))
lagged_death_model <- lm(Deaths ~ Confirmed + days_since_100+ lag(Confirmed, abs(confirmed_lag))+lag(Confirmed_diff,abs(change_lag)), data = ccf_data)
summary(lagged_death_model)

library(modelr)
processed %>% 
  filter(area!="hubei") %>% 
  select(datetime, area, Confirmed, Active, Deaths) %>% 
  group_by(area) %>% 
  filter(some(Confirmed, function(x)x>2000)) %>% 
  filter(Confirmed>2000) %>% 
  mutate(days_since_100 = 1:n()) %>% 
  mutate(Confirmed_diff = Confirmed-lag(Confirmed,1)) %>% 
  add_predictions(lagged_death_model) %>%
  add_residuals(lagged_death_model) %>%
  filter(!is.na(pred)) %>%
  ungroup() %>% 
  select(Deaths, pred) %>% 
  corrr::correlate()
  

processed %>% 
  filter(area!="hubei") %>% 
  select(datetime, area, Confirmed, Active, Deaths) %>% 
  group_by(area) %>% 
  filter(some(Confirmed, function(x)x>2000)) %>% 
  filter(Confirmed>2000) %>% 
  mutate(days_since_100 = 1:n()) %>% 
  mutate(Confirmed_diff = Confirmed-lag(Confirmed,1)) %>% 
  add_predictions(lagged_death_model) %>%
  add_residuals(lagged_death_model) %>%
  filter(!is.na(pred)) %>% 
  ggplot()+
  aes(x = days_since_100, y = resid, group = area)+
  geom_line()
  



# 11.0 Percent Increases NYC ----

hubei_growth_rate <- 
  processed %>% 
  filter(area=="hubei") %>% 
  select(area, datetime, Deaths) %>% 
  mutate(`Wuhan Deaths` = CAGR_formula(Deaths, lag(Deaths,1))) %>% 
  mutate(days_since_reported = 1:n()+14)

NYC_reports %>% 
  bind_rows(hubei_growth_rate) %>% 
  select(days_since_reported
         , "NYC Deaths" = `Death CAGR`
         , `Wuhan Deaths`) %>% 
  gather(Var, Value, -days_since_reported) %>% 
  na.omit() %>% 
  filter(is.finite(Value)) %>% 
  ggplot()+
  aes(x = days_since_reported, y = Value, fill = Var)+
  geom_col(position = "dodge")+
  scale_fill_tq()+
  scale_y_continuous(labels = scales::percent)+
  coord_cartesian(ylim = c(0,1))+
  theme(plot.title.position = "plot")+
  labs(x = "Days elapsed"
       , y = "Growth rate"
       , fill = NULL
       , title = "NYC Covid-19 Daily Growth Rates")


# 12.0 Mortaility rate over time ----

mortality_rate_over_time <- 
  processed %>% 
  mutate(`Mortality Rate` = Deaths/Confirmed) %>% 
  group_by(area) %>% 
  filter(some(Deaths,function(x)x>100))%>% 
  filter(Deaths>20) %>% 
  mutate(Days = 1:n()) %>% 
  ggplot()+
  aes(x=Days, y = `Mortality Rate` , color = area)+
  geom_line()+
  theme_tq()+
  scale_color_tq()+
  scale_y_continuous(labels = scales::percent)+
  theme(plot.title.position = "plot")+
  labs(x = "Days since reaching 20 deaths")


# 13.1 Cases per 1 million ----

populations_2015 <- 
  read_csv("data/UN-population-projection-medium-variant.csv") %>% 
  filter(Year==2015) %>% 
  select(Entity, "Population" = `Estimates, 1950 - 2015: Total population by broad age group, both sexes combined (thousands) - Total`) %>% 
  mutate(Population_1M = Population/1000000) %>% 
  mutate(Entity = str_to_lower(Entity)) %>% 
  mutate(Entity = case_when(
    Entity == "united states" ~ "us"
    , TRUE ~ Entity
  ))

active_per_1_million <- 
  processed %>% 
  filter(Country!="Other") %>% 
  filter(Active>20) %>% 
  group_by(area) %>% 
  mutate(days_since_reported=1:n()) %>% 
  mutate(label = ifelse(days_since_reported==max(days_since_reported) #& Active>2000
                        , area, NA_character_)) %>% 
  inner_join(populations_2015, by = c(area = "Entity")) %>% 
  mutate(Active_per_1M_people = Active / Population_1M) %>% 
  ggplot()+
  aes(x = days_since_reported
      , y = Active_per_1M_people
      , group = area, label = area, color = area)+
  geom_line()+
  geom_text(aes(label = label))+
  scale_color_tq()+
  theme_tq()+
  theme(legend.position = "none")+
  theme(plot.title.position = "plot")+
  labs(title = "Active Cases Per 1 Million People"
       , x = "days since reacing 20 active cases"
       , y = "Active cases per 1 million people")




jpeg(paste0('img/active-per-1-million',Sys.Date(),'.jpeg')
     , width = 480*2
     , height = 480*2
     , res = 200
)
active_per_1_million
dev.off()


# 13.2 Cases per 1 million pt2 ----

populations_2015 <- 
  read_csv("data/UN-population-projection-medium-variant.csv") %>% 
  filter(Year==2015) %>% 
  select(Entity, "Population" = `Estimates, 1950 - 2015: Total population by broad age group, both sexes combined (thousands) - Total`) %>% 
  mutate(Population_1M = Population/1000000) %>% 
  mutate(Entity = str_to_lower(Entity)) %>% 
  mutate(Entity = case_when(
    Entity == "united states" ~ "us"
    , TRUE ~ Entity
  ))

active_per_1_million_2 <- 
  processed %>% 
  filter(Country!="Other") %>% 
  filter(Active>500) %>% 
  group_by(area) %>% 
  mutate(days_since_reported=1:n()) %>% 
  mutate(label = ifelse(days_since_reported==max(days_since_reported) #& Active>2000
                        , area, NA_character_)) %>% 
  inner_join(populations_2015, by = c(area = "Entity")) %>% 
  mutate(Active_per_1M_people = Active / Population_1M) %>% 
  ggplot()+
  aes(x = days_since_reported
      , y = Active_per_1M_people
      , group = area, label = area, color = area)+
  geom_line()+
  geom_text(aes(label = label))+
  scale_color_tq()+
  theme_tq()+
  theme(legend.position = "none")+
  theme(plot.title.position = "plot")+
  labs(title = "Active Cases Per 1 Million People"
       , x = "days since reacing 500 active cases"
       , y = "Active cases per 1 million people")


jpeg(paste0('img/active-per-1-million',Sys.Date(),'.jpeg')
     , width = 480*2
     , height = 480*2
     , res = 200
)
active_per_1_million_2
dev.off()



# 14.0 US Flattening? ----

processed %>% 
  filter(area%in% c("us","italy","canada","france","australia"
                    ,"germany","israel","uk","greece","spain", "hubei")) %>% 
  group_by(area) %>% 
  filter(Confirmed>20) %>% 
  mutate(days_since_reported = 1:n()) %>% 
  mutate(label = if_else(days_since_reported == max(days_since_reported) 
                         , as.character(area), NA_character_)) %>%
  ggplot()+
  aes(x = days_since_reported, y = Active, color = area)+
  geom_line()+
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE) +
  theme_tq()+
  scale_color_tq()+
  labs(x = "Days since reaching 20 cases"
       , y = "Active Cases")

processed %>% 
  filter(area%in% c("us")) %>% 
  group_by(area) %>% 
  filter(Confirmed>20) %>% 
  mutate(days_since_reported = 1:n()) %>% 
  mutate(label = if_else(days_since_reported == max(days_since_reported) 
                         , as.character(area), NA_character_)) %>%
  select(datetime, area, Confirmed) %>% 
  mutate(New_Cases = c(NA, diff(Confirmed))) %>% 
  ggplot()+
  aes(x = days_since_reported, y = New_Cases, color = area)+
  geom_line()+
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE) +
  theme_tq()+
  scale_color_tq()+
  labs(x = "Days since reaching 20 cases"
       , y = "Active Cases")


# 15.0 Deaths Per Million ----

# 13.2 Cases per 1 million pt2 ----

populations_2015 <- 
  read_csv("data/UN-population-projection-medium-variant.csv") %>% 
  filter(Year==2015) %>% 
  select(Entity, "Population" = `Estimates, 1950 - 2015: Total population by broad age group, both sexes combined (thousands) - Total`) %>% 
  mutate(Population_1M = Population/1000000) %>% 
  mutate(Entity = str_to_lower(Entity)) %>% 
  mutate(Entity = case_when(
    Entity == "united states" ~ "us"
    , TRUE ~ Entity
  ))


 max_days <- 
  processed %>% 
  #filter(Country!="Other") %>% 
  filter(Deaths>100) %>% 
  group_by(area) %>% 
  mutate(days_since_reported=1:n()) %>% 
  ungroup() %>% 
  summarise(days_since_reported = max(days_since_reported))

deaths_per_1_million <- 
  processed %>% 
  #filter(Country!="Other") %>% 
  filter(Deaths>100) %>% 
  group_by(area) %>% 
  mutate(days_since_reported=1:n()) %>% 
  mutate(label = ifelse(days_since_reported==max(days_since_reported) & Deaths>1000
                        , paste0(area," ",scales::comma(round(Deaths/1000,2)),"K Deaths"), NA_character_)) %>%
  inner_join(populations_2015, by = c(area = "Entity")) %>% 
  mutate(Deaths_per_1M_people = Deaths / Population_1M) %>% 
  ggplot()+
  aes(x = days_since_reported
      , y = Deaths_per_1M_people
      , group = area, label = area, color = area)+
  geom_line()+
  geom_text(aes(label = label))+
  scale_color_tq()+
  theme_tq()+
  theme(legend.position = "none")+
  theme(plot.title.position = "plot")+
  labs(title = "Deaths Per 1 Million People"
       , subtitle = Sys.Date()
       , x = "days since reacing 100 deaths"
       , y = "Deaths per 1 million people")


deaths_per_1_million

jpeg(paste0('img/deaths-per-1-million',Sys.Date(),'.jpeg')
     , width = 480*2
     , height = 480*2
     , res = 200
)
deaths_per_1_million
dev.off()
