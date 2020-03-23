


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


# NYC vs. italy spain countires ----


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




processed %>% 
  filter(area%in% c("us","canada","france","australia"
                    ,"germany","israel","uk","greece","spain")) %>% 
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

jpeg(paste0('img/covid-NYC-majors-first-20-days-',Sys.Date(),'.jpeg')
     , width = 480*2
     , height = 480*2
     , res = 200
)
nyc_first_20_days
dev.off()

