

# UPDATE 6/3 NEW SOURCE:
# browseURL("https://docs.google.com/spreadsheets/d/12x3bbOzjNY2roOggAA8JjapNwp8y_J4bj4xpK4IL4EA/edit#gid=1661850317")


library(tidyverse)
library(rvest)
library(sf)
library(readxl)
library(lubridate)
library(tidyquant)
library(googlesheets)

ma_towns <- read_sf('data/ma_towns_2.geojson') %>% 
  select(city_town, geometry)

MA_town_data <- 
  read_sheet("https://docs.google.com/spreadsheets/d/12x3bbOzjNY2roOggAA8JjapNwp8y_J4bj4xpK4IL4EA/edit#gid=1661850317")

# some columns read in as lists (?)
MA_town_data <- MA_town_data %>% mutate_if(is.list, function(x)readr::parse_number(unlist(x)))

glimpse(MA_town_data)

infection_rate_df <- 
  MA_town_data %>% 
  filter(Town!="Statewide Total") %>% 
  select(-contains("Percent Increase"),-`Increase Since First Report On April 15`) %>% 
  pivot_longer(-Town,names_to = "Date",values_to = "Count") %>% 
  mutate(Date = str_remove_all(Date, "[/]2020")
         , Date = paste0(Date,"/2020")
         , Date = lubridate::mdy(Date)
         ) 

infection_rate_sf <- 
  ma_towns %>% 
  inner_join(infection_rate_df, by = c("city_town"="Town"))

# most interested:   
infection_rate_df%>% 
  filter(`Town`=="Amesbury")

infection_rate_df %>% 
  filter(`Town`=="Beverly")

infection_rate_df %>% 
  filter(`Town`=="Topsfield")


# 1.0 Boston Vs. Other Towns ----

infection_rate_df %>% 
  mutate(boston = ifelse(`Town`=="Boston","Boston","Other")) %>% 
  group_by(Date, boston) %>% 
  summarise(Count = sum(Count, na.rm = T)
            #, Rate = mean(Rate, na.rm = T)
            ) %>% 
  arrange(boston, Date) %>% 
  group_by(boston) %>% 
  mutate(`Percent Change` = replace_na(Count/lag(Count,1)-1,NA)) %>% 
  #select(-Rate) %>% 
  gather(Var, Value, -boston, -Date) %>% 
  ggplot()+
  aes(x = Date, y = Value, color = boston)+
  geom_line(size = 1.5)+
  facet_wrap(~Var, scales = "free_y", ncol = 1)+
  theme_tq()+
  theme(legend.position = "top")+
  scale_color_tq()+
  labs(color = NULL
       , y = NULL
       , x = NULL
       , caption = Sys.Date()
       , title = "Total Count and % Change, Boston vs. Other Cities/Towns")

# 2.0 Map Of Increases In Counts This Week (>0) ----
library(mapview)
infection_rate_sf %>% 
  group_by(city_town) %>% 
  arrange(city_town, Date) %>% 
  mutate(change_in_count = Count/lag(Count,1)-1
         , increase_in_count = Count-lag(Count,1)
         #, change_in_rate = Rate/lag(Rate,1)-1
         ) %>% 
  filter(Date==max(Date)) %>% 
  filter(increase_in_count>0) %>% 
  mapview(., zcol = "increase_in_count", cex = "increase_in_count"
          , label = paste0(.$city_town, " ",.$increase_in_count))  

# 2.1 Map Of % Increases in Count This Week (>10% and >10 cases)
infection_rate_sf %>% 
  group_by(city_town) %>% 
  arrange(city_town, Date) %>% 
  mutate(percent_change_in_count = Count/lag(Count,1)-1
         , increase_in_count = Count-lag(Count,1)
  ) %>% 
  filter(Date==max(Date)) %>% 
  filter(!is.na(percent_change_in_count), !is.infinite(percent_change_in_count)) %>% 
  filter(percent_change_in_count>.1, increase_in_count>10) %>% 
  mapview(., zcol = "percent_change_in_count", cex = "percent_change_in_count"
          , label = paste0("Town: ",.$city_town
                           , "\n"
                           , "% change: ",.$percent_change_in_count
                           , "\n"
                           , "increase: ", .$increase_in_count
          )
  )  

# 3.0 SlopeGraphs for Select MA Towns ----

library(CGPfunctions)

custom_colors <- 
  infection_rate_sf %>% 
  st_set_geometry(NULL) %>% 
  select(city_town, contains("Count")) %>%
  mutate(trend = case_when(
    city_town %in% c("Topsfield","Newburyport", "Amesbury"
                     ,"Danvers","Beverly","Georgetown", "Great Barrington"
                     , "Amherst","Williamstown","Boston", "West Newbury", "Newbury","Merrimac"
                     , "Middleton", "North Andover") ~ "darkgreen",
    TRUE ~ "gray"
  )
  ) %>%
  select(city_town, trend) %>%
  tibble::deframe()

counts_slope <- 
  infection_rate_sf %>% 
  mutate(custome_colors = custom_colors) %>% 
  st_set_geometry(NULL) %>% 
  filter(custom_colors=="darkgreen"
         , city_town!="Boston") %>% 
  select(city_town, Date, contains("Count")) %>% 
  mutate(Date = factor(Date)) %>% 
  newggslopegraph(dataframe = .
                  , Times = Date
                  , Measurement = Count
                  , Grouping = city_town
                  , LineColor = custom_colors
                  , Title = "Infection Counts in select MA towns/cities"
                  , SubTitle = NULL
                  , Caption = NULL
  )

library(patchwork)
counts_slope




# 4.0 FASTEST GROWERS ----

rank_by_percent_change <- 
  infection_rate_df %>% 
  arrange(`Town`, Date) %>% 
  group_by(`Town`) %>% 
  mutate(percent_change = Count/lag(Count,1)-1) %>% 
  mutate(percent_change = ifelse(is.infinite(percent_change),NA,percent_change)) %>% 
  filter(Date==max(Date)) %>% 
  ungroup() %>% 
  arrange(desc(percent_change)) %>% 
  select(`Town`, percent_change)


# (>10 increase in count only)
top_25_fastest_growers_by_percent_change <- 
  infection_rate_df %>% 
  arrange(`Town`, Date) %>% 
  group_by(`Town`) %>% 
  mutate(percent_change = Count/lag(Count,1)-1) %>% 
  mutate(percent_change = ifelse(is.infinite(percent_change),NA,percent_change)) %>% 
  mutate(Increase_In_Count= Count-lag(Count,1)) %>% 
  filter(Date==max(Date)) %>% 
  filter(Increase_In_Count>10) %>% 
  ungroup() %>% 
  arrange(desc(percent_change)) %>% 
  head(25) %>% 
  print(n=Inf) %>% 
  select(`Town`,percent_change)






map_to_colors <- 
  top_25_fastest_growers_by_percent_change %>% 
  mutate(color = cut(percent_change,5)) %>% 
  count(color) %>% 
  bind_cols(tibble(value = colorRampPalette(c("#F6CBA8", "#ff1900"))(nrow(.))))


custom_colors<-
  top_25_fastest_growers_by_percent_change %>% 
  mutate(color = cut(percent_change,5)) %>% 
  left_join(map_to_colors, by = "color") %>% 
  select(`Town`, value ) %>% 
  deframe()

# 4.1 SLOPEGRAPH TOP 25 PERCENT CHANGE ----
infection_rate_df %>% 
  arrange(`Town`, Date) %>% 
  group_by(`Town`) %>% 
  mutate(percent_change = Count/lag(Count,1)-1) %>% 
  mutate(percent_change = ifelse(is.infinite(percent_change),NA,percent_change)) %>% 
  inner_join(select(top_25_fastest_growers_by_percent_change,-percent_change), by = c("Town")) %>% 
  mutate(Date = factor(Date)) %>% 
  filter(!is.na(percent_change)) %>% 
  mutate(percent_change=round(percent_change,2)) %>% 
  newggslopegraph(dataframe = .
                  , Times = Date
                  , Measurement = percent_change
                  , Grouping = `Town`
                  , LineColor = custom_colors
                  , Title = "25 largest % changes in virus counts this week"
                  , SubTitle = Sys.Date()
                  , Caption = NULL
  )


map_data <- 
  infection_rate_sf %>% 
  inner_join(rank_by_percent_change, by = c("city_town"="Town")) %>% 
  group_by(city_town) %>% 
  filter(Date==max(Date)) %>% 
  mutate(`Percent Change Since Last Week` = percent_change) %>% 
  ungroup() %>% 
  arrange(desc(percent_change)) %>% 
  head(100)


map_data %>% 
  mapview(., zcol = "Percent Change Since Last Week"
          , cex = "Percent Change Since Last Week"
          , text = paste0(.$city_town, " ",.$percent_change))  %>% 
  
  addStaticLabels(.,label = map_data$city_town)

