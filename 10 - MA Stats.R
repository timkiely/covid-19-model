
library(tidyverse)
library(rvest)
library(sf)
library(readxl)
library(lubridate)
library(tidyquant)

ma_towns <- read_sf('data/ma_towns_2.geojson') %>% 
  select(city_town, geometry)

the_link_4_22 <- "https://www.boston.com/news/coronavirus/2020/04/22/massachusetts-cities-towns-coronavirus-cases-numbers"
the_link_4_29 <- "https://www.boston.com/news/coronavirus/2020/04/29/latest-massachusetts-cities-towns-covid-19-cases-numbers"
the_link_5_06 <- "https://www.boston.com/news/coronavirus/2020/05/06/massachusetts-city-town-coronavirus-numbers"
the_link_5_13 <- "https://www.boston.com/news/coronavirus/2020/05/13/massachusetts-town-city-covid-19-cases-numbers"
the_link_5_20 <- "https://www.boston.com/news/coronavirus/2020/05/20/latest-massachusetts-town-city-covid-19-numbers"

raw_data_5_27 <- 
  read_excel("data/WEEKLY covid-19-dashboard-5-27-2020.xlsx") %>% 
  filter(!is.na(`City/Town`)) %>% 
  mutate_at(vars(Count, Rate, `Percent Positivity`), readr::parse_number) %>% 
  mutate(Report_date=ymd("2020 05 27"))

list(the_link_4_22
     , the_link_4_29
     , the_link_5_06
     , the_link_5_13
     , the_link_5_20
     ) %>% 
  map(~{
    message(.x)
    the_page <- read_html(.x)

    the_page %>% 
      html_table(., header = T) %>% 
      .[[1]] %>% 
      as_tibble() %>% 
      mutate(Report_date = .x %>% 
               str_remove("https://www.boston.com/news/coronavirus/") %>% 
               str_remove("/massachusetts-cities-towns-coronavirus-cases-numbers") %>% 
               str_remove("/latest-massachusetts-cities-towns-covid-19-cases-numbers") %>% 
               str_remove("/massachusetts-town-city-covid-19-cases-numbers") %>%  
               str_remove("/latest-massachusetts-town-city-covid-19-numbers") %>% 
               lubridate::ymd())
  }) -> MA_city_town_data


infection_rate_df <- 
  MA_city_town_data %>% 
  bind_rows() %>% 
  mutate_at(vars(Count, Rate), readr::parse_number) %>%
  bind_rows(raw_data_5_27) %>% 
  filter(`City/Town`!="State Total")

infection_rate_sf <- 
  ma_towns %>% 
  inner_join(infection_rate_df, by = c("city_town"="City/Town"))

# most interested:   
infection_rate_df%>% 
  filter(`City/Town`=="Amesbury")

infection_rate_df %>% 
  filter(`City/Town`=="Beverly")

infection_rate_df %>% 
  filter(`City/Town`=="Topsfield")



# 1.0 Boston Vs. Other Towns ----

infection_rate_df %>% 
  mutate(boston = ifelse(`City/Town`=="Boston","Boston","Other")) %>% 
  group_by(Report_date, boston) %>% 
  summarise(Count = sum(Count, na.rm = T)
            , Rate = mean(Rate, na.rm = T)) %>% 
  arrange(boston, Report_date) %>% 
  group_by(boston) %>% 
  mutate(`Percent Change` = replace_na(Count/lag(Count,1)-1,NA)) %>% 
  select(-Rate) %>% 
  gather(Var, Value, -boston, -Report_date) %>% 
  ggplot()+
  aes(x = Report_date, y = Value, color = boston)+
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
  arrange(city_town, Report_date) %>% 
  mutate(change_in_count = Count/lag(Count,1)-1
         , increase_in_count = Count-lag(Count,1)
         , change_in_rate = Rate/lag(Rate,1)-1) %>% 
  filter(Report_date==max(Report_date)) %>% 
  filter(increase_in_count>0) %>% 
  mapview(., zcol = "increase_in_count", cex = "increase_in_count"
          , label = paste0(.$city_town, " ",.$increase_in_count))  

# 2.1 Map Of % Increases in Count This Week (>10% and >10 cases)
infection_rate_sf %>% 
  group_by(city_town) %>% 
  arrange(city_town, Report_date) %>% 
  mutate(percent_change_in_count = Count/lag(Count,1)-1
         , increase_in_count = Count-lag(Count,1)
         ) %>% 
  filter(Report_date==max(Report_date)) %>% 
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
  select(city_town, contains("Rate")) %>% 
  mutate(difference = Rate - lag(Rate,1)) %>%
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

rates_slope <- 
infection_rate_sf %>% 
  mutate(custome_colors = custom_colors) %>% 
  st_set_geometry(NULL) %>% 
  filter(custom_colors=="darkgreen") %>% 
  select(city_town, Report_date, contains("Rate")) %>% 
  mutate(Report_date = factor(Report_date)) %>% 
  newggslopegraph(dataframe = .
                  , Times = Report_date
                  , Measurement = Rate
                  , Grouping = city_town
                  , LineColor = custom_colors
                  , Title = "Infection Rate"
                  , Caption = NULL
  )


counts_slope <- 
infection_rate_sf %>% 
  mutate(custome_colors = custom_colors) %>% 
  st_set_geometry(NULL) %>% 
  filter(custom_colors=="darkgreen"
         , city_town!="Boston") %>% 
  select(city_town, Report_date, contains("Count")) %>% 
  mutate(Report_date = factor(Report_date)) %>% 
  newggslopegraph(dataframe = .
                  , Times = Report_date
                  , Measurement = Count
                  , Grouping = city_town
                  , LineColor = custom_colors
                  , Title = "Infection Counts in select MA towns/cities"
                  , SubTitle = NULL
                  , Caption = NULL
  )

library(patchwork)
counts_slope/rates_slope




# 4.0 FASTEST GROWERS ----

rank_by_percent_change <- 
  infection_rate_df %>% 
  arrange(`City/Town`, Report_date) %>% 
  group_by(`City/Town`) %>% 
  mutate(percent_change = Count/lag(Count,1)-1) %>% 
  mutate(percent_change = ifelse(is.infinite(percent_change),NA,percent_change)) %>% 
  filter(Report_date==max(Report_date)) %>% 
  ungroup() %>% 
  arrange(desc(percent_change)) %>% 
  select(`City/Town`, percent_change)


# (>10 increase in count only)
top_25_fastest_growers_by_percent_change <- 
  infection_rate_df %>% 
  arrange(`City/Town`, Report_date) %>% 
  group_by(`City/Town`) %>% 
  mutate(percent_change = Count/lag(Count,1)-1) %>% 
  mutate(percent_change = ifelse(is.infinite(percent_change),NA,percent_change)) %>% 
  mutate(Increase_In_Count= Count-lag(Count,1)) %>% 
  filter(Report_date==max(Report_date)) %>% 
  filter(Increase_In_Count>10) %>% 
  ungroup() %>% 
  arrange(desc(percent_change)) %>% 
  head(25) %>% 
  print(n=Inf) %>% 
  select(`City/Town`,percent_change)






map_to_colors <- 
  top_25_fastest_growers_by_percent_change %>% 
  mutate(color = cut(percent_change,5)) %>% 
  count(color) %>% 
  bind_cols(tibble(value = colorRampPalette(c("#F6CBA8", "#ff1900"))(nrow(.))))


custom_colors<-
  top_25_fastest_growers_by_percent_change %>% 
  mutate(color = cut(percent_change,5)) %>% 
  left_join(map_to_colors, by = "color") %>% 
  select(`City/Town`, value ) %>% 
  deframe()

# 4.1 SLOPEGRAPH TOP 25 PERCENT CHANGE ----
infection_rate_df %>% 
  arrange(`City/Town`, Report_date) %>% 
  group_by(`City/Town`) %>% 
  mutate(percent_change = Count/lag(Count,1)-1) %>% 
  mutate(percent_change = ifelse(is.infinite(percent_change),NA,percent_change)) %>% 
  inner_join(select(top_25_fastest_growers_by_percent_change,-percent_change), by = c("City/Town")) %>% 
  mutate(Report_date = factor(Report_date)) %>% 
  filter(!is.na(percent_change)) %>% 
  mutate(percent_change=round(percent_change,2)) %>% 
  newggslopegraph(dataframe = .
                  , Times = Report_date
                  , Measurement = percent_change
                  , Grouping = `City/Town`
                  , LineColor = custom_colors
                  , Title = "25 largest % changes in virus counts this week"
                  , SubTitle = Sys.Date()
                  , Caption = NULL
  )


map_data <- 
  infection_rate_sf %>% 
  inner_join(all_growers, by = c("city_town"="City/Town")) %>% 
  group_by(city_town) %>% 
  filter(Report_date==max(Report_date)) %>% 
  mutate(`Percent Change Since Last Week` = percent_change) %>% 
  ungroup() %>% 
  arrange(desc(percent_change)) %>% 
  head(100)
  
  
map_data %>% 
mapview(., zcol = "Percent Change Since Last Week"
          , cex = "Percent Change Since Last Week"
          , text = paste0(.$city_town, " ",.$percent_change))  %>% 

addStaticLabels(.,label = map_data$city_town)

