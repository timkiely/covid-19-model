
library(tidyverse)
library(rvest)
library(sf)

ma_towns <- read_sf('data/ma_towns_2.geojson') %>% 
  select(city_town, geometry)

the_link_4_22 <- "https://www.boston.com/news/coronavirus/2020/04/22/massachusetts-cities-towns-coronavirus-cases-numbers"
the_link_4_29 <- "https://www.boston.com/news/coronavirus/2020/04/29/latest-massachusetts-cities-towns-covid-19-cases-numbers"
the_link_5_06 <- "https://www.boston.com/news/coronavirus/2020/05/06/massachusetts-city-town-coronavirus-numbers"
the_link_5_13 <- "https://www.boston.com/news/coronavirus/2020/05/13/massachusetts-town-city-covid-19-cases-numbers"


the_page_4_22 <- read_html(the_link_4_22)
the_page_4_29 <- read_html(the_link_4_29)

infection_rates_by_town_22 <- 
  html_table(the_page_4_22, header = T)[[1]] %>% 
  as_tibble() %>% 
  rename("Count 4/22" = Count, "Rate 4/22" = Rate)

infection_rates_by_town_29 <- 
  html_table(the_page_4_29, header = T)[[1]] %>% 
  as_tibble() %>% 
  rename("Count 4/29" = Count, "Rate 4/29" = Rate)


list(the_link_4_22
     , the_link_4_29
     , the_link_5_06
     , the_link_5_13
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
               lubridate::ymd())
  }) -> MA_city_town_data


infection_rate_df <- 
  MA_city_town_data %>% 
  bind_rows()
    
infection_rate_sf <- 
  ma_towns %>% 
  inner_join(infection_rate_df, by = c("city_town"="City/Town")) %>% 
  mutate_at( vars(c(Count,Rate)), as.numeric)



library(mapview)

infection_rate_sf %>% 
  group_by(city_town) %>% 
  arrange(city_town, Report_date) %>% 
  mutate(change_in_count = Count/lag(Count,1)-1
         , increase_in_count = Count-lag(Count,1)
         , change_in_rate = Rate/lag(Rate,1)-1) %>% 
  filter(Report_date==max(Report_date)) %>% 
  mapview(., zcol = "increase_in_count", cex = "increase_in_count"
          , label = paste0(.$city_town, " ",.$increase_in_count))  


library(CGPfunctions)

custom_colors <- 
  infection_rate_sf %>% 
  st_set_geometry(NULL) %>% 
  select(city_town, contains("Rate")) %>% 
  mutate(difference = Rate - lag(Rate,1)) %>%
  mutate(trend = case_when(
    city_town %in% c("Topsfield","Newburyport", "Amesbury"
                     ,"Danvers","Beverly","Georgetown"
                     , "Amherst","Williamstown","Boston"
                     , "Middleton") ~ "darkgreen",
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


# FASTEST GROWERS ----


infection_rate_df%>% 
  filter(`City/Town`=="Amesbury")

infection_rate_df %>% 
  filter(`City/Town`=="Beverly")


all_growers <- 
  infection_rate_df %>% 
  filter(`City/Town`!="State Total") %>% 
  group_by(`City/Town`) %>% 
  mutate(Count = as.numeric(Count)) %>% 
  arrange(`City/Town`, Report_date) %>% 
  mutate(percent_change = Count/lag(Count,1)-1) %>% 
  filter(Report_date==max(Report_date)) %>% 
  ungroup() %>% 
  arrange(desc(percent_change)) %>% 
  select(`City/Town`, percent_change)

fastest_growers <- 
  infection_rate_df %>% 
  filter(`City/Town`!="State Total") %>% 
  group_by(`City/Town`) %>% 
  mutate(Count = as.numeric(Count)) %>% 
  arrange(`City/Town`, Report_date) %>% 
  mutate(percent_change = Count/lag(Count,1)-1) %>% 
  filter(Report_date==max(Report_date)) %>% 
  ungroup() %>% 
  arrange(desc(percent_change)) %>% 
  head(25) %>% 
  print(n=Inf) %>% 
  select(`City/Town`, percent_change)


custom_colors<-
  fastest_growers %>% 
  mutate(color = cut(percent_change,5)) %>% 
  select(`City/Town`, color ) %>% 
  deframe()

infection_rate_sf %>% 
  inner_join(fastest_growers, by = c("city_town"="City/Town")) %>% 
  st_set_geometry(NULL) %>% 
  mutate(Report_date = factor(Report_date)) %>% 
  newggslopegraph(dataframe = .
                  , Times = Report_date
                  , Measurement = Rate
                  , Grouping = city_town
                  , LineColor = custom_colors
                  , Title = "25 fastest growing infection rates in MA"
                  , Caption = NULL
  )



map_data <- infection_rate_sf %>% inner_join(all_growers, by = c("city_town"="City/Town")) %>% 
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

