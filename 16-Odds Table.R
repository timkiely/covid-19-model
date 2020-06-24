


# DATA FROM JHU
# browseURL("https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data")

library(tidyverse)
library(rvest)
library(lubridate)
library(readxl)


# 0.1 JHU TIME SERIES ----

# NOTE: currently JHU not offering a county level view of "recovered" cases so not useful over time
# NOTE: revisit this link periodically to see if they add a 'recovered' US file:
"https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series" %>% 
  read_html() %>% 
  html_table() %>% 
  .[[1]] %>% 
  filter(str_detect(Name, "recover"))

jhu_timeseries_confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv", col_types = cols())
jhu_timeseries_deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv", col_types = cols())
jhu_timeseries_recovered <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv", col_types = cols())


# 0.2 JHU Daily Reports ----
all_jhu_daily_report_files <- 
  "https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports" %>% 
  read_html() %>% 
  html_table() %>% 
  .[[1]] %>% 
  as_tibble() %>% 
  mutate(Date = mdy(Name)) %>% 
  filter(!is.na(Date)) %>% 
  select(-Type)


current_JHU_file_copies <- dir('data/JHU Daily Virus Data')

files_for_download <- 
  all_jhu_daily_report_files[!all_jhu_daily_report_files$Name%in%current_JHU_file_copies,] %>% 
  mutate(day = lubridate::wday(Date)) %>% 
  arrange(desc(Date))


# 0.2.2 DOWNLOAD NEW DATA ----
message("Synching most recent JHU daily update files")
if(nrow(files_for_download)!=0){
  jhu_data_list <- list()
  for(i in 1:nrow(files_for_download)){
    # i <- 1
    raw_jhu_github_stub <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/"
    the_row <- files_for_download[i,]
    message(i," of ",nrow(files_for_download)," ",the_row$Name)
    
    jhu_file <- the_row$Name
    
    download.file(paste0(raw_jhu_github_stub,jhu_file)
                  , destfile = paste0('data/JHU Daily Virus Data/',jhu_file)
                  , quiet = T)
    suppressMessages(
      weekly_report <- read_csv(paste0(raw_jhu_github_stub,jhu_file)) %>% mutate(Date = the_row$Date)
    )
    jhu_data_list[[jhu_file]] <- weekly_report
  }
}

# 0.2.3 LOAD LATEST DATA ----

all_latest_data <- 
  dir('data/JHU Daily Virus Data') %>% 
  enframe() %>% 
  distinct(value, .keep_all = T) %>% 
  mutate(date = lubridate::mdy(str_remove_all(value, "[.csv]"))) %>% 
  filter(date>=lubridate::ymd("2020 03 22")) %>% 
  pull(value) %>%
  map(~suppressMessages(read_csv
                        (
                          paste0('data/JHU Daily Virus Data/',.x)
                          , col_types = cols(FIPS = col_character()
                                             , Last_Update = col_character())
                        )
  ) %>% 
    mutate(Date = .x)
  ) %>% 
  bind_rows() %>% 
  mutate(super_key = paste0(Combined_Key, Date)) %>% 
  distinct(super_key, .keep_all = T) %>% 
  filter(Country_Region == "US") %>% 
  select("County" = Admin2, "State" = Province_State, "Country" = Country_Region,
         Lat, "Long"=Long_,Confirmed:Date, -Combined_Key, FIPS) %>% 
  mutate(Date = lubridate::mdy(str_remove_all(Date, "[.csv]"))) %>% 
  group_by(State, County) %>% 
  arrange(State, County, desc(Date)) %>%
  mutate(day_in_sample = 1:n()) %>% 
  arrange(State, County, Date) %>%
  mutate(new_cases = c(NA, diff(Confirmed)) %>% replace(.,.<0,0)
         , new_deaths = c(NA, diff(Deaths)) %>% replace(.,.<0,0)
         , new_recovered = c(NA, diff(Recovered)) %>% replace(.,.<0,0)
  ) %>% 
  mutate(FIPS = str_pad(FIPS, width = 5, side = "left", pad = "0")) 


jhu_daily_data_last_2_weeks_us <- all_latest_data %>% filter(day_in_sample%in%1:14) 


# 0.2.4 ESTIMATING RECOVERY COUNTS ----

# Oklahoma state methodology:
# https://looker-dashboards.ok.gov/embed/dashboards/44
# *Recovered: currently not hospitalized or deceased 
# and 14 days after onset/report. Recoveries calculated 
# beginning on 4/6/2020.

# 0.2.4.2 STRATEGY 2 ----

# SHIFT CONFIRMED FORWARD 14 DAYS
# SUBTRACT DEATHS FROM SHIFTED CONFIRMED (NET CONFIRMED)
# CALCULATE COUNTY % OF STATE FOR NET CONFIRMED
# ALLOCATE STATE RECOVERY BY %

# US STATE LEVEL CASES/RECOVERIES
covid_tracking_data <- 
  jsonlite::fromJSON("https://covidtracking.com/api/v1/states/daily.json") %>% 
  as_tibble() %>% 
  select(date, state, positive, negative, pending, total, hospitalizedCurrently, hospitalizedCumulative
         , inIcuCurrently, inIcuCumulative, recovered, death, posNeg) %>% 
  mutate(date = ymd(date))


all_state_dates <- 
  expand.grid(
    "state" = state.abb 
    , "date" = seq.Date(from  = range(covid_tracking_data$date)[1]
                        , to  = range(covid_tracking_data$date)[2]
                        , by = "day")) %>% 
  mutate(state=as.character(state)) %>% 
  as_tibble() %>% 
  left_join(tibble("state" = state.abb,"state_name"=state.name), by = "state") %>% 
  select(state, state_name, date)

state_level_recoveries <- 
  covid_tracking_data %>% 
  select(date, state, recovered) %>% 
  group_by(state) %>% 
  arrange(state, date) %>% 
  filter(!is.na(recovered)) %>% 
  ungroup() %>% 
  rename("state_recovered" = recovered) %>% 
  right_join(all_state_dates, by = c("state","date")) %>% 
  select(date, "state" = state_name, state_recovered) %>% 
  arrange(state, date) %>% 
  mutate(state_recovered=replace_na(state_recovered,0))


Recovered_Estimate_By_County <- 
  all_latest_data %>%
  select(County, State, Date, Confirmed, Deaths, Recovered, Active) %>%
  group_by(State, County) %>% 
  arrange(State, County, Date) %>% 
  mutate(shifted_confirmed = lag(Confirmed,14)) %>% 
  mutate(net_confirmed = shifted_confirmed-Deaths) %>% 
  mutate(net_confirmed = ifelse(net_confirmed<0,0,net_confirmed)) %>% 
  filter(!is.na(net_confirmed)) %>% 
  ungroup() %>% 
  group_by(State, Date) %>% 
  arrange(State, Date) %>% 
  mutate(percent_net_confirmed = net_confirmed/sum(net_confirmed)) %>% 
  select(State, County, Date,percent_net_confirmed) %>% 
  left_join(state_level_recoveries, by = c("State" = "state", "Date" = "date")) %>% 
  filter(!is.na(state_recovered)) %>% 
  mutate(Recovered=as.integer(state_recovered*percent_net_confirmed)) %>% 
  filter(!is.na(Recovered)) %>% 
  ungroup() %>% 
  select(State, County, Date, "Est Recovered" = Recovered) %>% 
  arrange(State, County, Date)


# matches up well vs. official Oklahoma estimates: 
# browseURL("https://looker-dashboards.ok.gov/embed/dashboards/44")
OK_recoveries_estimated <- 
  Recovered_Estimate_By_County %>% 
  filter(State=="Oklahoma") %>% 
  filter(Date == max(Date)) %>% 
  arrange(-`Est Recovered`)

all_data_with_recoveries <- 
  all_latest_data %>% 
  inner_join(Recovered_Estimate_By_County, by = c("County", "State", "Date")) %>% 
  group_by(State, County) %>% 
  arrange(State, County, Date) %>% 
  mutate(Recovered = `Est Recovered`
         , new_recovered = c(NA,diff(Recovered))) %>% 
  mutate(Active = Confirmed-Deaths-Recovered) %>% 
  mutate(Active = ifelse(Active<0,0,Active))


# 0.2.5 Combine Recoveries Active ----

# 0.3 ZIP/COUNTY POP ----
# browseURL("https://simplemaps.com/data/us-zips")
zip_county <- read_csv("data/uszips 2020 04 19.csv", col_types = cols()) %>% 
  select(zip, "zip pop" = population, "zip density per sq km" = density
         , county_fips, county_name, state_name, state_id) %>% 
  mutate(zip = as.numeric(zip)) %>% 
  filter(!is.na(county_fips))

NYC_Special_pop <- 
  zip_county %>% 
  filter(state_name=="New York") %>% 
  filter(county_name %in% c("New York","Kings","Queens","Richmond","Bronx")) %>% 
  group_by(county_fips = "36061") %>% 
  summarise(population = sum(`zip pop`, na.rm =T)) %>% 
  mutate(county_fips=str_pad(county_fips, width = 5, side = "left", pad = "0")) 

county_pop_per_100k <- 
  zip_county %>% 
  group_by(county_fips) %>% 
  summarise(population = sum(`zip pop`, na.rm =T)) %>%
  mutate(county_fips=str_pad(county_fips, width = 5, side = "left", pad = "0")) %>% 
  filter(county_fips != "36061") %>% 
  bind_rows(NYC_Special_pop)



# 0.4 CASE COUNTS ALL COUNTIES ----

# TOTAL CASES
# ACTIVE CASES
# CHANGE IN TOTAL
# CHANGE IN ACTIVE

jhu_county_daily_report_data_total_active <- 
  all_data_with_recoveries %>% 
  left_join(county_pop_per_100k, by = c("FIPS"="county_fips")) %>%
  select(State, County, FIPS, population, Lat, Long, Date
         , "New Cases" = new_cases
         , "Total Cases" = Confirmed
         , "Total Deaths" = Deaths
         , "Total Recovered" = `Est Recovered`
         , "Active Cases" = `Active`
  ) %>% 
  mutate(`Active Cases` = replace(`Active Cases`, `Active Cases`<0,0))

identity <- function(x) x

odds <- function(x){
  # x <- odds_table$`Odds 0-49`
  frac <- as.character(MASS::fractions(x,max.denominator = 1000000))
  
  tibble(frac) %>% 
      mutate(numerator = as.numeric(str_remove(str_extract(frac,".*[/]"),"[/]"))
             , denominator = as.numeric(str_remove(str_extract(frac,"[/].*"),"[/]"))
             , odds = denominator/numerator
             , odds_text = paste0("1 out of ",scales::comma(floor(odds)))) %>% 
      pull(odds_text)
    
}

odds_table <- 
  jhu_county_daily_report_data_total_active %>% 
  ungroup() %>% 
  transmute(
    State
    , County
    , Date
    , `Active Cases`
    , population
    , `Symptomatic Cases` = 0.65
    , Prevelence = `Active Cases`/population
    , `SFR 0-49` = 	0.0005 #0.05%
    , `SFR 50-64` =	0.0020 #0.20%
    , `SFR 65+`	= 0.0130 #1.30%
    
    , `Odds 0-49 rate` = Prevelence*`Symptomatic Cases`*`SFR 0-49`
    , `Odds 50-64 rate` = Prevelence*`Symptomatic Cases`*`SFR 50-64`
    , `Odds 65+ rate` = Prevelence*`Symptomatic Cases`*`SFR 65+`
    
    , `Odds 0-49` = Prevelence*`Symptomatic Cases`*`SFR 0-49`
    , `Odds 50-64` = Prevelence*`Symptomatic Cases`*`SFR 50-64`
    , `Odds 65+` = Prevelence*`Symptomatic Cases`*`SFR 65+`
  ) %>% 
  mutate_at(vars(`Odds 0-49`:`Odds 65+`), odds) 


odds_table %>% 
  filter(State=="New Jersey") %>% 
  ggplot()+
  aes(Date, `Active Cases`, group = County)+
  geom_line()


library(tidyquant)
odds_table %>% 
  filter(State=="New Jersey") %>% 
  select(County, Date, contains("rate")) %>% 
  rename(
    "0-49" = `Odds 0-49 rate`
    ,"50-64" = `Odds 50-64 rate`
    ,"65+" = `Odds 65+ rate`
  ) %>% 
  gather(Var, Value, -County, -Date) %>% 
  ggplot()+
  aes(Date, Value, group = County)+
  geom_line()+
  geom_hline(yintercept = 1/8000, color ="red")+
  facet_wrap(~Var, ncol = 1)+
  theme_tq()+
  scale_color_tq()+
  theme_tq()+
  labs(title = "NEW JERSEY"
       , subtitle = "All-in probability of contracting and subsequently dying from COVID"
       , y = NULL)






