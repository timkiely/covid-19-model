

library(tidyverse)
library(lubridate)
library(tidyquant)
library(grid)
library(gridExtra)

identity <- function(x) x
cumulative <- function(x) cumsum(as.numeric(x, na.rm = T))


# updated daily at 4 pm
# browseURL("https://www1.nyc.gov/site/doh/covid/covid-19-data.page")
nyc_daily_data <- 
  read_csv("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/case-hosp-death.csv") %>% 
  mutate_if(is.numeric, function(x)ifelse(is.na(x),0,x)) %>% 
  mutate_at(vars(NEW_COVID_CASE_COUNT:DEATH_COUNT), lst(identity, cumulative)) %>% 
  select(-contains("identity")) 

NYC_reports <- 
  nyc_daily_data %>% 
  mutate(`Mortality Rate` = as.numeric(DEATH_COUNT_cumulative)/as.numeric(NEW_COVID_CASE_COUNT_cumulative)) %>% 
  mutate(`Death Percent Increase` = DEATH_COUNT_cumulative/lag(DEATH_COUNT_cumulative,1)-1) %>% 
  mutate(`Case Percent Increase` = NEW_COVID_CASE_COUNT_cumulative/lag(NEW_COVID_CASE_COUNT_cumulative,1)-1) 


max_date <- NYC_reports %>% mutate(DATE_OF_INTEREST = lubridate::mdy(DATE_OF_INTEREST)) %>% summarise(dt = max(DATE_OF_INTEREST)) %>% pull(dt)

nyc_case_counts_p1 <- 
  NYC_reports %>% 
  select(DATE_OF_INTEREST
         , "NEW CASE COUNT"=NEW_COVID_CASE_COUNT
         , "TOTAL CASES" = NEW_COVID_CASE_COUNT_cumulative
         , `Case Percent Increase`
         ) %>% 
  mutate(`Case Percent Increase` = ifelse(`Case Percent Increase`>1|
                                            is.infinite(`Case Percent Increase`)
                                          , NA
                                          , `Case Percent Increase`)) %>% 
  gather(Var, Value, -DATE_OF_INTEREST) %>% 
  mutate(DATE_OF_INTEREST = lubridate::mdy(DATE_OF_INTEREST)) %>% 
  mutate(Var_group = ifelse(str_detect(Var, "CASE|case|Case"),"CASES","DEATHS")
         , Var_type = case_when(
           str_detect(Var, "TOTAL") ~ "Total"
           , str_detect(Var, "Percent") ~ "Percent Change"
           , TRUE ~ "Count"
         )) %>% 
  ggplot()+
  aes(x = DATE_OF_INTEREST, y = Value, fill = Var_group)+
  geom_col()+
  geom_smooth(se = F)+
  facet_grid(Var_type~Var_group, scales = "free_y")+
  theme_tq()+
  theme(legend.position = "none", plot.title.position = "plot")+
  scale_fill_manual(values = "#E31A1C")+
  scale_y_continuous(labels=scales::comma)+
  labs(x = NULL, y = NULL, fill = NULL, caption = max_date)


nyc_case_counts_p2 <- 
  NYC_reports %>% 
  select(DATE_OF_INTEREST
         , "DEATHS" = DEATH_COUNT
         , "TOTAL DEATHS" = DEATH_COUNT_cumulative
         , `Death Percent Increase`
  ) %>% 
  mutate(`Death Percent Increase` = ifelse(`Death Percent Increase`>1|
                                             is.infinite(`Death Percent Increase`)
                                           , NA
                                           , `Death Percent Increase`)) %>% 
   gather(Var, Value, -DATE_OF_INTEREST) %>% 
  mutate(DATE_OF_INTEREST = lubridate::mdy(DATE_OF_INTEREST)) %>% 
  mutate(Var_group = ifelse(str_detect(Var, "CASE|case|Case"),"CASES","DEATHS")
         , Var_type = case_when(
           str_detect(Var, "TOTAL") ~ "Total"
           , str_detect(Var, "Percent") ~ "Percent Change"
           , TRUE ~ "Count"
         )) %>% 
  ggplot()+
  aes(x = DATE_OF_INTEREST, y = Value, fill = Var_group)+
  geom_col()+
  geom_smooth(se = F)+
  facet_grid(Var_type~Var_group, scales = "free_y")+
  theme_tq()+
  theme(legend.position = "none", plot.title.position = "plot")+
  scale_fill_manual(values = "#2C3E50")+
  scale_y_continuous(labels=scales::comma)+
  labs(x = NULL, y = NULL, fill = NULL, caption = max_date)

grid.arrange(nyc_case_counts_p1, nyc_case_counts_p2, ncol = 2)


jpeg(paste0('img/new_nyc_cases_slowing_',Sys.Date(),'.jpeg')
     , width = 480*2
     , height = 480*2
     , res = 200
)
nyc_case_counts
dev.off()