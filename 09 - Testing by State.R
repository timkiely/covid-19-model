

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

# new tests per day
covid_tracking_data %>% 
  group_by(date) %>% 
  summarise(total_tests = sum(total)) %>% 
  mutate(test_per_day = c(NA, diff(total_tests))) %>% 
  ggplot()+
  aes(x = date, y = test_per_day)+
  geom_col()+
  theme_tq()+
  scale_y_continuous(labels = scales::comma)+
  labs(y = "New Test Per Day"
       , x = NULL)



# SOUTH KOREA TESTING DATA
# from: browseURL("https://www.statista.com/statistics/1102777/south-korea-covid-19-daily-new-cases/")
# testing data from browseURL("https://www.latimes.com/world-nation/story/2020-03-14/south-koreas-rapid-coronavirus-testing-far-ahead-of-the-u-s-could-be-a-matter-of-life-and-death")
s_korea_new_cases <- 
  tribble(~Date, ~`New Cases`, ~Tests
          , "2-18-2020", 2  , 1094
          , "2-19-2020", 34 , 1146
          , "2-20-2020", 16 , 1750
          , "2-21-2020", 74 , 2655
          , "2-22-2020", 190, 4805
          , "2-23-2020", 210, 3012
          , "2-24-2020", 207, 5982
          , "2-25-2020", 130, 8101
          , "2-26-2020", 253, 9411
          , "2-27-2020", 449, 11863
          , "2-28-2020", 427, 12950
          , "2-29-2020", 909, 14753
          , "3-01-2020", 595, 11292
          , "3-02-2020", 686, 12606
          , "3-03-2020", 600, 16260
          , "3-04-2020", 516, 10856
          , "3-05-2020", 438, 9834
          , "3-06-2020", 518, 18199
          , "3-07-2020", 483, 13449
  ) %>% 
  mutate(Date = mdy(Date))


# SOUTH KOREA TESTING CAPACITY AS OF LATE MARCH
# According to this quora answer:
# https://www.quora.com/How-long-will-it-take-for-Covid-19-tests-to-outpace-demand-How-did-South-Korea-manage-to-have-enough-testing-kits-to-gather-epidemiological-data
# South Korea is producing 50K tests per week or 200k/month
# with the capacity to ramp up to 400K per month if needed
SOUTH_KOREA_CAPACITY_PER_WEEK <- 
  s_korea_new_cases %>% 
  filter(Date>=ymd("2020 02 29")
         , Date <=ymd("2020 03 06")) %>% 
  summarise(tests = sum(Tests)) %>% 
  pull(tests)



# WHAT WAS THE TEST POSITIVE RATE AT SOUTH KOREAS PEAK?
# ~5 PERCENT

korea_positives <- 
  s_korea_new_cases %>% 
  filter(Date>=ymd("2020 02 29")
         , Date <=ymd("2020 03 06")) %>% 
  summarise(pos = sum(`New Cases`)) %>% 
  pull(pos)

korea_positive_rate <- 
  s_korea_new_cases %>% 
  filter(Date>=ymd("2020 02 29")
         , Date <=ymd("2020 03 06")) %>% 
  summarise(rate = sum(`New Cases`)/SOUTH_KOREA_CAPACITY_PER_WEEK) %>% 
  pull(rate)


s_korea_rate_plot <- 
  s_korea_new_cases %>% 
  ggplot()+
  aes(Date, `New Cases`)+
  geom_line(group = 1, size = 2)+
  geom_rect(aes(xmin = ymd("2020 02 29")
                , xmax = ymd("2020 03 06")
                , ymin = -Inf
                , ymax = Inf)
            , fill='#18BC9C'
            , alpha=0.03)+
  geom_text(x = ymd("2020 02 29"), y = 250, label = paste0(scales::comma(SOUTH_KOREA_CAPACITY_PER_WEEK)," tests with\n",scales::comma(korea_positives)," positives"), hjust = 0)+
  theme_tq()+
  theme(plot.title.position = "plot"
        , plot.caption = element_text(hjust = 0))+
  labs(title = paste0("At peak outbreak, S. Korea had a positive test rate of ~",scales::percent(korea_positive_rate))
       , subtitle = "New Cases in South Korea During Peak Outbreak (2/29 to 3/6)"
       , y = "New Cases Reported by South Korea"
       , caption = "Sources: COVID Tracking Project, South Korean Center for Disease Control, \nvarious news sources")




# US TARGET NEEDS TO BE AROUND 9% MINIMUM

plot_dates <- 
  covid_tracking_data %>% 
  group_by(date) %>% 
  summarise(total_tests = sum(positive+negative, na.rm = T)
            , positive = sum(positive, na.rm = T)
  ) %>% 
  ungroup() %>% 
  summarise(min_date = min(date)
            , max_date = max(date))

daily_tests <- 
  covid_tracking_data %>% 
  group_by(date) %>% 
  summarise(total_tests = sum(positive+negative, na.rm = T)
            , positive = sum(positive, na.rm = T)
  ) %>% 
  mutate(daily_tests = c(NA, diff(total_tests))
         , daily_positives = c(NA, diff(positive))) %>% 
  mutate(positive_rate = daily_positives/daily_tests) %>% 
  mutate(test_wed_need = daily_positives/korea_positive_rate) %>% 
  select(date, "Current Daily Tests US" = daily_tests
         , test_wed_need) %>% 
  rename(!! paste0("Tests Needed to get to ",scales::percent(korea_positive_rate)):=test_wed_need) %>% 
  gather(Var, Value, -date) %>% 
  group_by(Var) %>% 
  mutate(label = ifelse(date == max(date), Var, NA)) %>% 
  ggplot()+
  aes(x = date, y = Value, group = Var, color = Var, label = label)+
  geom_line(size = 2)+
  ggrepel::geom_label_repel(nudge_y = -100000)+
  theme_tq()+
  theme(legend.position = "none", plot.title.position = "plot")+
  scale_color_tq()+
  scale_y_continuous(labels = scales::comma)+
  labs(y = NULL
       , x = NULL
       , color = NULL
       , title = "US needs to increase testing capacity to reach S. Korean levels"
       , subtitle = "Current Test Results Per Day in US and Gap to Reach Containment Levels"
       )+
  scale_x_date(limits = c(plot_dates$min_date, plot_dates$max_date))

positive_rate_av <-
  covid_tracking_data %>% 
  group_by(date) %>% 
  summarise(total_tests = sum(positive+negative, na.rm = T)
            , positive = sum(positive, na.rm = T)
  ) %>% 
  mutate(daily_tests = c(NA, diff(total_tests))
         , daily_positives = c(NA, diff(positive))) %>% 
  mutate(positive_rate = daily_positives/daily_tests) %>% 
  filter(is.finite(positive_rate), !is.na(positive_rate)) %>%
  filter(date>Sys.Date()-6) %>% 
  ungroup() %>% 
  summarise(positive_rate = mean(positive_rate, na.rm = T))


max_date_plot <- 
  covid_tracking_data %>% 
  summarise(dt = max(date)) %>% 
  pull(dt)

positive_rate <- 
  covid_tracking_data %>% 
  group_by(date) %>% 
  summarise(total_tests = sum(positive+negative, na.rm = T)
            , positive = sum(positive, na.rm = T)
  ) %>% 
  mutate(daily_tests = c(NA, diff(total_tests))
         , daily_positives = c(NA, diff(positive))) %>% 
  mutate(positive_rate = daily_positives/daily_tests) %>% 
  filter(is.finite(positive_rate), !is.na(positive_rate)) %>% 
  filter(positive_rate<1) %>% 
  
  mutate(korean_label = ifelse(date==max(date),"5% threshold", NA)) %>% 
  mutate(current_label = ifelse(date==max(date),paste0("US Last 7 Day Average: ",scales::percent(round(positive_rate_av$positive_rate,2))), NA)) %>% 
  
  ggplot()+
  aes(x = date, y = positive_rate)+
  geom_line(colour = "#1F78B4", size = 2)+
  

  geom_hline(yintercept = positive_rate_av$positive_rate, color = "#1F78B4", linetype = 2, size = 1)+
  ggrepel::geom_label_repel(aes(label = current_label)
                            , y = positive_rate_av$positive_rate
                            , nudge_y = 0.5
                            , color = "#1F78B4")+
  
  geom_hline(yintercept = korea_positive_rate, color = "#E31A1C", linetype = 2, size = 1)+
  ggrepel::geom_label_repel(aes(label = korean_label)
                            , y = korea_positive_rate
                            , nudge_y = -1
                            , color = "#E31A1C")+
  
  theme_tq()+
  theme(legend.position = "top", plot.title.position = "plot")+
  scale_y_continuous(labels = scales::percent, limits = c(0,NA))+
  labs(y = NULL
       , x = NULL
       , subtitletitle = "US Positive Test Rate"
       )+
  scale_x_date(limits = c(plot_dates$min_date, plot_dates$max_date))

# PATCHWORK PLOTS
testing_plot_us_v_korea <- s_korea_rate_plot + (daily_tests /positive_rate)
testing_plot_us_v_korea

daily_us_tests <- (daily_tests /positive_rate )
daily_us_tests

jpeg(paste0('img/us-tests-vs-korea-',Sys.Date(),'.jpeg')
     , width = 480*2.5
     , height = 480*2.5
     , res = 100
)

testing_plot_us_v_korea

dev.off()


# hospitalizations vs. confirmed cases ----

diff_metric <- function(x) c(NA, diff(x))

covid_tracking_data %>% 
  group_by(date) %>% 
  summarise(hospitalizedCumulative = sum(hospitalizedCumulative, na.rm = T)
            , inIcuCumulative = sum(inIcuCumulative, na.rm = T)
            , death  = sum(death , na.rm = T)) %>% 
  gather(Var, Value, -date) %>% 
  filter(date>ymd("2020 03 15")) %>% 
  ggplot()+
  aes(x = date, y = Value)+
  geom_line()+
  facet_wrap(~Var, scales = "free_y", ncol = 1)




admissions_icus_deaths <- 
  covid_tracking_data %>% 
  arrange(state, date) %>% 
  mutate(state = case_when(state == "NY" ~ "NY", TRUE ~ "ALL OTHER STATES")) %>% 
  mutate(state = factor(state, levels = c("ALL OTHER STATES","NY"))) %>% 
  group_by(state, date) %>%  
  summarise_at(vars(hospitalizedCumulative, inIcuCurrently, death), sum, na.rm = T) %>% 
  mutate_at(vars("New Hospitalizations" = hospitalizedCumulative
                 , "Change In ICU Patients" = inIcuCurrently
                 , "New Deaths" = death), diff_metric) %>% 
  select(state, date, `New Hospitalizations`, `Change In ICU Patients`, `New Deaths`) %>% 
  gather(Var, Value, -date, -state) %>% 
  mutate(Var = factor(Var, c("New Hospitalizations","Change In ICU Patients","New Deaths"))) %>% 
  filter(date>ymd("2020 03 15")) %>% 
  ggplot()+
  aes(x = date, y = Value, fill = state)+
  geom_col()+
  theme(legend.position = "right")+
  facet_wrap(~Var
             #, scales = "free_y"
             , ncol = 1)+
  labs(y = NULL, x = NULL)+
  scale_fill_tq()+
  theme_tq()+
  labs(title = "Hospitalizations, ICU Admissions and Deaths"
       , subtitle = Sys.Date())



jpeg(paste0('img/admissions-icu-deaths',Sys.Date(),'.jpeg')
     , width = 480*2
     , height = 480*2
     , res = 200
)
admissions_icus_deaths
dev.off()


# Positive Tests ----
covid_tracking_data %>%
  arrange(state, date) %>% 
  mutate(positive = c(NA, diff(positive))) %>% 
  filter(date>ymd("2020 03 20")) %>% 
  group_by(date) %>% 
  summarise(positive = sum(positive, na.rm = T)) %>% 
  ggplot()+
  aes(x = date, y = positive)+
  geom_line()


