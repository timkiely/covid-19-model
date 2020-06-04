


china_names <- 
  cases_data %>% 
  select(anhui:zhejiang) %>% 
  names()

us_names <- 
  cases_data %>% 
  select(us) %>% 
  names()

us_state_names <- 
  cases_data %>% 
  select(one_of(str_to_lower(state.name))) %>% 
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
spain_names <- cases_data %>% select(spain) %>% names()


# The first sequel represents the number of confirmed cases, 
# the second sequel represents suspected cases, the third sequel 
# represents cured cases, the fourth sequel represents death cases.
processed <-
  cases_data %>% 
  gather(area, cases, -datetime) %>% 
  separate(cases, into = c("Confirmed","Suspected","Cured","Deaths")
           , sep = "-", extra = "warn", remove = F) %>% 
  mutate_at(vars(Confirmed:Deaths), function(x)ifelse(is.na(x),0,x)) %>% 
  mutate_at(vars(Confirmed:Deaths), as.numeric) %>% 
  mutate(Active = Confirmed-Cured-Deaths) %>% 
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
    , area %in% us_state_names ~ "US"
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
    , area %in% spain_names ~ "spain"
    
    , TRUE ~ "Other"
  ))

