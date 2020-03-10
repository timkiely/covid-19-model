
# https://rviews.rstudio.com/2020/03/05/covid-19-epidemiology-with-r/



library(tidyverse)


# https://github.com/CSSEGISandData/COVID-19

# Incidence data collated by John Hopkins University
jhu_url <- paste("https://raw.githubusercontent.com/CSSEGISandData/", 
                 "COVID-19/master/csse_covid_19_data/", "csse_covid_19_time_series/", 
                 "time_series_19-covid-Confirmed.csv", sep = "")

us_confirmed_long_jhu <- read_csv(jhu_url) %>% rename(province = "Province/State", 
                                                      country_region = "Country/Region") %>% 
  pivot_longer(-c(province, 
                  country_region, Lat, Long), names_to = "Date", values_to = "cumulative_cases") %>% 
  # adjust JHU dates back one day to reflect US time, more or
  # less
  mutate(Date = mdy(Date) - days(1)) %>% filter(country_region == 
                                                  "US") %>% arrange(province, Date) %>% group_by(province) %>% 
  mutate(incident_cases = c(0, diff(cumulative_cases))) %>% 
  ungroup() %>% select(-c(country_region, Lat, Long, cumulative_cases)) %>% 
  filter(str_detect(province, "Diamond Princess", negate = TRUE))



library(rvest)
wp_page_url <- "https://en.wikipedia.org/w/index.php?title=2020_coronavirus_outbreak_in_the_United_States&oldid=944107102"
# read the page using the rvest package.
outbreak_webpage <- read_html(wp_page_url)

# parse the web page and extract the data from the eighth
# table
outbreak_webpage %>% 
  html_nodes("table") %>% 
  html_table(fill = TRUE) %>% 
  enframe() %>% print(n=Inf)

us_cases <- 
  outbreak_webpage %>% 
  html_nodes("table") %>% 
  .[[39]] %>% 
  html_table(fill = TRUE, header = F) %>% 
  tibble() %>% 
  slice(-1) 

us_cases_names <- head(us_cases,1) %>% unlist() %>% as.character()

us_cases_body <- 
  us_cases %>% 
  slice(-1) %>% 
  .[[1]] 
  
names(us_cases_body) <- us_cases_names
us_cases_body <- as_tibble(us_cases_body)

# utility function to remove wikipedia references in square
# brackets
rm_refs <- function(x) stringr::str_split(x, "\\[", simplify = TRUE)[, 1]

# now remove references from CaseNo column, convert it to
# integer, convert the date column to date type and then lose
# all rows which then have NA in CaseNo or NA in the date
# column
us_cases_clean <- us_cases_body %>% 
  mutate(CaseNo = rm_refs(`Case no.`)) %>% 
  mutate(CaseNo = as.integer(CaseNo)
         , Date = as.Date(parse_date_time(`Date announced`, c("%B %d, %Y", "%d %B, %Y")))) %>% 
  filter(!is.na(CaseNo), !is.na(Date)) %>% # convert the various versions of unknown into NA in the
  # OriginTypeCDC column
  mutate(OriginTypeCDC = if_else(`CDC origin type` %in% c("Unknown", "Undisclosed"), NA_character_, `CDC origin type`)) %>% 
  mutate(State = str_remove_all(State, "[']|[s]|[mid-]"))

us_cases_clean %>% count(State) %>% print(n=Inf)
  


# Fitting a log-linear model to the epidemic curve ----
library(earlyR)
library(EpiEstim)
library(incidence)

us_incidence_fit <- incidence::fit(local_cases_obj, split = NULL)

# plot the incidence data and the model fit
plot(local_cases_obj) %>% add_incidence_fit(us_incidence_fit)