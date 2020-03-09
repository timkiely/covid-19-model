
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
  .[[38]] %>% 
  html_table(fill = TRUE, header = F) %>% 
  tibble() %>% 
  slice(-1) 

us_cases_body <- 
  us_cases %>% 
  slice(-1) %>% 
  .[[1]] %>% 
  tibble() %>% 
  select(-1)

names(us_cases_body) <- NULL
tibble(us_cases_body[[1]])

names(us_cases_body) <- as.character(unlist(us_cases[1,]))

# The automatically assigned column names are OK except that
# instead of County/city and State columns we have two
# columns called Location, due to the unfortunate use of
# colspans in the header row.  The tidyverse abhors
# duplicated column names, so we have to fix those, and make
# some of the other colnames a bit more tidyverse-friendly.
us_cases_colnames <- colnames(us_cases)
us_cases_colnames[min(which(us_cases_colnames == "Location"))] <- "CityCounty"
us_cases_colnames[min(which(us_cases_colnames == "Location"))] <- "State"
us_cases_colnames <- us_cases_colnames %>% str_replace("Location", 
                                                       "CityCounty") %>% str_replace("Location", "State") %>% str_replace("Case no.", 
                                                                                                                          "CaseNo") %>% str_replace("Date announced", "Date") %>% str_replace("CDC origin type", 
                                                                                                                                                                                              "OriginTypeCDC") %>% str_replace("Treatment facility", "TreatmentFacility")
colnames(us_cases) <- us_cases_colnames

# utility function to remove wikipedia references in square
# brackets
rm_refs <- function(x) stringr::str_split(x, "\\[", simplify = TRUE)[, 1]

# now remove references from CaseNo column, convert it to
# integer, convert the date column to date type and then lose
# all rows which then have NA in CaseNo or NA in the date
# column
us_cases_clean <- us_cases %>% 
  mutate(CaseNo = rm_refs(CaseNo)) %>% 
  mutate(CaseNo = as.integer(CaseNo), Date = as.Date(parse_date_time(Date, c("%B %d, %Y", "%d %B, %Y")))) %>% 
  filter(!is.na(CaseNo), !is.na(Date)) %>% # convert the various versions of unknown into NA in the
  # OriginTypeCDC column
  mutate(OriginTypeCDC = if_else(OriginTypeCDC %in% c("Unknown", "Undisclosed"), NA_character_, OriginTypeCDC))





# Fitting a log-linear model to the epidemic curve ----
library(earlyR)
library(EpiEstim)
library(incidence)

us_incidence_fit <- incidence::fit(local_cases_obj, split = NULL)

# plot the incidence data and the model fit
plot(local_cases_obj) %>% add_incidence_fit(us_incidence_fit)