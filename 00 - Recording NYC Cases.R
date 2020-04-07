


# COLLECTIONG NYC COVID DATA ----
# Tim Kiely, March 2020



# Daily stat source: 
# NOTE: as of 4/6 this page no longer displays data
# https://www1.nyc.gov/assets/doh/downloads/pdf/imm/covid-19-daily-data-summary.pdf
# browseURL("https://www1.nyc.gov/assets/doh/downloads/pdf/imm/covid-19-daily-data-summary.pdf")


# NEW DATA 4/5:
# https://github.com/nychealth/coronavirus-data
# browseURL("https://github.com/nychealth/coronavirus-data")

suppressPackageStartupMessages({
  library(tabulizer)
  library(tidyverse)
  library(pdftools)
})

CAGR_formula <- function(FV, PV, n = 1) {
  values <- ((FV/PV)^(1/n)-1)
  return(values)
}


# 1.0 MANUAL RECORDING ----
NYC_reports <- 
  dplyr::mutate(
    tibble::tribble(~days_since_reported, ~Confirmed, ~Deaths
                    , 1, 0, NA
                    , 4, 43, NA # Wednesday 3/11
                    , 5, 100, NA # Thursday 3/12
                    , 6, 170, NA # Friday 3/13
                    , 7, 213, NA # Saturday 3/14
                    , 8, 329, NA # Sunday 3/15 
                    , 9, 463, NA # Monday 3/16 
                    , 10, 814, NA # Tuesday 3/17
                    , 11, 1339, 10 # Wednesday 3/18 at 3:00 pm
                    , 12, 3615, 22 # Thursday 3/19 at 4:00 pm
                    , 13, 5151, 29 # Friday 3/20
                    , 17, 12339, 99 # 3/24
                    , 18, 14776, 131 # 3/25
                    , 19, 15597, 192 # 3/26
                    , 20, 21873, 281 # 3/27
                    , 22, 33474, 776 # 3/29
                    , 24, 40900, 932 # 3/31
                    , 25, 56624, 1139 # 4/1
                    
    ) 
    , area = "NYC", Country = "US")

identity <- function(x) x
cumulative <- function(x) cumsum(as.numeric(x))

nyc_daily_data <- 
  read_csv("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/case-hosp-death.csv") %>% 
  mutate_at(vars(NEW_COVID_CASE_COUNT:DEATH_COUNT), lst(identity, cumulative)) %>% 
  select(-contains("identity"))

NYC_reports <- 
  nyc_daily_data %>% 
  mutate(`Mortality Rate` = as.numeric(DEATH_COUNT_cumulative)/as.numeric(NEW_COVID_CASE_COUNT_cumulative)) %>% 
  mutate(`Death Percent Increase` = DEATH_COUNT_cumulative/lag(DEATH_COUNT_cumulative,1)-1) %>% 
  mutate(`Case Percent Increase` = NEW_COVID_CASE_COUNT_cumulative/lag(NEW_COVID_CASE_COUNT_cumulative,1)-1)

# 3.0 SCRAPE NYC DAILY SHEET ----
daily_stat_sheet <- "https://www1.nyc.gov/assets/doh/downloads/pdf/imm/covid-19-daily-data-summary.pdf"
total_nyc_cases <- "https://www1.nyc.gov/assets/doh/downloads/pdf/imm/covid-19-daily-data-summary-04052020-1.pdf"
total_nyc_deaths <- "https://www1.nyc.gov/assets/doh/downloads/pdf/imm/covid-19-daily-data-summary-deaths-04052020-1.pdf"
total_nyc_hospitalizations <- "https://www1.nyc.gov/assets/doh/downloads/pdf/imm/covid-19-daily-data-summary-hospitalizations-04042020-1.pdf"
#  browseURL(daily_stat_sheet)


# Extract the table
# out <- extract_tables(daily_stat_sheet)

# parse table
# parsed_table <- 
#   out[[1]] %>% 
#   as_tibble() %>% 
#   select(-V2) %>% 
#   slice(-1) %>% 
#   setNames(c("Var","Value")) %>% 
#   mutate(Value = readr::parse_character(Value)) %>% 
#   separate(Value, into = c("value","percent"), sep = " ") %>% 
#   mutate(percent = readr::parse_number(percent)) %>% 
#   print(n=Inf)


# extracted_data <- 
#   parsed_table %>% 
#   filter(!is.na(value), Var!="-  Unknown") %>% 
#   select(-percent) %>%
#   mutate(Var = readr::parse_character(str_remove_all(Var, "-"))) %>% 
#   spread(Var, value) %>% 
#   mutate(Date = report_date) %>%
#   select(
#     `Date`
#     , `Total`
#     , Deaths
#     , `Median Age (Range)`
#     , `0 to 17`
#     , `18 to 44`
#     , `45 to 64`
#     , `65 to 74`
#     , `75 and over`
#     , `Bronx`
#     , `Brooklyn`
#     , `Manhattan`
#     , `Male`
#     , `Female`
#     , `Queens`
#     , `Staten Island`
#   ) %>% 
#   mutate_all(as.character)

# write_file_path <- paste0('data/nyc-daily-stat-sheets/nyc-daily-covid-stats-extracted-', format(Sys.Date(),"%Y-%m-%d"),".csv")



# CAGR_formula <- function(FV, PV, n = 1) {
#   values <- ((FV/PV)^(1/n)-1)
#   return(values)
# }
# 
# final_data <- 
#   bind_rows(latest_file, extracted_data) %>%
#   arrange(Date) %>% 
#   distinct(Date, .keep_all = T) %>% 
#   mutate(Date = as.Date(Date)) %>% 
#   mutate(days_elapsed = replace_na(as.numeric(Date - lag(Date, 1)),1)) %>% 
#   mutate(`Mortality Rate` = scales::percent(as.numeric(Deaths)/as.numeric(Total))) %>% 
#   mutate(`Death CAGR` = scales::percent(CAGR_formula(as.numeric(Deaths), lag(as.numeric(Deaths),1), n = days_elapsed))) %>% 
#   mutate(`Case CAGR` = scales::percent(CAGR_formula(as.numeric(Total), lag(as.numeric(Total),1), n = days_elapsed))) %>% 
#   select(Date, days_elapsed, Total, `Case CAGR`, Deaths, `Death CAGR`, `Mortality Rate`, everything())
#   
# 
# select(final_data, Date, days_elapsed, Total, `Case CAGR`, Deaths, `Death CAGR`, `Mortality Rate`)
# 
# if(!file.exists(write_file_path)){
#   message("Writing latest file to: ",write_file_path)
#   write_csv(final_data, write_file_path)
# }












