
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
                  
                  
  ) 
  , area = "NYC", Country = "US")
