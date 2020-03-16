
NYC_reports <- 
  dplyr::mutate(
  tibble::tribble(~days_since_reported, ~Confirmed
                  , 1, 0
                  , 4, 43 # Wednesday 3/11
                  , 5, 100 # Thursday 3/12
                  , 6, 170 # Friday 3/13
                  , 7, 213 # Saturday 3/14
                  , 8, 329 # Sunday 3/15 as of 10:00 pm
                  
  ) 
  , area = "NYC", Country = "US")
