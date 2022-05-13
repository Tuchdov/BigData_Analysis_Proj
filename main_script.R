library(tidyverse)
library(data.table)
library(visdat)
library(naniar)

url_listings = "https://raw.githubusercontent.com/ophirbetser/EinBDW_github/master/datasets/listings_clean.csv"
url_calendar = "https://raw.githubusercontent.com/ophirbetser/EinBDW_github/master/datasets/calendar_clean.csv"

listings_clean= fread(url_listings)
calendar_clean = fread(url_calendar)

vis_dat(listings_clean)
vis_miss(listings_clean)



# Dealing with nans in the futere' no reviews counts as none

listings_clean %>% 
  select(host_response_time , host_is_superhost) %>% 
  count(host_response_time, host_is_superhost)
