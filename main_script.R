library(tidyverse)
library(data.table)
library(visdat)
library(naniar)
library(lubridate)

url_listings = "https://raw.githubusercontent.com/ophirbetser/EinBDW_github/master/datasets/listings_clean.csv"
url_calendar = "https://raw.githubusercontent.com/ophirbetser/EinBDW_github/master/datasets/calendar_clean.csv"

listings_clean= fread(url_listings)
calendar_clean = fread(url_calendar)

vis_dat(listings_clean)
vis_miss(listings_clean)

# 
# listings_clean %>%  
#   count(host_is_superhost)
# 
# 
# listings_clean %>% 
#   count(neighbourhood)

# categorizing charecter vars
response_lvls = 
  listings_clean %>%  
  filter(host_response_time != "N/A") %>% 
  distinct(host_response_time) %>% 
  # pull will return a vec of of charecters
  pull()


# cleaning the data
listings <- 
  tibble(listings_clean) %>% 
  mutate( # updating vars
          host_response_time = parse_factor(host_response_time, levels = response_lvls, ordered =  T),
          host_is_superhost = recode(host_is_superhost, t = TRUE, f = FALSE),
          property_type = na_if(property_type, ""),
          property_type = factor(property_type),
          neighbourhood = na_if(neighbourhood, ""),
          neighbourhood = factor(neighbourhood),
          #adding new var month of host subscribing
          month_listed = month(host_since),
          host_since = ymd(host_since),
          myr_listed = format_ISO8601(host_since , precision = "ym"),
          myr_listed = ym(myr_listed)
  ) %>% 
  rename(superhost = host_is_superhost)

# I want to check if there is a seasonality for people joining airbnb
# need to under stand how to make the bars faster, and explain that each bar is a uear
ggplot(listings, mapping = aes(x = myr_listed))+
  geom_bar() +
  facet_grid(cols = vars(month_listed))
 


