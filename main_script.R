library(tidyverse)
library(data.table)

url_listings = "https://raw.githubusercontent.com/ophirbetser/EinBDW_github/master/datasets/listings_clean.csv"
url_calendar = "https://raw.githubusercontent.com/ophirbetser/EinBDW_github/master/datasets/calendar_clean.csv"
listings_raw = fread(url)
