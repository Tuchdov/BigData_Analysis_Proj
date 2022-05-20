library(tidyverse)
library(data.table)
library(visdat)
library(naniar)
library(lubridate)
library(scales)

url_listings = "https://raw.githubusercontent.com/ophirbetser/EinBDW_github/master/datasets/listings_clean.csv"
url_calendar = "https://raw.githubusercontent.com/ophirbetser/EinBDW_github/master/datasets/calendar_clean.csv"

listings_clean= fread(url_listings)
calendar_clean = fread(url_calendar)

vis_dat(listings_clean)
vis_miss(listings_clean)



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
 

ggplot(listings, mapping= aes(x = superhost, fill = superhost))+
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent)+
  labs(title = "Distribution of superhosts and hosts")+
  # put title in the middle
  theme(plot.title = element_text(hjust = 0.5))+
  xlab(label = "Is superhost? ")+
  ylab(label = "")

text <- as.character(round(rpm_avg, 3))

ggplot(listings, mapping = aes(x = reviews_per_month))+
  geom_histogram()+
  stat_bin(binwidth = 1)+
  geom_vline(xintercept = rpm_avg, linetype="dotted")+
  annotate("text",x = rpm_avg,y = 20, label = text)+
  xlab(label = "Reviews per month")+
  ylab(label = "n")

# Where are the super hosts?
ggplot(listings, aes( x =superhost )) +
  geom_bar(aes(fill = factor(superhost)))+
  facet_wrap(vars(neighbourhood))
  


ggplot(listings, aes( y =price_dollars, x = review_scores_rating )) +
     geom_point(aes(colour = factor(superhost)))+
     facet_wrap(vars(neighbourhood))+
     ylim(c(0,1000))
vis_dat(listings)
# percentege of superhost

#what kind of properties are most visited?

mu <- plyr::ddply(listings, "property_type", summarise, grp.mean=mean(reviews_per_month, na.rm = T))
# keeping props larger then 10
props_good <- listings %>%
  group_by(property_type) %>% 
  select(property_type)%>% 
  count() %>% 
  filter(n > 5) %>% 
  pull(property_type)


mu_tot <- listings %>%
  select(property_type, reviews_per_month)%>% 
  summarise(mu_tot = mean(reviews_per_month, na.rm = T)) %>%
  pull()


# Plot
theme_set(theme_classic())
 ggplot(listings %>% 
          filter(property_type %in% props_good), aes(reviews_per_month))+
 geom_density(aes(fill=factor(property_type)), alpha=0.8)+
   facet_wrap(vars(property_type))+
   geom_vline(data=mu %>% 
                filter(property_type %in% props_good), aes(xintercept=grp.mean, color=property_type),
              linetype="dashed" ,show.legend = F , size = 1.5)+
   geom_vline(aes(xintercept = mu_tot), linetype = "dashed", show.legend = F)+
  labs(title="What kind of properties are most visited?", 
       subtitle="Monthly reviews grouped by property type",
       caption=c("black verical line = total avg monthly review", "color dashed line = avg month review by group"),
       x="Monthly Reviews",
       fill="Property Type")+
 theme(plot.caption = element_text(hjust=c(1, 0.8)))