library(tidyverse)
library(data.table)
library(visdat)
library(naniar)
library(lubridate)
library(scales)
library(patchwork)

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
 
 
 tab <- table(as.Date(cut(listings_clean$host_since, 'month')))
 
 first_metric <- data.frame(Date=as.Date(names(tab)),
                            Frequency=as.vector(tab))
 first_metric$Date <- as.Date(first_metric$Date, format="%m-%Y")
 
 first_metric$MonthN <- as.numeric(format(as.Date(first_metric$Date),"%m")) # Month's number
 
 first_metric$year <- as.numeric(format(as.Date(first_metric$Date),"%y"))
 for(row in 2:nrow(first_metric)){
   first_metric[row, 2] <- first_metric[row, 2] + first_metric[row-1, 2]
 }
 
 length(factor(first_metric$MonthN))
 g <- ggplot(data = first_metric, aes(x = MonthN, y = Frequency, group = year, colour=factor(year))) + 
   ylab("Hosts") + labs(colour = "Year") +
   geom_line() + geom_point() +
   scale_color_brewer(palette = "Set1") + 
   scale_x_continuous(name="Month", breaks=1:12)
 g
 
 #by_neighbourhood <- group_by(listings_clean, neighbourhood)
 second_metric <- group_by(listings_clean, neighbourhood)
 second_metric$Month <- format(as.Date(second_metric$host_since), "%m")
 second_metric$Year <- format(as.Date(second_metric$host_since), "%Y")
 second_metric$neighbourhood_Year <- paste(second_metric$neighbourhood, second_metric$Year, sep="#")
 result <- data.frame(table(second_metric$neighbourhood_Year))
 result$Var1 <- as.character(result$Var1)
 result$Year <- sapply(strsplit(result$Var1, "#"), "[[", 2)
 result$neighbourhood <- sapply(strsplit(result$Var1, "#"), "[[", 1)
 for(row in 2:nrow(result)){
   if(result[row,4] == result[row-1,4]){
     result[row, 2] <- result[row, 2] + result[row-1, 2]
   }
 }
 g2 <- ggplot(data = result, aes(x = Year, y = Freq, group = neighbourhood, colour=factor(neighbourhood))) + 
   ylab("Hosts") + 
   geom_line() + geom_point()
 g2
 
 third_metric <- calendar_clean
 third_metric$Month_Yr <- format(as.Date(third_metric$date), "%Y-%m")
 third_metric$price_dollars <- parse_number(third_metric$price_dollars)
 res <- aggregate(third_metric$price_dollars, list(third_metric$Month_Yr), FUN=median, na.rm = TRUE)
 g3 <- ggplot(data = res, aes(x = Group.1, y = x, group=1)) + 
   xlab("Month") + ylab("Median Price") + geom_line()
 g3

# part 3

full_data <-  calendar_clean %>% 
   rename(id = listing_id ) %>% 
   left_join(listings, by = "id")
 

glimpse(full_data)

full_data %>% 
  group_by(date) %>% 
  summarise(prop_full = mean(available_category)) %>% 
  ggplot(aes(x = date, y = prop_full))+
  geom_line()+
  labs(x = "Date", y = "p", title = "proportion of booked places",
       captions = "1 = fully booked, 0 = fully available")



p3_half_yr = full_data %>% 
  group_by(date) %>% 
  summarise(prop_full = mean(available_category)) %>% 
  filter(date > "2017-03-10" ) %>% 
  ggplot(aes(x = date, y = prop_full))+
  geom_line()+
  labs(x = "Date", y = "p", title = "proportion of booked places",
       )
  

p3_2weeks = full_data %>% 
  group_by(date) %>% 
  summarise(prop_full = mean(available_category)) %>% 
  filter(date > "2017-04-10" & date <"2017-04-22") %>% 
  ggplot(aes(x = date, y = prop_full))+
  geom_line()+
  labs(x = "Date", y = "p", title = "proportion of booked places",
       captions = "1 = fully booked, 0 = fully available")


p3_half_yr + p3_2weeks

