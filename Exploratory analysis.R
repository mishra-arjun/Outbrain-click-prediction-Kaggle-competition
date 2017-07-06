################################## CSE6242 Project Exploratory Analysis ====================
setwd("G:/Georgia Tech/CS 6242/Assignments/Project/Data")
#Loading packages

#install.packages("data.table")
#install.packages("logspline")
#install.packages("fitdistrplus")
#install.packages("vcd")
#install.packages("dplyr")
#install.packages("lubridate")

require(ggplot2)
require(lattice)
require(data.table)
require(fitdistrplus)
require(logspline)
require(dplyr)
require(vcd)
require(ggthemes)
require(lubridate)

#We will go one by one with the datasets

################################ Clicks_train =======================================
#Reading in the dataset
clicks_train = fread("clicks_train.csv")
head(clicks_train)

summary(clicks_train)

#Converting all three to categorical variables
clicks_train$clicked = as.factor(clicks_train$clicked)
clicks_train$display_id = as.factor(clicks_train$display_id)
clicks_train$ad_id  = as.factor(clicks_train$ad_id)
clicks_train$helper = 1

summary(clicks_train)

#Looking at the max number of occurences of the display ID
clicks_train %>%
  group_by(display_id) %>%
  summarize(cnt = sum(helper)) %>%
  arrange(desc(cnt))
#The maximum times a display ID appears is 12.

#Occurences of Ad ID
AD_occur = clicks_train %>%
              group_by(ad_id) %>%
              summarize(cnt = sum(helper)) %>%
              arrange(desc(cnt))

#We want the analysis by the the clicked proportion and we want the top 50 ads overall
#Taking the top 50 appearing ads from Ad_occur
AD_100 = AD_occur[1:50,]

#Now grouping by the clicked variable as well
AD_occur_click = clicks_train[clicks_train$ad_id %in% AD_100$ad_id,] %>%
  group_by(ad_id, clicked) %>%
  summarize(cnt = sum(helper)) %>%
  arrange(desc(cnt), desc(ad_id))


#Looking for the distribution of top 100 ad occurance
ggplot(AD_occur_click, aes(x = reorder(ad_id, cnt), y = cnt, fill = clicked)) + 
  geom_bar(stat = "identity")  + coord_flip() + 
  labs(x = "Ad_Id", y = "Count", color = "Clicked", title = "Clicks proportion for top 50 ads") + 
  theme_bw() + 
  theme(axis.text=element_text(size=8))

#There isn't much else to visualize in clicks_train

#################################### Page_Views =====================================
page_views = fread("page_views_sample.csv")
head(page_views)

summary(page_views)
str(page_views)

page_views$document_id = as.factor(page_views$document_id)
#Converting the milliseconds passed to days passed
page_views$timestamp = (page_views$timestamp + 1465876799998)/(1000*60*60*24)

#Making a subset table only to see the events in one day, 2016-06-15  = 16967
page_views = page_views[page_views$timestamp < 16967,] 

#Making R format dates
page_views$timestamp = as.Date(page_views$timestamp, origin = "1970-01-01")

#Getting the exact datetimes
page_views$timestamp <- .POSIXct(unclass(page_views$timestamp)*86400, tz="GMT")

#Extract the hour
page_views$hour = hour(page_views$timestamp)

page_views$platform = as.factor(page_views$platform)

page_views$helper = 1



#Grouping the views based on the hour, for each platform as separate
hour_add = page_views %>%
            group_by(hour, platform) %>%
            summarise(cnt = sum(helper)) %>%
            arrange(desc(cnt))

#Making plot to see the users activity according to the time of the day on platform
ggplot(hour_add, aes(x = hour, y = cnt, color = platform)) + geom_point() + 
  geom_line() + 
  scale_color_discrete(labels = c("Desktop", "Mobile", "Tablet")) + 
  labs(x = "Hour of the day", y = "Users active", color = "Platform", 
       title = "Users activity for 2016-06-15") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))



#Grouping based on the hour, for each traffic source
hour_add_traffic = page_views %>%
              group_by(hour, traffic_source) %>%
              summarise(cnt = sum(helper)) %>%
              arrange(desc(cnt))

#Making plot to see the users activity according to the time of the day by traffic source
ggplot(hour_add_traffic, aes(x = hour, y = cnt, color = traffic_source)) + geom_point() + 
  geom_line() + 
  scale_color_few(labels = c("Internal", "Search", "Social")) + 
  labs(x = "Hour of the day", y = "Users active", color = "Traffic Source", 
       title = "Users activity for 2016-06-15") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))



#Extracting the Country
page_views$Country = substr(page_views$geo_location, 1, 2)

#Grouping based on the hour, for each platform, country
hour_add_country_plat = page_views %>%
  group_by(Country, platform) %>%
  summarise(cnt = sum(helper)) %>%
  arrange(desc(cnt))

top50countries = unique(hour_add_country_plat$Country)[1:50]

#Looking for the distribution of top 50 countries occurance
ggplot(hour_add_country_plat[hour_add_country_plat$Country %in% top50countries,], 
       aes(x = reorder(Country, cnt), y = log(cnt), fill = platform)) + 
  geom_bar(stat = "identity")  + coord_flip() + 
  scale_fill_discrete(labels = c("Desktop", "Mobile", "Tablet")) +
  labs(x = "Country", y = "Log of Number of Users", color = "Platform", title = "Active Users by Platform in top 50 Country on 2016-06-15") + 
  theme_bw() + 
  theme(axis.text=element_text(size=8)) + 
  theme(plot.title = element_text(hjust = 0.5))

#As the normal value of US is extremely HIGH, we use the LOG scale.


################################# Documents Meta =======================================

doc_meta = fread("documents_meta.csv")
head(doc_meta)
str(doc_meta)

#Adding the count helper
doc_meta$helper = 1

#Converting to factors
doc_meta$document_id = factor(doc_meta$document_id)
doc_meta$publisher_id = factor(doc_meta$publisher_id)


length(unique(doc_meta$publisher_id))

#Getting  a distribution of the top 50 publishers
count_pub = doc_meta %>%
              group_by(publisher_id) %>%
              summarise(cnt = sum(helper)) %>%
              arrange(desc(cnt))


#Get a distribution of the top 50 publishers 
ggplot(count_pub[1:50,], 
       aes(x = reorder(publisher_id, cnt), y = cnt)) + 
        geom_bar(stat = "identity", fill = "lightblue", color = "white")  + coord_flip() + 
        labs(x = "Publisher", y = "Number of Documents Published", 
             title = "Distribution of documents published by publishers") + 
        theme(axis.text=element_text(size=8)) + 
        theme(plot.title = element_text(hjust = 0.5))


#Changing the date variable

#Making Date column
doc_meta$date = as.POSIXct(strptime(doc_meta$publish_time, format = "%Y-%m-%d %H:%M:%S"), 
                           tz = "GMT")

#Making Hour column
doc_meta$hour <- hour(doc_meta$date)

unique(doc_meta$hour)

sum(is.na(doc_meta$hour))

#The publishing Hour does not matter.

#Taking a look at the publishing year
doc_meta$year = year(doc_meta$date)

sum(is.na(doc_meta$year))
#A lot of NAs in the Publish_time variable

count_publish_year = doc_meta %>%
  group_by(year) %>%
  summarise(cnt = sum(helper)) %>%
  arrange(desc(cnt))

ggplot(count_publish_year[count_publish_year$year < 2017 & count_publish_year$year > 1990 
                          &!is.na(count_publish_year$year),],
       aes(x = year, y = cnt)) + geom_point() + 
  geom_line(color = "blue") + 
  labs(x = "Year", y = "Articles Published", 
       title = "Articles Published Over the Years") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

doc_cat = fread("documents_categories.csv")
length(unique(doc_cat$category_id))

doc_cat$helper = 1

#Looking at the different number of categories the documents belong to
cat_add = doc_cat %>%
            group_by(category_id) %>%
            summarise(cnt = sum(helper)) %>%
            arrange(desc(cnt))

ggplot(cat_add[1:50,], 
       aes(x = reorder(category_id, cnt), y = cnt)) + 
  geom_bar(stat = "identity", fill = "lightblue", color = "white")  + coord_flip() + 
  labs(x = "Category", y = "Number of Articles of a Category", 
       title = "Distribution of Articles belonging to a category") + 
  theme(axis.text=element_text(size=8)) + 
  theme(plot.title = element_text(hjust = 0.5))
#Looks like a half normal

#None of the other Document datasets have anything worth visualizing

####################################### Promoted Content =================================

prom_cont = fread("promoted_content.csv")
head(prom_cont)

#We can visualize the number of ads per advertiser and the number of ads in a campaign
length(unique(prom_cont$campaign_id))
#Too many campaigns. No point in visualizing.

length(unique(prom_cont$advertiser_id))
#Too many advertisers as well but we will visualize the top 50

prom_cont$helper = 1

ad_count = prom_cont %>%
  group_by(advertiser_id) %>%
  summarise(cnt = sum(helper)) %>%
  arrange(desc(cnt))

ggplot(ad_count[1:50,], 
       aes(x = reorder(advertiser_id, cnt), y = cnt)) + 
  geom_bar(stat = "identity", fill = "lightblue", color = "white")  + coord_flip() + 
  labs(x = "Advertiser", y = "Number of ads", 
       title = "Distribution of ads run by an Advertiser") + 
  theme(axis.text=element_text(size=8)) + 
  theme(plot.title = element_text(hjust = 0.5))

#That is all there is to visualize in this dataset


#################################### Events ============================================
events = fread("Events.csv")
head(events)

events$helper = 1

#We can see the click events distribution by platform and by time and by country

#By Platform
plat_add = events %>%
  group_by(platform) %>%
  summarise(cnt = sum(helper)) %>%
  arrange(desc(cnt))

plat_add$platform[plat_add$platform == 1] = "Desktop"
plat_add$platform[plat_add$platform == 2] = "Mobile"
plat_add$platform[plat_add$platform == 3] = "Tablet"

ggplot(plat_add[plat_add$platform %in% c("Desktop","Mobile","Tablet"), ], 
       aes(x = reorder(platform, cnt), y = cnt)) + 
  geom_bar(stat = "identity", fill = "lightblue", color = "white")  + 
  labs(x = "Platform", y = "Number of events", 
       title = "Distribution of ads clicked by Platform") + 
  theme_bw() + 
  theme(axis.text=element_text(size=8)) + 
  theme(plot.title = element_text(hjust = 0.5))

#By Country and Platform
events$country = substr(events$geo_location, 1, 2)

country_add = events %>%
  group_by(country, platform) %>%
  summarise(cnt = sum(helper)) %>%
  arrange(desc(cnt))

top_50_country = unique(country_add$country)[1:50]

ggplot(country_add[country_add$platform %in% c(1, 2, 3) & 
                     country_add$country %in% top_50_country, ], 
       aes(x = reorder(country, cnt), y = log(cnt), fill = platform)) + 
  geom_bar(stat = "identity")  + coord_flip() +
  #scale_fill_discrete(labels = c("Desktop", "Mobile", "Tablet")) +
  labs(x = "Country", y = "Log of number of events", 
       title = "Distribution of ads clicked by Platform in Top 50 Countries (by number of clicks)") + 
  theme_bw() + 
  theme(axis.text=element_text(size=8)) + 
  theme(plot.title = element_text(hjust = 0.5))


#By Time

#Converting the milliseconds passed to seconds passed
events$timestamp = (events$timestamp + 1465876799998)/(1000)

#Making R format dates
events$timestamp = as.Date(events$timestamp, origin = "1970-01-01")

#Getting the exact datetimes
events$timestamp <- .POSIXct(unclass(events$timestamp), tz="GMT")

#Extract the hour
events$hour = hour(events$timestamp)

events$day = day(events$timestamp)

events$weekday = wday(events$timestamp)

#Getting the Events graph by day
day_count = events %>%
  group_by(day, platform) %>%
  summarise(cnt = sum(helper)) %>%
  arrange(desc(cnt))


ggplot(day_count[day_count$platform !="\\N",],
       aes(x = day, y = cnt, color = platform)) + geom_point() + 
  geom_line() + 
  scale_color_discrete(labels = c("Desktop", "Mobile", "Tablet")) + 
  labs(x = "Day of Month", y = "Number of clicks", 
       title = "Number of total events in the dataset given by day of month") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#Getting Events by day of week
weekday_count = events %>%
  group_by(weekday, platform) %>%
  summarise(cnt = sum(helper)) %>%
  arrange(desc(cnt))

ggplot(weekday_count[weekday_count$platform !="\\N",],
       aes(x = weekday, y = cnt, color = platform)) + geom_point() + 
  geom_line() + 
  scale_color_discrete(labels = c("Desktop", "Mobile", "Tablet")) + 
  labs(x = "Weekday", y = "Number of clicks", 
       title = "Number of total events in the dataset given by weekday") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

