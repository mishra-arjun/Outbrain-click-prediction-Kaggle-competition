############################### 6242 Project Datasets merge ===============================
setwd("G:/Georgia Tech/CS 6242/Assignments/Project/Data")

#############Loading packages
require(ggplot2)
require(data.table)
require(dplyr)
require(ggthemes)
require(lubridate)

################################ Documents datasets =====================================
doc_meta = fread("documents_meta.csv")
doc_cat = fread("documents_categories.csv")
doc_ent = fread("documents_entities.csv")
doc_top = fread("documents_topics.csv")

length(unique(doc_ent$entity_id))
#1326009 unique entities in the dataset
length(unique(doc_cat$category_id))
#97 unique categories in the dataset
length(unique(doc_top$topic_id))
#300 unique topics in the dataset


#These topics/categories/entities should be useful in determining user behavior.
#But we only have a confidence level from outbrain of whether a particular
#topic/cat/entity is present in the website.

#Very low confidence things should be removed without any doubt.
#Still, anything below a 50% chance of being present or not, might give us the wrong
#information.


#Looking at the numbers, category is the most exclusive info.
#Thus each document should have a category and also only
#We will retain the category with the maximum conf. level for each document id

doc_cat1 = doc_cat %>%
  group_by(document_id) %>%
  summarise(conf_level = sum(confidence_level))

summary(doc_cat1)
#There is just one document with conf_level == 2
hist(doc_cat1$conf_level)
which(doc_cat1$conf_level == 2)

#Looking at the dataset rearranged
head(arrange(doc_cat1, desc(conf_level)), 50)
#There are only 7 conf_level > 1.

#We can safely just take one category (the max one) as the document's category.
#That too when that max is atleast greater than 50%.
doc_cat1 = doc_cat %>%
  filter(confidence_level > 0.5) %>%
  group_by(document_id, category_id) %>%
  summarise(conf_level = max(confidence_level))



#Next we take a look at the topics
sum((doc_top$confidence_level > 0.5))
#Just 29601 topics with > 50% confidence
length(unique(doc_top$document_id))
#There are obviously multiple topics for each document

##################To keep the merged dataset small, we should ensure that there is only
#one topic per document id so that multiple rows are not formed for each doc id.

#Checking different conf levels
sum((doc_top$confidence_level > 0.30))
#218162 topics with confidence level > 0.3

#Only topics with 0.3 confidence level make sense to keep
doc_top1 = doc_top %>%
  filter(confidence_level > 0.3) %>%
  group_by(document_id, topic_id) %>%
  summarise(conf_level = max(confidence_level))

################Using the 0.3 filter will introduce nulls for the remaining documents
#when we merge but right now we can think of making different model of docs with nulls.



#Taking a look at entities dataset
sum((doc_ent$confidence_level > 0.5))
#1919094 entities with confidence level > 0.5 so we can just go ahead with this

doc_ent1 = doc_ent %>%
  filter(confidence_level > 0.5) %>%
  group_by(document_id, entity_id) %>%
  summarise(conf_level = max(confidence_level))



#Documents meta
#Publish time is a tricky variable.
#Based on the month it is and the year the article was published in, it can matter that
#a particular ad might get clicked.
#Older articles might mean that they have a mature readership and those particular ads
#might get read.
#Articles published in december might have more chance of getting gift ads clicked.

#Thus we will extract only the publish month and year.

#Making Date column
doc_meta$date = as.POSIXct(strptime(doc_meta$publish_time, format = "%Y-%m-%d %H:%M:%S"), 
                           tz = "GMT")

#Making year and month column
doc_meta$publish_year <- year(doc_meta$date)
doc_meta$publish_month <- month(doc_meta$date)

#Publisher id is definitely a useful variable and so is source id.
#A CNN audience is different from a magazine audience. (Publisher)
#An editorial reader is also different from a headlines reader (Source).
doc_meta1 = doc_meta[,c(1,2,3,6,7)]


#Removing the datasets not needed
rm(doc_top, doc_ent, doc_cat, doc_meta)
gc() #Garbage cleaning to free up space on memory



#Merging the datasets
#doc_meta has the maximum number of observations and is the main doc dataset.

doc_dataset = doc_meta1 %>% left_join(doc_cat1, by = "document_id")

doc_dataset = doc_dataset %>% left_join(doc_ent1, by = "document_id")

doc_dataset = doc_dataset %>% left_join(doc_top1, by = "document_id")

head(doc_dataset)

#The confidence levels themselves are not important predictors in the overall model.
#We just want to see which category/entity/topic is mentioned in the article.
doc_dataset$conf_level.x = NULL
doc_dataset$conf_level.y = NULL
doc_dataset$conf_level = NULL

#Checking how many NAs
sum(is.na(doc_dataset$category_id)) #1110009
sum(is.na(doc_dataset$entity_id)) #1754561
sum(is.na(doc_dataset$topic_id)) #3398093

#Getting datasets with - no NA's
doc_nona = doc_dataset %>% filter(!is.na(category_id) & !is.na(entity_id) & !is.na(topic_id))
#Only 129938 observations

#We will make these datasets when making the final predictions

#Removing the interim datasets
rm(doc_cat1, doc_ent1, doc_meta1, doc_top1, doc_nona)



################################## Promoted Content ==================================
prom_cont = fread("promoted_content.csv")

#There will be duplicates of the document ids as Ads are listed out
length(unique(prom_cont$document_id))

#This dataset can be joined as it is to the train.



############################### Events =======================================
eve = fread("events.csv")
#23 million rows

#These are all events that took place - i.e. the clicks.
#Checking how many document_ids are there

length(unique(eve$document_id)) #894060

# These are all the clicks that took place. There is no sense in joining these
# to the final train dataset - it does not make any sense.
# We can do 2 things:
# Use this along with the full page views dataset and see which display id gets clicked
# how often.
# Use the information given in the dataset to form clusters for the display ids.
# The idea is that some display_id's gets clicked at some time more than other, 
# from some region more than others and from some platform more than others.
# Those display ids will be clustered together. Even though we don't have the time, 
# platform, location in the final datasets, we can expect that a click for a
# particular display is supposed to go in a certain way.

#The cluster variable can finally be used in the clicks datasets to aid in prediction.

#Don't need the UID or Doc_ID for this solution.
eve$uuid = NULL
eve$document_id = NULL

#Manipulate the time and geo variables 

#Get the country
eve$country = substr(eve$geo_location, 1, 2)
#Not going to use the state and zip as there are too many of them and will also give a
#lot of NAs

#Looking at the country NAs
sum(is.na(eve$country)) #0

length(unique(eve$country))


#Getting the time variables
eve = eve %>%
  mutate(timestamp = (timestamp + 1465876799998)/(1000*60*60*24)) %>%
  mutate(timestamp = as.Date(timestamp, origin = "1970-01-01")) %>%
  mutate(timestamp = .POSIXct(unclass(timestamp)*86400, tz="GMT")) %>%
  mutate(hour = hour(timestamp)) %>%
  mutate(month = month(timestamp)) %>%
  mutate(day = day(timestamp)) %>%
  mutate(weekday = wday(timestamp)) %>%
  mutate(year = year(timestamp))


#Removing variables not needed
eve$timestamp = NULL
eve$geo_location = NULL

#Checking class of the variables
str(eve)


#Too many countries. Taking only top 20 and making the rest as others
countries = data.table(eve$country)
countries$help = 1

countries_add = countries %>%
  group_by(V1) %>%
  summarise(cnt = sum(help)) %>%
  arrange(desc(cnt))

top20_countries = countries_add$V1[21:231]

eve$country[eve$country %in% top20_countries] = "Other"
unique(eve$country)

#Removing the interim variables
rm(countries, countries_add, top20_countries)

#Making the variables for clustering
#The only manipulations we need is with the time variables

#Dividing the day into three time zones
eve$morning = as.integer(eve$hour > 7 & eve$hour < 16)
eve$evening = as.integer(eve$hour > 15 & eve$hour < 24)
eve$night = as.integer(eve$hour > -1 & eve$hour < 8)
  
#Dividing the week into three
eve$weekstart = as.integer(eve$weekday %in% c(2,3,4))
eve$laterweek = as.integer(eve$weekday %in% c(5,6))
eve$weekend = as.integer(eve$weekday %in% c(1,7))

#Dividing the month into three
eve$monthstart = as.integer(eve$month < 11)
eve$midmonth = as.integer(eve$month > 10 & eve$month < 21)
eve$monthend = as.integer(eve$month > 20)

#Dividing the year into quarters - don't know if this will make a difference
eve$firstquat = as.integer(eve$month %in% c(1,2,3))
eve$secquat = as.integer(eve$month %in% c(4,5,6))
eve$thirdquat = as.integer(eve$month %in% c(7,8,9))
eve$fourthquat = as.integer(eve$month %in% c(10,11,12))

#There is only one year
unique(eve$year)
head(eve)

#Removing variables not needed anymore
eve$hour = NULL
eve$year = NULL
eve$day = NULL
eve$weekday = NULL
eve$month = NULL

#Making a data matrix out of the remaining categorical variables
#bin_matrix = model.matrix(~ country + platform - 1, data = eve)
#head(bin_matrix)

#eve = cbind(eve, model.matrix(~ country + platform - 1, data = eve))

#################### Vector is too large for machine to handle. Removing countries
eve = cbind(eve, model.matrix(~ platform - 1, data = eve))


#There might have been 5 rows with no platform
eve$'platform\\N' = NULL


#Removing the vars no to used in clustering
eve$platform = NULL
eve$country = NULL
head(eve)

########################### Events Clustering ====================================
#Could not make hierarchical cluster as the distances matrix is too large

#eve_cluster = kmeans(eve[2:17], centers = 6)

#K-means also not working

########################### Joining datasets with clicks ============================
clicks_train = fread("clicks_train.csv")
clicks_test = fread("clicks_test.csv")

#binding the two together to get the join on both
#Getting the length of the train to segregate the two later on
nrow(clicks_train) #87141731

#Making a dummy predictor var for test set to do a row bind
clicks_test$clicked = 0
clicks = rbind(clicks_train, clicks_test)

rm(clicks_test, clicks_train)

#Checking how to join - clicks only has display and ad IDs
length(unique(prom_cont$ad_id)) #559583

length(unique(clicks$ad_id)) #544300

#Thus, the number of unique ads in clicks is more than prom_cont.

#Joining prom_cont and doc_dataset using document id
join1 = prom_cont %>% left_join(doc_dataset)

rm(prom_cont, doc_dataset)
#Joining clicks with join1 using ad_id
clicks = clicks %>% left_join(join1)
rm(join1)
gc()

fwrite(clicks[130000000:135137214,], file = "clicks_test_joined5.csv", showProgress = TRUE)
saveRDS(clicks[1:87141731,], file = "clicks_train_joined.rds")
saveRDS(clicks[87141732:135137214,], file = "clicks_test_joined.rds")

saveRDS(clicks_test, file = "clicks_test_total.rds")
fwrite(clicks_test[1:12000000, ], file= "clicks_test_total1.csv")
fwrite(clicks_test[12000001:25000000, ], file= "clicks_test_total2.csv")
fwrite(clicks_test[25000001:nrow(clicks_test), ], file= "clicks_test_total3.csv")

################################### Validation set =======================================

val_train1 = fread("training_set_1.csv")

val_train1 = val_train1 %>% left_join(join1)

saveRDS(val_train1[1:44343940,], file = "val_train1_half1.rds")
saveRDS(val_train1[44343941:88687880,], file = "val_train1_half2.rds")
rm(val_train1)



val_train2 = fread("training_set_1.csv")
val_train2 = val_train2 %>% left_join(join1)

saveRDS(val_train2[1:44343940,], file = "val_train2_half1.rds")
saveRDS(val_train2[44343941:88687880,], file = "val_train2_half2.rds")

rm(val_train2)



val_test1 = fread("test_set_1.csv")

val_test1 = val_test1 %>% left_join(join1)

saveRDS(val_test1, file = "val_test1.rds")

rm(val_test1)



val_test2 = fread("test_set_2.csv")

val_test2 = val_test2 %>% left_join(join1)

saveRDS(val_test2, file = "val_test2.rds")

rm(val_test2)
