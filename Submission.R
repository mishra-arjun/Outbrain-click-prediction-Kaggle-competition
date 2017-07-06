############################### 6242 Project Merge Probabilities ===============================
setwd("G:/Georgia Tech/CS 6242/Assignments/Project/Analysis/Algo")

#############Loading packages
require(ggplot2)
require(data.table)
require(dplyr)
require(ggthemes)
require(lubridate)

############Reading in the prepared files

xg1 = fread("xgb1_test.csv")

xg1$V1 = NULL

#Getting one probability value for each ad and display id pair
xg1 = xg1 %>%
  group_by(display_id, ad_id) %>%
  summarise(clicked = mean(clicked)) %>%
  arrange(display_id, desc(clicked))


###########Second dataset

xg2 = fread("xgb2_9.csv")

xg2$V1 = NULL

#Getting one probability value for each ad and display id pair
xg2 = xg2 %>%
  group_by(display_id, ad_id) %>%
  summarise(clicked = mean(clicked)) %>%
  arrange(display_id, desc(clicked))

##############Third dataset

nn2 = fread("nn2_9.csv")

nn2$V1 = NULL

#Getting one probability value for each ad and display id pair
nn2 = nn2 %>%
  group_by(display_id, ad_id) %>%
  summarise(clicked = mean(clicked)) %>%
  arrange(display_id, desc(clicked))


######Getting all the datasets in submission format
xg1_subm = xg1 %>%
  group_by(display_id) %>%
  summarise(ad_id = paste(ad_id, collapse = " "))

xg2_subm = xg2 %>%
  group_by(display_id) %>%
  summarise(ad_id = paste(ad_id, collapse = " "))

nn2_subm = nn2 %>%
  group_by(display_id) %>%
  summarise(ad_id = paste(ad_id, collapse = " "))

#Writing the csv files
fwrite(xg1_subm, file = "xg1_subm.csv")

fwrite(xg2_subm, file = "xg2_subm.csv")

fwrite(nn2_subm, file = "nn2_subm.csv")


#Removing duplicates for the validation set
train9 = fread("clicks_train_joined9.csv")

cols = names(train9)[-12]

dots <- lapply(cols, as.symbol)


train9_new = train9 %>%
  select(-entity_id) %>%
  group_by_(.dots = dots)

train9_new = unique(train9_new)

fwrite(train9_new, file = "clicks_train_joined9_nodup.csv")
  

View(head(train9, 100))

xg1 = fread("xg1_subm.csv")


#Read in the test dataset
test = fread("displayadids.csv")

test$V1 = NULL

#Making the final test dataset, with duplicates
test_fin = cbind(test, xg1)

test_fin = test_fin %>%
  group_by(display_id, ad_id) %>%
  summarise(clicked = mean(V2)) %>%
  arrange(display_id, desc(clicked))

#Should have 32225162 records.

test_fin = test_fin %>%
  group_by(display_id) %>%
  summarise(ad_id = paste(ad_id, collapse = " "))

#Keeping only display_ids that are in the test set
test_fin = test_fin[test_fin$display_id %in% clicks_test$display_id, ]



######################Reading in total test data
test1 = fread("clicks_test_total1.csv")
test2 = fread("clicks_test_total2.csv")
test3 = fread("clicks_test_total3.csv")

test_total = rbind(test1, test2, test3)

rm(test1, test2, test3)

#Joining ad_id and display_id with prediction prob.

xg1 = fread("xgb1test.csv")

xg1 = cbind(test_total[ , c(1,2)], xg1[,2])

xg1 = xg1 %>%
  group_by(display_id, ad_id) %>%
  summarise(clicked = mean(V2)) %>%
  arrange(display_id, desc(clicked))

xg1_subm = xg1 %>%
  group_by(display_id) %>%
  summarise(ad_id = paste(ad_id, collapse = " "))

#fwrite(xg1_subm, file = "xg1subm.csv")


#Second xg file

xg2 = fread("xgb2test.csv")

xg2 = cbind(test_total[ , c(1,2)], xg2[,2])

xg2 = xg2 %>%
  group_by(display_id, ad_id) %>%
  summarise(clicked = mean(V2)) %>%
  arrange(display_id, desc(clicked))

xg2_subm = xg2 %>%
  group_by(display_id) %>%
  summarise(ad_id = paste(ad_id, collapse = " "))

#fwrite(xg2_subm, file = "xg2subm.csv")

#Mean of the two xgs

xg_mean = xg1
xg_mean$clicked2 = xg2$clicked

head(xg_mean)

xg_mean = xg_mean %>%
  mutate(click_mean = (clicked + clicked2)/2)

xg_mean$clicked = NULL
xg_mean$clicked2 = NULL

xg_mean = xg_mean %>%
  group_by(display_id, ad_id) %>%
  summarise(clicked = mean(click_mean)) %>%
  arrange(display_id, desc(clicked))

xg_mean_subm = xg_mean %>%
  group_by(display_id) %>%
  summarise(ad_id = paste(ad_id, collapse = " "))

fwrite(xg_mean_subm, file = "xg_mean_subm.csv")

#Neural Net

nn1 = fread("nn1test.csv")

nn1 = cbind(test_total[ , c(1,2)], nn1[,2])

nn1 = nn1 %>%
  group_by(display_id, ad_id) %>%
  summarise(clicked = mean(V2)) %>%
  arrange(display_id, desc(clicked))

nn1_subm = nn1 %>%
  group_by(display_id) %>%
  summarise(ad_id = paste(ad_id, collapse = " "))

fwrite(nn1_subm, file = "nn1subm.csv")