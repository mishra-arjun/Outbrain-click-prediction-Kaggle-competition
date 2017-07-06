setwd("~/Documents/6242kaggle")
library(data.table)
library(xgboost)
library(nnet)
#library(readr)

####Load Train and Test
train<- fread( "clicks_train.csv")
train <- train[(1:10000000),]
# train <- fread( "clicks_test.csv")

# events <- read_csv("events.csv")
events <- fread('events.csv')
# events$platform[is.na(events$platform)] <- 2

m <- merge(train, events, by = "display_id")
rm(train)
rm(events)
gc()

doc1 <- fread("documents_topics.csv")
m1 <- merge(doc1, m, by = "document_id", allow.cartesian=TRUE)
m1$platform <- as.integer(m1$platform)

rm(doc1)
rm(m)
gc()

# views  <- fread('page_views_sample.csv')
# views  <- views[ , .N , keyby=c("document_id","uuid") ] 

# events[, leak := views[J(events$document_id,events$uuid)]$N  ]
# setkeyv( events, c("display_id","document_id") )
# rm(views);gc() 

model_xgb <- xgboost(data=as.matrix(m1),
                     label=as.matrix(m1$clicked),
                     objective="binary:logistic",
                     missing = NaN,
                     nround=200,
                     eta = 0.05,
                     max_depth = 4,
                     subsample = 0.75,
                     colsample_bytree = 0.8,
                     eval_metric = "auc")

model_nn <- nnet(clicked~.,
                 data = m1,
                 size = 3)
########################################################################################################

pred <- predict(model_nn,as.matrix(m1))

########################################################################################################
print("Now Build the Submission")
setorderv(pred, c("display_id","prob"), c(1,-1)  );gc() #Sort by -prob
sub <- pred[,.(ad_id=paste0(ad_id,collapse=" ")), keyby="display_id" ];gc()#Build submission

write.csv(sub,file = "submissionx.csv",row.names = F)