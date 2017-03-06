# Kaggle Competetion (Avito Contextual Ad Click)
# I got a 0.05104 on the public leaderboard

# Loading required libraries
library("data.table")
library("RSQLite")
library("caret")
library("randomForest")
library("pROC")
library("ROCR")

# Connecting to Database
db <- dbConnect(SQLite(), dbname="database.sqlite")
dbListTables(db)

# Define constants to improve readability of large number
thousand <- 1000
million  <- thousand * thousand 
billion  <- thousand * million

# Creating a query which fetches required records and returns as a data frome
fetch  <- function(db, query, n = -1) {
  result <- dbSendQuery(db, query)
  data <- dbFetch(result, n)
  dbClearResult(result)
  return(as.data.table(data))
}

# Select contextual Ads (OnjectType=3)
trainSearchStreamContextual <- fetch(db, "select Position, HistCTR, IsClick from trainSearchStream where ObjectType=3", 10 * million)
m <- nrow(trainSearchStreamContextual)

# Create stratified sample 
sampleSize <- 1 * million #100 * million
sampleRatio <- sampleSize / m
sampleIndex <- createDataPartition(trainSearchStreamContextual$IsClick, p = sampleRatio, list=FALSE)
trainSearchStreamContextualSample <- trainSearchStreamContextual[as.vector(sampleIndex), ]

# Compare click-ratio in full set and sample to verify stratification
print(paste("Clickratio full dataset:", sum(trainSearchStreamContextual$IsClick)/m))
print(paste("Clickratio sample:", sum(trainSearchStreamContextualSample$IsClick)/sampleSize))

# Create stratified random split ...
trainSampleIndex <- createDataPartition(y = trainSearchStreamContextualSample$IsClick, p = .80, list = FALSE)

# ... and partition data-set into train- and validation-set
trainSearchStreamContextualTrainSample <- trainSearchStreamContextualSample[as.vector(trainSampleIndex),]
trainSearchStreamContextualValidationSample <- trainSearchStreamContextualSample[-as.vector(trainSampleIndex),]

# Build a logistic regression ...
model <- glm(IsClick ~ HistCTR+Position, data = trainSearchStreamContextualTrainSample, family="binomial")
summary(model)
varImp(model)

# Loss-function to evaluate result
logloss <- function(y, yHat){
  threshold <- 10^(-15)
  yHat <- pmax(pmin(yHat, 1-threshold), threshold)
  loss <- -mean(y*log(yHat) + (1-y)*log(1-yHat))
  return(loss)
}

# ... and predict data on validation data-set
prediction <- predict(model, trainSearchStreamContextualValidationSample, type="response")
print(logloss(trainSearchStreamContextualValidationSample$IsClick, prediction))

pred1 <- prediction(prediction, trainSearchStreamContextualValidationSample$IsClick)
perf1 <- performance(pred1, measure = "tpr", x.measure = "fpr")
plot(perf1)
auc1 <- performance(pred1, measure = "auc")
auc1 <- auc1@y.values[[1]]
auc1

# Random Forest
rfModel <- randomForest(IsClick ~ HistCTR+Position, data = trainSearchStreamContextualTrainSample, ntree=50,
                        do.trace=2,replace=FALSE,verboseiter=FALSE)

summary(rfModel)
varImp(rfModel)

conf <- rfModel$confusion
conf

# Predicting on test data-set
rfPrediction <- predict(rfModel, trainSearchStreamContextualValidationSample, type="response")
postResample(rfPrediction, trainSearchStreamContextualValidationSample$IsClick)


#=======================   More Features   ===========================

# Creating more models with more features
# Extracting required fields from multiple tables
trainSearchStream<-dbGetQuery(db, "SELECT trainSearchStream.SearchID,trainSearchStream.AdID,trainSearchStream.Position,
                              trainSearchStream.objectType, trainSearchStream.HistCTR,trainSearchStream.IsClick,
                              SearchInfo.SearchDate, SearchInfo.UserID,SearchInfo.CategoryID as SearchCategoryID,
                              AdsInfo.Price,AdsInfo.LocationID,AdsInfo.CategoryID FROM trainSearchStream, 
                              SearchInfo, UserInfo, AdsInfo where trainSearchStream.SearchID = SearchInfo.SearchID 
                              and SearchInfo.UserID=UserInfo.UserID and trainSearchStream.AdID=AdsInfo.AdID 
                              and AdsInfo.IsContext=1 limit 100000")

#Number of PhoneRequest per user
NumPhoneRequest<-dbGetQuery(db,"select UserID, count(UserID) as NumPhoReq from PhoneRequestsStream group by UserID")

#Number of View per user
NumViews<-dbGetQuery(db,"select UserID, count(UserID) as NumView from VisitsStream group by UserID ")

#length(NumPhoneRequest)
#head(NumPhoneRequest)

trainSearchStream[is.na(trainSearchStream)]<--1
#Convert Position and Price to numeric (train)
position <- as.factor(trainSearchStream$Position)
price <- as.numeric(trainSearchStream$Price)
isClick<-as.numeric(trainSearchStream$IsClick)
HistCTR<-as.numeric(trainSearchStream$HistCTR)
LocationID<-as.factor(trainSearchStream$LocationID)
CategoryID<-as.factor(trainSearchStream$CategoryID)
SearchCategoryID<-as.factor(trainSearchStream$SearchCategoryID)


#DataFrame for train data
finalData <- data.frame("isClick"=isClick,"Position"=position,"Price"=price, "HistCTR"=HistCTR,
                        "CategoryID"=CategoryID, "SearchCategoryID"=SearchCategoryID,
                        "UserID"=trainSearchStream$UserID)

finalData$UserID<-NULL

trainIndex <- createDataPartition(finalData$isClick, p = 0.7, list=FALSE)
data_train <- finalData[as.vector(trainIndex), ]
data_train[is.na(data_train)]<-(-1)
head(data_train)

data_test <- finalData[-as.vector(trainIndex),]
data_test[is.na(data_test)]<-(-1)
head(data_test)

# Building a random forest model
rfModel1 <- randomForest(isClick ~., data = data_train, ntree=50,
                      do.trace=2,replace=FALSE,verboseiter=FALSE)
summary(rfModel1)
varImp(rfModel1)

conf1 <- rfModel1$confusion
conf1

# Predicting on test data-set
rfPrediction1 <- predict(rfModel1, data_test, type="response")
postResample(rfPrediction1, data_test$isClick)

# Building a Logistic Regression Model 
glmModel <- glm(formula = isClick ~ Position + HistCTR + SearchCategoryID + Price, 
                data=data_train, family = binomial("logit"))
summary(glmModel)
round(varImp(glmModel))

# Predicting on test data-set
glmPrediction <- predict(glmModel, data_test, type="response")
print(logloss(data_test$IsClick, glmPrediction))

pred <- prediction(glmPrediction, data_test$isClick)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

# Disconnecting Database
dbDisconnect(db)
