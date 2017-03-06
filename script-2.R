options(sqldf.driver = "SQLite") # as per FAQ #7 force SQLite
options(gsubfn.engine = "R")

library("RSQLite")
library("sqldf")
#library("tcltk2")
library("caret")

#connect to the database
db <- dbConnect(SQLite(), dbname="database.sqlite")
#List of tables
dbListTables(db)

#Load data
trainSearchStream<-dbGetQuery(db, "SELECT trainSearchStream.SearchID,trainSearchStream.AdID,trainSearchStream.Position,trainSearchStream.objectType, trainSearchStream.HistCTR,trainSearchStream.IsClick,SearchInfo.SearchDate, SearchInfo.UserID,SearchInfo.CategoryID as SearchCategoryID,AdsInfo.Price,AdsInfo.LocationID,AdsInfo.CategoryID FROM trainSearchStream, SearchInfo, UserInfo, AdsInfo where trainSearchStream.SearchID = SearchInfo.SearchID and SearchInfo.UserID=UserInfo.UserID and trainSearchStream.AdID=AdsInfo.AdID and AdsInfo.IsContext=1 limit 5000000")
testSearchStream<-dbGetQuery(db, "SELECT testSearchStream.TestId, testSearchStream.Position, testSearchStream.HistCTR,AdsInfo.Price,AdsInfo.LocationID,AdsInfo.CategoryID, SearchInfo.UserID, SearchInfo.CategoryID as SearchCategoryID FROM testSearchStream, SearchInfo, AdsInfo where testSearchStream.SearchID = SearchInfo.SearchID and testSearchStream.AdID=AdsInfo.AdID and AdsInfo.IsContext=1 ")

# Nummber of PhoneRequest per user
NumPhoneRequest<-dbGetQuery(db,"select UserID, count(UserID) as NumPhoReq from PhoneRequestsStream group by UserID ")

#Number of View per user
NumViews<-dbGetQuery(db,"select UserID, count(UserID) as NumView from VisitsStream group by UserID ")

#length(NumPhoneRequest)
#head(NumPhoneRequest)


#Convert Position and Price to numeric (train)
position <- as.numeric(trainSearchStream$Position)
price <- as.numeric(trainSearchStream$Price)
isClick<-as.factor(trainSearchStream$IsClick)
HistCTR<-as.numeric(trainSearchStream$HistCTR)
LocationID<-as.numeric(trainSearchStream$LocationID)
CategoryID<-as.numeric(trainSearchStream$CategoryID)
SearchCategoryID<-as.numeric(trainSearchStream$SearchCategoryID)


#DataFrame for train data
#data_train<-data.frame("isClick"=isClick,"Position"=position,"Price"=price, "HistCTR"=HistCTR, "LocationID"=LocationID,"CategoryID"=CategoryID, "SearchCategoryID"=SearchCategoryID)
data_train<-data.frame("isClick"=isClick,"Position"=position,"Price"=price, "HistCTR"=HistCTR, "LocationID"=LocationID,"CategoryID"=CategoryID, "SearchCategoryID"=SearchCategoryID,"UserID"=trainSearchStream$UserID)
data_train<-sqldf("select data_train.UserID, isClick, Position, price, HistCTR,LocationID, CategoryID, SearchCategoryID, NumPhoReq from data_train left join NumPhoneRequest on data_train.UserID=NumPhoneRequest.UserID")
data_train<-sqldf("select isClick, Position, price, HistCTR,LocationID, CategoryID, SearchCategoryID, NumPhoReq,NumView from data_train left join NumViews on data_train.UserID=NumViews.UserID")

data_train <- data_train[,c(-6)]
data_train$isClick <- as.factor(data_train$isClick)

#model
model<-train(formula = isClick ~ ., data=data_train, method="rpartCost", na.action = na.omit)

fit <- rpart(isClick ~ ., data=data_train, method="class", 
             parms=list(split="information"), 
             control=rpart.control(usesurrogate=0,maxsurrogate=0))
table(predict(fit, type = "class"))
summary(model)
#Convert Position and Price to numeric (test)
position <- as.numeric(testSearchStream$Position)
price <- as.numeric(testSearchStream$Price)
HistCTR<-as.numeric(testSearchStream$HistCTR)
LocationID<-as.numeric(testSearchStream$LocationID)
CategoryID<-as.numeric(testSearchStream$CategoryID)
SearchCategoryID<-as.numeric(testSearchStream$SearchCategoryID)

length(LocationID)
length(CategoryID)

#DataFrame for test data
data_test<-data.frame("Position"=position,"Price"=price, "HistCTR"=HistCTR, "LocationID"=LocationID,"CategoryID"=CategoryID, "SearchCategoryID"=SearchCategoryID,"UserID"=testSearchStream$UserID)
data_test<-sqldf("select data_test.UserID,Position, price, HistCTR,LocationID, CategoryID, SearchCategoryID, NumPhoReq from data_test left join NumPhoneRequest on data_test.UserID=NumPhoneRequest.UserID")
data_test<-sqldf("select Position, price, HistCTR,LocationID, CategoryID, SearchCategoryID, NumPhoReq,NumView from data_test left join NumViews on data_test.UserID=NumViews.UserID")
save.image()

#prediction
predictions<-predict(model,data_test, type = "response")
predictions[is.na(predictions)] <- 0


#submission
write.csv(data.frame("ID"=testSearchStream$TestId, "IsClick"=predictions),"prediction.csv",quote=F,row.names=F)
