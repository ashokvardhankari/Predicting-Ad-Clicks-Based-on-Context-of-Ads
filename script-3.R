# Kaggle-competititon "Avito Context Ad Clicks"
# See https://www.kaggle.com/c/avito-context-ad-clicks

# In order to run this script on Kaggle-scripts I had to limit the number of entries to read
# from the database as well as to decrease the sample-size. With the full dataset from the database as well
# as a sample of 10 millions entries I got a 0.05104 on the public leaderboard

library("data.table")
library("RSQLite")
library("caret")

# ----- Prepare database -------------------------------------------------------

db <- dbConnect(SQLite(), dbname="database.sqlite")
dbListTables(db)

# ----- Utitlies ---------------------------------------------------------------

# Define constants to improve readability of large number
thousand <- 1000
million  <- thousand * thousand 
billion  <- thousand * million

# Runs the query, fetches the given number of entries and returns a
# data.table
fetch  <- function(db, query, n = -1) {
  result <- dbSendQuery(db, query)
  data <- dbFetch(result, n)
  dbClearResult(result)
  return(as.data.table(data))
}

# Select contextual Ads (OnjectType=3), results in 190.157.735 entries
# Warning: Takes a few minutes
sample <- fetch(db, 
    "select * from trainSearchStream 
        inner join searchinfo on trainSearchStream.SearchID=searchinfo.SearchID 
        inner join userinfo on searchinfo.UserID=userinfo.UserID 
        inner join adsinfo on trainSearchStream.AdID=adsinfo.AdID 
        where trainSearchStream.isClick==1", 1500)
head(sample)

visits <- fetch(db, 
    "select * from visitsstream where UserID=59703", 1500)
head(visits)

phonerequests <- fetch(db, 
    "select * from phonerequestsstream where UserID=59703", 1500)
head(phonerequests)
