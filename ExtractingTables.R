# Loading required Packages
options(sqldf.driver = "SQLite") # as per FAQ #7 force SQLite
options(gsubfn.engine = "R")
library(parallel)
library("RSQLite")
library("sqldf")
library("caret")
library("data.table")
#connect to the database
db <- dbConnect(SQLite(), dbname="database.sqlite")
#List of tables
dbListTables(db)

# Runs the query, fetches the given number of entries and returns a
# data.table
fetch  <- function(db, query, n = -1) {
  result <- dbSendQuery(db, query) 
  data <- dbFetch(result, n)
  dbClearResult(result)
  return(as.data.table(data))
}

Cat_Pos_All <- fetch(db,"SELECT AdsInfo.AdID, AdsInfo.CategoryID, trainSearchStream.IsClick,
                      trainSearchStream.Position, Category.ParentCategoryID FROM AdsInfo
                     INNER JOIN trainSearchStream ON trainSearchStream.AdID=AdsInfo.AdID
                     INNER JOIN Category ON Category.CategoryID=AdsInfo.CategoryID
                     WHERE trainSearchStream.ObjectType=3 AND trainSearchStream.IsClick=1
                     LIMIT 1000000")

write.csv(Cat_Pos_All, "Cat_Pos_IsClickAll.csv")

Cat_Pos <- fetch(db,"SELECT AdsInfo.AdID, AdsInfo.CategoryID, trainSearchStream.IsClick, 
                  trainSearchStream.Position, Category.ParentCategoryID FROM AdsInfo 
                  INNER JOIN trainSearchStream ON trainSearchStream.AdID=AdsInfo.AdID
                  INNER JOIN Category ON Category.CategoryID=AdsInfo.CategoryID
                  WHERE trainSearchStream.IsClick=1 AND trainSearchStream.ObjectType=3
                  LIMIT 1000")

format(object.size(Cat_Pos), units = "Mb")
Parent_Pos_IsClick <- merge(Cat_Pos, ParentCat_Pos, by.x = "AdID", by.y = "AdID")
table(Cat_Pos$IsClick)

Loc_Reg_Time <- fetch(db,"SELECT Location.RegionID, SearchInfo.IsUserLoggedOn, trainSearchStream.IsCLick,
                    Category.ParentCategoryID, SearchInfo.SearchID, VisitsStream.ViewDate FROM SearchInfo 
                    INNER JOIN Location ON Location.LocationID=SearchInfo.LocationID 
                    INNER JOIN Category ON SearchInfo.CategoryID=Category.CategoryID
                    INNER JOIN trainSearchStream ON trainSearchStream.SearchID=SearchInfo.SearchID
                    INNER JOIN VisitsStream ON VisitsStream.UserID=SearchInfo.UserID
                    WHERE trainSearchStream.IsClick=1
                    LIMIT 100000")

split_date <- function(Avito) {
  Avito$Years = strftime(strptime(Avito$ViewDate, "%Y-%m-%d %H:%M:%S"),"%Y")
  Avito$Month = strftime(strptime(Avito$ViewDate, "%Y-%m-%d %H:%M:%S"),"%m")
  Avito$DayOfMonth = strftime(strptime(Avito$ViewDate, "%Y-%m-%d %H:%M:%S"),"%d")
  Avito$Hour = strftime(strptime(Avito$ViewDate, "%Y-%m-%d %H:%M:%S"),"%H")
  return(Avito)
}

Loc_Reg_Time <- split_date(Loc_Reg_Time)
write.csv(Loc_Reg_Time, "Loc_Reg_Time.csv")

HistCTR_Pos <- fetch(db, "Select trainSearchStream.HistCTR, trainSearchStream.IsClick, trainSearchStream.Position
                          FROM trainSearchStream LIMIT 100000000")
format(object.size(HistCTR_Pos), units = "Mb")
write.csv(HistCTR_Pos, "HistCTR_Pos.csv")
save.image()

HistCTR_Pos_Pct10 <- fetch(db, "Select trainSearchStream.HistCTR, trainSearchStream.IsClick, trainSearchStream.Position
                          FROM trainSearchStream LIMIT 1000000")
format(object.size(HistCTR_Pos_Pct10), units = "Mb")
write.csv(HistCTR_Pos_Pct10, "HistCTR_Pos_Pct10.csv")
save.image()
