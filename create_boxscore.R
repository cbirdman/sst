library(jsonlite)
library(data.table)

gameid<-"0021600036"
url <- fromJSON(paste0("http://stats.nba.com/stats/boxscoretraditionalv2?GameID=",
                        gameid,"&StartPeriod=0&EndPeriod=0&StartRange=0&",
                        "EndRange=0&RangeType=0"))
head<-url$resultSets$headers[[1]]
box<-as.data.table(url$resultSets$rowSet[[1]])
setnames(box,head)
rm(head,url)
