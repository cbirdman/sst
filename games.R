library(jsonlite)
library(stringr)
library(zoo)
library(plyr)
library(dplyr)
library(data.table)

path<-"C:/Users/brocatoj/Documents/Basketball/"
games<-fromJSON(paste0(path,"Tracking/meta/games.json"))

schedule<-data.table()
for(i in 2014:2017){
    sch<-fread(paste0(path,"Game/",i,"/",i,"_schedule.csv")) 
    schedule<-rbind(schedule,sch)
    rm(sch)
}

schedule<-schedule[,.(Game,Date,Away,Home,gamecode)]
setnames(schedule,c("ids_id","date","away","home","game"))
schedule[,ids_id:=paste0("00",ids_id)]
schedule[,date:=as.Date(date,"%m/%d/%Y")]
games<-left_join(games,schedule,by="ids_id")
games<-arrange(games,desc(id))
write.csv(games,"C:/Users/brocatoj/Documents/Basketball/Tracking/meta/games.csv",
          row.names=F)