library(XML)
library(jsonlite)
library(stringr)
library(zoo)
library(plyr)
library(dplyr)
library(data.table)

# gameid<-readline('Enter Game Code: (e.g., 2016061909) ')
# location<-readline('At Work? (Y/N) ')
gameid<-"2016042306"
location<-"N"

# Rename game to make more easily understandable
games<-fread("C:/Users/brocatoj/Documents/Basketball/Tracking/games.csv")
games<-games[stats_id==gameid,.(stats_id,id,game)]
game_name<-games$game[1]
ss_id<-games$id[1]

# IMPORT MARKINGS
js<-fromJSON(paste0("C:/Users/brocatoj/Documents/Basketball/eagle/updates/",
                    ss_id,".json"))

# EXTRACT TRACKING DATA
svu<-data.table()

for(i in c("Q1","Q2","Q3","Q4","OT1","OT2","OT3","OT4","OT5")){
    if(!file.exists(
        paste0("C:/Users/brocatoj/Documents/Basketball/svu/2016/xml/",
               "NBA_FINAL_SEQUENCE_OPTICAL$",gameid,"_",i,".XML"))&
       !file.exists(paste0("S:/NBA_FINAL_SEQUENCE_OPTICAL$",gameid,"_",i,".XML")))
        break
    ifelse(location=="N",
           q<-paste0("C:/Users/brocatoj/Documents/Basketball/svu/2016/xml/",
                     "NBA_FINAL_SEQUENCE_OPTICAL$",gameid,"_",i,".XML"),
           q<-paste0("S:/NBA_FINAL_SEQUENCE_OPTICAL$",gameid,"_",i,".XML"))
    q<-xmlRoot(xmlTreeParse(q,useInternalNodes = T))
    q<-as.data.table(do.call(rbind,xpathApply(q,"//moment",xmlAttrs)))
    q[,idx:=rownames(q)][,idx:=as.numeric(idx)-1]
    q[,period:=ifelse(substr(i,1,1)=="Q",
                     as.numeric(substr(i,nchar(i),nchar(i))),
                     as.numeric(substr(i,nchar(i),nchar(i)))+4)]
    svu<-rbind(svu,q)
    rm(q)
}

# CLEAN TRACKING DATA
svu[,locations:=ifelse(str_count(locations,";")==9,
                       paste0("-1,-1,0,0,0;",locations),locations)]
svu[str_count(locations,";")==10]
svu<-svu %>%
    separate(locations,c("ball_tm","ball","ball_x","ball_y","ball_z","hp1_tm",
                         "hp1","hp1_x","hp1_y","hp1_z","hp2_tm","hp2","hp2_x",
                         "hp2_y","hp2_z","hp3_tm","hp3","hp3_x","hp3_y","hp3_z",
                         "hp4_tm","hp4","hp4_x","hp4_y","hp4_z","hp5_tm","hp5",
                         "hp5_x","hp5_y","hp5_z","ap1_tm","ap1","ap1_x","ap1_y",
                         "ap1_z","ap2_tm","ap2","ap2_x","ap2_y","ap2_z","ap3_tm",
                         "ap3","ap3_x","ap3_y","ap3_z","ap4_tm","ap4","ap4_x",
                         "ap4_y","ap4_z","ap5_tm","ap5","ap5_x","ap5_y",
                         "ap5_z"),sep="\\,|\\;")

svu[,game_code:=ss_id]
svu[,home_team:=js$meta$home_id]
svu[,away_team:=js$meta$away_id]
svu<-svu[,c(62,60,61,2,1,63:64,7:9,11:13,16:18,21:23,26:28,31:33,36:38,41:43,
            46:48,51:53,56:58),with=F]
setnames(svu,c("time","game-clock"),c("utcTime","gameClock"))