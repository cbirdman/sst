library(jsonlite)
library(stringr)
library(tidyr)
library(zoo)
library(plyr)
library(dplyr)
library(data.table)

# gameid<-readline('Enter Game Code: (e.g., 2016061909) ')
#gameid<-"2016112526"

# Read in games and players
games<-fread("C:/Users/brocatoj/Documents/Basketball/Tracking/meta/games.csv")
games<-games[id==gameid,.(id,game)]
game_name<-games$game[1]
ss_id<-games$id[1]
players<-fread("C:/Users/brocatoj/Documents/Basketball/Tracking/meta/players_plus.csv")

# Extract raw data
frames<-readLines(paste0("E:/Tracking/frames/",ss_id,".json"))
frames<-paste0(frames,",")
frames<-paste(frames,collapse="")
frames<-substr(frames,1,nchar(frames)-1)
frames<-paste0("[",frames,"]")
frames<-fromJSON(frames)
frames<-subset(frames,!is.null(home_players)&!is.null(away_players))

# Extract Ball Coorinates
frames$ball_x<-frames$ball$x
frames$ball_y<-frames$ball$y
frames$ball_z<-frames$ball$z

# # Extract away player coordinates
frames<-frames %>%
    separate(away_players,c("ap1_y","ap2_y","ap3_y","ap4_y","ap5_y",
                            "ap1_x","ap2_x","ap3_x","ap4_x","ap5_x",
                            "ap1","ap2","ap3","ap4","ap5"), sep=", ") %>%
    separate(home_players,c("hp1_y","hp2_y","hp3_y","hp4_y","hp5_y",
                            "hp1_x","hp2_x","hp3_x","hp4_x","hp5_x",
                            "hp1","hp2","hp3","hp4","hp5"), sep=", ") %>%
    select(game_code,idx,period,utcTime,gameClock,home_team,away_team,ball_x,
          ball_y,ball_z,hp1,hp1_x,hp1_y,hp2,hp2_x,hp2_y,hp3,hp3_x,hp3_y,hp4,
          hp4_x,hp4_y,hp5,hp5_x,hp5_y,ap1,ap1_x,ap1_y,ap2,ap2_x,ap2_y,ap3,
          ap3_x,ap3_y,ap4,ap4_x,ap4_y,ap5,ap5_x,ap5_y) %>%
    mutate(hp1=as.numeric(substr(hp1,8,24)),ap1=as.numeric(substr(ap1,8,24)),
           hp1_x=as.numeric(substr(hp1_x,7,23)),ap1_x=as.numeric(substr(ap1_x,7,23)),
           hp1_y=as.numeric(substr(hp1_y,12,27)),ap1_y=as.numeric(substr(ap1_y,12,27)),
           hp5=as.numeric(gsub("))","",hp5)),ap5=as.numeric(gsub("))","",ap5)),
           hp5_x=as.numeric(gsub(")","",hp5_x)),ap5_x=as.numeric(gsub(")","",ap5_x)),
           hp5_y=as.numeric(gsub(")","",hp5_y)),ap5_y=as.numeric(gsub(")","",ap5_y)))

# setDT(frames)
# frames[,hp1_y:=ifelse(hp1_y=="",NA,hp1_y)]
# frames[,ap1_y:=ifelse(hp1_y=="",NA,ap1_y)]
# for(i in c("ap1_y","ap2_y","ap3_y","ap4_y","ap5_y","ap1_x","ap2_x","ap3_x",
#            "ap4_x","ap5_x","ap1","ap2","ap3","ap4","ap5","hp1_y","hp2_y","hp3_y",
#            "hp4_y","hp5_y","hp1_x","hp2_x","hp3_x","hp4_x","hp5_x","hp1","hp2",
#            "hp3","hp4","hp5")){
#     frames[[i]]<-na.locf(frames[[i]])
# }

for(i in c("ap2_y","ap3_y","ap4_y","ap5_y","ap2_x","ap3_x","ap4_x","ap2","ap3",
    "ap4","hp2_y","hp3_y","hp4_y","hp2_x","hp3_x","hp4_x","hp2","hp3","hp4")){
    frames[[i]]<-as.numeric(frames[[i]])
}
 
# MAP NBA IDs ONTO FRAMES
for(i in c("hp1","hp2","hp3","hp4","hp5","ap1","ap2","ap3","ap4","ap5")){
    ids<-players[,.(id,ids_id)]
    ids<-ids[!is.na(id)]
    setnames(ids,c(i,paste0(i,"_2")))
    ids[[i]]<-as.numeric(ids[[i]])
    frames<-left_join(frames,ids,by=i)
}

setDT(frames)
frames<-frames[,c(1:10,41,12:13,42,15:16,43,18:19,44,21:22,45,24:25,46,27:28,47,
                  30:31,48,33:34,49,36:37,50,39:40),with=F]
setnames(frames,c("hp1_2","hp2_2","hp3_2","hp4_2","hp5_2",
                  "ap1_2","ap2_2","ap3_2","ap4_2","ap5_2"),
                c("hp1","hp2","hp3","hp4","hp5","ap1","ap2","ap3","ap4","ap5"))

frames<-frames[,.(game_code,idx,period,utcTime,gameClock,home_team,away_team,
                  ball_x,ball_y,ball_z,hp1,hp1_x,hp1_y,hp2,hp2_x,hp2_y,hp3,
                  hp3_x,hp3_y,hp4,hp4_x,hp4_y,hp5,hp5_x,hp5_y,ap1,ap1_x,ap1_y,
                  ap2,ap2_x,ap2_y,ap3,ap3_x,ap3_y,ap4,ap4_x,ap4_y,ap5,ap5_x,
                  ap5_y)]

# Write csv for quicker future parsing
fwrite(frames,paste0("E:/Tracking/frames_csv/",ss_id,".csv"))
#fwrite(frames,paste0("C:/Users/brocatoj/Documents/Basketball/Tracking/frames_csv/",gameid,".csv"))