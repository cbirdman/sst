library(jsonlite)
library(stringr)
library(tidyr)
library(zoo)
library(plyr)
library(dplyr)
library(data.table)

# gameid<-readline('Enter Game Code: (e.g., 2016061909) ')
gameid<-"2016102525"

# Extract raw data
frames<-readLines(paste0("C:/Users/brocatoj/Documents/Basketball/Tracking/frames/",
                    gameid,".json"))
frames<-paste0(frames,",")
frames<-paste(frames,collapse="")
frames<-substr(frames,1,nchar(frames)-1)
frames<-paste0("[",frames,"]")
frames<-fromJSON(frames)

# Extract Ball Coorinates
frames$ball_x<-frames$ball$x
frames$ball_y<-frames$ball$y
frames$ball_z<-frames$ball$z

# Extract away player coordinates
setDT(frames)
for(i in 1:5){
frames[,(paste0("ap",i)):=away_players[[i]]$id]
frames[,(paste0("ap",i,"_x")):=away_players[[i]]$x]
frames[,(paste0("ap",i,"_y")):=frames$away_players[[i]]$y]
}

# Extract home player coordinates
for(i in 1:5){
frames[,(paste0("hp",i)):=home_players[[i]]$id]
frames[,(paste0("hp",i,"_x")):=home_players[[i]]$x]
frames[,(paste0("hp",i,"_y")):=frames$home_players[[i]]$y]
}
frames<-select(frames,-home_players,-away_players,-ball)
frames[,.(game_code,idx,period,utcTime,gameClock,home_team,away_team,ball_x,
          ball_y,ball_z,hp1,hp1_x,hp1_y,hp2,hp2_x,hp2_y,hp3,hp3_x,hp3_y,hp4,
          hp4_x,hp4_y,hp5,hp5_x,hp5_y,ap1,ap1_x,ap1_y,ap2,ap2_x,ap2_y,ap3,
          ap3_x,ap3_y,ap4,ap4_x,ap4_y,ap5,ap5_x,ap5_y)]

# Write csv for quicker future parsing
#fwrite(frames,paste0("J:/eagle/frames_csv/",gameid,".csv"))
fwrite(frames,paste0("C:/Users/brocatoj/Documents/Basketball/Tracking/frames_csv/",gameid,".csv"))