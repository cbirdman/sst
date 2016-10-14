library(jsonlite)
library(bit64)
library(stringr)
library(zoo)
library(plyr)
library(dplyr)
library(data.table)
source("functions.R")

# read in relevant files
gameid<-"2016042307"
# frames<-fread(paste0("J:/eagle/frames/",gameid,".csv"))
# js<-fromJSON(paste0("J:/eagle/markings/",gameid,".json"))
frames<-fread(paste0("C:/Users/brocatoj/Documents/Basketball/Tracking/frames/",gameid,".csv"))
js<-fromJSON(paste0("C:/Users/brocatoj/Documents/Basketball/Tracking/markings/",gameid,".json"))

# Add markings
    # From shots
    shots<-as.data.table(js$shots)
    blocks<-shots[blocked==TRUE]
    blocks<-blocks[,frame_idx:=frame_idx+1]
    blocks[,event:="BLK"]
    blocks<-blocks[,.(period,frame_idx,event,player_id,dplayer_id)]
    shots<-shots[sfl_fgx==FALSE]
    shots[,event:=ifelse(made==TRUE,ifelse(is_three==TRUE,"3PM","2PM"),
                        ifelse(is_three==TRUE,"3PX","2PX"))]
    shots<-shots[,.(period,frame_idx,event,player_id,dplayer_id)]
    markings<-rbind(shots,blocks)
    setnames(markings,"frame_idx","frame")
    
    # From passes
    passes<-as.data.table(js$passes)
    passes[,event:=ifelse(is_to==FALSE,"PASS","TO")][,dplayer_id:=NA]
    passes<-passes[,.(period,frame,event,passer,dplayer_id)]
    setnames(passes,"passer","player_id")
    
    # From touches
    touches<-as.data.table(js$touches_all)
    touches[,event:="POSS"]
    touches<-touches[,.(period,frame,event,toucher_id,defender_id)]
    setnames(touches,c("toucher_id","defender_id"),c("player_id","dplayer_id"))
    
    # From dribbles
    dribbles<-as.data.table(js$dribbles)
    dribbles[,event:="DRIB"][,dplayer_id:=NA]
    dribbles<-dribbles[,.(period,frame,event,ballhandler_id,dplayer_id)]
    setnames(dribbles,"ballhandler_id","player_id")

    # From fouls
    fouls<-as.data.table(js$fouls)
    fouls[,event:=ifelse(shooting==TRUE,"SF","FOUL")]
    fouls<-fouls[,.(period,frame,event,fouled,fouler)]
    setnames(fouls,c("fouled","fouler"),c("player_id","dplayer_id"))
    
    # From rebounds
    rebs<-as.data.table(js$pbp)
    rebs<-rebs[event%in%c("ORB","DRB"),.(chance_id,event)]
    rebounds<-as.data.table(js$rebounds)
    rebounds<-rebounds[,.(rebounder,chance_id)]
    setkey(rebs,chance_id);setkey(rebounds,chance_id)
    rebounds<-rebs[rebounds]
    rebs<-as.data.table(js$chances)
    rebs<-rebs[,.(period,end_frame,chance_id)]
    setkey(rebs,chance_id)
    rebounds<-rebs[rebounds]
    rebounds[,dplayer_id:=NA]
    rebounds<-rebounds[,.(period,end_frame,event,rebounder,dplayer_id)]
    setnames(rebounds,c("end_frame","rebounder"),c("frame","player_id"))
    markings<-rbind(markings,passes,touches,dribbles,fouls,rebounds)
    
    # From picks
    picks<-as.data.table(js$picks)
    picks[,event:=ifelse(handoff_pick==TRUE,"DHO","SCR")]
    picks<-picks[,.(period,frame,event,ballhandler,ballhandler_defender)]
    setnames(picks,c("ballhandler","ballhandler_defender"),
                   c("player_id","dplayer_id"))
    
    # From drives
    drives<-as.data.table(js$drives)
    drives[,event:="DRV"]
    drives<-drives[,.(period,frame,event,bhr,defender)]
    setnames(drives,c("bhr","defender"),c("player_id","dplayer_id"))
    
    # From pbp
    other<-as.data.table(js$pbp)
    other<-other[event%in%c("SUB","TMO","JMP","SPD","EPD")]
    other[,frame:=substr(possession_id,13,50)][,player_id:=NA][,dplayer_id:=NA]
    other<-other[,.(period,frame,event,player_id,dplayer_id)]

markings<-rbind(markings,passes,touches,dribbles,fouls,rebounds,picks,drives,other)
rm(list= ls()[!(ls() %in% c('frames','markings'))])
markings<-markings[order(period, frame)]
markings[,mid:=paste0(period,"_",frame)]
markings[,order:=ifelse(event%in%c("DRIB","POSS","PASS"),2,1)]
markings<-markings[order(order)]
markings<-distinct(markings,mid,.keep_all=T)
markings<-markings[,.(mid,event,player_id,dplayer_id)]
frames[,mid:=paste0(period,"_",idx)]
frames<-left_join(frames,markings,by="mid")