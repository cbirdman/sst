library(jsonlite)
library(bit64)
library(stringr)
library(zoo)
library(plyr)
library(dplyr)
library(data.table)
source("functions.R")

# read in relevant files
gameid<-"2016103011"
# frames<-fread(paste0("J:/eagle/frames/",gameid,".csv"))
# js<-fromJSON(paste0("J:/eagle/markings/",gameid,".json"))
path<-"C:/Users/brocatoj/Documents/Basketball/Tracking/"
js<-fromJSON(paste0(path,"markings/",gameid,".json"))
players<-fread(paste0(path,"meta/players_plus.csv"))
frames<-fread(paste0(path,"frames/",gameid,".csv"))


# Add markings
    # From shots
    shots<-as.data.table(js$shots)
    blocks<-shots[blocked==TRUE]
    blocks<-blocks[,frame_idx:=frame_idx+1]
    blocks[,event:="BLK"]
    blocks<-blocks[,.(period,frame_idx,event,player_id,dplayer_id)]
    shots[,event:=ifelse(sfl_fgx==T,"SF",
                    ifelse(made==TRUE,ifelse(is_three==TRUE,"3PM","2PM"),
                        ifelse(is_three==TRUE,"3PX","2PX")))]
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
    
    tov<-as.data.table(js$touches_all)
    tov[,event:="TO"]
    tov<-tov[outcomes%in%"TOV",.(period,end_frame,event,toucher_id,defender_id)]
    setnames(tov,c("end_frame","toucher_id","defender_id"),
                 c("frame","player_id","dplayer_id"))
    
    # From dribbles
    dribbles<-as.data.table(js$dribbles)
    dribbles[,event:="DRIB"][,dplayer_id:=NA]
    dribbles<-dribbles[,.(period,frame,event,ballhandler_id,dplayer_id)]
    setnames(dribbles,"ballhandler_id","player_id")

    # From fouls
    fouls<-as.data.table(js$fouls)
    fouls[,event:="FOUL"]
    fouls<-fouls[,.(period,frame,event,fouled,fouler)]
    setnames(fouls,c("fouled","fouler"),c("player_id","dplayer_id"))
    
    #From rebounds
    rebounds<-as.data.table(js$rebounds)
    rebounds[,event:=ifelse(rebounder%in%c(off_player_0_id,off_player_1_id,
                                           off_player_2_id,off_player_3_id,
                                           off_player_4_id),"ORB","DRB")]
    rebounds[,frame:=substr(id,14,20)][,player_id:=rebounder][,dplayer_id:=NA]
    rebounds<-rebounds[,.(period,frame,event,player_id,dplayer_id)]
    
    
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
    
    # From transitions
    fb<-as.data.table(js$transitions)
    fb<-fb[,frame:=frame-1]
    fb[,event:="FB"]
    fb<-fb[,.(period,frame,event,off_player1,def_player1)]
    setnames(fb,c("off_player1","def_player1"),c("player_id","dplayer_id"))
    
    # From pbp
    other<-as.data.table(js$pbp)
    other[,event:=ifelse(event=="FTM"&(str_count(text,"1 of 1")|
                                       str_count(text,"2 of 2")|
                                       str_count(text,"3 of 3")),"FTML",event)]
    other[,frame:=ifelse(event%in%c("TO","FTML","DRB","ORB"),
                  substr(shift(possession_id,type="lead"),13,50),
                  substr(possession_id,13,50))][,player_id:=NA][,dplayer_id:=NA]
    other<-other[event%in%c("TO","FTML","DRB","ORB","SUB","TMO","JMP","SPD","EPD")]
    other<-other[,.(period,frame,event,player_id,dplayer_id)]

# Clean markings
markings<-rbind(markings,passes,touches,dribbles,fouls,#rebounds,
                picks,drives,fb,other)
markings<-markings[order(period, frame)]
markings[,mid:=paste0(period,"_",frame)]
markings[,order:=ifelse(event%in%c("DRIB","POSS","PASS"),2,1)]
markings<-markings[order(order)]
markings<-distinct(markings,mid,.keep_all=T)
markings<-markings[,.(mid,event,player_id,dplayer_id)]

# # Map NBA ids to markings
ids<-players[,.(id,ids_id)]
ids<-ids[!is.na(id)]
setnames(ids,"id","player_id")
markings<-markings[,lapply(.SD, as.character)]
markings<-left_join(markings,ids,by="player_id");setDT(markings)
markings<-select(markings,-player_id)
setnames(markings,"ids_id","player_id")
setnames(ids,"player_id","dplayer_id")
markings<-left_join(markings,ids,by="dplayer_id");setDT(markings)
markings<-select(markings,-dplayer_id)
setnames(markings,"ids_id","dplayer_id")
markings$player_id<-as.numeric(markings$player_id)
markings$dplayer_id<-as.numeric(markings$dplayer_id)

# Merge markings onto frames
frames[,mid:=paste0(period,"_",idx)]
frames<-left_join(frames,markings,by="mid");setDT(frames)
frames<-select(frames,-home_team,-away_team,-mid)

# Remove unnecessary dataframes
rm(list= ls()[!(ls() %in% c('frames','markings','js','players'))])