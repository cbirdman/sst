library(plyr)
library(dplyr)
library(stringr)
library(zoo)
library(data.table)
source("functions.R")


for(i in c("hp1","hp2","hp3","hp4","hp5")){
    frames[,d1:=pdist(frames[[paste0(i,"_x")]],ap1_x,frames[[paste0(i,"_y")]],ap1_y)]
    frames[,d2:=pdist(frames[[paste0(i,"_x")]],ap2_x,frames[[paste0(i,"_y")]],ap2_y)]
    frames[,d3:=pdist(frames[[paste0(i,"_x")]],ap3_x,frames[[paste0(i,"_y")]],ap3_y)]
    frames[,d4:=pdist(frames[[paste0(i,"_x")]],ap4_x,frames[[paste0(i,"_y")]],ap4_y)]
    frames[,d5:=pdist(frames[[paste0(i,"_x")]],ap5_x,frames[[paste0(i,"_y")]],ap5_y)]
    frames[,(gsub("p","d",i)):=
      ifelse(d1==pmin(d1,d2,d3,d4,d5),ap1,ifelse(d2==pmin(d1,d2,d3,d4,d5),ap2,
      ifelse(d3==pmin(d1,d2,d3,d4,d5),ap3,ifelse(d4==pmin(d1,d2,d3,d4,d5),ap4,
      ifelse(d5==pmin(d1,d2,d3,d4,d5),ap5,NA)))))]
}
for(i in c("ap1","ap2","ap3","ap4","ap5")){
    frames[,d1:=pdist(frames[[paste0(i,"_x")]],hp1_x,frames[[paste0(i,"_y")]],hp1_y)]
    frames[,d2:=pdist(frames[[paste0(i,"_x")]],hp2_x,frames[[paste0(i,"_y")]],hp2_y)]
    frames[,d3:=pdist(frames[[paste0(i,"_x")]],hp3_x,frames[[paste0(i,"_y")]],hp3_y)]
    frames[,d4:=pdist(frames[[paste0(i,"_x")]],hp4_x,frames[[paste0(i,"_y")]],hp4_y)]
    frames[,d5:=pdist(frames[[paste0(i,"_x")]],hp5_x,frames[[paste0(i,"_y")]],hp5_y)]
    frames[,(gsub("p","d",i)):=
      ifelse(d1==pmin(d1,d2,d3,d4,d5),hp1,ifelse(d2==pmin(d1,d2,d3,d4,d5),hp2,
      ifelse(d3==pmin(d1,d2,d3,d4,d5),hp3,ifelse(d4==pmin(d1,d2,d3,d4,d5),hp4,
      ifelse(d5==pmin(d1,d2,d3,d4,d5),hp5,NA)))))]
}
frames<-frames[,c(1:41,47:56),with=F]

# Set new columns to zero
for(i in c("ad1_x","ad1_y","ad2_x","ad2_y","ad3_x","ad3_y","ad4_x","ad4_y","ad5_x","ad5_y",
           "hd1_x","hd1_y","hd2_x","hd2_y","hd3_x","hd3_y","hd4_x","hd4_y","hd5_x","hd5_y")){
    frames[,(i):=0]
}

# Identify coordinates for defender of each player
for(i in c("ap1","ap2","ap3","ap4","ap5","hp1","hp2","hp3","hp4","hp5")){
    for(d in c("ad1","ad2","ad3","ad4","ad5","hd1","hd2","hd3","hd4","hd5")){
        frames[,(paste0(d,"_x")):=
                ifelse(frames[[i]]==frames[[d]],
                       frames[[paste0(i,"_x")]],frames[[paste0(d,"_x")]])]
        frames[,(paste0(d,"_y")):=
                ifelse(frames[[i]]==frames[[d]],
                       frames[[paste0(i,"_y")]],frames[[paste0(d,"_y")]])]
    }
}

# Identify shooter,passer,ballhandler for each frame
frames[,shooter:=ifelse(event%in%c("2PM","2PX","3PM","3PX","SF"),player_id,NA)]
frames[,shooter:=na.locf(shooter,fromLast = T)]
frames[,passer:=ifelse(idx==1,1,ifelse(event=="PASS",player_id,NA))]
frames[,passer:=na.locf(passer)]
frames[,bh:=ifelse(idx==1|idx==nrow(frames),0,
              ifelse(!is.na(event)&
                     !event%in%c("ORB","DRB","DRIB","POSS","SCR","DHO","DRV"),0,
                   ifelse(event%in%c("ORB","DRB","POSS"),player_id,NA)))]
frames[,bh:=na.locf(bh)]

# Set new columns to zero
for(i in c("defender","bhd","s_x","s_y","d_x","d_y","bh_x","bh_y",
           "bhd_x","bhd_y","obh_x","obh_y","obhd_x","obhd_y","obhd")){
    frames[,(i):=0]
}

# Identify Defender of eventual shot and Ballhandler defender at all times
for(i in c("ap1","ap2","ap3","ap4","ap5","hp1","hp2","hp3","hp4","hp5")){
    frames[,defender:=ifelse(shooter==frames[[i]],frames[[gsub("p","d",i)]],defender)]
    frames[,bhd:=ifelse(bh==0,0,ifelse(bh==frames[[i]],frames[[gsub("p","d",i)]],bhd))]
}

# Identify coordinates of shooter, defender, ballhandler, etc.
for(i in c("ap1","ap2","ap3","ap4","ap5","hp1","hp2","hp3","hp4","hp5")){
    frames[,s_x:=ifelse(shooter==frames[[i]],frames[[paste0(i,"_x")]],s_x)]
    frames[,s_y:=ifelse(shooter==frames[[i]],frames[[paste0(i,"_y")]],s_y)]
    frames[,d_x:=ifelse(defender==frames[[i]],frames[[paste0(i,"_x")]],d_x)]
    frames[,d_y:=ifelse(defender==frames[[i]],frames[[paste0(i,"_y")]],d_y)]
    frames[,bh_x:=ifelse(bh==frames[[i]],frames[[paste0(i,"_x")]],bh_x)]
    frames[,bh_y:=ifelse(bh==frames[[i]],frames[[paste0(i,"_y")]],bh_y)]
    frames[,bhd_x:=ifelse(bhd==frames[[i]],frames[[paste0(i,"_x")]],bhd_x)]
    frames[,bhd_y:=ifelse(bhd==frames[[i]],frames[[paste0(i,"_y")]],bhd_y)]
    frames[,obh_x:=ifelse(bh==frames[[i]],shift(frames[[paste0(i,"_x")]],25),obh_x)]
    frames[,obh_y:=ifelse(bh==frames[[i]],shift(frames[[paste0(i,"_y")]],25),obh_y)]
    frames[,obhd:=ifelse(bh==0,0,ifelse(bh==frames[[i]],
                                     shift(frames[[gsub("p","d",i)]],25),obhd))]
}

# Identify coordinates for "OLD" ballhandler defender for use in bbc
for(i in c("ap1","ap2","ap3","ap4","ap5","hp1","hp2","hp3","hp4","hp5")){
    frames[,obhd_x:=ifelse(obhd==frames[[i]],frames[[paste0(i,"_x")]],obhd_x)]
    frames[,obhd_y:=ifelse(obhd==frames[[i]],frames[[paste0(i,"_y")]],obhd_y)]
}

# Determine what side of the court the goal is on
frames[,pend:=ifelse(event%in%c("FTML","SF","2PM","3PM","2PX","3PX","DRB","TO",
                                "DRV"),
                ifelse(pdist(shift(ball_x),4,shift(ball_y),25)<
                       pdist(shift(ball_x),90,shift(ball_y),25),1,0),NA)]
frames[,pend:=na.locf(pend,fromLast=T)]