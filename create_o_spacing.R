frames_g<-frames[pdist(bh_x,ifelse(pend==1,4,90),bh_y,25)<25]
frames_g[,hoop_x:=ifelse(pend==1,4,90)]
frames_g[,hoop_y:=25]

#Defender Distance
for(i in c("ap1","ap2","ap3","ap4","ap5","hp1","hp2","hp3","hp4","hp5")){
    frames_g[,(paste0("ddist",i)):=
      ifelse(bh%in%c(ap1,ap2,ap3,ap4,ap5)&str_count(i,"a"),
          pdist(frames_g[[paste0(i,"_x")]],frames_g[[paste0(gsub("p","d",i),"_x")]],
                frames_g[[paste0(i,"_y")]],frames_g[[paste0(gsub("p","d",i),"_y")]]),
      ifelse(bh%in%c(hp1,hp2,hp3,hp4,hp5)&str_count(i,"h"),
          pdist(frames_g[[paste0(i,"_x")]],frames_g[[paste0(gsub("p","d",i),"_x")]],
                frames_g[[paste0(i,"_y")]],frames_g[[paste0(gsub("p","d",i),"_y")]]),NA))]
}

#HOOP DIST
for(i in c("ap1","ap2","ap3","ap4","ap5","hp1","hp2","hp3","hp4","hp5")){
    frames_g[,(paste0("hdist",i)):=
      ifelse(bh%in%c(ap1,ap2,ap3,ap4,ap5)&str_count(i,"a"),
        pdist(frames_g[[paste0(i,"_x")]],hoop_x,frames_g[[paste0(i,"_y")]],hoop_y),
      ifelse(bh%in%c(hp1,hp2,hp3,hp4,hp5)&str_count(i,"h"),
        pdist(frames_g[[paste0(i,"_x")]],hoop_x,frames_g[[paste0(i,"_y")]],hoop_y),NA))]
}

#BALL DIST
for(i in c("ap1","ap2","ap3","ap4","ap5","hp1","hp2","hp3","hp4","hp5")){
    frames_g[,(paste0("bdist",i)):=
      ifelse(bh%in%c(ap1,ap2,ap3,ap4,ap5)&str_count(i,"a"),
        pdist(frames_g[[paste0(i,"_x")]],ball_x,frames_g[[paste0(i,"_y")]],ball_y),
      ifelse(bh%in%c(hp1,hp2,hp3,hp4,hp5)&str_count(i,"h"),
        pdist(frames_g[[paste0(i,"_x")]],ball_x,frames_g[[paste0(i,"_y")]],ball_y),NA))]
}

gravity<-data.frame(c(frames_g$hp1,frames_g$hp2,frames_g$hp3,frames_g$hp4,
                      frames_g$hp5,frames_g$ap1,frames_g$ap2,frames_g$ap3,
                      frames_g$ap4,frames_g$ap5),
                    c(frames_g$ddisthp1,frames_g$ddisthp2,frames_g$ddisthp3,
                      frames_g$ddisthp4,frames_g$ddisthp5,frames_g$ddistap1,
                      frames_g$ddistap2,frames_g$ddistap3,frames_g$ddistap4,
                      frames_g$ddistap5),
                    c(frames_g$hdisthp1,frames_g$hdisthp2,frames_g$hdisthp3,
                      frames_g$hdisthp4,frames_g$hdisthp5,frames_g$hdistap1,
                      frames_g$hdistap2,frames_g$hdistap3,frames_g$hdistap4,
                      frames_g$hdistap5),
                    c(frames_g$bdisthp1,frames_g$bdisthp2,frames_g$bdisthp3,
                      frames_g$bdisthp4,frames_g$bdisthp5,frames_g$bdistap1,
                      frames_g$bdistap2,frames_g$bdistap3,frames_g$bdistap4,
                      frames_g$bdistap5))
names(gravity)<-c("player_id","ddist","hdist","bdist")
setDT(gravity)
gravity[,ddist:=mean(ddist,na.rm=T),by="player_id"]
gravity[,hdist:=mean(hdist,na.rm=T),by="player_id"]
gravity[,bdist:=mean(bdist,na.rm=T),by="player_id"]
gravity<-distinct(gravity,player_id,.keep_all = T)
gravity$player_id<-as.character(gravity$player_id)

# PUS: define
frames[,pus:=
      # If there isn't a stoppage in time
      ifelse(gameClock!=shift(gameClock)&
        # If there wasn't a shot in the last 5 seconds
        !Reduce("|",shift(event%in%c("3PX","2PX","3PM","2PM","SF","TO","FTML"),0:200))&
        # And there isn't a shot in the next second
        !Reduce("|",shift(event%in%c("2PM","2PX","3PM","3PX","SF","TO"),0:60,type="lead"))&
        # And the ball is within 24 feet of the goal
        ((pend==1&bh_x<28)|(pend==0&bh_x>66))&
        #And the defender is over 8 feet away
        pdist(bh_x,bhd_x,bh_y,bhd_y)>10,
        # Then the ballhandler passed up the shot. Otherwise he didn't
        bh,0)]

# PUS: adjust for time
frames[,pus:=ifelse(shift(pus)==pus&shift(pus,2)==pus&shift(pus,3)==pus&
                    shift(pus,4)==pus&shift(pus,5)==pus,pus,0)]

# PUS: remove repeats
frames[,pus:=ifelse(Reduce("|",shift(pus!=0,1:60)),0,pus),]

#PUP: define
frames[,pup:=
  # If there isn't a stoppage in time
  ifelse(gameClock!=shift(gameClock)&
    # And bh is within 25 feet of hoop
    pdist(bh_x,ifelse(pend==1,4,90),bh_y,25)<25&
    # And a teammate is open
    (pdist(ap1_x,ad1_x,ap1_y,ad1_y)>10&bh!=ap1|
     pdist(ap2_x,ad2_x,ap2_y,ad2_y)>10&bh!=ap2|
     pdist(ap3_x,ad3_x,ap3_y,ad3_y)>10&bh!=ap3|
     pdist(ap4_x,ad4_x,ap4_y,ad4_y)>10&bh!=ap4|
     pdist(ap5_x,ad5_x,ap5_y,ad5_y)>10&bh!=ap5)&
    # And there isn't a pass in the next second
    !Reduce("|",shift(event%in%c("PASS"),0:60,type="lead")),
    # Then bh passed up a pass. Otherwise he didn't.
    bh,0)]

# PUP: only keep occurences that lasted 2+ seconds
frames[,pup:=ifelse(Reduce("&",shift(pup!=0,1:50)),pup,0),]
# PUP: remove repeats
frames[,pup:=ifelse(Reduce("|",shift(pup!=0,1:75)),0,pup),]

# Create Markings
bst<-frames[pup!=0|pus!=0]
bst[,ball_stop_type:=ifelse(pup!=0,"PUP",ifelse(pus!=0,"PUS",NA))]
bst[,player_id:=ifelse(pus==0,pup,pus)]
bst<-bst[,.(game_code,idx,period,gameClock,player_id,bhd,ball_stop_type)]
setnames(bst,c("idx","bhd"),c("frame","dplayer_id"))
#bst2<-toJSON(bst)
#markings_plus<-paste0(markings_plus,'"spacing": ',bst2,",")

bst[,TL:=paste(floor(gameClock/60),".",gameClock%%60,sep="")]
bst$player_id<-as.character(bst$player_id)
ap<-select(players,ids_id,james_id)
setnames(ap,c("player_id","player"))
bst<-left_join(bst,ap,by="player_id");setDT(bst)
chances<-as.data.table(js$chances)
chances<-chances[,.(period,start_frame,id,possession_id)]
setnames(chances,c("period","frame","chance_id","possession_id"))
bst<-full_join(bst,chances,by=c("frame","period"));setDT(bst)
bst<-arrange(bst,period,frame);setDT(bst)
bst<-na.locf(bst)
bst<-bst[!is.na(game_code)]
bst<-distinct(bst,gameClock,.keep_all=T)
bst[,id:=paste0(game_code,"_",period,"_",frame)]
bst[,season:=ifelse(as.numeric(substr(gameid,5,6))>7,
                    as.numeric(substr(gameid,1,4)),
                    as.numeric(substr(gameid,1,4))-1)]
bst<-bst[,.(id,season,game_code,period,frame,gameClock,TL,possession_id,chance_id,
            ball_stop_type,player_id,player,dplayer_id)]

# Write to file
write.csv(bst,paste0("C:/Users/brocatoj/Documents/Basketball/Tracking/j_markings/",
          gameid,"_spacing.csv"),row.names=F)

# Remove unnecessary dataframes
rm(list= ls()[!(ls() %in% c('gameid','frames','markings','js','players','pdist',
                           'bst','gravity'))])