# GRAVITY: define
# for(i in c("ap1","ap2","ap3","ap4","ap5","hp1","hp2","hp3","hp4","hp5")){
#     frames[,(paste0("gravity",i)):=
#         ifelse(pend==1&bh_x<30|pend!=1&bh_x>64,
#             ifelse(bh%in%c(ap1,ap2,ap3,ap4,ap5)&str_count(i,"A"),
#               (pmax(pdist(frames[[paste0(i,"_x")]],Ball_x,frames[[paste0(i,"_y")]],Ball_y)+10,1)+
#               pmax((pdist(frames[[paste0(i,"_x")]],ifelse(pend==1,4,90),frames[[paste0(i,"_y")]],25))-10,1))/
#               pdist(frames[[paste0(i,"_x")]],frames[[paste0(gsub("P","D",i),"_x")]],
#                      frames[[paste0(i,"_y")]],frames[[paste0(gsub("P","D",i),"_y")]]),NA),
#             ifelse(bh%in%c(hp1,hp2,hp3,hp4,hp5)&str_count(i,"H"),
#               (pmax(pdist(frames[[paste0(i,"_x")]],Ball_x,frames[[paste0(i,"_y")]],Ball_y)+10,1)+
#                pmax((pdist(frames[[paste0(i,"_x")]],ifelse(pend==1,4,90),frames[[paste0(i,"_y")]],25))-10,1))/
#                      pdist(frames[[paste0(i,"_x")]],frames[[paste0(gsub("P","D",i),"_x")]],
#                            frames[[paste0(i,"_y")]],frames[[paste0(gsub("P","D",i),"_y")]]),NA))]
# }

# PUS: define
frames[,pus:=
      # If there isn't a stoppage in time
      ifelse(gameClock!=shift(gameClock)&
        # If there wasn't a shot in the last second
        !Reduce("|",shift(event%in%c("2PM","2PX","3PM","3PX","SF"),0:60))&
        # And there isn't a shot in the next second
        !Reduce("|",shift(event%in%c("2PM","2PX","3PM","3PX","SF"),0:60,type="lead"))&
        # And the ball is within 24 feet of the goal
        ((pend==1&bh_x<28)|(pend!=1&bh_x>66))&
        #And the defender is over 8 feet away
        pdist(bh_x,bhd_x,bh_y,bhd_y)>12,
        # Then the ballhandler passed up the shot. Otherwise he didn't
        bh,0)]

# PUS: adjust for time
frames[,pus:=ifelse(shift(pus)==pus&shift(pus,2)==pus&shift(pus,3)==pus&
                 shift(pus,4)==pus&shift(pus,5)==pus,pus,0)]

# PUS: remove repeats
frames[,pus:=ifelse(Reduce("|",shift(pus!=0,1:60)),0,pus),]
# frames[,ad1_x:=NULL][,ad2_x:=NULL][,ad3_x:=NULL][,ad4_x:=NULL][,ad5_x:=NULL]
# frames[,hd1_x:=NULL][,hd2_x:=NULL][,hd3_x:=NULL][,hd4_x:=NULL][,hd5_x:=NULL]
# frames[,ad1_y:=NULL][,ad2_y:=NULL][,ad3_y:=NULL][,ad4_y:=NULL][,ad5_y:=NULL]
# frames[,hd1_y:=NULL][,hd2_y:=NULL][,hd3_y:=NULL][,hd4_y:=NULL][,hd5_y:=NULL]


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
    pdist(ap5_x,ad5_x,ap5_y,ad5_y)>10&bh!=ap5),
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
bst<-toJSON(bst)
markings_plus<-paste0(markings_plus,'"spacing": ',bst,",")