frames_fb<-frames[Reduce("|",shift(event=="FB",0:80,type="lead"))]
frames_fb[,pend:=ifelse(shift(event=="FB",80,type="lead"),
                        shift(pend,90,type="lead"),pend)]
fb<-as.data.table(js$transitions)
ap<-players[,.(id,ids_id)]
setnames(ap,c("off_player1","off_player1_id"))
fb$off_player1<-as.character(fb$off_player1)
fb<-left_join(fb,ap,by="off_player1");setDT(fb)
setnames(fb,c("off_player1","off_player1_id"),c("off_player1_id","off_player1"))
fb[,frame:=frame-1]
fb[,mid:=paste0(period,"_",frame)]
fb<-fb[,.(mid,off_player1)]
frames_fb[,mid:=paste0(period,"_",idx)]
frames_fb<-left_join(frames_fb,fb,by="mid")
frames_fb<-select(frames_fb,-mid)
setDT(frames_fb)
frames_fb[,off_player1:=na.locf(off_player1,fromLast = T)]
#frames_fb<-frames[(bh_x>56&bh_x<58)|(bh_x>36&bh_x<38)]
frames_fb<-frames_fb[shift(event=="FB",90,type="lead")]
frames_fb[,bit:=""][,bbb:=""]


# BIT: define
for(i in c("ap1","ap2","ap3","ap4","ap5","hp1","hp2","hp3","hp4","hp5")){
    # Define "x" to save space
    eks<-frames_fb[[paste0(i,"_x")]]
    frames_fb[,bit:=
      # If hoop is on RIGHT and offense is away and the loop is on home
      ifelse(pend!=1&off_player1%in%c(ap1,ap2,ap3,ap4,ap5)&str_count(i,"h"),
        # If an away player beat the home player
        ifelse((((hp1_x>eks)+(hp2_x>eks)+(hp3_x>eks)+(hp4_x>eks)+(hp5_x>eks))-
                ((ap1_x>eks)+(ap2_x>eks)+(ap3_x>eks)+(ap4_x>eks)+(ap5_x>eks)))<0,
        # Then he was beat in transition. Otherwise he wasn't.
        paste(bit,frames_fb[[i]]),bit),
      # If hoop is on RIGHT and bh is home
      ifelse(pend!=1&off_player1%in%c(hp1,hp2,hp3,hp4,hp5)&str_count(i,"a"),
        # If a home player beat the away player
        ifelse((((hp1_x>eks)+(hp2_x>eks)+(hp3_x>eks)+(hp4_x>eks)+(hp5_x>eks))-
                ((ap1_x>eks)+(ap2_x>eks)+(ap3_x>eks)+(ap4_x>eks)+(ap5_x>eks)))>0,
        # Then he was beat in transition. Otherwise he wasn't
        paste(bit,frames_fb[[i]]),bit),
     # If hoop is on LEFT and bh gets in front court and bh is away
     ifelse(pend==1&off_player1%in%c(ap1,ap2,ap3,ap4,ap5)&str_count(i,"h"),
       # If an away player beat the home player
       ifelse((((hp1_x>eks)+(hp2_x>eks)+(hp3_x>eks)+(hp4_x>eks)+(hp5_x>eks))-
               ((ap1_x>eks)+(ap2_x>eks)+(ap3_x>eks)+(ap4_x>eks)+(ap5_x>eks)))>0,
       # Then he was beat in transition. Otherwise he wasn't.
       paste(bit,frames_fb[[i]]),bit),
     # IF the hoop is on the LEFT and bh gets i front court and bh is home
     ifelse(pend==1&off_player1%in%c(hp1,hp2,hp3,hp4,hp5)&str_count(i,"a"),
      # If a home player beat the away player
      ifelse((((hp1_x>eks)+(hp2_x>eks)+(hp3_x>eks)+(hp4_x>eks)+(hp5_x>eks))-
              ((ap1_x>eks)+(ap2_x>eks)+(ap3_x>eks)+(ap4_x>eks)+(ap5_x>eks)))<0,
      # Then he was beat in transition. Otherwise he wasn't.
      paste(bit,frames_fb[[i]]),bit),bit))))]
    # Define Beat by Ball
    frames_fb[,bbb:=
      ifelse((pend==1&off_player1%in%c(hp1,hp2,hp3,hp4,hp5)&str_count(i,"a")&ball_x<eks)|
             (pend==1&off_player1%in%c(ap1,ap2,ap3,ap4,ap5)&str_count(i,"h")&ball_x<eks)|
             (pend!=1&off_player1%in%c(hp1,hp2,hp3,hp4,hp5)&str_count(i,"a")&ball_x>eks)|
             (pend!=1&off_player1%in%c(ap1,ap2,ap3,ap4,ap5)&str_count(i,"h")&ball_x>eks),
             paste0(bbb," ",frames_fb[[i]]),bbb)]
}

fb<-frames_fb[,.(game_code,idx,period,gameClock,event,bit,bbb)]
#fb[,TL:=paste(floor(gameClock/60),".",gameClock%%60,sep="")]

# fb[,bit:=shift(bit,fill="")]
# fb[,bbb:=shift(bbb,fill="")]
# fb<-fb[event=="FB"]
# trans<-as.data.table(js$transitions)
# trans<-trans[order(period,frame)]
# trans[,mid:=paste0(period,"_",frame)]
# fb[,mid:=paste0(period,"_",idx)]
# fb<-fb[,.(mid,bit,bbb)]
# trans<-left_join(trans,fb,by="mid");setDT(trans)
# for(i in 1:5){
#     trans[,(paste0("def_player_",i,"_beat_in_transition")):=
#               ifelse(str_count(bit,as.character(trans[[paste0("def_player_",i,"_id")]])),T,F)]
#     trans[,(paste0("def_player_",i,"_beat_by_ball")):=
#               ifelse(str_count(bbb,as.character(trans[[paste0("def_player_",i,"_id")]])),T,F)]
# }
#trans<-toJSON(trans)
#markings_plus<-paste0(markings_plus,'"transition_plus": ',trans,"}")