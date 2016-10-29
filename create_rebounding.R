# REBOUNdING
frames[,box:=""][,crash:=""][,leak:=""][,bw:=""][,ora:=0][,go:=""]
for(i in c("ap1","ap2","ap3","ap4","ap5","hp1","hp2","hp3","hp4","hp5")){
    frames[,box:=
      # If there is a missed shot
      ifelse(event%in%c("2PX","3PX","FTX")&
        # and the player's team has a drb opportunity
        ((player_id%in%c(hp1,hp2,hp3,hp4,hp5)&str_count(i,"a"))|
         (player_id%in%c(ap1,ap2,ap3,ap4,ap5)&str_count(i,"h")))&
        # and the player's closest opponent is within 2 feet of him
        (pdist(shift(frames[[paste0(gsub("p","d",i),"_x")]],45,type="lead"),
              shift(frames[[paste0(i,"_x")]],45,type="lead"),
              shift(frames[[paste0(gsub("p","d",i),"_y")]],45,type="lead"),
              shift(frames[[paste0(i,"_y")]],45,type="lead"))<2|
        pdist(shift(frames[[paste0(gsub("p","d",i),"_x")]],30,type="lead"),
              shift(frames[[paste0(i,"_x")]],30,type="lead"),
              shift(frames[[paste0(gsub("p","d",i),"_y")]],30,type="lead"),
              shift(frames[[paste0(i,"_y")]],30,type="lead"))<2)&
        #and the opponent doesn't get the rebound
        !Reduce("|",shift(event=="ORB",1:120,type="lead"))&
        # and the player is within 15 feet of the hoop
        pdist(shift(frames[[paste0(i,"_x")]],30,type="lead"),ifelse(pend==1,4,90),
              shift(frames[[paste0(i,"_y")]],30,type="lead"),25)<15,
        # Then it's a boxout. Otherwise it isn't.
        paste(box,paste0("box",frames[[i]])),box)]
    frames[,crash:=
      # If there is a missed shot
      ifelse(event%in%c("2PX","3PX","FTX")&
        # and the player's team has a drb opportunity
         ((player_id%in%c(hp1,hp2,hp3,hp4,hp5)&str_count(i,"a"))|
          (player_id%in%c(ap1,ap2,ap3,ap4,ap5)&str_count(i,"h")))&box==""&
        # and dist between player and hoop seconds after shot is less than 8 ft
        (pdist(shift(frames[[paste0(i,"_x")]],45,type="lead"),ifelse(pend==1,4,90),
              shift(frames[[paste0(i,"_y")]],45,type="lead"),25)<8|
         pdist(shift(frames[[paste0(i,"_x")]],30,type="lead"),ifelse(pend==1,4,90),
              shift(frames[[paste0(i,"_y")]],30,type="lead"),25)<8|
         pdist(shift(frames[[paste0(i,"_x")]],60,type="lead"),ifelse(pend==1,4,90),
               shift(frames[[paste0(i,"_y")]],60,type="lead"),25)<8)&
        # and the player moved 2 ft toward the hoop in the last x seconds
        pdist(shift(frames[[paste0(i,"_x")]],45,type="lead"),frames[[paste0(i,"_x")]],
               shift(frames[[paste0(i,"_y")]],45,type="lead"),frames[[paste0(i,"_y")]])>2,
        # Then the player crashed
        paste(crash,paste0("crash",frames[[i]])),crash)]
    frames[,leak:=
      # If there is a missed shot
      ifelse(event%in%c("2PX","3PX","FTX")&
        # and the player's team has a drb opportunity
        ((player_id%in%c(hp1,hp2,hp3,hp4,hp5)&str_count(i,"a"))|
         (player_id%in%c(ap1,ap2,ap3,ap4,ap5)&str_count(i,"h")))&
        # and the away player is 26 feet from the hoop seconds after the shot
        ifelse(pend==1,shift(frames[[paste0(i,"_x")]],20,type="lead")>30,
         shift(frames[[paste0(i,"_x")]],20,type="lead")<60),
        # Then it's a leakout. Otherwise it's not.
        paste(leak,paste0("leak",frames[[i]])),leak)]
    frames[,bw:=
      # If there is a missed shot
      ifelse(event%in%c("2PX","3PX","FTX")&
        # and the player's team has a drb opportunity
        ((player_id%in%c(hp1,hp2,hp3,hp4,hp5)&str_count(i,"a"))|
         (player_id%in%c(ap1,ap2,ap3,ap4,ap5)&str_count(i,"h")))&
        # and there isn't a crash, leak, or box
        box==""&crash==""&leak==""&
        # and the away player didn't move much from 15 to 45
        pdist(shift(frames[[paste0(i,"_x")]],15,type="lead"),
              shift(frames[[paste0(i,"_x")]],45,type="lead"),
              shift(frames[[paste0(i,"_y")]],15,type="lead"),
              shift(frames[[paste0(i,"_y")]],45,type="lead"))<3&
        # and the away player is between 8 and 15 at +45
        pdist(ifelse(pend==1,4,90),
              shift(frames[[paste0(i,"_x")]],45,type="lead"),25,
              shift(frames[[paste0(i,"_y")]],45,type="lead"))>8,
        # Then it's a ballwatch
        paste(bw,paste0("bw",frames[[i]])),bw)]
    frames[,ora:=
      ifelse(event=="ORB",
        ifelse(player_id==frames[[i]],paste0("ora",shift(frames[[gsub("p","d",i)]],25)),
               ora),ora)]
    frames[,go:=
      # If there is a missed shot
      ifelse(event%in%c("2PX","3PX","FTX")&
        # and the player's team has an orb opportunity
        ((player_id%in%c(hp1,hp2,hp3,hp4,hp5)&str_count(i,"h"))|
         (player_id%in%c(ap1,ap2,ap3,ap4,ap5)&str_count(i,"a")))&
        (pdist(ifelse(pend==1,4,90),
        shift(frames[[paste0(i,"_x")]],45,type="lead"),25,
        shift(frames[[paste0(i,"_y")]],45,type="lead"))<8|
        pdist(ifelse(pend==1,4,90),
        shift(frames[[paste0(i,"_x")]],30,type="lead"),25,
        shift(frames[[paste0(i,"_y")]],30,type="lead"))<8),
        paste(go,paste0("go",frames[[i]])),go)]
}

reb<-frames[event%in%c("2PX","3PX","ORB","DRB")]
reb<-reb[,.(idx,event,box,crash,leak,bw,ora,go)]
reb[,box:=shift(box,fill="")][,crash:=shift(crash,fill="")]
reb[,leak:=shift(leak,fill="")][,bw:=shift(bw,fill="")][,go:=shift(go,fill="")]
reb<-reb[event%in%c("ORB","DRB")]
rebounds<-as.data.table(js$rebounds)
rebounds<-rebounds[order(period,-(rim_game_clock))]