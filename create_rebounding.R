# REBOUNDING
frames_reb[,box:=""][,crash:=""][,leak:=""][,bw:=""][,ora:=0][,go:=""]
for(i in c("ap1","ap2","ap3","ap4","ap5","hp1","hp2","hp3","hp4","hp5")){
    frames_reb[,box:=
      # If there is a missed shot
      ifelse(event%in%c("2PX","3PX")&
        # and the player's team has a drb opportunity
        ((player_id%in%c(hp1,hp2,hp3,hp4,hp5)&str_count(i,"a"))|
         (player_id%in%c(ap1,ap2,ap3,ap4,ap5)&str_count(i,"h")))&
        # and the player's closest opponent is within 2 feet of him
        (pdist(shift(frames_reb[[paste0(gsub("p","d",i),"_x")]],45,type="lead"),
              shift(frames_reb[[paste0(i,"_x")]],45,type="lead"),
              shift(frames_reb[[paste0(gsub("p","d",i),"_y")]],45,type="lead"),
              shift(frames_reb[[paste0(i,"_y")]],45,type="lead"))<2|
        pdist(shift(frames_reb[[paste0(gsub("p","d",i),"_x")]],30,type="lead"),
              shift(frames_reb[[paste0(i,"_x")]],30,type="lead"),
              shift(frames_reb[[paste0(gsub("p","d",i),"_y")]],30,type="lead"),
              shift(frames_reb[[paste0(i,"_y")]],30,type="lead"))<2)&
        #and the opponent doesn't get the rebound
        #!Reduce("|",shift(event=="ORB",1:120,type="lead"))&
        # and the player is within 15 feet of the hoop
        pdist(shift(frames_reb[[paste0(i,"_x")]],30,type="lead"),ifelse(pend==1,4,90),
              shift(frames_reb[[paste0(i,"_y")]],30,type="lead"),25)<15,
        # Then it's a boxout. Otherwise it isn't.
        paste(box,paste0("box",frames_reb[[i]])),box)]
    frames_reb[,crash:=
      # If there is a missed shot
      ifelse(event%in%c("2PX","3PX")&
        # and the player's team has a drb opportunity
         ((player_id%in%c(hp1,hp2,hp3,hp4,hp5)&str_count(i,"a"))|
          (player_id%in%c(ap1,ap2,ap3,ap4,ap5)&str_count(i,"h")))&box==""&
        # and dist between player and hoop seconds after shot is less than 8 ft
        (pdist(shift(frames_reb[[paste0(i,"_x")]],45,type="lead"),ifelse(pend==1,4,90),
              shift(frames_reb[[paste0(i,"_y")]],45,type="lead"),25)<8|
         pdist(shift(frames_reb[[paste0(i,"_x")]],30,type="lead"),ifelse(pend==1,4,90),
              shift(frames_reb[[paste0(i,"_y")]],30,type="lead"),25)<8|
         pdist(shift(frames_reb[[paste0(i,"_x")]],60,type="lead"),ifelse(pend==1,4,90),
               shift(frames_reb[[paste0(i,"_y")]],60,type="lead"),25)<8)&
        # and the player moved 2 ft toward the hoop in the last x seconds
        pdist(shift(frames_reb[[paste0(i,"_x")]],45,type="lead"),frames_reb[[paste0(i,"_x")]],
               shift(frames_reb[[paste0(i,"_y")]],45,type="lead"),frames_reb[[paste0(i,"_y")]])>2,
        # Then the player crashed
        paste(crash,paste0("crash",frames_reb[[i]])),crash)]
    frames_reb[,leak:=
      # If there is a missed shot
      ifelse(event%in%c("2PX","3PX")&
        # and the player's team has a drb opportunity
        ((player_id%in%c(hp1,hp2,hp3,hp4,hp5)&str_count(i,"a"))|
         (player_id%in%c(ap1,ap2,ap3,ap4,ap5)&str_count(i,"h")))&
        # and the away player is 26 feet from the hoop seconds after the shot
        ifelse(pend==1,shift(frames_reb[[paste0(i,"_x")]],20,type="lead")>30,
         shift(frames_reb[[paste0(i,"_x")]],20,type="lead")<60),
        # Then it's a leakout. Otherwise it's not.
        paste(leak,paste0("leak",frames_reb[[i]])),leak)]
    frames_reb[,bw:=
      # If there is a missed shot
      ifelse(event%in%c("2PX","3PX")&
        # and the player's team has a drb opportunity
        ((player_id%in%c(hp1,hp2,hp3,hp4,hp5)&str_count(i,"a"))|
         (player_id%in%c(ap1,ap2,ap3,ap4,ap5)&str_count(i,"h")))&
        # and there isn't a crash, leak, or box
        box==""&crash==""&leak==""&
        # and the away player didn't move much from 15 to 45
        pdist(shift(frames_reb[[paste0(i,"_x")]],15,type="lead"),
              shift(frames_reb[[paste0(i,"_x")]],45,type="lead"),
              shift(frames_reb[[paste0(i,"_y")]],15,type="lead"),
              shift(frames_reb[[paste0(i,"_y")]],45,type="lead"))<3&
        # and the away player is between 8 and 15 at +45
        pdist(ifelse(pend==1,4,90),
              shift(frames_reb[[paste0(i,"_x")]],45,type="lead"),25,
              shift(frames_reb[[paste0(i,"_y")]],45,type="lead"))>8,
        # Then it's a ballwatch
        paste(bw,paste0("bw",frames_reb[[i]])),bw)]
    frames_reb[,ora:=
      ifelse(event=="ORB",
        ifelse(player_id==frames_reb[[i]],paste0("ora",obhd),
               ora),ora)]
}

reb<-frames_reb[event%in%c("2PX","3PX","ORB","DRB")]
reb<-reb[,.(idx,period,gameClock,event,box,crash,leak,bw,ora)]
reb[,box:=shift(box,fill="")][,crash:=shift(crash,fill="")]
reb[,leak:=shift(leak,fill="")][,bw:=shift(bw,fill="")]
reb<-reb[event%in%c("ORB","DRB")]
reb[,box:=ifelse(event=="ORB","",box)]
rebounds<-as.data.table(js$rebounds)
rebounds<-rebounds[order(period,-(rebound_game_clock))]
rebounds[,pid:=paste0(period,"_",rebound_game_clock)]
reb[,pid:=paste0(period,"_",gameClock)]
reb<-reb[,.(pid,idx,event,box,crash,leak,bw,ora)]
rebounds<-left_join(rebounds,reb,by="pid");setDT(rebounds)
ap<-players[!is.na(id),.(id,ids_id)]
ap$id<-as.numeric(ap$id)
for(i in 0:4){
    setnames(ap,c(paste0("def_player_",i,"_id"),paste0("def_player_",i,"_id2")))
    rebounds<-left_join(rebounds,ap,by=paste0("def_player_",i,"_id"))
    setnames(ap,c(paste0("off_player_",i,"_id"),paste0("off_player_",i,"_id2")))
    rebounds<-left_join(rebounds,ap,by=paste0("off_player_",i,"_id"))
    setDT(rebounds)
    rebounds[,(paste0("def_player_",i,"_id")):=NULL]
    rebounds[,(paste0("off_player_",i,"_id")):=NULL]
    setnames(rebounds,paste0("def_player_",i,"_id2"),paste0("def_player_",i,"_id"))
    setnames(rebounds,paste0("off_player_",i,"_id2"),paste0("off_player_",i,"_id"))
}


for(i in 0:4){
    rebounds[,(paste0("def_player_",i,"_box_out")):=
                 ifelse(str_count(box,rebounds[[paste0("def_player_",i,"_id")]]),T,F)]
    rebounds[,(paste0("def_player_",i,"_crash")):=
                 ifelse(str_count(crash,rebounds[[paste0("def_player_",i,"_id")]]),T,F)]
    rebounds[,(paste0("def_player_",i,"_leak_out")):=
                 ifelse(str_count(leak,rebounds[[paste0("def_player_",i,"_id")]]),T,F)]
    rebounds[,(paste0("def_player_",i,"_ball_watch")):=
                 ifelse(str_count(bw,rebounds[[paste0("def_player_",i,"_id")]]),T,F)]
    rebounds[,(paste0("def_player_",i,"_offensive_rebound_allowed")):=
                 ifelse(str_count(ora,rebounds[[paste0("def_player_",i,"_id")]]),T,F)]
}

rebounds<-rebounds %>%
    select(id,chance_id,season,period,idx,event,rim_game_clock,rebound_game_clock,
           rebounded,rebounder,rebound_x,rebound_y,
           off_player_0_id,off_player_1_id,off_player_2_id,off_player_3_id,off_player_4_id,
           off_player_0_had_opportunity,off_player_1_had_opportunity,off_player_2_had_opportunity,off_player_3_had_opportunity,off_player_4_had_opportunity,
           off_player_0_crash,off_player_1_crash,off_player_2_crash,off_player_3_crash,off_player_4_crash,
           off_player_0_rb_pct_shot,off_player_1_rb_pct_shot,off_player_2_rb_pct_shot,off_player_3_rb_pct_shot,off_player_4_rb_pct_shot,
           off_player_0_shot_loc_x,off_player_1_shot_loc_x,off_player_2_shot_loc_x,off_player_3_shot_loc_x,off_player_4_shot_loc_x,
           off_player_0_shot_loc_y,off_player_1_shot_loc_y,off_player_2_shot_loc_y,off_player_3_shot_loc_y,off_player_4_shot_loc_y,
           off_player_0_rb_pct_rim,off_player_1_rb_pct_rim,off_player_2_rb_pct_rim,off_player_3_rb_pct_rim,off_player_4_rb_pct_rim,
           off_player_0_rebound_loc_x,off_player_1_rebound_loc_x,off_player_2_rebound_loc_x,off_player_3_rebound_loc_x,off_player_4_rebound_loc_x,
           off_player_0_rebound_loc_y,off_player_1_rebound_loc_y,off_player_2_rebound_loc_y,off_player_3_rebound_loc_y,off_player_4_rebound_loc_y,
           def_player_0_id,def_player_1_id,def_player_2_id,def_player_3_id,def_player_4_id,
           def_player_0_had_opportunity,def_player_1_had_opportunity,def_player_2_had_opportunity,def_player_3_had_opportunity,def_player_4_had_opportunity,
           def_player_0_crash,def_player_1_crash,def_player_2_crash,def_player_3_crash,def_player_4_crash,
           def_player_0_box_out,def_player_1_box_out,def_player_2_box_out,def_player_3_box_out,def_player_4_box_out,
           def_player_0_leak_out,def_player_1_leak_out,def_player_2_leak_out,def_player_3_leak_out,def_player_4_leak_out,
           def_player_0_ball_watch,def_player_1_ball_watch,def_player_2_ball_watch,def_player_3_ball_watch,def_player_4_ball_watch,
           def_player_0_offensive_rebound_allowed,def_player_1_offensive_rebound_allowed,def_player_2_offensive_rebound_allowed,def_player_3_offensive_rebound_allowed,def_player_4_offensive_rebound_allowed,
           def_player_0_rb_pct_shot,def_player_1_rb_pct_shot,def_player_2_rb_pct_shot,def_player_3_rb_pct_shot,def_player_4_rb_pct_shot,
           def_player_0_shot_loc_x,def_player_1_shot_loc_x,def_player_2_shot_loc_x,def_player_3_shot_loc_x,def_player_4_shot_loc_x,
           def_player_0_shot_loc_y,def_player_1_shot_loc_y,def_player_2_shot_loc_y,def_player_3_shot_loc_y,def_player_4_shot_loc_y,
           def_player_0_rb_pct_rim,def_player_1_rb_pct_rim,def_player_2_rb_pct_rim,def_player_3_rb_pct_rim,def_player_4_rb_pct_rim,
           def_player_0_rebound_loc_x,def_player_1_rebound_loc_x,def_player_2_rebound_loc_x,def_player_3_rebound_loc_x,def_player_4_rebound_loc_x,
           def_player_0_rebound_loc_y,def_player_1_rebound_loc_y,def_player_2_rebound_loc_y,def_player_3_rebound_loc_y,def_player_4_rebound_loc_y)

# Write to file
write.csv(rebounds,paste0("C:/Users/brocatoj/Documents/Basketball/Tracking/j_markings/",
          gameid,"_rebounds.csv"),row.names=F)

# Create JSON file if necessary
#rebounds<-toJSON(rebounds)
#markings_plus<-paste0(markings_plus,'"rebounds_plus": ',rebounds,"}")

#Remove unnecessary dataframes
rm(list= ls()[!(ls() %in% c('gameid','frames2','frames_reb','frames_tov','frames_hc',
                            'markings','js','pdist','players','bst','gravity',
                            'trans','shots','passes','de','rebounds'))])