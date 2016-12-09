frames2<-frames[Reduce("|",shift(event%in%c("2PM","2PX","3PM","3PX","SF"),0:151,type="lead"))|
                Reduce("|",shift(event%in%c("2PM","2PX","3PM","3PX","SF"),0:151))|
                Reduce("|",shift(ball_x>49&bh_x<58,0:51,type="lead"))|
                Reduce("|",shift(ball_x>36&bh_x<45,0:51,type="lead"))]
frames2<-select(frames2,game_code:pend)
frames_reb<-frames[Reduce("|",shift(event%in%c("2PM","2PX","3PM","3PX","SF"),0:151))]
frames_tov<-frames[event=="TO"]
rm(frames)

# Remove event NAs
frames2[,event:=ifelse(is.na(event),0,event)]

# Tip defense: when the player defends a tip shot
frames2[,def_type:=
  # If there is a missed shot or shooting foul
  ifelse(event%in%c("2PM","2PX","3PM","3PX","SF")&
    # And the shot was immediately after an ORB, it's tip defense
    Reduce("|",shift(event=="ORB",1:20)),"defti",NA)]

# Help defense: when a player rotates to defend
frames2[,def_type:=
  # If there is a missed shot or shooting foul
  ifelse(event%in%c("2PM","2PX","3PM","3PX","SF")&is.na(def_type)&
  #And the defender is different from who it was a short time ago,
  (defender!=shift(defender,25)|defender!=shift(defender,15)|defender!=shift(defender,10)),
  "defhe",def_type)]
frames2[,def_type:=
  ifelse(def_type=="defhe"&
    # If it's close to the hoop, it's defhe, if not it's defclhe
    !((s_x<17&s_y>10&s_y<40)|(s_x>77&s_y>10&s_y<40)),"defclhe",def_type)]

# Closeoust defense: when a player closes out to defend a shot
frames2[,def_type:=
  # If there is a missed shot or shooting foul
  ifelse(event%in%c("2PM","2PX","3PM","3PX","SF")&is.na(def_type)&
   # And the defender moved more than 5 feet from 20 frames2 ago
   pdist(d_x,shift(d_x,20),d_y,shift(d_y,20))>5&
   # And defender was 2+ feet away from shooter 20 frames2 ago
   pdist(shift(s_x,20),shift(d_x,20),shift(s_y,20),shift(d_y,20))>2&
   # And the shot isn't close to the hoop, then it's defcl
   !((s_x<17&s_y>10&s_y<40)|(s_x>77&s_y>10&s_y<40)),"defcl",def_type)]
      
# Recovering defense: when the player has to navigate a screen to defend a shot
frames2[,def_type:=
  # If there is a missed shot or shooting foul
  ifelse(event%in%c("2PM","2PX","3PM","3PX","SF")&is.na(def_type)&   
    # And there was a screen in the last 75 frames2
    (Reduce("|",shift(event=="SCR",1:75))|
      # Or there was a pass in the last 60 frames2
      Reduce("|",shift(event=="PASS",1:60)))&
      # And defender was 2+ feet away from shooter 20 frames2 ago
      pdist(shift(s_x,20),shift(d_x,20),shift(s_y,20),shift(d_y,20))>=2,
    # If the shot was close to the hoop, it's defre, else it's defcl
    ifelse((s_x<17&s_y>10&s_y<40)|(s_x>77&s_y>10&s_y<40),"defre","defcl"),def_type)]
    
# NA defnese: when a player has no real chance of defending a shot
frames2[,def_type:=
  # If there is a missed shot or shooting foul
  ifelse(event%in%c("2PM","2PX","3PM","3PX","SF")&is.na(def_type)&     
    # And the shooter is 10+ feet from the defender
    pdist(d_x,s_x,d_y,s_y)>10&
    # And the shot is close to the hoop, then it's defna
    ((s_x<17&s_y>10&s_y<40)|(s_x>77&s_y>10&s_y<40)),"defna",def_type)]
    
# Man defense: when a player is guarding the shot straight up
frames2[,def_type:=
  # If there is a missed shot or shooting foul
  ifelse(event%in%c("2PM","2PX","3PM","3PX","SF")&is.na(def_type)&
  # And the shooter is 10+ feet and it's away from the hoop it's defcl, otherwise it's def
  pdist(d_x,s_x,d_y,s_y)>10,"defcl",def_type)]

frames2[,def_type:=
  ifelse(event%in%c("2PM","2PX","3PM","3PX","SF")&is.na(def_type),
         "def",def_type)]

# Update shots markings
shots<-as.data.table(js$shots)
shots[,mid:=paste0(period,"_",frame_idx)]
dt<-frames2[,.(period,idx,def_type)]
dt[,mid:=paste0(period,"_",idx)]
dt<-dt[,.(mid,def_type)]
shots<-left_join(shots,dt,by="mid");setDT(shots)

# Add nbacom id and player name
ap<-players[!is.na(id),.(id,ids_id)]
ap$id<-as.numeric(ap$id)
setnames(ap,c("player_id","player_id_"))
shots<-left_join(shots,ap,by="player_id");setDT(shots)
shots[,player_id:=NULL]
setnames(shots,"player_id_","player_id")
setnames(ap,c("passer_id","passer_id_"))
shots<-left_join(shots,ap,by="passer_id");setDT(shots)
shots[,passer_id:=NULL]
setnames(shots,"passer_id_","passer_id")

for(i in 1:3){
    setnames(ap,c(paste0("dplayer",i,"_id"),paste0("dplayer",i,"_id2")))
    shots<-left_join(shots,ap,by=paste0("dplayer",i,"_id"))
    setnames(ap,c(paste0("contester",i,"_id"),paste0("contester",i,"_id2")))
    shots<-left_join(shots,ap,by=paste0("contester",i,"_id"))
    setDT(shots)
    shots[,(paste0("dplayer",i,"_id")):=NULL]
    shots[,(paste0("contester",i,"_id")):=NULL]
    setnames(shots,paste0("dplayer",i,"_id2"),paste0("dplayer",i,"_id"))
    setnames(shots,paste0("contester",i,"_id2"),paste0("contester",i,"_id"))
}
shots[,c("approach_1","approach_2","approach_3","approach_4"):=
           tstrsplit(dplayer1_approach, ",", fixed=T)]
shots[,approach_1:=substr(approach_1,3,19)]
shots[,approach_2:=gsub(",","",approach_2)][,approach_3:=gsub(",","",approach_3)]
shots[,approach_4:=gsub(")","",approach_4)]

shots<-shots %>%
    select(id,possession_id,chance_id,season,game_code,period,
           frame_idx,frame_time,game_clock,home_team_id,away_team_id,o_team,
           d_team,score_home,score_away,is_home,shot_clock,
           possible_anomaly,corrected,
           player_id,receive_loc_x,receive_loc_y,receive_time,
           location_x,location_y,release_time,dribbles_before,shot_angle,shot_dist,
           simple_shot_type,complex_shot_type,made,is_three,catch_and_shoot,
           putback,blocked,contested,fouled,sefg,pa_sefg,velocity_mag,velocity_angle,
           passer_id,passer_loc_x,passer_loc_y,potential_assist,created_from_paint,
           dplayer1_id,dplayer2_id,dplayer3_id,
           approach_1,approach_2,approach_3,approach_4,
           dplayer1_dist,dplayer2_dist,dplayer3_dist,
           dplayer1_velocity_angle,dplayer2_velocity_angle,dplayer3_velocity_angle,
           dplayer1_velocity_mag,dplayer2_velocity_mag,dplayer3_velocity_mag,
           contester1_id,contester2_id,contester3_id,
           dplayer1_angle,dplayer2_angle,dplayer3_angle,def_type)
shots<-shots[order(period,frame_idx)]

# Write to file
write.csv(shots,paste0("C:/Users/brocatoj/Documents/Basketball/Tracking/j_markings/",
          gameid,"_shots.csv"),row.names=F)

# Create json file if necessary
#shots<-toJSON(shots)
#markings_plus<-paste0('{"shots_plus": ',shots,",")

# Remove unnecessary dataframes2
rm(list= ls()[!(ls() %in% c('gameid','frames2','frames_reb','frames_tov',
                            'markings','js','pdist','players','bst','gravity',
                            'trans','shots'))])

# # Simple Shots for Checking
# simple_shots<-shots[,.(period,game_clock,dplayer1_id,def_type)]
# simple_shots<-arrange(simple_shots,period,desc(game_clock))
# simple_shots<-simple_shots %>%
#     mutate(TL=paste(floor(game_clock/60),".",game_clock%%60,sep=""))
# simple_shots$dplayer1_id<-as.character(simple_shots$dplayer1_id)
# ap<-players[!is.na(id),.(ids_id,james_id)]
# setnames(ap,c("dplayer1_id","player"))
# simple_shots<-left_join(simple_shots,ap,by="dplayer1_id")
# simple_shots<-select(simple_shots,period,TL,player,def_type)