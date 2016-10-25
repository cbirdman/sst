# Remove event NAs
frames[,event:=ifelse(is.na(event),0,event)]

# Tip defense: when the player defends a tip shot
frames[,def_type:=
  # If there is a missed shot or shooting foul
  ifelse(event%in%c("2PM","2PX","3PM","3PX","SF")&
    # And the shot was immediately after an ORB, it's tip defense
    Reduce("|",shift(event=="ORB",1:20)),"defti",NA)]

# Help defense: when a player rotates to defend
frames[,def_type:=
  # If there is a missed shot or shooting foul
  ifelse(event%in%c("2PM","2PX","3PM","3PX","SF")&is.na(def_type)&
  #And the defender is different from who it was a short time ago,
  defender!=shift(defender,25)|defender!=shift(defender,15)|defender!=shift(defender,10),
    # If it's close to the hoop, it's defhe, if not it's defclhe
    ifelse(((s_x<17&s_y>10&s_y<40)|(s_x>77&s_y>10&s_y<40)),"defhe","defclhe"),def_type)]

# Closeoust defense: when a player closes out to defend a shot
frames[,def_type:=
  # If there is a missed shot or shooting foul
  ifelse(event%in%c("2PM","2PX","3PM","3PX","SF")&is.na(def_type)&
   # And the defender moved more than 5 feet from 20 frames ago
   pdist(d_x,shift(d_x,20),d_y,shift(d_y,20))>5&
   # And defender was 2+ feet away from shooter 20 frames ago
   pdist(shift(s_x,20),shift(d_x,20),shift(s_y,20),shift(d_y,20))>2&
   # And the shot isn't close to the hoop, then it's defcl
   !((s_x<17&s_y>10&s_y<40)|(s_x>77&s_y>10&s_y<40)),"defcl",def_type)]
      
# Recovering defense: when the player has to navigate a screen to defend a shot
frames[,def_type:=
  # If there is a missed shot or shooting foul
  ifelse(event%in%c("2PM","2PX","3PM","3PX","SF")&is.na(def_type)&   
    # And there was a screen in the last 75 frames
    (Reduce("|",shift(event=="SCR",1:75))|
      # Or there was a pass in the last 60 frames
      Reduce("|",shift(event=="PASS",1:60)))&
      # And defender was 2+ feet away from shooter 20 frames ago
      pdist(shift(s_x,20),shift(d_x,20),shift(s_y,20),shift(d_y,20))>=2,
    # If the shot was close to the hoop, it's defre, else it's defcl
    ifelse((s_x<17&s_y>10&s_y<40)|(s_x>77&s_y>10&s_y<40),"defre","defcl"),def_type)]
    
# NA defnese: when a player has no real chance of defending a shot
frames[,def_type:=
  # If there is a missed shot or shooting foul
  ifelse(event%in%c("2PM","2PX","3PM","3PX","SF")&is.na(def_type)&     
    # And the shooter is 10+ feet from the defender
    pdist(d_x,s_x,d_y,s_y)>10&
    # And the shot is close to the hoop, then it's defna
    ((s_x<17&s_y>10&s_y<40)|(s_x>77&s_y>10&s_y<40)),"defna",def_type)]
    
# Man defense: when a player is guarding the shot straight up
frames[,def_type:=
  # If there is a missed shot or shooting foul
  ifelse(event%in%c("2PM","2PX","3PM","3PX","SF")&is.na(def_type)&
  # And the shooter is 10+ feet and it's away from the hoop it's defcl, otherwise it's def
  pdist(d_x,s_x,d_y,s_y)>10,"defcl",
    ifelse(is.na(def_type),"def",def_type))]

# Update shots markings
shots<-as.data.table(js$shots)
shots[,pf:=paste0(period,"_",frame_idx)]
dt<-frames[event%in%c("2PM","2PX","3PM","3PX","SF")]
dt<-dt[,.(period,idx,def_type)]
dt[,pf:=paste0(period,"_",idx)]
dt<-dt[,.(pf,def_type)]
shots<-left_join(shots,dt,by="pf");setDT(shots)
shots<-shots[,c(1:67,69),with=F]
# shots<-toJSON(shots)
# shots<-paste0("{shots_plus: ",shots,",")
# 
# # Remove unnecessary dataframes
# rm(list= ls()[!(ls() %in% c('frames','markings','js','shots','pdist','players'))])
shot<-shots[,.(period,game_clock,dplayer_id,def_type)]
shot<-arrange(shot,period,desc(game_clock))
shot<-shot%>%mutate(TL=paste(floor(game_clock/60),".",game_clock%%60,sep=""))
shot$dplayer_id<-as.character(shot$dplayer_id)
ap<-players[,.(id,james_id)]
setnames(ap,c("dplayer_id","player"))
shot<-left_join(shot,ap,by="dplayer_id")
shot<-select(shot,period,TL,player,def_type)