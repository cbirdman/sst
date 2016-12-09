# Forced turnovers
# Identify Defender of passer
frames_tov[,t_defender:=NA]
for(i in c("ap1","ap2","ap3","ap4","ap5","hp1","hp2","hp3","hp4","hp5")){
    frames_tov[,t_defender:=ifelse(player_id==frames_tov[[i]],
                                   frames_tov[[gsub("p","d",i)]],t_defender)]
}

tov<-as.data.table(js$turnovers)
tov<-cbind(tov,frames_tov$t_defender)
ap<-players[!is.na(id),.(id,ids_id)]
ap$id<-as.numeric(ap$id)
setnames(ap,c("to_forcing_id","fort"))
tov<-left_join(tov,ap,by="to_forcing_id")
setDT(tov)
tov[,to_forcing_id:=NULL]
setnames(tov,"fort","to_forcing_id")
setnames(ap,c("to_commiting_id","to_committing_id"))
tov<-left_join(tov,ap,by="to_commiting_id")
setDT(tov)
tov[,to_commiting_id:=NULL]
tov[,to_forcing_id:=ifelse(is.na(to_forcing_id),V2,to_forcing_id)]
tov[,to_forcing_id:=ifelse(is.na(to_committing_id),NA,to_forcing_id)]
tov[,V2:=NULL]                           
tov<-tov[,.(id,possession_id,chance_id,season,game_code,period,game_clock,frame,
            oteam,dteam,location_x,location_y,team_to,to_committing_id,
            to_forcing_id)]

# Write to file
write.csv(tov,paste0("C:/Users/brocatoj/Documents/Basketball/Tracking/j_markings/",
          gameid,"_turnovers.csv"),row.names=F)

# # KIF: define
# frames[,kif:=
#   # If the distance between the ballhandler and the hoop
#   ifelse((pdist(bh_x,ifelse(pend==1,4,90),bh_y,25)-
#           # is 2+ feet greater than it was a second ago
#           pdist(shift(bh_x,25),ifelse(pend==1,4,90),shift(bh_y,25),25))>2&
#           # And the ballhanlder is within 27 feet of the hoop
#           pdist(bh_x,ifelse(pend==1,4,90),bh_y,25)<27&
#           # And the bh & bhd are continuous
#           bh==shift(bh,25)&bhd==shift(bhd,25),
#   bhd,0)]
# 
# # KIF: remove replicates
# frames[,kif:=ifelse(Reduce("|",shift(kif!=0,1:50)),0,kif),]