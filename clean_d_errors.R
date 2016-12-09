frames2[,oc:=ifelse(is.na(oc),0,oc)]
# DE: define
frames2[,de:=
  # If there is an oc
  ifelse(oc!=0,
    # If there isn't a bb,bbcl,hn,bit, or bbc within the last 4 or next 2 secs
    ifelse(Reduce("|",shift(bb!=0|bbcl!=0|hn!=0|bbc!=0,0:120))|
           Reduce("|",shift(bb!=0|hn!=0|bbc!=0,0:60,type="lead")),0,
    # If there isn't a screen in the last 4 or next 2 secs
    ifelse(Reduce("|",shift(event=="SCR",0:120))|
           Reduce("|",shift(event=="SCR",0:60,type="lead"))|
      # Or the bhd is within 4 ft of where he was 1 sec ago
      pdist(bhd_x,shift(bhd_x,20),bhd_y,shift(bhd_y,20))<4,
      # Then it's help needed. Otherwise it's an error
      paste("hn",bhd),paste("de",bhd))),
    # If the defender on the shot is helping
    ifelse(def_type%in%c("defhe","defclhe"),
      # If there isn't already a defensive error
      ifelse(Reduce("|",shift(bb!=0|bbcl!=0|hn!=0|bbc!=0,0:120))|
             Reduce("|",shift(bb!=0|hn!=0|bbc!=0,0:60,type="lead")),0,
    # If there's a screen
    ifelse(Reduce("|",shift(event=="SCR",0:120))|
           Reduce("|",shift(event=="SCR",0:60,type="lead")),
      # Then it's help needed
      paste("hn",bhd),
        # Otherwise it's a defensive error
        ifelse(defender!=shift(defender,20),paste("de",shift(defender,20)),
        ifelse(defender!=shift(defender,15),paste("de",shift(defender,15)),
        ifelse(defender!=shift(defender,10),paste("de",shift(defender,10)),0))))),0))]

frames2[,de:=ifelse(is.na(de),0,de)]
frames2[,hn2:=ifelse(event=="SCR"&Reduce("|",shift(de=="hn 0",1:150,type="lead")),
                    shift(bhd,10),NA)]
frames2[,hn2:=ifelse(idx==1,0,hn2)]
frames2[,hn2:=na.locf(hn2)]
frames2[,de:=ifelse(de=="hn 0",paste("hn",hn2),de)]
frames2[,hn2:=NULL]


# DE: remove repeats
frames2[,de:=ifelse(Reduce("|",shift(!is.na(de)&de!=0,1:150)),0,de)]

# Put de 20 frames before the shot...
frames2[,de:=shift(de,20,type="lead")]

# Merge defensive errors
frames2[,detype:=substr(de,1,2)]
frames2[,de:=substr(de,4,nchar(de))]
frames2$de<-as.numeric(frames2$de)
frames2[,de:=ifelse(is.na(de),0,de)]
frames2[,de:=ifelse(bb!=0&!is.na(bb),bb,
                    ifelse(hn!=0&!is.na(hn),hn,
                           ifelse(bbcl!=0&!is.na(bbcl),bbcl,
                                  ifelse(bbc!=0&!is.na(bbc),bbc,de))))]
frames2[,detype:=ifelse(detype!="0",detype,
                       ifelse(bb!=0,"bb",
                              ifelse(hn!=0,"shn",
                                     ifelse(bbcl!=0,"bbcl",
                                            ifelse(bbc!=0,"bbc",NA)))))]
frames2[,detype:=gsub("hn","shn",detype)]
frames2[,detype:=gsub("sshn","shn",detype)]
frames2[,detype:=gsub("de","hn",detype)]
frames2<-frames2[,c(1:91,94:100),with=F]

# Adjust DE for recovery
frames2[,fdefender:=ifelse(idx==nrow(frames2),0,NA)]
frames2[,fdefender:=ifelse(def_type%in%c("defhe","defclhe"),defender,fdefender)]
frames2[,fdefender:=na.locf(fdefender,fromLast=T)]
frames2[,fdefender:=ifelse(Reduce("|",shift(def_type%in%c("defhe","defclhe"),0:60,
                                           type="lead")),fdefender,0)]
frames2[,def_type:=ifelse(is.na(def_type),0,def_type)]
dtypes<-c("defti","defhe","defclhe","defcl","defre","defna")
frames2[,de:=
           #If def_type isn't "def"
           ifelse(Reduce("|",shift(def_type%in%dtypes,0:150,type="lead"))&
                      # And there wasn't an ORB in the last 40 frames2
                      !Reduce("|",shift(event%in%"ORB",1:40))&
                      # And the error isn't attributed to the help defender
                      fdefender!=de,
                  # Then the error stands. Otherwise it doesn't.
                  de,0)]
frames2<-frames2[,(1:98),with=F]

# Create DE Markings
de<-frames2
de[,bh:=ifelse(detype%in%c("hn","shn"),shift(bh,20),bh)]
de<-de[de!=0]
de<-de[,.(game_code,idx,period,gameClock,detype,de,bh)]
setnames(de,c("idx","bh","de","detype"),
         c("frame","ballhandler","defender","error_type"))
de<-arrange(de,period,frame)
chances<-as.data.table(js$chances)
chances<-chances[,.(period,start_frame,id,possession_id)]
setnames(chances,c("period","frame","chance_id","possession_id"))
de<-full_join(de,chances,by=c("frame","period"));setDT(de)
de<-arrange(de,period,frame);setDT(de)
de<-na.locf(de)
de<-de[!is.na(game_code)]
de<-distinct(de,gameClock,.keep_all=T);setDT(de)
de$frame<-as.numeric(de$frame)
de[,id:=paste0(game_code,"_",period,"_",frame)]
de[,season:=ifelse(as.numeric(substr(gameid,5,6))>7,
                    as.numeric(substr(gameid,1,4)),
                    as.numeric(substr(gameid,1,4))-1)]
de<-de[,.(id,possession_id,chance_id,season,game_code,period,frame,gameClock,
            error_type,defender,ballhandler)]
de$defender<-as.character(as.numeric(de$defender))
de$ballhandler<-as.character(as.numeric(de$ballhandler))
t2<-trans
t2[,bit:=ifelse(
    T%in%c(def_player_1_beat_in_transition,def_player_2_beat_in_transition,
           def_player_3_beat_in_transition,def_player_4_beat_in_transition,
           def_player_5_beat_in_transition),1,0)]
t2<-t2[,.(chance_id,bit)]
de<-left_join(de,t2,by=c("chance_id"));setDT(de)
rm(t2)
de<-de[is.na(bit)|(bit==1&error_type%in%c("bb","shn"))]
de[,bit:=NULL]

# Write to file
write.csv(de,paste0("C:/Users/brocatoj/Documents/Basketball/Tracking/j_markings/",
          gameid,"_defensive_errors.csv"),row.names=F)

# Create json file if necessary
# de2<-toJSON(de)
# markings_plus<-paste0(markings_plus,'"defensive_errors": ',de2,",")
# rm(frames2)

# # Create simple de file for viewing purposes
# setDT(de)
# simple_de<-de
# simple_de$gameClock<-as.numeric(simple_de$gameClock)
# simple_de[,TL:=paste(floor(gameClock/60),".",gameClock%%60,sep="")]
# ap<-players[!is.na(id),.(ids_id,james_id)]
# setnames(ap,c("defender","player"))
# simple_de<-left_join(simple_de,ap,by="defender")
# simple_de<-select(simple_de,period,TL,chance_id,player,error_type)
# chances<-js$chances
# chances<-select(chances,id,end_shot_clock,past_hc_shot_clock)
# setnames(chances,"id","chance_id")
# simple_de<-left_join(simple_de,chances,by="chance_id")
# write.csv(simple_de,"reg/ok.csv",row.names=F)

# Remove unnecessary dataframes
# rm(list= ls()[!(ls() %in% c('gameid','frames2','frames_reb','markings','js',
#                             'pdist','players','bst','trans','shots','passes',
#                             'de'))])