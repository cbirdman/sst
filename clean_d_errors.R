frames[,oc:=ifelse(is.na(oc),0,oc)]
# DE: define
frames[,de:=
  # If there is an oc
  ifelse(oc!=0,
    # If there isn't a bb,bbcl,hn,bit, or bbc within the last 4 or next 2 secs
    ifelse(Reduce("|",shift(bb!=0|bbcl!=0|hn!=0|bit!=0|bbc!=0,1:120))|
           Reduce("|",shift(bb!=0|hn!=0|bit!=0|bbc!=0,1:60,type="lead")),0,
    # If there isn't a screen in the last 4 or next 2 secs
    ifelse(Reduce("|",shift(event=="SCR",1:120))|
           Reduce("|",shift(event=="SCR",1:60,type="lead"))|
      # Or the bhd is within 4 ft of where he was 1 sec ago
      pdist(bhd_x,shift(bhd_x,20),bhd_y,shift(bhd_y,20))<4,
      # Then it's help needed. Otherwise it's an error
      paste("hn",bhd),paste("de",bhd))),
    # If the defender on the shot is helping
    ifelse(def_type%in%c("defhe","defclhe"),
      # If there isn't already a defensive error
      ifelse(Reduce("|",shift(bb!=0|bbcl!=0|hn!=0|bit!=0|bbc!=0,1:120))|
             Reduce("|",shift(bb!=0|hn!=0|bit!=0|bbc!=0,1:60,type="lead")),0,
    # If there's a screen
    ifelse(Reduce("|",shift(event=="SCR",1:120))|
           Reduce("|",shift(event=="SCR",1:60,type="lead")),
      # Then it's help needed
      paste("hn",bhd),
        # Otherwise it's a defensive error
        ifelse(defender!=shift(defender,20),paste("de",shift(defender,20)),
        ifelse(defender!=shift(defender,15),paste("de",shift(defender,15)),
        ifelse(defender!=shift(defender,10),paste("de",shift(defender,10)),0))))),0))]

frames[,de:=ifelse(is.na(de),0,de)]
frames[,hn2:=ifelse(event=="SCR"&Reduce("|",shift(de=="hn 0",1:150,type="lead")),
                    shift(bhd,10),NA)]
frames[,hn2:=ifelse(idx==1,0,hn2)]
frames[,hn2:=na.locf(hn2)]
frames[,de:=ifelse(de=="hn 0",paste("hn",hn2),de)]
frames[,hn2:=NULL]


# DE: remove repeats
frames[,de:=ifelse(Reduce("|",shift(!is.na(de)&de!=0,1:150)),0,de)]


# Merge defensive errors
frames[,bit:=ifelse(is.na(bit),0,bit)]
frames[,detype:=substr(de,1,2)]
frames[,de:=substr(de,4,nchar(de))]
frames$de<-as.numeric(frames$de)
frames[,de:=ifelse(is.na(de),0,de)]
frames[,de:=bb+hn+bbcl+bbc+de]
frames[,detype:=ifelse(detype!="0",detype,
                       ifelse(bb!=0,"bb",
                              ifelse(hn!=0,"shn",
                                     ifelse(bbcl!=0,"bbcl",
                                            ifelse(bbc!=0,"bbc",NA)))))]
frames[,detype:=gsub("de","hn",detype)]
frames<-frames[,c(1:91,95:101),with=F]

# Adjust DE for recovery
frames[,fdefender:=ifelse(idx==nrow(frames),0,NA)]
frames[,fdefender:=ifelse(def_type%in%c("defhe","defclhe"),defender,fdefender)]
frames[,fdefender:=na.locf(fdefender,fromLast=T)]
frames[,fdefender:=ifelse(Reduce("|",shift(def_type%in%c("defhe","defclhe"),0:60,
                                           type="lead")),fdefender,0)]
frames[,def_type:=ifelse(is.na(def_type),0,def_type)]
dtypes<-c("defti","defhe","defclhe","defcl","defre","defna")
frames[,de:=
           #If def_type isn't "def"
           ifelse(Reduce("|",shift(def_type%in%dtypes,0:150,type="lead"))&
                      # And there wasn't an ORB in the last 40 frames
                      !Reduce("|",shift(event%in%"ORB",1:40))&
                      # And the error isn't attributed to the help defender
                      fdefender!=de,
                  # Then the error stands. Otherwise it doesn't.
                  de,0)]
frames<-frames[,(1:98),with=F]

# Create DE Markings
de<-frames[de!=0]
de<-de[,.(game_code,idx,period,gameClock,bh,de,detype)]
setnames(de,c("idx","bh","de","detype"),
         c("frame","ballhandler","defender","error_type"))

de<-toJSON(de)
markings_plus<-paste0(markings_plus,'"defensive_errors": ',de,",")

# de[,TL:=paste(floor(gameClock/60),".",gameClock%%60,sep="")]
# ap<-players[,.(id,james_id)]
# setnames(ap,c("defender","player"))
# ap[,defender:=substr(defender,1,14)]
# de[,defender:=substr(defender,1,14)]
# de<-left_join(de,ap,by="defender")
# de<-select(de,period,TL,player,error_type)