frames2[,oc:=ifelse(is.na(oc),0,oc)]
# DE: define
frames2[,de:=
  # If there is an oc
  ifelse(oc!=0,
    # If there isn't a bb,bbcl,hn,bit, or bbc within the last 4 or next 2 secs
    ifelse(Reduce("|",shift(bb!=0|bbcl!=0|hn!=0|bbc!=0,1:120))|
           Reduce("|",shift(bb!=0|hn!=0|bbc!=0,1:60,type="lead")),0,
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
      ifelse(Reduce("|",shift(bb!=0|bbcl!=0|hn!=0|bbc!=0,1:120))|
             Reduce("|",shift(bb!=0|hn!=0|bbc!=0,1:60,type="lead")),0,
    # If there's a screen
    ifelse(Reduce("|",shift(event=="SCR",1:120))|
           Reduce("|",shift(event=="SCR",1:60,type="lead")),
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


# Merge defensive errors
frames2[,detype:=substr(de,1,2)]
frames2[,de:=substr(de,4,nchar(de))]
frames2$de<-as.numeric(frames2$de)
frames2[,de:=ifelse(is.na(de),0,de)]
frames2[,de:=bb+hn+bbcl+bbc+de]
frames2[,detype:=ifelse(detype!="0",detype,
                       ifelse(bb!=0,"bb",
                              ifelse(hn!=0,"shn",
                                     ifelse(bbcl!=0,"bbcl",
                                            ifelse(bbc!=0,"bbc",NA)))))]
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
de<-frames2[de!=0]
de<-de[,.(game_code,idx,period,gameClock,detype,de,bh)]
de[,defender2:=NA][,defender3:=NA][,defender4:=NA][,defender5:=NA]
setnames(de,c("idx","bh","de","detype"),
         c("frame","ballhandler","defender","error_type"))
de<-de[,.(game_code,frame,period,gameClock,error_type,defender,defender2,defender3,
          defender4,defender5,ballhandler)]
# bit<-frames2[bit!=0]
# bit<-bit[,.(game_code,idx,period,gameClock,bit,bh)]
# bit[,bit:=gsub("0 ","",bit)]
# bit[,c("bit","defender2","defender3","defender4","defender5"):=tstrsplit(bit," ")]
# bit[,defender3:=ifelse(defender3==bit,NA,defender3)]
# bit[,defender4:=ifelse(defender4==defender2,NA,defender4)]
# bit[,defender5:=ifelse(defender5==defender3,NA,defender5)]
# setnames(bit,c("idx","bit","bh"),c("frame","defender","ballhandler"))
# bit[,error_type:="bit"]
# bit<-bit[,.(game_code,frame,period,gameClock,error_type,defender,defender2,
#             defender3,defender4,defender5,ballhandler)]
#de<-rbind(de,bit)
de<-arrange(de,period,frame)

de2<-toJSON(de)
markings_plus<-paste0(markings_plus,'"defensive_errors": ',de2,",")
rm(frames2)

setDT(de)
de[,TL:=paste(floor(gameClock/60),".",gameClock%%60,sep="")]
ap<-players[,.(ids_id,james_id)]
setnames(ap,c("defender","player"))
de$defender<-as.character(de$defender)
de<-left_join(de,ap,by="defender")
de<-select(de,period,TL,player,error_type)