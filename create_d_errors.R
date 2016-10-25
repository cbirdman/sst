# Blowby: define
frames[,bb:=
  # If the ballhandler remains continous
  ifelse(bhd==shift(bhd,25)&bh==shift(bh,25)&
    # And bhd remains continuous and it's not the beginning of a quarter
    bhd==shift(bhd,10)&bh==shift(bh,10)&gameClock!=720&
    # And the ballhandler moved 4ft closer to hoop in the last second
    (pdist(shift(bh_x,25),ifelse(pend==1,4,90),shift(bh_y,25),25)-
    pdist(bh_x,ifelse(pend==1,4,90),bh_y,25))>4&
    # And the ballhandler is closer to the hoop than his defender
    (pdist(bhd_x,ifelse(pend==1,4,90),bhd_y,25)-
     pdist(bh_x,ifelse(pend==1,4,90),bh_y,25))>0&
    # And the defender was (nearly) closer to the hoop a second ago
    (pdist(shift(bh_x,25),ifelse(pend==1,4,90),shift(bh_y,25),25)-
     pdist(shift(bhd_x,25),ifelse(pend==1,4,90),shift(bhd_y,25),25))>-0.5,
    # Then the defender was blown by. Otherwise he wasn't.
    bhd,0)]

# Blowby: remove repeats
frames[,bb:=ifelse(Reduce("|",shift(bb!=0,1:50)),0,bb),]

# Blowby on screener defender: not guarding him x frames ago, but beat by him in sam way as bb...
frames[,bigbb:=
  # If the ballhandler defender is not continous
  ifelse(pend==1&bhd!=shift(bhd,25)&bh==shift(bh,25)&
    # And bhd remains continuous and it's not the beginning of a quarter
    bhd==shift(bhd,10)&bh==shift(bh,10)&gameClock!=720&
    # And the ballhandler moved 4ft closer to hoop in the last second
    (pdist(shift(bh_x,10),ifelse(pend==1,4,90),shift(bh_y,10),25)-
     pdist(bh_x,ifelse(pend==1,4,90),bh_y,25))>4&
    # And the ballhandler is closer to the hoop than his defender
    (pdist(bhd_x,ifelse(pend==1,4,90),bhd_y,25)-
     pdist(bh_x,ifelse(pend==1,4,90),bh_y,25))>0&
    # And the defender was (nearly) closer to the hoop a second ago
    (pdist(shift(bh_x,10),ifelse(pend==1,4,90),shift(bh_y,10),25)-
     pdist(shift(bhd_x,10),ifelse(pend==1,4,90),shift(bhd_y,10),25))>-0.5&
    # And there is a ball screen involved
    (Reduce("|",shift(event=="SCR",0:75))|
     Reduce("|",shift(event=="SCR",0:50,type="lead"))),
    # Then the big was blown by. Otherwise he wasn't.
    bhd,0)]

# BIGBB: remove repeats
frames[,bigbb:=ifelse(Reduce("|",shift(bigbb!=0,1:50)),0,bigbb)]

# HN: define
frames[,hn:=ifelse((Reduce("|",shift(event=="SCR",0:75))|
                    Reduce("|",shift(event=="SCR",0:50,type="lead")))&bb!=0,bb,0)]

# BBCL: define
frames[,bbcl:=ifelse(Reduce("|",shift(event=="PASS",1:75))&
                      bh_x>15&bh_x<79&bb!=0&hn==0,bb,0)]
# 
# # BB: remove those categorized as hn, bbcl, or bigbb
frames[,bigbb:=ifelse(is.na(bigbb),0,bigbb)]
frames[,bb:=ifelse(hn!=0|bbcl!=0|Reduce("|",shift(bigbb!=0,0:60))|
                Reduce("|",shift(bigbb!=0,1:60,type="lead")),0,bb)]

# HN: remove those categorized as bigbb or bbcl
frames[,hn:=ifelse(Reduce("|",shift(bigbb!=0|bbcl!=0,1:60))|
                Reduce("|",shift(bigbb!=0,1:60,type="lead")),0,hn)]

# BB: Add in bigbb and remove bigbb column
frames[,bb:=ifelse(bigbb!=0,bigbb,bb)]
frames<-frames[,c(1:92,94:95),with=F]

# BIT: first take from leftover bb
frames[,bit:=ifelse(bbcl!=0&pend==1&bh_x>47,paste0("bit",bbcl),
             ifelse(bbcl!=0&pend!=1&bh_x<=47,paste0("bit",bbcl),0))]
frames[,bbcl:=ifelse(bit!=0,0,bbcl)]

# BIT: define
for(i in c("ap1","ap2","ap3","ap4","ap5","hp1","hp2","hp3","hp4","hp5")){
    # Define "x" to save space
    eks<-frames[[paste0(i,"_x")]]
    frames[,bit:=
      # If hoop is on RIGHT and bh gets in front court and bh is away
      ifelse(pend!=1&bh_x>56&bh_x<58&bh%in%c(ap1,ap2,ap3,ap4,ap5)&
        # And bh was on the LEFT 20 frames ago and the loop is on home
        shift(bh_x,20)<47&eks<64&str_count(i,"h"),
        # If an away player beat the home player
        ifelse((((hp1_x>eks)+(hp2_x>eks)+(hp3_x>eks)+(hp4_x>eks)+(hp5_x>eks))-
                ((ap1_x>eks)+(ap2_x>eks)+(ap3_x>eks)+(ap4_x>eks)+(ap5_x>eks)))<0,
        # Then he was beat in transition. Otherwise he wasn't.
        paste(bit,paste0("bit",frames[[i]])),bit),
      # If hoop is on RIGHT and bh gets in front court and bh is home
      ifelse(pend!=1&bh_x>56&bh_x<58&bh%in%c(hp1,hp2,hp3,hp4,hp5)&
        # And bh was on the LEFT 20 frames ago and the loop is on away
        shift(bh_x,20)<47&eks<64&str_count(i,"a"),
        # If a home player beat the away player
        ifelse((((hp1_x>eks)+(hp2_x>eks)+(hp3_x>eks)+(hp4_x>eks)+(hp5_x>eks))-
                ((ap1_x>eks)+(ap2_x>eks)+(ap3_x>eks)+(ap4_x>eks)+(ap5_x>eks)))>0,
        # Then he was beat in transition. Otherwise he wasn't
        paste(bit,paste0("bit",frames[[i]])),bit),
     # If hoop is on LEFT and bh gets in front court and bh is away
     ifelse(pend==1&bh_x>36&bh_x<38&bh%in%c(ap1,ap2,ap3,ap4,ap5)&
       # And bh was on the RIGHT 20 frames ago and the loop is on home
       shift(bh_x,20)>47&eks>30&str_count(i,"H"),
       # If an away player beat the home player
       ifelse((((hp1_x>eks)+(hp2_x>eks)+(hp3_x>eks)+(hp4_x>eks)+(hp5_x>eks))-
               ((ap1_x>eks)+(ap2_x>eks)+(ap3_x>eks)+(ap4_x>eks)+(ap5_x>eks)))>0,
       # Then he was beat in transition. Otherwise he wasn't.
       paste(bit,paste0("bit",frames[[i]])),bit),
     # IF the hoop is on the LEFT and bh gets i front court and bh is home
     ifelse(pend==1&bh_x>36&bh_x<38&bh%in%c(hp1,hp2,hp3,hp4,hp5)&
      # And bh was on the RIGHT 20 frames ago and the loop is on away
      shift(bh_x,20)>47&eks>30&str_count(i,"A"),
      # If a home player beat the away player
      ifelse((((hp1_x>eks)+(hp2_x>eks)+(hp3_x>eks)+(hp4_x>eks)+(hp5_x>eks))-
              ((ap1_x>eks)+(ap2_x>eks)+(ap3_x>eks)+(ap4_x>eks)+(ap5_x>eks)))<0,
      # Then he was beat in transition. Otherwise he wasn't.
      paste(bit,paste0("bit",frames[[i]])),bit),bit))))]
}

# BIT: remove repeats
frames[,bit:=ifelse(Reduce("|",shift(bit!=0,1:75)),0,bit),]

# BIT: remove those categorized as bb
frames[,bit:=ifelse(Reduce("|",shift(bb!=0,1:60))|
                    Reduce("|",shift(bb!=0,1:60,type="lead")),0,bit)]

# BBC: define
frames[,bbc:=
  # If there wasn't a bb in the last 50 frames
  ifelse(gameClock!=720&!Reduce("|",shift(bb!=0,1:50))&bh!=shift(bh,25)&
    # And bh moved 4+ feet closer to hoop than 1 sec ago and now < 12 ft
    (pdist(obh_x,ifelse(pend==1,4,90),obh_y,25)-
     pdist(bh_x,ifelse(pend==1,4,90),bh_y,25))>4&
     pdist(bh_x,ifelse(pend==1,4,90),bh_y,25)<12&
    # And bh is closer to hoop than bhd of 1 sec ago
    (pdist(obhd_x,ifelse(pend==1,4,90),obhd_y,25)-
     pdist(bh_x,ifelse(pend==1,4,90),bh_y,25))>0&
    # And there wasn't an ORB or screen in the last x seconds
    !Reduce("|",shift(event%in%"ORB",0:25))&
    !Reduce("|",shift(event%in%"SCR",0:75))&
    # And there was a pass in the last second
    Reduce("|",shift(event=="PASS",0:40)),
    # Then the defender was beat by a cut. Otherwise he wasn't.
    obhd,0)]

# BBC: remove repeats
frames[,bbc:=ifelse(Reduce("|",shift(bbc!=0,1:75)),0,bbc),]

# BBC: no error in next x or last x
frames[,bbc:=ifelse(Reduce("|",shift(bit!=0|hn!=0|bb!=0|bbcl!=0,1:60,type="lead"))|
                    Reduce("|",shift(bit!=0|hn!=0|bb!=0|bbcl!=0,1:60)),0,bbc),]

# TAST: define
frames[,tast:=
  # If there was a pass/ast in the last 60
  ifelse(Reduce("|", shift(event=="PASS",1:60))&
    # And the result is a fga or shooting foul
    event%in%c("2PM","3PM","2PX","3PX","SF")&
    # And either there was a bb in the last sec or the def_type isn't def
    (Reduce("|",shift(bb!=0,1:60))|def_type!="def"),
    # Then the passer is credited with a tast. Otherwise he isn't.
    passer,0)]

# OC: define
frames[,oc:=
  # If there is an assist within 6 seconds
  ifelse(Reduce("|",shift(tast!=0,1:150,type="lead"))&
    # And there is a pass within a second
    Reduce("|",shift(event=="PASS",1:25,type="lead"))&
    # And there isn't a dho within a second
    !Reduce("|",shift(event%in%"DHO",1:25,type="lead"))&
    # And there wasn't an orb in the last 2 seconds
    !Reduce("|",shift(event%in%"ORB",1:50))&
    # And the ballhandler is within 40 feet of the baseline
    (pend==1&bh_x<40|pend==0&bh_x>=54)&
    # And the shooter isn't the bh and the bhd is within 10 feet of the bh
    shooter!=bh&pdist(bh_x,bhd_x,bh_y,bhd_y)<10,
    # If the shooter drew a second defender
    ifelse(
        ifelse(bh%in%c(ap1,ap2,ap3,ap4,ap5),
          ((pdist(bh_x,hp1_x,bh_y,hp1_y)<8)+(pdist(bh_x,hp2_x,bh_y,hp2_y)<8)+
           (pdist(bh_x,hp3_x,bh_y,hp3_y)<8)+(pdist(bh_x,hp4_x,bh_y,hp4_y)<8)+
           (pdist(bh_x,hp5_x,bh_y,hp5_y)<8))>=2,
          ((pdist(bh_x,ap1_x,bh_y,ap1_y)<8)+(pdist(bh_x,ap2_x,bh_y,ap2_y)<8)+
           (pdist(bh_x,ap3_x,bh_y,ap3_y)<8)+(pdist(bh_x,ap4_x,bh_y,ap4_y)<8)+
           (pdist(bh_x,ap5_x,bh_y,ap5_y)<8))>=2)|
      # Or bh blew by his defender
      bb!=0|bbc!=0|bit!=0,
      # Then bh is credited with an oc. Otherwise he isn't.
      bh,0),0)]

# OC: remove repeats
frames[,oc:=ifelse(Reduce("|",shift(oc!=0,1:150)),0,oc),]

# OC: types
frames[,octype:=ifelse(oc!=0,
             ifelse(Reduce("|",shift(event=="SCR",0:75))|
                    Reduce("|",shift(event=="SCR",0:50,type="lead")),"poc","oc"),0)]

# TAST: Change to the pass not the shot
frames[,tast1:=
        ifelse(event=="PASS"&
                   Reduce("|", shift(tast!=0,1:60,type="lead"))&
                  !Reduce("|",shift(event%in%"PASS",1:60,type="lead")),
               shift(bh),0)]
frames<-frames[,c(1:96,98:100),with=F]
setnames(frames,"tast1","tast")

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

# de<-toJSON(de)
# shots<-paste0("{shots_plus: ",shots,",")
# 
de<-de%>%mutate(TL=paste(floor(gameClock/60),".",gameClock%%60,sep=""))
setnames(ap,c("defender","player"))
de<-left_join(de,ap,by="defender")
de<-select(de,period,TL,player,error_type)