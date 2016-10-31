# TAST: define
frames2[,tast:=
           # If there was a pass/ast in the last 60
           ifelse(Reduce("|", shift(event=="PASS",1:60))&
                      # And the result is a fga or shooting foul
                      event%in%c("2PM","3PM","2PX","3PX","SF")&
                      # And either there was a bb in the last sec or the def_type isn't def
                      (Reduce("|",shift(bb!=0,1:60))|def_type!="def"),
                  # Then the passer is credited with a tast. Otherwise he isn't.
                  passer,0)]

# OC: define
frames2[,oc:=
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
frames2[,oc:=ifelse(Reduce("|",shift(oc!=0,1:150)),0,oc),]

# OC: types
frames2[,octype:=ifelse(oc!=0,
                    ifelse(Reduce("|",shift(event=="SCR",0:75))|
                           Reduce("|",shift(event=="SCR",0:50,type="lead")),"poc","oc"),0)]

# Update passes markings
oc<-frames2[oc!=0|tast!=0|event=="PASS",.(period,idx,gameClock,event,tast,octype)]
oc[,tast:=shift(tast,type="lead",fill=0)][,octype:=shift(octype,fill=0)]
oc<-oc[event=="PASS"]
oc[,tast:=ifelse(tast==0,FALSE,TRUE)][,octype:=ifelse(octype==0,FALSE,TRUE)]
oc<-oc[,.(period,idx,gameClock,tast,octype)]
setnames(oc,c("period","frame","gameClock","true_assist","opportunity_created"))
oc[,mid:=paste0(period,"_",frame)]
passes<-as.data.table(js$passes)
passes[,mid:=paste0(period,"_",frame)]
passes<-left_join(passes,oc,by="mid")
passes<-toJSON(passes)
markings_plus<-paste0(markings_plus,'"passes": ',passes,",")

# Add markings to frames
#oc2<-oc2[,.(mid,true_assist,opportunity_created)]
#setnames(oc2,c("true_assist","opportunity_created"),c("tast","oc"))
#frames<-left_join(frames,oc2,by="mid");setDT(frames)