# Blowby: define
frames2[,bb:=
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
frames2[,bb:=ifelse(Reduce("|",shift(bb!=0,1:50)),0,bb),]

# Blowby on screener defender: not guarding him x frames2 ago, but beat by him in sam way as bb...
frames2[,bigbb:=
  # If the ballhandler defender is not continous
  ifelse(bhd!=shift(bhd,25)&bhd==shift(bhd,10)&
    # And bh remains continuous and it's not the beginning of a quarter
    bh==shift(bh,10)&bh==shift(bh,25)&gameClock!=720&
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
frames2[,bigbb:=ifelse(Reduce("|",shift(bigbb!=0,1:50)),0,bigbb)]

# HN: define
frames2[,hn:=ifelse((Reduce("|",shift(event%in%c("SCR","DHO"),0:75))|
                    Reduce("|",shift(event%in%c("SCR","DHO"),0:50,type="lead")))&bb!=0,bb,0)]

# BBCL: define
frames2[,bbcl:=ifelse(Reduce("|",shift(event=="PASS",1:75))&
                      bh_x>15&bh_x<79&bb!=0&hn==0,bb,0)]
# 
# # BB: remove those categorized as hn, bbcl, or bigbb
frames2[,bigbb:=ifelse(is.na(bigbb),0,bigbb)]
frames2[,bb:=ifelse(hn!=0|bbcl!=0|Reduce("|",shift(bigbb!=0,0:60))|
                Reduce("|",shift(bigbb!=0,1:60,type="lead")),0,bb)]

# HN: remove those categorized as bigbb or bbcl
frames2[,hn:=ifelse(Reduce("|",shift(bigbb!=0|bbcl!=0,1:60))|
                Reduce("|",shift(bigbb!=0,1:60,type="lead")),0,hn)]

# BB: Add in bigbb and remove bigbb column
frames2[,bb:=ifelse(bigbb!=0,bigbb,bb)]
frames2<-frames2[,c(1:92,94:95),with=F]

# BBC: define
frames2[,bbc:=
  # If there wasn't a bb in the last 50 frames2
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
frames2[,bbc:=ifelse(Reduce("|",shift(bbc!=0,1:75)),0,bbc),]

# BBC: no error in next x or last x
frames2[,bbc:=ifelse(Reduce("|",shift(hn!=0|bb!=0|bbcl!=0,1:60,type="lead"))|
                     Reduce("|",shift(hn!=0|bb!=0|bbcl!=0,1:60)),0,bbc),]

# BBC: remove those that happen on wrong side
frames2[,bbc:=ifelse(bbc!=0&pend==1&bh_x>47,0,
               ifelse(bbc!=0&pend!=1&bh_x<=47,0,bbc))]
frames2[,bbcl:=ifelse(bbcl!=0&pend==1&bh_x>47,0,
               ifelse(bbcl!=0&pend!=1&bh_x<=47,0,bbcl))]