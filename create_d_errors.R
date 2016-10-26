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