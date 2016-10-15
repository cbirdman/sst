source("functions.R")

df<-frames[Reduce("|",shift(event%in%c("2PM","2PX","3PM","3PX","SF"),
                                    0:76,type="lead"))]
df[,Shooter:=ifelse(event%in%c("2PM","2PX","3PM","3PX","SF"),player_id,NA)]
df[,Shooter:=na.locf(Shooter,fromLast = T)]
df[,defender:=NA]
for(i in c("hp1","hp2","hp3","hp4","hp5","ap1","ap2","ap3","ap4","ap5")){
  df[,d1:=pdist(df[[paste0(i,"_x")]],ifelse(grepl("h",i),ap1_x,hp1_x),
                df[[paste0(i,"_y")]],ifelse(grepl("h",i),ap1_y,hp1_y))]
  df[,d2:=pdist(df[[paste0(i,"_x")]],ifelse(grepl("h",i),ap2_x,hp2_x),
                    df[[paste0(i,"_y")]],ifelse(grepl("h",i),ap2_y,hp2_y))]
  df[,d3:=pdist(df[[paste0(i,"_x")]],ifelse(grepl("h",i),ap3_x,hp3_x),
                df[[paste0(i,"_y")]],ifelse(grepl("h",i),ap3_y,hp3_y))]
  df[,d4:=pdist(df[[paste0(i,"_x")]],ifelse(grepl("h",i),ap4_x,hp4_x),
                    df[[paste0(i,"_y")]],ifelse(grepl("h",i),ap4_y,hp4_y))]
  df[,d5:=pdist(df[[paste0(i,"_x")]],ifelse(grepl("h",i),ap5_x,hp5_x),
                    df[[paste0(i,"_y")]],ifelse(grepl("h",i),ap5_y,hp5_y))]
  df[,defender:=ifelse(Shooter==df[[i]],
    ifelse(d1==pmin(d1,d2,d3,d4,d5),ifelse(grepl("h",i),ap1,hp1),
    ifelse(d2==pmin(d1,d2,d3,d4,d5),ifelse(grepl("h",i),ap2,hp2),
    ifelse(d3==pmin(d1,d2,d3,d4,d5),ifelse(grepl("h",i),ap3,hp3),
    ifelse(d4==pmin(d1,d2,d3,d4,d5),ifelse(grepl("h",i),ap4,hp4),
    ifelse(d5==pmin(d1,d2,d3,d4,d5),ifelse(grepl("h",i),ap5,hp5),defender))))),defender)]
}
#df[,d1:=NULL][,d2:=NULL][,d3:=NULL][,d4:=NULL][,d5:=NULL]


# DEF TYPES: define
df[,deftype:=
  # If there is a missed shot or shooting foul
  ifelse(event%in%c("2PM","2PX","3PM","3PX","SF"),
    # If the shot was immediately after an ORB, it's tip defense
    ifelse(Reduce("|",shift(event=="ORB",1:20)),"defti",
    #If the defender is different from who it was a short time ago
    ifelse(defender!=shift(defender,25)|defender!=shift(defender,15)|defender!=shift(defender,10),
    # If it's close to the hoop, it's defhe, if not it's defclhe
    ifelse(((S.X<17&S.Y>10&S.Y<40)|(S.X>77&S.Y>10&S.Y<40)),"defhe","defclhe"),
    # If the defender moved more than 5 feet from 20 frames ago
    ifelse(pdist(D.X,shift(D.X,20),D.Y,shift(D.Y,20))>5&
      # And defender was 2+ feet away from shooter 20 frames ago
      pdist(shift(S.X,20),shift(D.X,20),shift(S.Y,20),shift(D.Y,20))>2&
      # And the shot isn't close to the hoop, then it's defcl
      !((S.X<17&S.Y>10&S.Y<40)|(S.X>77&S.Y>10&S.Y<40)),"defcl",
    # If there was a screen in the last 75 frames
    ifelse((Reduce("|",shift(pick==1,1:75))|
      # Or there was a pass in the last 60 frames
      Reduce("|",shift(event=="PASS",1:60)))&
      # And defender was 2+ feet away from shooter 20 frames ago
      pdist(shift(S.X,20),shift(D.X,20),shift(S.Y,20),shift(D.Y,20))>=2,
    # If the shot was close to the hoop, it's defre, else it's defcl
    ifelse((S.X<17&S.Y>10&S.Y<40)|(S.X>77&S.Y>10&S.Y<40),"defre","defcl"),
    # If the shooter is 10+ feet from the defender
    ifelse(pdist(D.X,S.X,D.Y,S.Y)>10&
      # And the shot is close to the hoop, then it's defna
      ((S.X<17&S.Y>10&S.Y<40)|(S.X>77&S.Y>10&S.Y<40)),"defna",
        # If the shooter is 10+ feet and it's away from the hoop it's defcl,
         # Otherwise that shit is def bruh
         ifelse(pdist(D.X,S.X,D.Y,S.Y)>10,"defcl","def")))))),NA)]