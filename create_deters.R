# Forced turnovers
frames[,fort:=0]
for(i in c("ap1","ap2","ap3","ap4","ap5","hp1","hp2","hp3","hp4","hp5")){
    frames[,fort:=ifelse(event=="TO",frames[[gsub("P","D",i)]],fort)]
}

# KIF: define
frames[,kif:=
  # If the distance between the ballhandler and the hoop
  ifelse((pdist(bh_x,ifelse(pend==1,4,90),bh_y,25)-
          # is 2+ feet greater than it was a second ago
          pdist(shift(bh_x,25),ifelse(pend==1,4,90),shift(bh_y,25),25))>2&
          # And the ballhanlder is within 27 feet of the hoop
          pdist(bh_x,ifelse(pend==1,4,90),bh_y,25)<27&
          # And the bh & bhd are continuous
          bh==shift(bh,25)&bhd==shift(bhd,25),
  bhd,0)]

# KIF: remove replicates
frames[,kif:=ifelse(Reduce("|",shift(kif!=0,1:50)),0,kif),]