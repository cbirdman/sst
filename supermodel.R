library(plyr)
library(dplyr)
library(data.table)
library(glmnet)
library(stringr)
library(Matrix)
library(randomForest)

ap<-data.frame()
# Load data
for (z in 2015:2017){
    matchups <- fread(paste0("C:/Users/brocatoj/Documents/Basketball/Game/",z,
                               "/ytd_matchups.csv"))
    matchups[,c("minutes","sec"):=tstrsplit(Time_Left,":",fixed=T)]
    matchups[,date:=as.Date(substr(Game,1,10))]
    matchups[,home_team:=substr(Game,16,18)][,away_team:=substr(Game,12,14)]
    matchups[,c("hp1","hp2","hp3","hp4","hp5"):= tstrsplit(hfive," - ",fixed=T)]
    matchups[,c("ap1","ap2","ap3","ap4","ap5"):= tstrsplit(afive," - ",fixed=T)]
    matchups<-matchups %>%
        select(Game,date:away_team,Period,hp1:ap5,min:margin)
    amatchups<-select(matchups,Game,date,away_team,home_team,Period,ap1:ap5,
                      min:secsleft,arest,hrest,aposs,hposs,apts,hpts)
    matchups<-select(matchups,Game:Period,hp1:hp5,min:arest,hposs:apts)
    setnames(matchups,c("Game","date","team","opponent","Period","p1","p2","p3",
                        "p4","p5","min","start","secsleft","rest","opp_rest",
                        "poss","opp_poss","pts","opp_pts"))
    setnames(amatchups,c("Game","date","team","opponent","Period","p1","p2","p3",
                        "p4","p5","min","start","secsleft","rest","opp_rest",
                        "poss","opp_poss","pts","opp_pts"))
    matchups[,ortg:=ifelse(poss==0,0,(pts/poss*100)-107)/5]
    matchups[,drtg:=ifelse(opp_poss==0,0,107-(opp_pts/opp_poss*100))/5]
    amatchups[,ortg:=ifelse(poss==0,0,(pts/poss*100)-107)/5]
    amatchups[,drtg:=ifelse(opp_poss==0,0,107-(opp_pts/opp_poss*100))/5]
    matchups<-rbind(matchups,amatchups)
    rm(amatchups)

    # Add supermodel
    sm<-fread("reg/supermodel.csv")
    sm<-sm[Year==z-1]
    sm<-select(sm,-Year)
    for(i in c("p1","p2","p3","p4","p5")){
        setnames(sm,"player",i)
        matchups<-left_join(matchups,sm,by=i)
        setDT(matchups)
        setnames(sm,i,"player")
    }
    # for(i in names(matchups[,190:231])){
    #     matchups[,(i):=(matchups[[i]]+matchups[[paste0(i,".x")]]+
    #                  matchups[[paste0(i,".y")]]+matchups[[paste0(i,".x.x")]]+
    #                  matchups[[paste0(i,".y.y")]])/5]
    # }
#matchups<-select(matchups,Game:drtg,ht.x:defhe_fga)
ap<-rbind(ap,matchups)
rm(matchups)
}
pg<-select(ap,poss:drtg,ht.x:defhe_fga.x)
sg<-select(ap,poss:drtg,ht.y:defhe_fga.y)
sf<-select(ap,poss:drtg,ht.x.x:defhe_fga.x.x)
pf<-select(ap,poss:drtg,ht.y.y:defhe_fga.y.y)
c<-select(ap,poss:drtg,ht:defhe_fga)
setnames(pg,names(c))
setnames(sg,names(c))
setnames(sf,names(c))
setnames(pf,names(c))
ap<-rbind(pg,sg,sf,pf,c)
rm(pg,sg,sf,pf,c)
clean<-na.exclude(ap)
fito<-lm(ortg~mpg+PTS+FG2A+FG3A+FTA+OREB+AST+TO+touches+passes+picks+past+
             oc+poc,data=clean,weights=poss)
fitd<-lm(drtg~.-poss-opp_poss-drtg-pts-opp_pts,data=clean,weights=opp_poss)
oproj<-predict(fito,sm)
#dproj<-predict(fitd,sm)
names(oproj)<-"oproj"
sm<-cbind(sm,oproj)
