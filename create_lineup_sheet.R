# ...
#gameid<-"2016042307"
path<-"C:/Users/brocatoj/Documents/Basketball/Tracking/"

js<-fromJSON(paste0(path,"markings/",gameid,".json"))

games<-fread(paste0(path,"meta/games.csv"))
games_current<-games[id==gameid,.(id,away,home)]

players<-fread(paste0(path,"meta/players_plus.csv"))
players_pos<-players[,.(id,pos_id)]
players_lc<-players[,.(pos_id,lcard_name)]

teams<-fread(paste0(path,"meta/teams.csv"))
teams_lc<-teams[,.(id,abbrev)]
setnames(teams_lc,"abbrev","team")
teams_lc<-teams_lc[,lapply(.SD, as.character)]

lineup<-as.data.table(js$chances)
lineup<-lineup[,.(period,end_game_clock,oteam,offense0,offense1,offense2,offense3,offense4)]
setnames(lineup,c("oteam","offense0","offense1","offense2","offense3","offense4"),
                c("id","p1","p2","p3","p4","p5"))
lineup<-lineup[,lapply(.SD, as.character)]
lineup<-left_join(lineup,teams_lc,by="id");setDT(lineup)
lineup<-select(lineup,-id)

for(i in c("p1","p2","p3","p4","p5")){
    setnames(lineup,i,"id")
    lineup<-left_join(lineup,players_pos,by="id");setDT(lineup)
    lineup<-select(lineup,-id)
    setnames(lineup,"pos_id",i)
}

lineup_p<-select(lineup,p1:p5)
lineup<-select(lineup,period:team)
lineup_p<-t(lineup_p)
lineup_p<-apply(lineup_p,2,sort,decreasing=F)
lineup_p<-as.data.table(t(lineup_p))
lineup<-cbind(lineup,lineup_p)
rm(lineup_p)

for(i in c("V1","V2","V3","V4","V5")){
    setnames(lineup,i,"pos_id")
    lineup<-left_join(lineup,players_lc,by="pos_id");setDT(lineup)
    lineup<-select(lineup,-pos_id)
    setnames(lineup,"lcard_name",i)
}

lineup$end_game_clock<-as.numeric(lineup$end_game_clock)

half1<-as.data.table(c("1_1", "1_2", "1_3", "1_4", "1_5", "1_6", 
                      "1_7", "1_8", "1_9", "1_10", "1_11", "1_12",
                      "2_1", "2_2", "2_3", "2_4", "2_5", "2_6", 
                      "2_7", "2_8", "2_9", "2_10", "2_11", "2_12"))
setnames(half1,"time")
half2<-as.data.table(c("3_1", "3_2", "3_3", "3_4", "3_5", "3_6", 
                      "3_7", "3_8", "3_9", "3_10", "3_11", "3_12",
                      "4_1", "4_2", "4_3", "4_4", "4_5", "4_6", 
                      "4_7", "4_8", "4_9", "4_10", "4_11", "4_12"))
setnames(half2,"time")

home<-lineup[team==games_current$home]
away<-lineup[team==games_current$away]

# Home/Away Split
dtList <- list(home=home, away=away)
dtList <- lapply(dtList, function(dt) {
    dt[,time:=floor(end_game_clock/60)]
    dt[,time:=12-time]
    dt[,time:=paste0(period,"_",time)]
    dt<-dt[!rev(duplicated(rev(dt$time))),]
    dt[,lineup:=paste0(V1,V2,V3,V4,V5)]
    dt[,filt:=ifelse(period!=shift(period),1,
                       ifelse(lineup!=shift(lineup),1,0))]
    dt[,filt:=ifelse(is.na(filt),1,filt)]
    dt<-dt[filt==1,.(time,V1,V2,V3,V4,V5)]
})
home<-dtList$home
away<-dtList$away
rm(dtList)
for(p in c("V1","V2","V3","V4","V5")){
    home[,(p):=ifelse(substr(time,3,4)==1&as.numeric(substr(time,1,1))%%2!=0,home[[p]],
                    ifelse(home[[p]]==shift(home[[p]]),NA,home[[p]]))]
}
for(p in c("V1","V2","V3","V4","V5")){
    away[,(p):=ifelse(substr(time,3,4)==1&as.numeric(substr(time,1,1))%%2!=0,away[[p]],
                    ifelse(away[[p]]==shift(away[[p]]),NA,away[[p]]))]
}

home_1<-left_join(half1,home,by="time")
home_2<-left_join(half2,home,by="time")
away_1<-left_join(half1,away,by="time")
away_2<-left_join(half2,away,by="time")

dfList <- list(home_1=home_1,home_2=home_2,away_1=away_1,away_2=away_2)
dfList <- lapply(dfList, function(df) {
    df[is.na(df)] <- ""
    df<-select(df,-time)
    df<-as.data.table(t(df))
    setnames(df,as.character(rep(1:12,2)))
})

write.csv(dfList$home_1,paste0(path,"lineup_sheets/",games_current$id,"_",
                               games_current$home,"_half1.csv"),row.names=F)
write.csv(dfList$home_2,paste0(path,"lineup_sheets/",games_current$id,"_",
                               games_current$home,"_half2.csv"),row.names=F)
write.csv(dfList$away_1,paste0(path,"lineup_sheets/",games_current$id,"_",
                               games_current$away,"_half1.csv"),row.names=F)
write.csv(dfList$away_2,paste0(path,"lineup_sheets/",games_current$id,"_",
                               games_current$away,"_half2.csv"),row.names=F)