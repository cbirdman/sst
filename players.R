library(RCurl)
library(XML)
library(jsonlite)
library(plyr)
library(dplyr)
library(data.table)

# Extract basic info from nba.com
js <- fromJSON(paste0("http://stats.nba.com/stats/commonallplayers/",
                       "?LeagueID=00&Season=2016-17&IsOnlyCurrentSeason=0"))
nba_codes <- as.data.table(js$resultSets$rowSet[[1]])
head<-js$resultSets$headers[[1]]
setnames(nba_codes,head)
nba_codes[,c("last", "first"):=tstrsplit(DISPLAY_LAST_COMMA_FIRST,", ",fixed=T)]
nba_codes<-nba_codes[,.(PERSON_ID,DISPLAY_FIRST_LAST,first,last,FROM_YEAR,
                        TO_YEAR,ROSTERSTATUS,TEAM_ID,TEAM_ABBREVIATION,
                        GAMES_PLAYED_FLAG)]
setnames(nba_codes,c("ids_id","name","first","last","from_year","to_year",
                     "on_roster","team_id","team","games_flag"))

# Extract different ids from second spectrum
players<-as.data.table(fromJSON(paste0("C:/Users/brocatoj/Documents/Basketball/",
                                        "Tracking/meta/players.json")))
players<-players[,.(ids_id,ids_id_alt,stats_id,id)]
setkey(players,ids_id)
setkey(nba_codes,ids_id)
players<-players[nba_codes]
players<-players[,lapply(.SD, as.character)]

# Extract extra info from local file
extra<-fread("C:/Users/brocatoj/Documents/Basketball/Tracking/meta/players_plus.csv")
extra<-extra[,.(ids_id,james_id,bref_id,cbref_id,ebref_id,dx_id,birth_date,ht,
                wt,pos_id,pos_name,draft_year)]
extra$ids_id<-as.character(extra$ids_id)
setkey(extra,ids_id)
players<-extra[players]
players<-players[,c(1,13:15,2:6,16:23,7:12),with=F]
players<-players[order(-to_year,team,-pos_id)]
players[,lcard_name:=paste0(substr(first,1,1),".",last)]
write.csv(players,"C:/Users/brocatoj/Documents/Basketball/Tracking/meta/players_plus.csv",
          row.names=F)

# Extract missing info from nba.com
missing<-players[is.na(ht)]
nba_ext<-data.table()
for (i in missing$ids_id){
    p<-fromJSON(paste0("http://stats.nba.com/stats/",
                       "commonplayerinfo/?PlayerID=",i))
    pt<-as.data.table(p$resultSets$rowSet[[1]])
    nba_ext<-rbind(nba_ext,pt)
}
head<-p$resultSets$headers[[1]]
setnames(nba_ext,head)
nba_ext<-nba_ext[,.(PERSON_ID,BIRTHDATE,HEIGHT,WEIGHT,POSITION)]

nba_ext[,c("feet", "inches"):=tstrsplit(HEIGHT,"-",fixed=T)]
nba_ext[,HT:=as.numeric(feet)*12+as.numeric(inches)]
nba_ext[,BDATE:=as.Date(substr(BIRTHDATE,1,10))]
nba_ext[,POS:=substr(POSITION,1,1)]
nba_ext<-nba_ext[,.(PERSON_ID,BDATE,HT,WEIGHT,POS)]
setnames(nba_ext,"PERSON_ID","ids_id")
players<-left_join(players,nba_ext,by="ids_id");setDT(players)
players[,birth_date:=ifelse(is.na(birth_date),BDATE,birth_date)]
players[,ht:=ifelse(is.na(ht),HT,ht)]
players[,wt:=ifelse(is.na(wt),WEIGHT,wt)]
players[,pos_name:=ifelse(is.na(pos_name),POS,pos_name)]
players<-players[,1:23,with=F]
write.csv(players,"C:/Users/brocatoj/Documents/Basketball/Tracking/meta/players_plus.csv",
          row.names=F)
players_json<-toJSON(players)
write(players_json,
      "C:/Users/brocatoj/Documents/Basketball/Tracking/meta/players_plus.json")