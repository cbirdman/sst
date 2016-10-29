path<-"C:/Users/brocatoj/Documents/Basketball/Tracking/"
gameids<-fread(paste0(path,"meta/games.csv"))
gameids<-gameids[date==Sys.Date()-1]
gameids<-as.character(gameids$id)
for(gameid in gameids){
    js<-fromJSON(paste0(path,"markings/",gameid,".json"))
    players<-fread(paste0(path,"meta/players_plus.csv"))
    source("sst/intern_contest_in.R")
}