# Update Games
source("sst/games.R")

# Loop Intern, Lineup Sheets,PBP, 
path<-"C:/Users/brocatoj/Documents/Basketball/Tracking/"
gameids<-fread(paste0(path,"meta/games.csv"))
gameids$date<-as.Date(gameids$date)
gameids<-gameids[order(date)]
gameids<-gameids[date==Sys.Date()-1]
game_ids<-as.character(gameids$id)
for(gameid in game_ids){
    print(gameid)
    js<-fromJSON(paste0(path,"markings/",gameid,".json"))
    players<-fread(paste0(path,"meta/players_plus.csv"))
    source("sst/intern_contest_in.R")
    source("sst/create_lineup_sheet.R")
}
game_ids<-as.character(gameids$ids_id)
for(gameid in game_ids){
    print(gameid)
    source("pbp.R")
}
game_ids<-as.character(gameids$game)
for(gamecode in game_ids){
    print(gamecode)
    source("game.R")
}

source("update_lineups.R")
