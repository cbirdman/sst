library(XML)
library(jsonlite)
library(stringr)
library(tidyr)
library(zoo)
library(plyr)
library(dplyr)
library(data.table)

all_games<-fread("C:/Users/brocatoj/Documents/Basketball/Tracking/meta/games.csv")
game_codes<-arrange(all_games,as.Date(date))
already<-list.files("E:/Tracking/frames_csv/",pattern=".csv")
already<-sapply(already,function(x){substr(x,1,10)})
already<-as.data.frame(already)
already<-mutate(already,id=already)
already$id<-as.integer(as.character(already$id))
game_codes<-left_join(game_codes,already,by="id")
game_codes<-game_codes %>%
    filter(is.na(already)) %>%
    select(stats_id) %>%
    filter(!stats_id%in%c("2013110215","2013120123","2014011601","2014031227",
                          "2014111216","2014111214","2014111724","2014111722",
                          "2014111729","2014111730","2014111717","2014111826",
                          "2014111823","2014111815","2014111801"))
game_codes<-as.character(game_codes$stats_id)


for (gameid in game_codes) {
    print(gameid)
    location <- "Y"
    source("sst/frames.R")
    gc()
}

rm(list = ls())