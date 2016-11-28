library(XML)
library(jsonlite)
library(stringr)
library(tidyr)
library(zoo)
library(plyr)
library(dplyr)
library(data.table)

all_games<-fread("C:/Users/brocatoj/Documents/Basketball/Tracking/meta/games.csv")
game_codes<-all_games %>%
    arrange(as.Date(date))
already<-list.files("E:/Tracking/frames_csv/",pattern=".csv")
already<-sapply(already,function(x){substr(x,1,10)})
already<-as.data.frame(already)
already<-mutate(already,id=already)
already$id<-as.integer(as.character(already$id))
game_codes<-left_join(game_codes,already,by="id")
game_codes<-game_codes %>%
    filter(is.na(already)) %>%
    select(id) %>%
    filter(!id%in%c("2013110217","2013120126","2014011601","2014031230",
                    "2014111218"))
game_codes<-as.character(game_codes$id)


for (gameid in game_codes) {
    print(gameid)
    location <- "Y"
    source("sst/frames_ss.R")
    gc()
}

rm(list = ls())