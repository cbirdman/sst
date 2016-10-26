library(data.table)

games<-fread("C:/Users/brocatoj/Documents/Basketball/Tracking/meta/games.csv")
games<-games[,.(stats_id)]


for (i in games) {
    print(i)
    location <- "Y"
    gameid <- i
    source("sst/frames.R")
    gc()
}

rm(list = ls())