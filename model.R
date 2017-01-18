library(jsonlite)
library(bit64)
library(stringr)
library(zoo)
library(plyr)
library(dplyr)
library(data.table)
library(leaps)
library(randomForest)
library(gbm)
library(e1071)
library(glmnet)
library(caret)

path<-"C:/Users/brocatoj/Documents/Basketball/Tracking/"
# games<-fread(paste0(path,"meta/games.csv"))
# games<-games[date<as.Date('2016-07-01')]
# games<-games[,.(id,game)]
# current<-as.data.frame(list.files(paste0(path,"j_markings/")))
# names(current)<-"file"
# current<-mutate(current,id=as.integer(substr(file,1,10)))
# current<-left_join(current,games,by="id")
# current<-filter(current,!is.na(game)&str_count(file,"boxscore"))
# current<-mutate(current,file=paste0(path,"j_markings/",file))
# files<-current$file
# tables <- lapply(files, fread)
# data2<-data.table::rbindlist(tables)
# data2<-as.data.frame(data2)
# data2<-mutate(data2,games=1)
# data2_sums<-data2 %>%
#     select(player_id,games,second_played,FGM:FGA,FG3M:FG3A,FTM:FTA,OREB:PTS,
#                touches:past,blkd,def_made_2:defhe_missed_3,sf:pup)
# data2_means<-data2 %>%
#     mutate(DFGA=def_made_2+def_missed_2+def_made_3+def_missed_3+defre_made_2+
#                defre_missed_2+defre_made_3+defre_missed_2+defre_missed_3+
#                defhe_made_2+defhe_missed_2+defhe_made_3+defhe_missed_3) %>%
#     select(player_id,second_played,FGA,DFGA,FG_PCT,FG3_PCT,sefg,shot_clock,dist,
#            opp_sefg,pa,ddist:bdist)
# data2_sums <- ddply(data2_sums, .(player_id), colwise(sum))
# data2_sums2 <- lapply(data2_sums[4:ncol(data2_sums)], function(x) {round(x/data2_sums$second_played*2160,1)})
# data2_sums <- cbind(data2_sums[1:3], data2_sums2)
# data2_means<-as.data.table(data2_means)
# data2_means[,sefg:=weighted.mean(sefg,FGA),by="player_id"]
# data2_means[,shot_clock:=weighted.mean(shot_clock,FGA),by="player_id"]
# data2_means[,dist:=weighted.mean(dist,FGA),by="player_id"]
# data2_means[,opp_sefg:=weighted.mean(opp_sefg,DFGA),by="player_id"]
# data2_means[,pa:=weighted.mean(pa,DFGA),by="player_id"]
# data2_means[,ddist:=weighted.mean(ddist,second_played),by="player_id"]
# data2_means[,hdist:=weighted.mean(hdist,second_played),by="player_id"]
# data2_means[,bdist:=weighted.mean(bdist,second_played),by="player_id"]
# data2_means<-data2_means %>%
#     distinct(player_id,.keep_all = T) %>%
#     select(player_id,second_played,sefg:bdist) %>%
#     mutate(sefg=ifelse(is.nan(sefg),mean(sefg,na.rm=T),sefg),
#            shot_clock=ifelse(is.nan(shot_clock),mean(shot_clock,na.rm=T),shot_clock),
#            dist=ifelse(is.nan(dist),mean(dist,na.rm=T),dist),
#            opp_sefg=ifelse(is.nan(opp_sefg),mean(opp_sefg,na.rm=T),opp_sefg),
#            pa=ifelse(is.nan(pa),mean(pa,na.rm=T),pa))
# data2<-data2 %>%
#     select(player_id,player) %>%
#     distinct(player,.keep_all=T)
# data2<-left_join(data2,data2_sums,by="player_id")
# data2_means<-select(data2_means,-second_played)
# data2<-left_join(data2,data2_means,by="player_id")
# rapm<-fread("reg/rapm_plus_age.csv")
# rapm[,split:=orapm*2-rapm]
# rapm<-rapm[,.(player,rapm,split)]
# players<-fread(paste0(path,"meta/players_plus.csv"))
# players<-players[,.(ids_id,james_id,ht,wt)]
# players$wt<-as.numeric(players$wt)
# setnames(players,c("ids_id","james_id"),c("player_id","player"))
# rapm<-left_join(rapm,players,by="player")
# rapm<-select(rapm,player_id,ht,wt,rapm:split)
# data2<-left_join(data2,rapm,by="player_id")
# data2<-na.exclude(data2)
# data2<-data2 %>%
#     mutate(second_played=round(second_played/60,1)) %>%
#     rename("min"=second_played) %>%
#     mutate(mpg=min/games,
#            FG2M=FGM-FG3M,
#            FG2A=FGA-FG3A,
#            FG2MS=FG2A-FG2M,
#            FG3MS=FG3A-FG3M,
#            FTMS=FTA-FTM,
#            def_pts=def_made_2*2+def_made_3*3,
#            def_fga=def_missed_2+def_missed_3+def_made_2+def_missed_2,
#            defre_pts=defre_made_2*2+defre_made_3*3,
#            defre_fga=defre_missed_2+defre_missed_3+defre_made_2+defre_missed_2,
#            defhe_pts=defhe_made_2*2+defhe_made_3*3,
#            defhe_fga=defhe_missed_2+defhe_missed_3+defhe_made_2+defhe_missed_2,
#            DE=bit+bb+bbc+hn)
# setDT(data2)
# d2_adj <- data2[,lapply(.SD, function(x){(x*min+weighted.mean(x,min)*100)/(min+100)}),.SDcols=
#   c("mpg","PTS","FG2A","FG3A","FTA","OREB","DREB","AST","STL","BLK","TO","touches",
#     "passes","picks","past","sf","oc","poc","forced_tov","boxout","ora","bbb",
#     "bit","bb","bbc","hn","shn","pus","pup","opp_sefg","pa","ddist","hdist",
#     "bdist","def_pts","def_fga","defre_pts","defre_fga","defhe_pts","defhe_fga")]
# data2<-data2[,.(player_id,player,games,min,ht,wt,rapm,split)]
# data2<-cbind(data2,d2_adj)
# x<-as.matrix(select(data2,ht:wt,mpg:defhe_fga))
# y<-as.matrix(select(data2,rapm))
# y2<-as.matrix(select(data2,split))
# weights<-as.matrix(select(data2,min))
# 
# rf_model<-randomForest(rapm~.-player_id-player-split-min-games,data=data2,
#                    importance =TRUE)
# rf_split<-randomForest(split~.-player_id-player-rapm-min-games,data=data2,
#                    importance =TRUE)
# gbm_model<-gbm(rapm~.-player_id-player-split-min-games,data=data2,
#            weights=min,distribution="gaussian",n.trees=5000,
#            interaction.depth=5,shrinkage=0.01)
# gbm_split<-gbm(split~.-player_id-player-rapm-min-games
#            ,data=data2,
#            weights=min,distribution="gaussian", n.trees=5000,
#            interaction.depth=5,shrinkage=0.01)
# svm_model<-svm(rapm~.-player_id-player-split-min-games,data=data2,weights=min)
# svm_split<-svm(split~.-player_id-player-rapm-min-games,data=data2,weights=min)
# grid <- 10^seq(10,-2,length=100)
# glm_model<-glmnet(x,y,weights=weights,alpha=1,lambda=grid,standardize=F)
# glm_split<-glmnet(x,y2,weights=weights,alpha=1,lambda=grid,standardize=F)

load(file = "sst/models/rf_model.rda")
load(file = "sst/models/gbm_model.rda")
load(file = "sst/models/svm_model.rda")
load(file = "sst/models/glm_model.rda")
load(file = "sst/models/rf_split.rda")
load(file = "sst/models/gbm_split.rda")
load(file = "sst/models/svm_split.rda")
load(file = "sst/models/glm_split.rda")

# Individual seasons
data1<-data.frame()
for(year in 2014:2017){
path<-"C:/Users/brocatoj/Documents/Basketball/Tracking/"
games<-fread(paste0(path,"meta/games.csv"))
games<-games[date>as.Date(paste0(as.numeric(year)-1,'-10-01'))&
             date<as.Date(paste0(year,'-07-01'))]
games<-games[,.(id,game)]
current<-as.data.frame(list.files(paste0(path,"j_markings/")))
names(current)<-"file"
current<-mutate(current,id=as.integer(substr(file,1,10)))
current<-left_join(current,games,by="id")
current<-filter(current,!is.na(game)&str_count(file,"boxscore"))
current<-mutate(current,file=paste0(path,"j_markings/",file))
files<-current$file
tables <- lapply(files, fread)
data<-data.table::rbindlist(tables)
data<-as.data.frame(data)
games<-fread(paste0(path,"meta/games.csv"))
games<-games[,.(id,date)]
setnames(games,"id","game_id")
games$game_id<-as.character(games$game_id)
data<-left_join(data,games,by="game_id");setDT(data)
#data[,r_wt:=Sys.Date()-date]
data<-mutate(data,Year=year,games=1)
data_sums<-data %>%
    select(player_id,games,second_played,FGM:FGA,FG3M:FG3A,FTM:FTA,OREB:PTS,
           touches:past,blkd,def_made_2:defhe_missed_3,sf:pup)
data_means<-data %>%
    mutate(DFGA=def_made_2+def_missed_2+def_made_3+def_missed_3+defre_made_2+
               defre_missed_2+defre_made_3+defre_missed_2+defre_missed_3+
               defhe_made_2+defhe_missed_2+defhe_made_3+defhe_missed_3) %>%
    select(player_id,second_played,FGA,DFGA,FG_PCT,FG3_PCT,sefg,shot_clock,dist,
           opp_sefg,pa,ddist:bdist)
data_sums <- ddply(data_sums, .(player_id), colwise(sum))
data_sums2 <- lapply(data_sums[4:ncol(data_sums)], function(x) {round(x/data_sums$second_played*2160,1)})
data_sums <- cbind(data_sums[1:3], data_sums2)
data_means<-as.data.table(data_means)
data_means[,sefg:=weighted.mean(sefg,FGA),by="player_id"]
data_means[,shot_clock:=weighted.mean(shot_clock,FGA),by="player_id"]
data_means[,dist:=weighted.mean(dist,FGA),by="player_id"]
data_means[,opp_sefg:=weighted.mean(opp_sefg,DFGA),by="player_id"]
data_means[,pa:=weighted.mean(pa,DFGA),by="player_id"]
data_means[,ddist:=weighted.mean(ddist,second_played),by="player_id"]
data_means[,hdist:=weighted.mean(hdist,second_played),by="player_id"]
data_means[,bdist:=weighted.mean(bdist,second_played),by="player_id"]
data_means<-data_means %>%
    distinct(player_id,.keep_all = T) %>%
    select(player_id,second_played,sefg:bdist) %>%
    mutate(sefg=ifelse(is.nan(sefg),mean(sefg,na.rm=T),sefg),
           shot_clock=ifelse(is.nan(shot_clock),mean(shot_clock,na.rm=T),shot_clock),
           dist=ifelse(is.nan(dist),mean(dist,na.rm=T),dist),
           opp_sefg=ifelse(is.nan(opp_sefg),mean(opp_sefg,na.rm=T),opp_sefg),
           pa=ifelse(is.nan(pa),mean(pa,na.rm=T),pa))
data<-data %>%
    select(player_id,player,Year) %>%
    distinct(player,.keep_all=T)
data<-left_join(data,data_sums,by="player_id")
data_means<-select(data_means,-second_played)
data<-left_join(data,data_means,by="player_id")
players<-fread(paste0(path,"meta/players_plus.csv"))
players<-players[,.(ids_id,ht,wt)]
players$wt<-as.numeric(players$wt)
setnames(players,"ids_id","player_id")
data<-left_join(data,players,by="player_id");setDT(data)
data<-data %>%
    mutate(second_played=round(second_played/60,1)) %>%
    rename("min"=second_played) %>%
    mutate(mpg=min/games,
           FG2M=FGM-FG3M,
           FG2A=FGA-FG3A,
           FG2MS=FG2A-FG2M,
           FG3MS=FG3A-FG3M,
           FTMS=FTA-FTM,
           def_pts=def_made_2*2+def_made_3*3,
           def_fga=def_missed_2+def_missed_3+def_made_2+def_missed_2,
           defre_pts=defre_made_2*2+defre_made_3*3,
           defre_fga=defre_missed_2+defre_missed_3+defre_made_2+defre_missed_2,
           defhe_pts=defhe_made_2*2+defhe_made_3*3,
           defhe_fga=defhe_missed_2+defhe_missed_3+defhe_made_2+defhe_missed_2,
           DE=bit+bb+bbc+hn)
data1<-rbind(data1,data)
}

setDT(data1)
d1_adj <- data1[,lapply(.SD, function(x){(x*min+weighted.mean(x,min)*100)/(min+100)}),.SDcols=
  c("mpg","PTS","FG2A","FG3A","FTA","OREB","DREB","AST","STL","BLK","TO","touches",
    "passes","picks","past","sf","oc","poc","forced_tov","boxout","ora","bbb",
    "bit","bb","bbc","hn","shn","pus","pup","opp_sefg","pa","ddist","hdist",
    "bdist","def_pts","def_fga","defre_pts","defre_fga","defhe_pts","defhe_fga")]
data1<-data1[,.(player_id,player,Year,games,min,ht,wt)]
data1<-cbind(data1,d1_adj)
data1<-mutate(data1,rapm=0,split=0)
rf_proj<-as.data.frame(predict(rf_model,data1))
rf_sproj<-as.data.frame(predict(rf_split,data1))
gbm_proj<-as.data.frame(predict(gbm_model,data1,n.trees=1000))
gbm_sproj<-as.data.frame(predict(gbm_split,data1,n.trees=1000))
svm_proj<-as.data.frame(predict(svm_model,data1))
svm_sproj<-as.data.frame(predict(svm_split,data1))
newx<-as.matrix(select(data1,ht:wt,mpg:defhe_fga))
glm_proj<-as.data.frame(predict(glm_model,s=0.02,newx,type="response"))
glm_sproj<-as.data.frame(predict(glm_split,s=0.02,newx,type="response"))
names(rf_proj)<-"rf_proj"
names(rf_sproj)<-"rf_sproj"
names(gbm_proj)<-"gbm_proj"
names(gbm_sproj)<-"gbm_sproj"
names(svm_proj)<-"svm_proj"
names(svm_sproj)<-"svm_sproj"
names(glm_proj)<-"glm_proj"
names(glm_sproj)<-"glm_sproj"
data1<-cbind(data1,rf_proj,rf_sproj,gbm_proj,gbm_sproj,svm_proj,svm_sproj,
             glm_proj,glm_sproj)
data1<-mutate(data1,rf_oproj=round((rf_proj+rf_sproj)/2,1),
                    rf_dproj=round(rf_proj-rf_oproj,1),
                    gbm_oproj=round((gbm_proj+gbm_sproj)/2,1),
                    gbm_dproj=round(gbm_proj-gbm_oproj,1),
                    svm_oproj=round((svm_proj+svm_sproj)/2,1),
                    svm_dproj=round(svm_proj-svm_oproj,1),
                    glm_oproj=round((glm_proj+glm_sproj)/2,1),
                    glm_dproj=round(glm_proj-glm_oproj,1),
                    off=round((rf_oproj+gbm_oproj+svm_oproj+glm_oproj)/4,1),
                    def=round((rf_dproj+gbm_dproj+svm_dproj+glm_dproj)/4,1),
                    impact=round(off+def,1))

# save(rf_model, file = "sst/models/rf_model.rda")
# save(gbm_model,file = "sst/models/gbm_model.rda")
# save(svm_model,file = "sst/models/svm_model.rda")
# save(glm_model,file = "sst/models/glm_model.rda")
# save(rf_split, file = "sst/models/rf_split.rda")
# save(gbm_split,file = "sst/models/gbm_split.rda")
# save(svm_split,file = "sst/models/svm_split.rda")
# save(glm_split,file = "sst/models/glm_split.rda")
# write.csv(data1,"reg/impact.csv",row.names=F)

players<-fread("C:/Users/brocatoj/Documents/Basketball/Tracking/meta/players_plus.csv")
players<-select(players,ids_id,james_id,birth_date)
setnames(players,c("player_id","player","birth_date"))
data1<-select(data1,-player)
data1<-left_join(data1,players,by="player_id")
data1<-select(data1,player_id,player,birth_date,Year:impact)

imp17<-data1 %>%
    filter(Year==2017) %>%
    select(player,min,off,def,impact) %>%
    rename("oimpact"=off,"dimpact"=def) %>%
    arrange(desc(impact))
write.csv(imp17,"C:/Users/brocatoj/Documents/Basketball/Game/2017/impact.csv",
          row.names = F)

age<-fread("reg/rapm_age_curve.csv")
age[,off:=(prior+prior_split)/2]
age[,def:=prior-off]
age[,odiff:=shift(off,type="lead")-off]
age[,ddiff:=shift(def,type="lead")-def]
age<-age[,.(age,odiff,ddiff)]

for(i in 2014:2017){
    df<-data1 %>%
        filter(Year==i) %>%
        mutate(age=paste0("age_",floor(as.numeric(as.Date(paste0(i,'-02-01'))-
                                                  as.Date(birth_date))/365.2422)))
    df<-left_join(df,age,by="age")
    if(i==2017){
        df<-select(df,player,min,off,def)
    }else{
    df<-df %>%
        mutate(off=off+odiff,def=def+ddiff) %>%
        select(player,min,off,def)
    }
    assign(paste0("impact",i),df)
    rm(df)
}
impact<-distinct(data1,player)
impact<-left_join(impact,impact2017,by="player")
impact<-left_join(impact,impact2016,by="player")
impact<-left_join(impact,impact2015,by="player")
impact<-left_join(impact,impact2014,by="player")
setDT(impact)
setnames(impact,c("player","min17","off17","def17","min16","off16","def16",
                    "min15","off15","def15","min14","off14","def14"))
impact[is.na(impact)] <- 0
impact[,oimpact:=round((off17*min17+off16*min16*0.5+off15*min15*0.25+off14*min14*0.125)/
             (min17+min16*0.5+min15*0.25+min14*0.125),2)]
impact[,dimpact:=round((def17*min17+def16*min16*0.5+def15*min15*0.25+def14*min14*0.125)/
             (min17+min16*0.5+min15*0.25+min14*0.125),2)]
impact[,min:=min17+min16+min15+min14]
impact[,oimpact:=ifelse(min<500|min17==0,round((-2*250+oimpact*min)/(250+min),2),oimpact)]
impact[,dimpact:=ifelse(min<500|min17==0,round((-1.5*250+dimpact*min)/(250+min),2),dimpact)]
impact[,min:=min17]
impact[,impact:=round(oimpact+dimpact,2)]
impact<-impact[,.(player,min,oimpact,dimpact,impact)]
impact<-impact %>%
    distinct(player,.keep_all=T) %>%
    arrange(desc(impact))
write.csv(impact,"C:/Users/brocatoj/Documents/Basketball/Game/2017/adj_impact.csv",row.names=F)

# Set priors
age<-fread("reg/rapm_age_curve.csv")
players<-fread("C:/Users/brocatoj/Documents/Basketball/Tracking/meta/players_plus.csv")
players[,age:=paste0("age_",
        floor(as.numeric(as.Date('2017-02-01')-as.Date(birth_date))/365.2422))]
players<-select(players,james_id,age)
setnames(players,c("player","age"))
priors<-left_join(impact,players,by="player")
priors<-left_join(priors,age,by="age")
acl<-fread("reg/acl_injuries.csv")
acl[,acl_prior:=0.9][,acl_split:=0.94]
acl<-acl[,.(Player,acl_prior,acl_split)]
setnames(acl,"Player","player")
achilles<-fread("reg/achilles_injuries.csv")
achilles[,achilles_prior:=3.08][,achilles_split:=2.02]
achilles<-achilles[,.(Player,achilles_prior,achilles_split)]
setnames(achilles,"Player","player")
priors<-left_join(priors,acl,by="player")
priors<-left_join(priors,achilles,by="player")
priors[is.na(priors)] <- 0
priors<-priors %>%
    mutate(p=impact-prior+acl_prior+achilles_prior,
           ps=(oimpact*2-impact)-prior_split+achilles_split+acl_split) %>%
    select(player,p,ps)
setnames(priors,"player","name")
rapm_priors<-fread("reg/rapm_priors.csv")
rapm_priors<-left_join(rapm_priors,priors,by="name")
rapm_priors<-rapm_priors %>%
    mutate(prior=ifelse(is.na(p),prior,p),
           prior_split=ifelse(is.na(ps),prior_split,ps)) %>%
    select(name:prior_split) %>%
    distinct(name,.keep_all=T)
write.csv(rapm_priors,"reg/rapm_priors.csv",row.names=F)
