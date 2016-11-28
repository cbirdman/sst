library(jsonlite)
library(data.table)

# Download boxscore from nba
games<-fread("C:/Users/brocatoj/Documents/Basketball/Tracking/meta/games.csv")
games<-games[id==gameid,.(id,ids_id)]
ids_id<-games$ids_id[1]
url <- fromJSON(paste0("http://stats.nba.com/stats/boxscoretraditionalv2?GameID=",
                        ids_id,"&StartPeriod=0&EndPeriod=0&StartRange=0&",
                        "EndRange=0&RangeType=0"))
head<-url$resultSets$headers[[1]]
box<-as.data.table(url$resultSets$rowSet[[1]])
setnames(box,head)
rm(head,url)
box<-box[!is.na(MIN)]
setnames(box,"PLAYER_ID","player_id")

# Read players for id lookup
ap<-players[,.(id,ids_id)]
setnames(ap,c("eagle_id","player_id"))

# think about: blkt/blko, tovl,tovd, ofd, charge, etc
# other still needed: kif, deter, gravity, 


# touches: passes,touches,dribbles
touches<-as.data.table(js$touches)
touches<-touches[,.(player,touches,dribbles,passes)]
touches$player<-as.character(touches$player)
setnames(touches,"player","eagle_id")
touches<-left_join(touches,ap,by="eagle_id")
box<-left_join(box,touches,by="player_id");setDT(box)

# shots
#   first offense:
off_shots<-shots[,.(player_id,potential_assist,sefg,shot_clock,blocked,shot_dist)]
off_shots[,past:=sum(potential_assist), by=player_id]
off_shots[,sefg:=mean(sefg),by=player_id][,shot_clock:=mean(shot_clock),by=player_id]
off_shots[,blkd:=sum(blocked),by=player_id][,dist:=mean(shot_dist),by=player_id]
off_shots<-off_shots[,.(player_id,past,sefg,shot_clock,blkd,dist)]
off_shots<-distinct(off_shots,player_id,.keep_all = T)
box<-left_join(box,off_shots,by="player_id");setDT(box)

#   then defense:
def_shots<-shots[,.(dplayer1_id,made,def_type,is_three,sefg,pa_sefg,fouled,contested)]
def_shots[,pa:=pa_sefg-sefg]
def_shots[,def_made_2:=sum(def_type=="def"&is_three==F&made==T),by="dplayer1_id"]
def_shots[,def_missed_2:=sum(def_type=="def"&is_three==F&made==F),by="dplayer1_id"]
def_shots[,def_made_3:=sum(def_type=="def"&is_three==T&made==T),by="dplayer1_id"]
def_shots[,def_missed_3:=sum(def_type=="def"&is_three==T&made==F),by="dplayer1_id"]
# recover
def_shots[,defre_made_2:=sum(def_type%in%c("defcl","defre")&is_three==F&made==T),by="dplayer1_id"]
def_shots[,defre_missed_2:=sum(def_type%in%c("defcl","defre")&is_three==F&made==F),by="dplayer1_id"]
def_shots[,defre_made_3:=sum(def_type%in%c("defcl","defre")&is_three==T&made==T),by="dplayer1_id"]
def_shots[,defre_missed_3:=sum(def_type%in%c("defcl","defre")&is_three==T&made==F),by="dplayer1_id"]
# help
def_shots[,defhe_made_2:=sum(def_type%in%c("defhe","defclhe")&is_three==F&made==T),by="dplayer1_id"]
def_shots[,defhe_missed_2:=sum(def_type%in%c("defhe","defclhe")&is_three==F&made==F),by="dplayer1_id"]
def_shots[,defhe_made_3:=sum(def_type%in%c("defhe","defclhe")&is_three==T&made==T),by="dplayer1_id"]
def_shots[,defhe_missed_3:=sum(def_type%in%c("defhe","defclhe")&is_three==T&made==F),by="dplayer1_id"]

def_shots[,sefg:=mean(sefg),by="dplayer1_id"][,pa:=mean(pa),by="dplayer1_id"]
def_shots[,sf:=sum(fouled),by="dplayer1_id"][,con:=sum(contested),by="dplayer1_id"]

def_shots<-select(def_shots,dplayer1_id,def_made_2:defhe_missed_3,sefg,pa,sf,con)
def_shots<-distinct(def_shots,dplayer1_id,.keep_all = T)
setnames(def_shots,c("dplayer1_id","sefg"),c("player_id","opp_sefg"))
box<-left_join(box,def_shots,by="player_id");setDT(box)

# passes
setDT(passes)
pass<-passes[true_assist==T|opportunity_created==T]
pass<-pass[,.(passer,true_assist,opportunity_created,from_pick)]
pass[,tast:=sum(true_assist),by="passer"]
pass[,oc:=sum(opportunity_created==T&from_pick==F),by="passer"]
pass[,poc:=sum(opportunity_created==T&from_pick==T),by="passer"]
pass<-pass[,.(passer,tast,oc,poc)]
pass<-distinct(pass,passer,.keep_all = T)
setnames(pass,"passer","player_id")
box<-left_join(box,pass,by="player_id");setDT(box)

# turnovers
#tov<-as.data.table(js$turnovers)
#   forced turnovers (i still have to create this lol)

# drives
drives<-as.data.table(js$drives)
drives[,drives:=1][,drives:=sum(drives),by="bhr"]
drives<-drives[,.(bhr,drives)]
drives<-distinct(drives,bhr,.keep_all = T)
setnames(drives,"bhr","eagle_id")
drives$eagle_id<-as.character(drives$eagle_id)
box<-left_join(box,drives,by="eagle_id");setDT(box)

# charges
# charges<-as.data.table(js$charges)
# charges<-charges[,charge:=1][,charge:=sum(charge),by="taker_id"]
# charges<-distinct(charges,taker_id,.keep_all=T)
# charges<-charges[,.(taker_id,charge)]
# setnames(charges,"taker_id","eagle_id")
# charges$eagle_id<-as.character(charges$eagle_id)
# box<-left_join(box,charges,by="eagle_id");setDT(box)
# box[,charge:=ifelse(is.na(charge),0,charge)]

# rebounds
reb<-rebounds
dt<-data.table()
for(i in 0:4){
    reb1<-reb[,.(
             rebounds[[paste0("def_player_",i,"_id")]],
             rebounds[[paste0("def_player_",i,"_crash")]],
             rebounds[[paste0("def_player_",i,"_box_out")]],
             rebounds[[paste0("def_player_",i,"_leak_out")]],
             rebounds[[paste0("def_player_",i,"_ball_watch")]],
             rebounds[[paste0("def_player_",i,"_offensive_rebound_allowed")]])]
    setnames(reb1,c("player_id","crash","boxout","leakout","ballwatch","ora"))
    dt<-rbind(dt,reb1)
    rm(reb1)
}
reb<-dt;rm(dt)
reb[is.na(reb)] <- F
reb<-reb[,lapply(.SD, as.numeric)]
reb[,crash:=sum(crash),by="player_id"]
reb[,boxout:=sum(boxout),by="player_id"]
reb[,leakout:=sum(leakout),by="player_id"]
reb[,ballwatch:=sum(ballwatch),by="player_id"]
reb[,ora:=sum(ora),by="player_id"]
reb<-distinct(reb,player_id,.keep_all = T)
reb$player_id<-as.character(reb$player_id)
box<-left_join(box,reb,by="player_id");setDT(box)

# transitions
transitions<-trans
dt<-data.table()
for(i in 1:5){
    trans1<-trans[,.(
        trans[[paste0("def_player",i)]],
        trans[[paste0("def_player_",i,"_beat_by_ball")]],
        trans[[paste0("def_player_",i,"_beat_in_transition")]]
    )]
    setnames(trans1,c("player_id","bbb","bit"))
    dt<-rbind(dt,trans1)
    rm(trans1)
}
transitions<-dt;rm(dt)
transitions[is.na(transitions)]<-F
transitions<-transitions[,lapply(.SD, as.numeric)]
transitions[,bbb:=sum(bbb),by="player_id"]
transitions[,bit:=sum(bit),by="player_id"]
transitions<-distinct(transitions,player_id,.keep_all = T)
transitions$player_id<-as.character(transitions$player_id)
box<-left_join(box,transitions,by="player_id");setDT(box)


# defensive errors
#   future plan: adjust defensive errors for who they're against (like pa_sefg)
setDT(de)
der<-de[,.(defender,error_type)]
der[,bb:=sum(error_type=="bb"),by="defender"]
der[,bbcl:=sum(error_type=="bbcl"),by="defender"]
der[,bbc:=sum(error_type=="bbc"),by="defender"]
der[,hn:=sum(error_type=="hn"),by="defender"]
der[,shn:=sum(error_type=="shn"),by="defender"]
der<-der[,.(defender,bb,bbcl,bbc,hn,shn)]
der<-distinct(der,defender,.keep_all=T)
setnames(der,"defender","player_id")
box<-left_join(box,der,by="player_id")

# spacing
spacing<-bst[,.(player_id,ball_stop_type)]
spacing[,pus:=sum(ball_stop_type=="PUS"),by="player_id"]
spacing[,pup:=sum(ball_stop_type=="PUP"),by="player_id"]
spacing<-spacing[,.(player_id,pus,pup)]
spacing<-distinct(spacing,player_id,.keep_all = T)
box<-left_join(box,spacing,by="player_id")
box<-left_join(box,gravity,by="player_id")

box[is.na(box)] <- 0

# Write to file
write.csv(box,paste0("C:/Users/brocatoj/Documents/Basketball/Tracking/j_markings/",
          gameid,"_boxscore.csv"),row.names=F)

#Remove unnecessary dataframes
rm(list= ls()[!(ls() %in% c('gameid','frames2','frames_reb','markings','js',
                            'pdist','players','bst','gravity','trans','shots',
                            'passes','de','rebounds','box'))])