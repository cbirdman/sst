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
for(i in names(box)[10:28]){
    box[[i]]<-as.numeric(box[[i]])
}
box[,c("min","sec"):=tstrsplit(MIN, ":", fixed=TRUE)]
box[,seconds_played:=as.numeric(min)*60+as.numeric(sec)][,min:=NULL][,sec:=NULL]

# Read players for id lookup
ap<-players[,.(id,ids_id)]
setnames(ap,c("eagle_id","player_id"))

# other still needed: kif, deter 

# touches: passes,touches,dribbles
touches<-as.data.table(js$touches)
touches<-touches[,.(player,touches,dribbles,passes)]
touches$player<-as.character(touches$player)
setnames(touches,"player","eagle_id")
touches<-left_join(touches,ap,by="eagle_id")
box<-left_join(box,touches,by="player_id");setDT(box)

# picks: picks
picks<-as.data.table(js$picks)
picks<-picks[,.(screener)]
picks$screener<-as.character(picks$screener)
picks[,picks:=1][,picks:=sum(picks),by="screener"]
picks<-distinct(picks,picks,.keep_all = T)
setnames(picks,"screener","eagle_id")
box<-left_join(box,picks,by="eagle_id");setDT(box)

# shots
#   first offense:
off_shots<-shots[,.(player_id,potential_assist,sefg,shot_clock,blocked,shot_dist)]
off_shots[,shot_clock:=ifelse(is.na(shot_clock),12,shot_clock)]
off_shots[,astd:=sum(potential_assist), by=player_id]
off_shots[,sefg:=mean(sefg),by=player_id][,shot_clock:=mean(shot_clock),by=player_id]
off_shots[,blkd:=sum(blocked),by=player_id][,dist:=mean(shot_dist),by=player_id]
off_shots<-off_shots[,.(player_id,astd,sefg,shot_clock,blkd,dist)]
off_shots<-distinct(off_shots,player_id,.keep_all = T)
box<-left_join(box,off_shots,by="player_id");setDT(box)

#   then defense:
def_shots<-shots[,.(dplayer1_id,made,def_type,is_three,sefg,pa_sefg,fouled,
                    contested,shot_dist)]
def_shots[,pa:=pa_sefg-sefg]
def_shots[,def_made_2:=sum(def_type=="def"&is_three==F&made==T),by="dplayer1_id"]
def_shots[,def_missed_2:=sum(def_type=="def"&is_three==F&made==F),by="dplayer1_id"]
def_shots[,def_made_3:=sum(def_type=="def"&is_three==T&made==T),by="dplayer1_id"]
def_shots[,def_missed_3:=sum(def_type=="def"&is_three==T&made==F),by="dplayer1_id"]
def_shots[,def_made_rim:=sum(def_type=="def"&made==T&shot_dist<6),by="dplayer1_id"]
def_shots[,def_missed_rim:=sum(def_type=="def"&made==F&shot_dist<6),by="dplayer1_id"]
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
def_shots[,defhe_made_rim:=sum(def_type=="defhe"&made==T&shot_dist<6),by="dplayer1_id"]
def_shots[,defhe_missed_rim:=sum(def_type=="defhe"&made==F&shot_dist<6),by="dplayer1_id"]

def_shots[,sefg:=mean(sefg),by="dplayer1_id"][,pa:=mean(pa),by="dplayer1_id"]
def_shots[,sf:=sum(fouled),by="dplayer1_id"][,con:=sum(contested),by="dplayer1_id"]

def_shots<-select(def_shots,dplayer1_id,def_made_2:defhe_missed_rim,sefg,pa,sf,con)
def_shots<-distinct(def_shots,dplayer1_id,.keep_all = T)
setnames(def_shots,c("dplayer1_id","sefg"),c("player_id","opp_sefg"))
box<-left_join(box,def_shots,by="player_id");setDT(box)

# passes
setDT(passes)
pass<-passes[assist_opp==T|opportunity_created==T]
pass[,past_from_paint:=ifelse(assist_opp==T&passer_y>18&passer_y<32&
                              (passer_x<19|passer_x>75),1,0)]
pass[,oc_from_paint:=ifelse(opportunity_created==T&passer_y>18&passer_y<32&
                              (passer_x<19|passer_x>75),1,0)]
pass[,past_to_paint:=ifelse(assist_opp==T&ball_end_y>17&ball_end_y<33&
                              (ball_end_x<10|ball_end_x>84),1,0)]
pass<-pass[,.(passer,assist_opp,opportunity_created,from_pick,past_from_paint,
              past_to_paint,oc_from_paint)]
pass[,past:=sum(assist_opp),by="passer"]
pass[,oc:=sum(opportunity_created==T&from_pick==F),by="passer"]
pass[,poc:=sum(opportunity_created==T&from_pick==T),by="passer"]
pass[,past_from_paint:=sum(past_from_paint),by="passer"]
pass[,oc_from_paint:=sum(oc_from_paint),by="passer"]
pass[,past_to_paint:=sum(past_to_paint),by="passer"]
pass<-pass[,.(passer,past,oc,poc,past_from_paint,past_to_paint,oc_from_paint)]
pass<-distinct(pass,passer,.keep_all = T)
setnames(pass,"passer","player_id")
box<-left_join(box,pass,by="player_id");setDT(box)

# turnovers
to<-tov[!is.na(to_forcing_id),.(to_forcing_id)]
to[,forced_tov:=1]
to[,forced_tov:=sum(forced_tov),by="to_forcing_id"]
to<-distinct(to,to_forcing_id,.keep_all = T);setDT(to)
setnames(to,"to_forcing_id","player_id")
box<-left_join(box,to,by="player_id");setDT(box)

# drives
drives<-as.data.table(js$drives)
drives[,drives:=1][,drives:=sum(drives),by="bhr"]
drives<-drives[,.(bhr,drives)]
drives<-distinct(drives,bhr,.keep_all = T)
setnames(drives,"bhr","eagle_id")
drives$eagle_id<-as.character(drives$eagle_id)
box<-left_join(box,drives,by="eagle_id");setDT(box)

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
             rebounds[[paste0("def_player_",i,"_offensive_rebound_allowed")]],
             rebounds[[paste0("def_player_",i,"_had_opportunity")]],
             rebounds[[paste0("def_player_",i,"_rb_pct_rim")]])]
    setnames(reb1,c("player_id","crash","boxout","leakout","ballwatch","ora",
                    "drb_chance","drb_prob"))
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
reb[,drb_chance:=sum(drb_chance),by="player_id"]
reb[,drb_prob:=mean(drb_prob),by="player_id"]
reb<-distinct(reb,player_id,.keep_all = T)

# ORBS
orb<-rebounds
dt2<-data.table()
for(i in 0:4){
    reb1<-orb[,.(
             rebounds[[paste0("off_player_",i,"_id")]],
             rebounds[[paste0("off_player_",i,"_crash")]],
             rebounds[[paste0("off_player_",i,"_had_opportunity")]],
             rebounds[[paste0("off_player_",i,"_rb_pct_rim")]])]
    setnames(reb1,c("player_id","go","orb_chance","orb_prob"))
    dt2<-rbind(dt2,reb1)
    rm(reb1)
}
orb<-dt2;rm(dt2)
orb[is.na(orb)] <- F
orb<-orb[,lapply(.SD, as.numeric)]
orb[,go:=sum(go),by="player_id"]
orb[,orb_chance:=sum(orb_chance),by="player_id"]
orb[,orb_prob:=mean(orb_prob),by="player_id"]
orb<-distinct(orb,player_id,.keep_all = T)
reb<-left_join(reb,orb,by="player_id")
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
box<-select(box,GAME_ID,TEAM_ABBREVIATION,player_id,PLAYER_NAME,MIN:bdist)
setnames(box,c("GAME_ID","TEAM_ABBREVIATION","PLAYER_NAME"),
             c("game_id","team_abbrev","player"))

# Add possessions
poss<-as.data.table(js$poss_matchups)
poss<-cbind(data.frame(c(poss$off_player_1,poss$off_player_2,poss$off_player_3,
                         poss$off_player_4,poss$off_player_5),
                       c(poss$def_player_1,poss$def_player_2,poss$def_player_3,
                         poss$def_player_4,poss$def_player_5)))
poss<-na.exclude(poss)
setDT(poss)
setnames(poss,c("off_player","def_player"))
poss[,poss:=1]
poss[,off_poss:=sum(poss),by=off_player]
poss[,def_poss:=sum(poss),by=def_player]
off_poss<-data.frame(cbind(c(poss$off_player,poss$def_player)))
names(off_poss)<-"off_player"
off_poss<-left_join(off_poss,poss,by="off_player")
off_poss<-off_poss %>%
    select(off_player,off_poss) %>%
    rename("def_player"=off_player)
poss<-poss[,.(def_player,def_poss)]
poss<-left_join(off_poss,poss,by="def_player")
poss<-poss %>%
    distinct(def_player,.keep_all = T) %>%
    rename("eagle_id"=def_player) %>%
    mutate(poss=off_poss+def_poss)
poss$eagle_id<-as.character(poss$eagle_id)
poss[is.na(poss)] <- 0
box<-left_join(box,poss,by="eagle_id")

# # Add time with ball, time guarding ball
frames_hc[,seconds_with_ball:=sum(secs),by="bh"]
frames_hc[,seconds_guarding_ball:=sum(secs),by="bhd"]
w_ball<-frames_hc %>%
    distinct(bh,.keep_all=T) %>%
    select(bh,seconds_with_ball) %>%
    rename("player_id"=bh)
on_ball<-frames_hc %>%
    distinct(bhd,.keep_all=T) %>%
    select(bhd,seconds_guarding_ball) %>%
    rename("player_id"=bhd)
w_ball$player_id<-as.character(w_ball$player_id)
on_ball$player_id<-as.character(on_ball$player_id)
box<-left_join(box,w_ball,by="player_id")
box<-left_join(box,on_ball,by="player_id")

# Add p&r defense
# think about this one...complicated...

# Add other pbp elements
games<-fread("C:/Users/brocatoj/Documents/Basketball/Tracking/meta/games.csv")
games<-games[id==gameid]
games[,season:=ifelse(date<as.Date('2014-07-01'),2014,
                      ifelse(date<as.Date('2015-07-01'),2015,
                             ifelse(date<as.Date('2016-07-01'),2016,2017)))]
game<-games$game
season<-games$season
simple_box<-fread(paste0("C:/Users/brocatoj/Documents/Basketball/Game/",
                         season,"/simplebox/",game,".csv"))
players<-fread("C:/Users/brocatoj/Documents/Basketball/Tracking/meta/players_plus.csv")
players<-players[,.(ids_id,james_id)]
setnames(players,c("player_id","player"))
simple_box<-left_join(simple_box,players,by="player")
simple_box<-select(simple_box,player_id,ofd:blkt,tovl,blkdt,ofl:cpfl,team_margin,outcome)
box<-left_join(box,simple_box,by="player_id")
box<-select(box,game_id:PLUS_MINUS,team_margin,outcome,seconds_played,poss,
            off_poss,def_poss,seconds_with_ball,seconds_guarding_ball,ofd:cpfl,
            eagle_id:bdist)
box[is.na(box)] <- 0

# # Write to file
# write.csv(box,paste0("C:/Users/brocatoj/Documents/Basketball/Tracking/j_markings/",
#           gameid,"_boxscore.csv"),row.names=F)
# 
#Remove unnecessary dataframes
rm(list= ls()[!(ls() %in% c('gameid','frames2','frames_reb','markings','js',
                            'pdist','players','bst','gravity','trans','shots',
                            'passes','de','rebounds','box'))])