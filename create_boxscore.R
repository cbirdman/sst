library(jsonlite)
library(data.table)

# Download boxscore from nba
gameid<-"0021600041"
url <- fromJSON(paste0("http://stats.nba.com/stats/boxscoretraditionalv2?GameID=",
                        gameid,"&StartPeriod=0&EndPeriod=0&StartRange=0&",
                        "EndRange=0&RangeType=0"))
head<-url$resultSets$headers[[1]]
box<-as.data.table(url$resultSets$rowSet[[1]])
setnames(box,head)
rm(head,url)
box<-box[!is.na(MIN)]

# Read players for id lookup
ap<-players[,.(id,ids_id)]
setnames(ap,c("player","PLAYER_ID"))

# think about: blkt/blko, tovl,tovd, ofd, charge, etc
# other still needed: kif, deter, gravity, 


# touches: passes,touches,dribbles
touches<-as.data.table(js$touches)
touches<-touches[,.(player,touches,dribbles,passes)]
touches$player<-as.character(touches$player)
touches<-left_join(touches,ap,by="player")
box<-left_join(box,touches,by="PLAYER_ID");setDT(box)

# shots
#   first offense:
off_shots<-shots[,.(player_id,potential_assist,sefg,shot_clock,blocked,shot_dist)]
off_shots[,past:=sum(potential_assist), by=player_id]
off_shots[,sefg:=mean(sefg),by=player_id][,shot_clock:=mean(shot_clock),by=player_id]
off_shots[,blkd:=sum(blocked),by=player_id][,dist:=mean(shot_dist),by=player_id]
off_shots<-off_shots[,.(player_id,past,sefg,shot_clock,blkd,dist)]
off_shots<-distinct(off_shots,player_id,.keep_all = T)
setnames(off_shots,"player_id","player")
off_shots$player<-as.character(off_shots$player)
box<-left_join(box,off_shots,by="player");setDT(box)

#   then defense:
def_shots<-shots[,.(dplayer_id,made,def_type,is_three,sefg,pa_sefg,fouled,contested)]
def_shots[,pa:=pa_sefg-sefg]
def_shots[,def_made_2:=sum(def_type=="def"&is_three==F&made==T),by="dplayer_id"]
def_shots[,def_missed_2:=sum(def_type=="def"&is_three==F&made==F),by="dplayer_id"]
def_shots[,def_made_3:=sum(def_type=="def"&is_three==T&made==T),by="dplayer_id"]
def_shots[,def_missed_3:=sum(def_type=="def"&is_three==T&made==F),by="dplayer_id"]
# recover
def_shots[,defre_made_2:=sum(def_type%in%c("defcl","defre")&is_three==F&made==T),by="dplayer_id"]
def_shots[,defre_missed_2:=sum(def_type%in%c("defcl","defre")&is_three==F&made==F),by="dplayer_id"]
def_shots[,defre_made_3:=sum(def_type%in%c("defcl","defre")&is_three==T&made==T),by="dplayer_id"]
def_shots[,defre_missed_3:=sum(def_type%in%c("defcl","defre")&is_three==T&made==F),by="dplayer_id"]
# help
def_shots[,defhe_made_2:=sum(def_type%in%c("defhe","defclhe")&is_three==F&made==T),by="dplayer_id"]
def_shots[,defhe_missed_2:=sum(def_type%in%c("defhe","defclhe")&is_three==F&made==F),by="dplayer_id"]
def_shots[,defhe_made_3:=sum(def_type%in%c("defhe","defclhe")&is_three==T&made==T),by="dplayer_id"]
def_shots[,defhe_missed_3:=sum(def_type%in%c("defhe","defclhe")&is_three==T&made==F),by="dplayer_id"]

def_shots[,sefg:=mean(sefg),by="dplayer_id"][,pa:=mean(pa),by="dplayer_id"]
def_shots[,sf:=sum(fouled),by="dplayer_id"][,con:=sum(contested),by="dplayer_id"]

def_shots<-select(def_shots,dplayer_id,def_made_2:defhe_missed_3,sefg,pa,sf,con)
def_shots<-distinct(def_shots,dplayer_id,.keep_all = T)
setnames(def_shots,c("dplayer_id","sefg"),c("player","opp_sefg"))
def_shots$player<-as.character(def_shots$player)
box<-left_join(box,def_shots,by="player");setDT(box)

# passes
setDT(passes)
pass<-passes[true_assist==T|opportunity_created==T]
pass<-pass[,.(passer,true_assist,opportunity_created,from_pick)]
pass[,tast:=sum(true_assist),by="passer"]
pass[,oc:=sum(opportunity_created==T&from_pick==F),by="passer"]
pass[,poc:=sum(opportunity_created==T&from_pick==T),by="passer"]
pass<-pass[,.(passer,tast,oc,poc)]
pass<-distinct(pass,passer,.keep_all = T)
setnames(pass,"passer","player")
pass$player<-as.character(pass$player)
box<-left_join(box,pass,by="player");setDT(box)

# turnovers
#tov<-as.data.table(js$turnovers)
#   forced turnovers (i still have to create this lol)

# drives
drives<-as.data.table(js$drives)
drives[,drives:=1][,drives:=sum(drives),by="bhr"]
drives<-drives[,.(bhr,drives)]
drives<-distinct(drives,bhr,.keep_all = T)
setnames(drives,"bhr","player")
drives$player<-as.character(drives$player)
box<-left_join(box,drives,by="player");setDT(box)

# charges
charges<-as.data.table(js$charges)
charges<-charges[,charge:=1][,charge:=sum(charge),by="taker_id"]
charges<-distinct(charges,taker_id,.keep_all=T)
charges<-charges[,.(taker_id,charge)]
setnames(charges,"taker_id","player")
charges$player<-as.character(charges$player)
box<-left_join(box,charges,by="player");setDT(box)

# rebounds
#       boxout
#       ora
#       crash/bw/go??

# transitions
#       bit
#       bbb

# defensive errors
#   future plan: adjust defensive errors for who they're against (ie pa_sefg)
setDT(de)
der<-de[,.(ballhandler,error_type)]
der[,bb:=sum(error_type=="bb"),by="ballhandler"]
der[,bbcl:=sum(error_type=="bbcl"),by="ballhandler"]
der[,bbc:=sum(error_type=="bbc"),by="ballhandler"]
der[,hn:=sum(error_type=="hn"),by="ballhandler"]
der[,shn:=sum(error_type=="shn"),by="ballhandler"]
der<-der[,.(ballhandler,bb,bbcl,bbc,hn,shn)]
der<-distinct(der,ballhandler,.keep_all=T)
setnames(der,"ballhandler","PLAYER_ID")
der$PLAYER_ID<-as.character(der$PLAYER_ID)
box<-left_join(box,der,by="PLAYER_ID")

# spacing
spacing<-bst[,.(player_id,ball_stop_type)]
spacing[,pus:=sum(ball_stop_type=="PUS"),by="player_id"]
spacing[,pup:=sum(ball_stop_type=="PUP"),by="player_id"]
spacing<-spacing[,.(player_id,pus,pup)]
spacing<-distinct(spacing,player_id,.keep_all = T)
setnames(spacing,"player_id","PLAYER_ID")
spacing$PLAYER_ID<-as.character(spacing$PLAYER_ID)
box<-left_join(box,spacing,by="PLAYER_ID")

box[is.na(box)] <- 0