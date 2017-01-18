library(plyr)
library(dplyr)
library(data.table)
library(glmnet)
library(stringr)
library(Matrix)

ap<-data.frame()
# Load data
for (z in 2014:2017){
    matchups <- fread(paste0("C:/Users/brocatoj/Documents/Basketball/Game/",z,
                               "/ytd_matchups.csv"))
    matchups[,c("minutes","sec"):=tstrsplit(Time_Left,":",fixed=T)]
    matchups[,date:=as.Date(substr(Game,1,10))]
    matchups[,home_team:=substr(Game,16,18)][,away_team:=substr(Game,12,14)]
    matchups[,c("hp1","hp2","hp3","hp4","hp5"):= tstrsplit(hfive," - ",fixed=T)]
    matchups[,c("ap1","ap2","ap3","ap4","ap5"):= tstrsplit(afive," - ",fixed=T)]
    matchups<-matchups %>%
        select(Game,date:away_team,Period,hp1:ap5,min:margin)
    
    # Add Age
    age<-fread("C:/Users/brocatoj/Documents/Basketball/Tracking/meta/players_plus.csv")
    age<-age[,.(james_id,birth_date)]
    #rp<-data.frame(james_id="Replacement Player",birth_date=paste0(z-23,"-01-01"))
    #age<-rbind(age,rp)
    age$birth_date<-as.Date(age$birth_date)
    #rp<-fread("reg/replacement_players.csv")
    for(i in c("hp1","hp2","hp3","hp4","hp5","ap1","ap2","ap3","ap4","ap5")){
        #matchups[,(i):=ifelse(matchups[[i]]%in%rp$name,"Replacement Player",matchups[[i]])]
        setnames(age,c(i,paste0("birth_date_",i)))
        matchups<-left_join(matchups,age,by=i)
        setDT(matchups)
        matchups[,age:=as.numeric(date-matchups[[paste0("birth_date_",i)]])/365.2422]
        setnames(matchups,"age",paste0("age_",i))
    }
    for(a in 18:44){
     matchups[,(paste0("home_age_",a)):=ifelse(floor(age_hp1)==a,1,0)+
                  ifelse(floor(age_hp2)==a,1,0)+
                  ifelse(floor(age_hp3)==a,1,0)+
                  ifelse(floor(age_hp4)==a,1,0)+
                  ifelse(floor(age_hp5)==a,1,0)] 
    matchups[,(paste0("away_age_",a)):=ifelse(floor(age_ap1)==a,1,0)+
                  ifelse(floor(age_ap2)==a,1,0)+
                  ifelse(floor(age_ap3)==a,1,0)+
                  ifelse(floor(age_ap4)==a,1,0)+
                  ifelse(floor(age_ap5)==a,1,0)]
    }
    for(a in 18:44){
    matchups[,(paste0("age_",a)):=matchups[[paste0("home_age_",a)]]-
                 matchups[[paste0("away_age_",a)]]]
    }
    for(a in 18:44){
    matchups[,(paste0("age2_",a)):=matchups[[paste0("home_age_",a)]]+
                 matchups[[paste0("away_age_",a)]]]
    }
    matchups[,home_ortg:=ifelse(hposs==0,0,hpts/hposs*100)]
    matchups[,away_ortg:=ifelse(aposs==0,0,apts/aposs*100)]
    matchups[,margin:=home_ortg-away_ortg]
    matchups[,sum:=home_ortg+away_ortg]
    matchups[,rest:=hrest-arest][,rest_sq:=I(hrest-arest)^2]
    matchups[,rest2:=hrest+arest][,rest_sq2:=I(hrest+arest)^2]
    
    # Add coaches
    coaches<-fread("reg/coaches_by_date.csv")
    coaches$date<-as.Date(coaches$date)
    setnames(coaches,c("home_team","date","home_coach"))
    matchups<-left_join(matchups,coaches,by=c("home_team","date"))
    setnames(coaches,c("away_team","date","away_coach"))
    matchups<-left_join(matchups,coaches,by=c("away_team","date"))
    setDT(matchups)
    matchups<-matchups %>%
        select(Game:ap5,home_coach,away_coach,poss,home_ortg,away_ortg,margin,sum,
               start,rest,rest_sq,rest2,rest_sq2,home_age_18:age2_44)
    
    # Add injuries
    acl<-fread("reg/acl_injuries.csv")
    acl<-acl[,.(Player,Date)]
    acl$Date<-as.Date(acl$Date)
    acl<-acl[Date>as.Date("1996-07-01")]
    achilles<-fread("reg/achilles_injuries.csv")
    achilles<-achilles[,.(Player,Date)]
    achilles$Date<-as.Date(achilles$Date)
    achilles<-achilles[Date>as.Date("1996-07-01")]
for(i in c("hp1","hp2","hp3","hp4","hp5","ap1","ap2","ap3","ap4","ap5")){
    # ACL
    setnames(acl,c(i,paste0(i,"_acl_date")))
    matchups<-left_join(matchups,acl,by=i)
    setDT(matchups)
    matchups[,(paste0(i,"_acl_date")):=
                 ifelse(date>matchups[[paste0(i,"_acl_date")]],1,0)]
    matchups[,(paste0(i,"_acl_date")):=
                 ifelse(is.na(matchups[[paste0(i,"_acl_date")]]),0,
                        matchups[[paste0(i,"_acl_date")]])]
    # ACHILLES
    setnames(achilles,c(i,paste0(i,"_achilles_date")))
    matchups<-left_join(matchups,achilles,by=i)
    setDT(matchups)
    matchups[,(paste0(i,"_achilles_date")):=
                 ifelse(date>matchups[[paste0(i,"_achilles_date")]],1,0)]
    matchups[,(paste0(i,"_achilles_date")):=
                 ifelse(is.na(matchups[[paste0(i,"_achilles_date")]]),0,
                        matchups[[paste0(i,"_achilles_date")]])]
}
matchups[,home_acl:=hp1_acl_date+hp2_acl_date+hp3_acl_date+hp4_acl_date+hp5_acl_date]
matchups[,away_acl:=ap1_acl_date+ap2_acl_date+ap3_acl_date+ap4_acl_date+ap5_acl_date]
matchups[,acl:=home_acl-away_acl]
matchups[,home_achilles:=hp1_achilles_date+hp2_achilles_date+hp3_achilles_date+hp4_achilles_date+hp5_achilles_date]
matchups[,away_achilles:=ap1_achilles_date+ap2_achilles_date+ap3_achilles_date+ap4_achilles_date+ap5_achilles_date]
matchups[,achilles:=home_achilles-away_achilles]

# priors
prior<-fread(paste0("reg/rapm_priors.csv"))
prior<-prior[,.(name,prior,prior_split)]
for(i in c("hp1","hp2","hp3","hp4","hp5","ap1","ap2","ap3","ap4","ap5",
           "home_coach","away_coach")){
    setnames(prior,c(i,paste0(i,"_prior"),paste0(i,"_split")))
    matchups<-left_join(matchups,prior,by=i);setDT(matchups)
}
matchups[,offset:=(hp1_prior+hp2_prior+hp3_prior+hp4_prior+hp5_prior+
                   start*-0.55+rest*0.64+rest_sq*-0.02+
                   home_acl*-0.67+home_achilles*-3.14+
                   home_age_18*-3.61+home_age_19*-2.55+home_age_20*-1.63+
                   home_age_21*-0.86+home_age_22*-0.21+home_age_23*0.31+
                   home_age_24*0.72+home_age_25*1.02+home_age_26*1.22+
                   home_age_27*1.33+home_age_28*1.36+home_age_29*1.30+
                   home_age_30*1.18+home_age_31*1+home_age_32*0.76+
                   home_age_33*0.47+home_age_34*0.15+home_age_35*-0.21+
                   home_age_36*-0.59+home_age_37*-0.98+home_age_38*-1.38+
                   home_age_39*-1.79+home_age_40*-2.18+home_age_41*-2.57+
                   home_age_42*-2.93+home_age_43*-3.26+home_age_44*-3.55)-
                  (ap1_prior+ap2_prior+ap3_prior+ap4_prior+ap5_prior+
                   away_acl*-0.67+away_achilles*-3.14+
                   away_age_18*-3.61+away_age_19*-2.55+away_age_20*-1.63+
                   away_age_21*-0.86+away_age_22*-0.21+away_age_23*0.31+
                   away_age_24*0.72+away_age_25*1.02+away_age_26*1.22+
                   away_age_27*1.33+away_age_28*1.36+away_age_29*1.30+
                   away_age_30*1.18+away_age_31*1+away_age_32*0.76+
                   away_age_33*0.47+away_age_34*0.15+away_age_35*-0.21+
                   away_age_36*-0.59+away_age_37*-0.98+away_age_38*-1.38+
                   away_age_39*-1.79+away_age_40*-2.18+away_age_41*-2.57+
                   away_age_42*-2.93+away_age_43*-3.26+away_age_44*-3.55)]
matchups[,offset2:=(hp1_split+hp2_split+hp3_split+hp4_split+hp5_split+
                   start*0.02+rest*-0.08+rest_sq*-0.02+
                   home_acl*-0.75+home_achilles*-1.57+
                   home_age_18*-1.85+home_age_19*-1.13+home_age_20*-0.53+
                   home_age_21*-0.04+home_age_22*0.34+home_age_23*0.62+
                   home_age_24*0.82+home_age_25*0.93+home_age_26*0.97+
                   home_age_27*0.96+home_age_28*0.88+home_age_29*0.77+
                   home_age_30*0.62+home_age_31*0.44+home_age_32*0.25+
                   home_age_33*0.04+home_age_34*-0.16+home_age_35*-0.36+
                   home_age_36*-0.53+home_age_37*-0.68+home_age_38*-0.8+
                   home_age_39*-0.87+home_age_40*-0.89+home_age_41*-0.85+
                   home_age_42*-0.73+home_age_43*-0.54+home_age_44*-0.25)-
                  (ap1_split+ap2_split+ap3_split+ap4_split+ap5_split+
                   away_acl*-0.75+away_achilles*-1.57+
                   away_age_18*-1.85+away_age_19*-1.13+away_age_20*-0.53+
                   away_age_21*-0.04+away_age_22*0.34+away_age_23*0.62+
                   away_age_24*0.82+away_age_25*0.93+away_age_26*0.97+
                   away_age_27*0.96+away_age_28*0.88+away_age_29*0.77+
                   away_age_30*0.62+away_age_31*0.44+away_age_32*0.25+
                   away_age_33*0.04+away_age_34*-0.16+away_age_35*-0.36+
                   away_age_36*-0.53+away_age_37*-0.68+away_age_38*-0.8+
                   away_age_39*-0.87+away_age_40*-0.89+away_age_41*-0.85+
                   away_age_42*-0.73+away_age_43*-0.54+away_age_44*-0.25)]
matchups<-select(matchups,Game:sum,offset,offset2,start:achilles)

la_playa<-cbind(data.frame(c(matchups$hp1,matchups$hp2,matchups$hp3,matchups$hp4,
                             matchups$hp5,matchups$ap1,matchups$ap2,matchups$ap3,
                             matchups$ap4,matchups$ap5,matchups$home_coach,
                             matchups$away_coach)),
                data.frame(rep(matchups$poss,12)))
la_playa<-setDT(la_playa)
setnames(la_playa,c("name","poss"))
la_playa[,poss:=sum(poss),by="name"]
la_playa<-distinct(la_playa,name,.keep_all = T)
ap<-rbind(ap,la_playa)
ap[,poss:=sum(poss),by="name"]
ap<-distinct(ap,name,.keep_all = T)

# Create data matrix
players<-fread("reg/rapm_players.csv")
players<-select(players,name)
players<-as.data.frame(players)

for(p in players[1:nrow(players),]) {
    matchups[,(p):=ifelse(p==hp1|p==hp2|p==hp3|p==hp4|p==hp5|p==home_coach,1,
                   ifelse(p==ap1|p==ap2|p==ap3|p==ap4|p==ap5|p==away_coach,-1,0))]
}


    # Create APM/RAPM
    if(z==2014){
        x<-select(matchups,-(Game:offset2),-(home_age_18:away_age_44),
                  -(age2_18:age2_44),-(hp1_acl_date:ap5_achilles_date),
                  -home_acl,-away_acl,-home_achilles,-away_achilles,-rest2,
                  -rest_sq2)
        x<-as.matrix(x)
        x<-Matrix(x,sparse=T)
        x2<-matchups %>%
            mutate(acl=home_acl+away_acl,
                   achilles=home_achilles+away_achilles) %>%
            select(-(Game:offset2),-(home_age_18:away_age_44),-(age_18:age_44),
                  -(hp1_acl_date:ap5_achilles_date),-home_acl,-away_acl,
                  -home_achilles,-away_achilles,-rest,-rest_sq)
        for(a in 18:44){setnames(x2,paste0("age2_",a),paste0("age_",a))}
        setnames(x2,c("rest2","rest_sq2"),c("rest","rest_sq"))
        x2<-as.matrix(x2)
        x2[x2==-1]<-1
        x2<-Matrix(x2,sparse=T)
        y<-as.matrix(matchups$margin)
        y2<-as.matrix(matchups$sum)
        offset<-as.matrix(matchups$offset)
        offset2<-as.matrix(matchups$offset2)
        # Weigh by recency
        matchups[,poss:=
          ifelse(date>as.Date('2013-10-01')&date<as.Date('2014-10-01'),poss*0.125,
          ifelse(date>as.Date('2014-10-01')&date<as.Date('2015-10-01'),poss*0.25,
          ifelse(date>as.Date('2015-10-01')&date<as.Date('2016-10-01'),poss*0.5,
                 poss)))]
        weights<-as.matrix(matchups$poss)
    } else {
        eks<-select(matchups,-(Game:offset2),-(home_age_18:away_age_44),
                  -(age2_18:age2_44),-(hp1_acl_date:ap5_achilles_date),
                  -home_acl,-away_acl,-home_achilles,-away_achilles,-rest2,-rest_sq2)
        eks<-as.matrix(eks)
        eks<-Matrix(eks,sparse=T)
        eks2<-matchups %>%
            mutate(acl=home_acl+away_acl,
                   achilles=home_achilles+away_achilles) %>%
            select(-(Game:offset2),-(home_age_18:away_age_44),-(age_18:age_44),
                  -(hp1_acl_date:ap5_achilles_date),-home_acl,-away_acl,
                  -home_achilles,-away_achilles,-rest,-rest_sq)
        for(a in 18:44){setnames(eks2,paste0("age2_",a),paste0("age_",a))}
        setnames(eks2,c("rest2","rest_sq2"),c("rest","rest_sq"))
        eks2<-as.matrix(eks2)
        eks2[eks2==-1]<-1
        eks2<-Matrix(eks2,sparse=T)
        why<-as.matrix(matchups$margin)
        why2<-as.matrix(matchups$sum)
        # Weigh by recency
        matchups[,poss:=
          ifelse(date>as.Date('2013-10-01')&date<as.Date('2014-10-01'),poss*0.125,
          ifelse(date>as.Date('2014-10-01')&date<as.Date('2015-10-01'),poss*0.25,
          ifelse(date>as.Date('2015-10-01')&date<as.Date('2016-10-01'),poss*0.5,
                 poss)))]
        waits<-as.matrix(matchups$poss)
        os<-as.matrix(matchups$offset)
        os2<-as.matrix(matchups$offset2)
        x<-rbind(x,eks)
        x2<-rbind(x2,eks2)
        y<-rbind(y,why)
        y2<-rbind(y2,why2)
        weights<-rbind(weights,waits)
        offset<-rbind(offset,os)
        offset2<-rbind(offset2,os2)
    }
    rm(matchups)
}

rm(list= ls()[!(ls() %in% c('x','x2','y','y2', 'weights','offset','offset2',
                            'pf','ap'))])

# Set up model
grid <- 10^seq(10,-2,length=100)

    # Separate model into training and test sets
    set.seed(101)
    train <- sample(1:nrow(x), nrow(x)/2)
    test <- (-train)
    y.test <- y[test]

    # Use cross validation to determine best value for lambda
    set.seed(101)
    cv.out <- cv.glmnet(x[train,],scale(y[train]),alpha=0,weights=weights[train])
    bestlam <- cv.out$lambda.min

# Create model using best lambda
out <- glmnet(x,y,alpha=0,lambda=grid,weights=weights,offset=offset,
              standardize = F,exclude=1:188)
ridge.coef <- predict(out,offset=offset,type="coefficients",s=3)
coef <- as.data.frame(as.matrix(ridge.coef))
coefnames <- as.data.frame(rownames(coef))
coef <- cbind(coefnames,coef);rm(coefnames)
names(coef)<-c("name","rapm")
rownames(coef) <- NULL
coef <- coef %>%
    mutate(rapm = round(rapm,2))

# Create Splits
out2<-glmnet(x2,y2,alpha=0,lambda=grid,weights=weights,offset=offset2,
             standardize = F,exclude=1:188)
ridge.coef2 <- predict(out2,offset=offset2,type="coefficients",s=3)
coef2 <- as.data.frame(as.matrix(ridge.coef2))
coefnames2 <- as.data.frame(rownames(coef2))
coef2 <- cbind(coefnames2,coef2);rm(coefnames2)
names(coef2)<-c("name","split")
rownames(coef2) <- NULL
coef2 <- coef2 %>%
    mutate(split = round(split,2))
coef<-left_join(coef,coef2,by="name")
coef<-coef %>%
    mutate(orapm=round((rapm+split)/2,2),
           drapm=round(rapm-orapm,2)) %>%
    select(name,orapm,drapm,rapm)

# adjust for use
priors<-fread("reg/rapm_priors.csv")
priors[,prior_off:=(prior+prior_split)/2][,prior_def:=prior-prior_off]
priors<-priors[,.(name,prior_off,prior_def)]
coef<-left_join(coef,priors,by="name")
age<-fread("reg/rapm_age_curve.csv")
age[,age_off:=(prior+prior_split)/2][,age_def:=prior-age_off]
age<-age[,.(age,age_off,age_def)]
players<-fread("C:/Users/brocatoj/Documents/Basketball/Tracking/meta/players_plus.csv")
players[,age:=paste0("age_",
        floor(as.numeric(as.Date('2017-02-01')-as.Date(birth_date))/365.2422))]
players<-select(players,james_id,age)
setnames(players,c("name","age"))
players<-left_join(players,age,by="age")
setDT(players)
players<-players[,.(name,age_off,age_def)]
coef<-left_join(coef,players,by="name")
acl<-fread("reg/acl_injuries.csv")
acl[,acl_off:=-0.92][,acl_def:=0.2]
acl<-acl[,.(Player,acl_off,acl_def)]
setnames(acl,"Player","name")
achilles<-fread("reg/achilles_injuries.csv")
achilles[,achilles_off:=-2.55][,achilles_def:=-0.53]
achilles<-achilles[,.(Player,achilles_off,achilles_def)]
setnames(achilles,"Player","name")
coef<-left_join(coef,acl,by="name")
coef<-left_join(coef,achilles,by="name")
coef[is.na(coef)] <- 0
setDT(coef)
coef[,orapm:=orapm+prior_off+age_off+acl_off+achilles_off]
coef[,drapm:=drapm+prior_def+age_def+acl_def+achilles_def]
coef[,rapm:=round(orapm+drapm,2)]
coef<-coef[,.(name,orapm,drapm,rapm)]
coef<-left_join(coef,ap,by="name")
coef[is.na(coef)] <- 0
setDT(coef)
coef[,oposs:=orapm*poss][,dposs:=drapm*poss]
coef[,oavg:=sum(oposs)/sum(poss)][,davg:=sum(dposs)/sum(poss)]
coef[,orapm:=round(orapm-oavg,2)][,drapm:=round(drapm-davg,2)]
coef[,rapm:=round(orapm+drapm,2)]
coef<-coef[,.(name,orapm,drapm,rapm)]
coef<-arrange(coef,desc(rapm))
impact<-fread("C:/Users/brocatoj/Documents/Basketball/Game/2017/adj_impact.csv")
setnames(coef,"name","player")
impact<-left_join(impact,coef,by="player")
players<-fread("C:/Users/brocatoj/Documents/Basketball/Tracking/meta/players_plus.csv")
players<-players[,.(james_id,team,pos_name,birth_date)]
players[,age:=floor(as.numeric(as.Date('2017-02-01')-as.Date(birth_date))/365.2422)]
players[,birth_date:=NULL]
setnames(players,c("james_id","pos_name"),c("player","pos"))
impact<-left_join(impact,players,by="player")
impact<-impact %>%
    select(player,team,pos,age,min,orapm,drapm,rapm) %>%
    arrange(desc(rapm)) %>%
    rename("off"=orapm,"def"=drapm,"impact"=rapm)
write.csv(impact,"reg/player_ratings.csv")