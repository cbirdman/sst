# DEF TYPES: define
frames[,deftype:=
  # If there is a missed shot or shooting foul
  ifelse(event%in%c("2PM","2PX","3PM","3PX","SF"),
    # If the shot was immediately after an ORB, it's tip defense
    ifelse(Reduce("|",shift(event=="ORB",1:20)),"defti",
  #If the defender is different from who it was a short time ago
  ifelse(defender!=shift(defender,25)|defender!=shift(defender,15)|defender!=shift(defender,10),
    # If it's close to the hoop, it's defhe, if not it's defclhe
    ifelse(((s_x<17&s_y>10&s_y<40)|(s_x>77&s_y>10&s_y<40)),"defhe","defclhe"),
    # If the defender moved more than 5 feet from 20 frames ago
    ifelse(pdist(d_x,shift(d_x,20),d_y,shift(d_y,20))>5&
      # And defender was 2+ feet away from shooter 20 frames ago
      pdist(shift(s_x,20),shift(d_x,20),shift(s_y,20),shift(d_y,20))>2&
      # And the shot isn't close to the hoop, then it's defcl
      !((s_x<17&s_y>10&s_y<40)|(s_x>77&s_y>10&s_y<40)),"defcl",
    # If there was a screen in the last 75 frames
    ifelse((Reduce("|",shift(event=="SCR",1:75))|
      # Or there was a pass in the last 60 frames
      Reduce("|",shift(event=="PASS",1:60)))&
      # And defender was 2+ feet away from shooter 20 frames ago
      pdist(shift(s_x,20),shift(d_x,20),shift(s_y,20),shift(d_y,20))>=2,
    # If the shot was close to the hoop, it's defre, else it's defcl
    ifelse((s_x<17&s_y>10&s_y<40)|(s_x>77&s_y>10&s_y<40),"defre","defcl"),
    # If the shooter is 10+ feet from the defender
    ifelse(pdist(d_x,s_x,d_y,s_y)>10&
      # And the shot is close to the hoop, then it's defna
      ((s_x<17&s_y>10&s_y<40)|(s_x>77&s_y>10&s_y<40)),"defna",
        # If the shooter is 10+ feet and it's away from the hoop it's defcl,
         # Otherwise that shit is def bruh
         ifelse(pdist(d_x,s_x,d_y,s_y)>10,"defcl","def")))))),NA)]