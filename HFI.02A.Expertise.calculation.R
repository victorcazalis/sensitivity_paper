

##############################
### CREATE EXPERTISE FILES ###
##############################

### Summarise chlist and observers per files and merge per QQ
setwd(paste0(GBrow, "0.Data/1.Exported4Expertise/", QQ))

Files<-list.files()

chlistSTOCK<-list()
observerSTOCK<-list()

for(i in 1:length(Files)){
  df<-readRDS(Files[i])
  
  
  ### CHECKLIST TABLE: I could change it to use data.table (lots of time won for few coding)
  chlist<-ddply(df, .(df$sampling_event_identifier), function(x){data.frame(
    date=x$observation_date[1],
    time=x$time_observations_started[1],
    distance=x$effort_distance_km[1],
    n.observers=x$number_observers[1],
    duration=x$duration_minutes[1],
    observer=x$observer_id[1],
    rich=nrow(x),
    lon=x$longitude[1],
    lat=x$latitude[1],
    protocol=x$protocol_type[1],
    year=x$year[1]
  )})

  
  names(chlist)<-replace(names(chlist), names(chlist)=="df$sampling_event_identifier", "checklist")
  
  chlist$day<-as.numeric(format(as.Date(chlist$date, "%d/%m/%Yd"), "%j"))
  
  chlist$time.min<-sapply(strsplit(as.character(chlist$time),":"),
                          function(x) {
                            x <- as.numeric(x)
                            x[1]*60+x[2] })
  
  chlistSTOCK[[i]]<-chlist
  
  
  ### OBSERVER TABLE
  observer.var<-ddply(df, .(df$observer_id), function(x){data.frame(Nb_obs=nrow(x), 
                                                                     Nb_spc=nlevels(droplevels(as.factor(x$scientific_name))), 
                                                                     Nb_checklist=nlevels(droplevels(as.factor(x$sampling_event_identifier))), 
                                                                     Species=paste(unique(x$taxonomic_order), collapse=";")
                                                                       )})
  names(observer.var)[1]<-"observer_id"
  
  observerSTOCK[[i]]<-observer.var
  
  cat(i)
  
  }




# Merge chlist per QQ
txt<-"chlistMERGE<-rbind(chlistSTOCK[[1]], chlistSTOCK[[2]]"
for(T in 3:length(Files)){txt<-paste0(txt, ", chlistSTOCK[[", T, "]]")}
txt<-paste0(txt, ")")

eval(parse(text=txt))

saveRDS(chlistMERGE, paste0(GBrow, "0.Data/1.Exported4Expertise/Qmerged/chlist.expertise.", QQ, "merged.rds"))

# Merge observer per QQ
txt<-"observerMERGE<-rbind(observerSTOCK[[1]], observerSTOCK[[2]]"
for(T in 3:length(Files)){txt<-paste0(txt, ", observerSTOCK[[", T, "]]")}
txt<-paste0(txt, ")")

eval(parse(text=txt))

saveRDS(observerMERGE, paste0(GBrow, "0.Data/1.Exported4Expertise/Qmerged/observer.expertise.", QQ, "merged.rds"))


