
### Charge all checklist files and merge
chlist1<-readRDS(paste0(GBrow, "0.Data/1.Exported4Expertise/Qmerged/chlist.expertise.Q1merged.rds"))
chlist2<-readRDS(paste0(GBrow, "0.Data/1.Exported4Expertise/Qmerged/chlist.expertise.Q2merged.rds"))
chlist3<-readRDS(paste0(GBrow, "0.Data/1.Exported4Expertise/Qmerged/chlist.expertise.Q3merged.rds"))
chlist4<-readRDS(paste0(GBrow, "0.Data/1.Exported4Expertise/Qmerged/chlist.expertise.Q4merged.rds"))
chlist5<-readRDS(paste0(GBrow, "0.Data/1.Exported4Expertise/Qmerged/chlist.expertise.Q5merged.rds"))
chlist6<-readRDS(paste0(GBrow, "0.Data/1.Exported4Expertise/Qmerged/chlist.expertise.Q6merged.rds"))
chlist7<-readRDS(paste0(GBrow, "0.Data/1.Exported4Expertise/Qmerged/chlist.expertise.Q7merged.rds"))

chlist<-rbind(chlist1, chlist2, chlist3, chlist4, chlist5, chlist6, chlist7)


### Charge all observer files and merge
observer1<-readRDS(paste0(GBrow, "0.Data/1.Exported4Expertise/Qmerged/observer.expertise.Q1merged.rds"))
observer2<-readRDS(paste0(GBrow, "0.Data/1.Exported4Expertise/Qmerged/observer.expertise.Q2merged.rds"))
observer3<-readRDS(paste0(GBrow, "0.Data/1.Exported4Expertise/Qmerged/observer.expertise.Q3merged.rds"))
observer4<-readRDS(paste0(GBrow, "0.Data/1.Exported4Expertise/Qmerged/observer.expertise.Q4merged.rds"))
observer5<-readRDS(paste0(GBrow, "0.Data/1.Exported4Expertise/Qmerged/observer.expertise.Q5merged.rds"))
observer6<-readRDS(paste0(GBrow, "0.Data/1.Exported4Expertise/Qmerged/observer.expertise.Q6merged.rds"))
observer7<-readRDS(paste0(GBrow, "0.Data/1.Exported4Expertise/Qmerged/observer.expertise.Q7merged.rds"))
observer<-rbind(observer1, observer2, observer3, observer4, observer5, observer6, observer7)

### For each observer, calculate the number of checklists, observations and species
obs<-ddply(observer, .(observer_id), function(x){data.frame(
  Nb_checklist=sum(x$Nb_checklist),
  Nb_obs=sum(x$Nb_obs),
  Nb_spc=length(unique(unlist(strsplit(as.character(x$Species), ";")))),
  Species=paste0(unique(unlist(strsplit(as.character(x$Species), ";"))), collapse=";")
)})


### Remove unexperienced observers
tapply(obs$Nb_checklist, obs$Nb_checklist>=50 & obs$Nb_spc>=100, sum) # Number of checklist I'll remove
tapply(obs$Nb_checklist, obs$Nb_checklist>=50 & obs$Nb_spc>=100, length) # Number of observers I'll remove

obs.to.exclude<-subset(obs, obs$Nb_spc<100 | obs$Nb_checklist<50)
chlist2<-subset(chlist, chlist$observer %not in% obs.to.exclude$observer_id)
chlist2$observer<-droplevels(chlist2$observer)



# Remove NAs
cat(paste(100*round(table(is.na(chlist2$duration))["TRUE"]/nrow(chlist2),2), " % of duration values are NA"))
chlist2<-chlist2[,c("checklist", "rich", "protocol", "duration", "n.observers", "time.min", "lon", "lat", "day", "observer")]
chlist3<-chlist2[complete.cases(chlist2),]
chlist3$observer<-droplevels(chlist3$observer)



#################################
### MAKE MODELS FOR EXPERTISE ###
#################################


### Modele in two steps
# GAM (for big datasets)
mod.fix<-mgcv::bam(
  rich ~ protocol + n.observers + s(duration) + s(time.min) + te(lon, lat, day),
  data=chlist3,
  family="nb"
)

# GAM (for big datasets)
chlist3$Residus<-residuals(mod.fix)
mod.random<-nlme::lme(
  Residus ~ 1, random =~1|observer,
  data=chlist3
)



### Predict
dfExp<-data.frame(
  protocol="Stationary",
  n.observers=median(chlist3$n.observers, na.rm=T),
  duration=median(chlist3$duration, na.rm=T),
  time.min=median(chlist3$time.min, na.rm=T),
  lon=median(chlist3$lon, na.rm=T),
  lat=median(chlist3$lat, na.rm=T),
  day=median(chlist3$day, na.rm=T)
)

# Extract fixed effect prediction (unique value)
Pred.fix<-predict(mod.fix, newdata=dfExp)

# Add random effects 
Pred.obs<-as.data.frame(nlme::ranef(mod.random))
names(Pred.obs)[1]<-"Pred"
Pred.obs$Pred<-Pred.obs$Pred + as.numeric(Pred.fix)


### SAVE THE EXPERTISE SCORE
obs$obsKelling<-Pred.obs$Pred[match(obs$observer_id, rownames(Pred.obs))]

saveRDS(obs, paste0(GBrow, "1.Tables/Expertise.scores.table.rds"))





############################################
### Graphics of fit + covariates effects ###
############################################
pdf(paste0("D:/eBird/HFI.project/Figures/Check.figures/Obs.expertise.HFI1.pdf"))

hist(chlist3$rich, breaks=50, main="Richness distribution (check Poisson distribution)")

hist(Pred.obs$Pred, breaks=30, xlab="Kelling observer expertise score", main="Expertise score distribution")

par(mfrow=c(2,2)) ; mgcv::gam.check(mod.fix) ; par(mfrow=c(1,1))


### Covariates effects
# Duration
ndf.dur<-data.frame(duration=c(min(chlist3$duration):max(chlist3$duration)), protocol="Stationary", n.observers=median(chlist3$n.observers, na.rm=T), time.min=median(chlist3$time.min, na.rm=T), day=median(chlist3$day, na.rm=T), lat=median(chlist3$lat, na.rm=T), lon=median(chlist3$lon, na.rm=T))
ndf.dur[,8:9]<-predict(mod.fix, ndf.dur, se.fit=T)
ndf.dur$min<-ndf.dur$fit-1.96*ndf.dur$se.fit
ndf.dur$max<-ndf.dur$fit+1.96*ndf.dur$se.fit
for(i in c(8,10,11)){ndf.dur[,i]<-exp(ndf.dur[,i])}
ggplot(ndf.dur)+
  geom_line(aes(x=duration, y=fit))+
  geom_line(aes(x=duration, y=min), linetype="dashed")+
  geom_line(aes(x=duration, y=max), linetype="dashed")+
  ggtitle("Model covariates: duration")+
  scale_y_continuous(limits=c(0, 2*max(ndf.dur$fit, na.rm=T)))

# Time.min
ndf.time<-data.frame(time.min=c(min(chlist3$time.min):max(chlist3$time.min)), protocol="Stationary", n.observers=median(chlist3$n.observers, na.rm=T), duration=median(chlist3$duration, na.rm=T), day=median(chlist3$day, na.rm=T), lat=median(chlist3$lat, na.rm=T), lon=median(chlist3$lon, na.rm=T))
ndf.time[,8:9]<-predict(mod.fix, ndf.time, se.fit=T)
ndf.time$min<-ndf.time$fit-1.96*ndf.time$se.fit
ndf.time$max<-ndf.time$fit+1.96*ndf.time$se.fit
for(i in c(8,10,11)){ndf.time[,i]<-exp(ndf.time[,i])}
ggplot(ndf.time)+
  geom_line(aes(x=time.min, y=fit))+
  geom_line(aes(x=time.min, y=min), linetype="dashed")+
  geom_line(aes(x=time.min, y=max), linetype="dashed")+
  ggtitle("Model covariates: Starting time")+
  scale_y_continuous(limits=c(0, 2*max(ndf.time$fit, na.rm=T)))

# n.observers
ndf.nobs<-data.frame(n.observers=c(min(chlist3$n.observers):max(chlist3$n.observers)), protocol="Stationary", duration=median(chlist3$duration, na.rm=T), time.min=median(chlist3$time.min, na.rm=T), day=median(chlist3$day, na.rm=T), lat=median(chlist3$lat, na.rm=T), lon=median(chlist3$lon, na.rm=T))
ndf.nobs[,8:9]<-predict(mod.fix, ndf.nobs, se.fit=T)
ndf.nobs$min<-ndf.nobs$fit-1.96*ndf.nobs$se.fit
ndf.nobs$max<-ndf.nobs$fit+1.96*ndf.nobs$se.fit
for(i in c(8,10,11)){ndf.nobs[,i]<-exp(ndf.nobs[,i])}
ggplot(ndf.nobs)+
  geom_line(aes(x=n.observers, y=fit))+
  geom_line(aes(x=n.observers, y=min), linetype="dashed")+
  geom_line(aes(x=n.observers, y=max), linetype="dashed")+
  ggtitle("Model covariates: Number of observers")+
  scale_y_continuous(limits=c(0, 2*max(ndf.nobs$fit, na.rm=T)))

# Day map
ndf.map<-as.data.frame(expand.grid(rowN=rownames(chlist3), PA="unPA", day=c(15,74,135,196,258,319), duration=median(chlist3$duration), protocol="Stationary", time.min=median(chlist3$time.min, na.rm=T), n.observers=median(chlist3$n.observers, na.rm=T)))
ndf.map$lon<-chlist3[match(ndf.map$rowN, rownames(chlist3)), "lon"]
ndf.map$lat<-chlist3[match(ndf.map$rowN, rownames(chlist3)), "lat"]
ndf.map$fit<-predict(mod.fix, ndf.map)
ndf.map$fit<-(ndf.map$fit)
ndf.map$day<-revalue(as.factor(ndf.map$day), c("15"="January","74"="March","135"="May","196"="July","258"="September","319"="November"))

ggplot(ndf.map)+
  stat_summary_hex(aes(x=lon, y=lat, z=DescTools::Winsorize(fit)))+
  ggtitle("Model covariates: Spatio-temporal variations")+
  facet_wrap(~day)


dev.off()






