library(mgcv) ; library(dplyr) ; library(cowplot)
GBrow<-"D:/These/eBird/HFI.project/Analyses/Birds/"
`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_)) 


### Charge the data I want
LETT<-"1a" # Choose between 1 and 7

if(LETT=="1a"){obs<-readRDS(paste0(GBrow, "1.Tables/Obs.0filled.07.AC.rds")) ; obs<-subset(obs, substr(obs$species,1,1) %in% c("A", "B"))}
if(LETT=="1b"){obs<-readRDS(paste0(GBrow, "1.Tables/Obs.0filled.07.AC.rds")); obs<-subset(obs, substr(obs$species,1,1) == "C")}
if(LETT==2){obs<-readRDS(paste0(GBrow, "1.Tables/Obs.0filled.07.DE.rds")) ; obs2$Site<-NULL}
if(LETT==3){obs<-readRDS(paste0(GBrow, "1.Tables/Obs.0filled.07.FK.rds")) ; obs3$Site<-NULL}
if(LETT==4){obs<-readRDS(paste0(GBrow, "1.Tables/Obs.0filled.07.LN.rds"))}
if(LETT==5){obs<-readRDS(paste0(GBrow, "1.Tables/Obs.0filled.07.OQ.rds"))}
if(LETT==6){obs<-readRDS(paste0(GBrow, "1.Tables/Obs.0filled.07.RS.rds"))}
if(LETT==7){obs.a<-readRDS(paste0(GBrow, "1.Tables/Obs.0filled.07.TZ.rds")) ; obs.b<-readRDS(paste0(GBrow, "1.Tables/Obs.0filled.07.Manual.adds.rds")) ; obs<-rbind(obs.a, obs.b) ; rm(obs.a, obs.b)}



### Charge data
species<-readRDS("D:/These/eBird/HFI.project/Analyses/Birds/1.Tables/Species.characteristics.rds")
chlist<-readRDS(paste0(GBrow, "1.Tables/Chlist.Script.07.rds"))



######################
### SELECT SPECIES ###
######################

### Calculate nb observations and nb inferred absences
sum(species$N.obs[species$Primary.Habitat %not in% c("Sea", "Coastal")])
sum(species$N.points[species$Primary.Habitat %not in% c("Sea", "Coastal")], na.rm=T)-sum(species$N.obs[species$Primary.Habitat %not in% c("Sea", "Coastal")])

###
species<-subset(species, (N.obs*(1-prop.X))>200 & # Remove species that had less than 200 observations (excluding Xs)
                         Primary.Habitat %not in% c("Sea", "Coastal") & 
                         prop.X<0.2)

obs<-as.data.frame(obs)
obs<-obs[obs$species %in% species$species,]





#########################
### MODEL SENSITIVITY ###
#########################

### Prepare
ND<-data.frame(hfp13=seq(0,50,by=0.05), duration=60, obsKelling=median(chlist$obsKelling, na.rm=T), N_obs=1, alt=median(chlist$alt, na.rm=T), alt2=median(chlist$alt, na.rm=T)^2, npp=median(chlist$npp, na.rm=T), npp2=median(chlist$npp, na.rm=T)^2)
species$HFI50<-NA

# Replaces Xs by NAs
obs$Ab<-replace(obs$Ab, obs$Ab=="X", NA) %>% as.character() %>% as.numeric()

# Prepare for parabolic responses
chlist$npp2<-chlist$npp^2
chlist$alt2<-chlist$alt^2


LEV<-levels(droplevels(as.factor(obs$species))) ; length(LEV)




### Run model

for(SP in 1:length(LEV)){ 
SPC<-LEV[SP]
  
  tryCatch({
obsSP<-subset(obs, obs$species==SPC)
chlist$OBS<-obsSP$Ab[match(chlist$Liste, obsSP$Liste)]

M<-mgcv::bam(OBS ~ s(hfp13, k=6) + log(duration) + obsKelling + log(N_obs) + alt+alt2 + npp+npp2 + te(lon, lat), data=chlist, family="nb")

ND$lon<-median(chlist$lon[chlist$OBS>0], na.rm=T) ; ND$lat<-median(chlist$lat[chlist$OBS>0], na.rm=T)
NDsub<-subset(ND, ND$hfp13>min(chlist$hfp13[is.na(chlist$OBS)==F], na.rm=T) & ND$hfp13<max(chlist$hfp13[is.na(chlist$OBS)==F], na.rm=T))
NDsub$Pred<-predict(M, newdata=NDsub, type="response", se.fit=FALSE)

species$HFI50[species$species==SPC]<-round(weighted.mean(NDsub$hfp13, NDsub$Pred),4)
species$HFImode[species$species==SPC]<-NDsub$hfp13[NDsub$Pred==max(NDsub$Pred,na.rm=T)]


save_plot(paste0("D:/eBird/HFI.project/Figures/Individual.sensitivity.K6/", round(species$HFI50[species$species==SPC]), ".", SPC, ".png"),
  ggplot(NDsub)+ 
  geom_area(aes(x=hfp13, y=Pred))+
  geom_vline(xintercept=species$HFI50[species$species==SPC], col="red")+
  scale_x_continuous(limits=c(0,50))+
  ggtitle(sub("[.]", " ", SPC)),
  base_width=3, base_height=3)


  if(SP/20 == round(SP/20)){cat(SP)
                            saveRDS(species, paste0("D:/SpeciesSens.TEMP.K6.", LETT, SP, ".rds"))
                            source("D:/eBird/HFI.project/Analyses/Birds/HFI.10b.footprint.models.controls.R")
  }

  } ,error=function(e){cat(paste0("BUG", SP, "\n"))})
}



saveRDS(species, paste0("D:/eBird/HFI.project/Analyses/Birds/1.Tables/Species.measured.sensitivity.K6.", LETT, ".rds"))



