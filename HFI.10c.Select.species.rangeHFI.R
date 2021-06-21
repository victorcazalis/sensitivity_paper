library(mgcv) ; library(dplyr) ; library(cowplot)
GBrow<-"D:/eBird/HFI.project/Analyses/Birds/"
`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_)) 



chlist<-readRDS(paste0(GBrow, "1.Tables/Chlist.Script.07.rds"))
library(data.table)


rangeList<-list()

for(i in 3:8){
### Charge the data I want
LETT<-c("1a", "1b", 2,3,4,5,6,7)[i]
if(LETT=="1a"){obs<-readRDS(paste0(GBrow, "1.Tables/Obs.0filled.07.AC.rds")) ; obs<-subset(obs, substr(obs$species,1,1) %in% c("A", "B"))}
if(LETT=="1b"){obs<-readRDS(paste0(GBrow, "1.Tables/Obs.0filled.07.AC.rds")); obs<-subset(obs, substr(obs$species,1,1) == "C")}
if(LETT==2){obs<-readRDS(paste0(GBrow, "1.Tables/Obs.0filled.07.DE.rds")) ; obs$Site<-NULL}
if(LETT==3){obs<-readRDS(paste0(GBrow, "1.Tables/Obs.0filled.07.FK.rds")) ; obs$Site<-NULL}
if(LETT==4){obs<-readRDS(paste0(GBrow, "1.Tables/Obs.0filled.07.LN.rds"))}
if(LETT==5){obs<-readRDS(paste0(GBrow, "1.Tables/Obs.0filled.07.OQ.rds"))}
if(LETT==6){obs<-readRDS(paste0(GBrow, "1.Tables/Obs.0filled.07.RS.rds"))}
if(LETT==7){obs.a<-readRDS(paste0(GBrow, "1.Tables/Obs.0filled.07.TZ.rds")) ; obs.b<-readRDS(paste0(GBrow, "1.Tables/Obs.0filled.07.Manual.adds.rds")) ; obs<-rbind(obs.a, obs.b) ; rm(obs.a, obs.b)}


obs$HFI<-chlist$hfp13[match(obs$Liste, chlist$Liste)]

obs<-as.data.table(obs)

rangeHF<-obs[, 
   .(HFmin=min(HFI, na.rm=T), HFmax=max(HFI, na.rm=T), HF1=quantile(HFI, probs=0.01, na.rm=T), HF99=quantile(HFI, probs=0.99, na.rm=T)),
   by=.(species)] 

rangeList[[i]]<-rangeHF
}


rangeHF<-rbind(rangeList[[1]], rangeList[[2]], rangeList[[3]], rangeList[[4]], rangeList[[5]], rangeList[[6]], rangeList[[7]], rangeList[[8]])
rangeHF<-subset(rangeHF, is.na(rangeHF$HF1)==FALSE) # Some species only with NA (probably all in Galapagos with no human footprint!)

rangeHF$range1<-rangeHF$HFmax-rangeHF$HFmin
rangeHF$range2<-rangeHF$HF99-rangeHF$HF1 


# speciesHFI<-readRDS("D:/eBird/HFI.project/Analyses/Birds/1.Tables/Species.measured.sensitivity.K6.rds")
# rangeHF$measured<-speciesHFI$HFI50[match(rangeHF$species, speciesHFI$species)]
# 
# ggplot(rangeHF)+geom_histogram(aes(range1, fill=is.na(measured)))
# ggplot(rangeHF)+geom_histogram(aes(range2, fill=is.na(measured)))


write.csv(rangeHF, "D:/eBird/HFI.project/Analyses/Birds/1.Tables/Species.footprint.ranged.in.checklists.csv", row.names=T)

