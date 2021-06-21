setwd("D:/These")
GBrow<-"D:/These/eBird/HFI.project/Analyses/Birds/"
`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_)) 
library(mgcv) ; library(dplyr)


##########################
### CHARGE ALL RESULTS ###
##########################
speciesHFI<-readRDS("eBird/HFI.project/Analyses/Birds/1.Tables/Species.measured.sensitivity.K6.rds")

rangeHF<-read.csv("D:/eBird/HFI.project/Analyses/Birds/1.Tables/Species.footprint.ranged.in.checklists.csv")
speciesHFI$rangeHF<-rangeHF$range2[match(speciesHFI$species, rangeHF$species)]
speciesHFI$HFI50[speciesHFI$rangeHF<25]<-NA # Remove estimate for species with insufficient contrast in the HF in range


### Put in the general table
species.char<-readRDS(paste0(GBrow, "1.Tables/Species.characteristics.rds"))
species.char$HFI50<-speciesHFI$HFI50[match(species.char$species, speciesHFI$species)] ; rm(speciesHFI)

## Remove species with range =0 (non American species)
species.char<-subset(species.char, species.char$range.km2 !=0 & species.char$Primary.Habitat %not in% c("Sea", "Coastal"))

## Change HFI50 for sensitivity
species.char$sensi<-(50-species.char$HFI50) ; species.char$HFI50<-NULL

## IUCN in quantitative variable
species.char$IUCN<-plyr::revalue(species.char$IUCN, c("LC"=1, "NT"=2, "VU"=3, "EN"=4, "CR"=5, "DD"=1)) %>% as.character(.) %>% as.numeric(.)


speciesMES<-subset(species.char, is.na(species.char$sensi)==F)




####################################
### MODEL DRIVERS OF SENSITIVITY ###
####################################
mod<-lm(sensi ~ Primary.Habitat + Primary.Diet + specialisation + log(Mass) + IUCN + log(range.km2) + Order + Mig, data=speciesMES)
par(mfrow=c(2,2)) ; plot(mod) ; par(mfrow=c(1,1))
summary(mod) ; car::Anova(mod)


# mod0<-lm((50-HFI50) ~ 1, data=speciesMES)
# 
# mod<-lmer((50-HFI50) ~ Primary.Habitat + log(specialisation) + log(Mass) + IUCN + log(range.km2) + Mig + (1|Family), data=speciesMES)
# piecewiseSEM::rsquared(mod)
# 
# sel<-step(mod0, scope=~ Primary.Habitat + Primary.Diet + log(specialisation) + log(Mass) + IUCN + log(range.km2) + Order + Mig , direction="both") 
# mod<-lm((50-HFI50) ~ Primary.Habitat + log(specialisation) + log(Mass) + log(range.km2) + Order + Mig, data=speciesMES)





#################################
### PLOT RESULTS OF THE MODEL ###
#################################

### Main habitat
ND<-data.frame(Primary.Habitat=c("Artificial", "Desert", "Forest", "Grassland", "Riparian", "Rocky", "Savanna", "Shrub", "Wetland", "Woodland"), specialisation=median(speciesMES$specialisation), Primary.Diet="Invertebrate", Mass=median(speciesMES$Mass, na.rm=T), IUCN=1, range.km2=median(speciesMES$range.km2, na.rm=T), Order="Passeriformes", Mig="Sedentary", Lat.centr=median(speciesMES$Lat.centr, na.rm=T))
ND$Pred<-predict(mod, ND, se.fit=F)
ND$se.fit<-predict(mod, ND, se.fit=T)$se.fit
ND$max<-ND$Pred+1.96*ND$se.fit ; ND$min<-ND$Pred-1.96*ND$se.fit

G_main<-ggplot(ND)+
  geom_pointrange(aes(x=Primary.Habitat, y=Pred, ymin=min, ymax=max))+
  ylab("Species sensitivity")+
  ggtitle("A) Primary habitat")+ 
  xlab("")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), plot.title=element_text(hjust=0.5, size=12, face="bold"), panel.background=element_rect(fill = "white"), panel.grid=element_line(colour="gray85"))


### Diet
ND<-data.frame(Primary.Habitat="Forest", specialisation=median(speciesMES$specialisation), Primary.Diet=levels(droplevels(speciesMES$Primary.Diet)), Mass=median(speciesMES$Mass, na.rm=T), IUCN=1, range.km2=median(speciesMES$range.km2, na.rm=T), Order="Passeriformes", Mig="Sedentary", Lat.centr=median(speciesMES$Lat.centr, na.rm=T))
ND$Pred<-predict(mod, ND, se.fit=F)
ND$se.fit<-predict(mod, ND, se.fit=T)$se.fit
ND$max<-ND$Pred+1.96*ND$se.fit ; ND$min<-ND$Pred-1.96*ND$se.fit
ND$Primary.Diet<-revalue(ND$Primary.Diet, c("Carnivore"="Carnivorous", "Fish"="Piscivorous", "Fruit"="Frugivorous", "Invertebrate"="Insectivorous", "Nectar"="Nectarivorous", "Omnivore"="Omnivorous", "Plant"="Herbivorous", "Seed"="Granivorous"))

G_diet<-ggplot(ND)+
  geom_pointrange(aes(x=Primary.Diet, y=Pred, ymin=min, ymax=max))+
  ylab("Species sensitivity")+
  ggtitle("C) Primary diet")+ 
  xlab("")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), plot.title=element_text(hjust=0.5, size=12, face="bold"), panel.background=element_rect(fill = "white"), panel.grid=element_line(colour="gray85"))


### Order
ND<-data.frame(Primary.Habitat="Forest", specialisation=median(speciesMES$specialisation), Primary.Diet="Invertebrate", Mass=median(speciesMES$Mass, na.rm=T), IUCN=1, range.km2=median(speciesMES$range.km2, na.rm=T), Order=levels(droplevels(speciesMES$Order)), Mig="Sedentary", Lat.centr=median(speciesMES$Lat.centr, na.rm=T))
ND$Pred<-predict(mod, ND, se.fit=F)
ND$se.fit<-predict(mod, ND, se.fit=T)$se.fit
ND$max<-ND$Pred+1.96*ND$se.fit ; ND$min<-ND$Pred-1.96*ND$se.fit
Ord_med<-ND$Order[which(ND$Pred==median(ND$Pred))]

G_Order<-ggplot(ND)+
  geom_pointrange(aes(x=Order, y=Pred, ymin=min, ymax=max))+
  ylab("Species sensitivity")+
  ggtitle("B) Order")+
  xlab("")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), plot.title=element_text(hjust=0.5, size=12, face="bold"), panel.background=element_rect(fill = "white"), panel.grid=element_line(colour="gray85"))


### IUCN
ND<-data.frame(Primary.Habitat="Forest", specialisation=median(speciesMES$specialisation), Primary.Diet="Invertebrate", Mass=median(speciesMES$Mass, na.rm=T), IUCN=seq(1, 2, length.out=100), range.km2=median(speciesMES$range.km2, na.rm=T), Order="Passeriformes", Mig="Sedentary", Lat.centr=median(speciesMES$Lat.centr, na.rm=T))
ND$Pred<-predict(mod, ND, se.fit=F)
ND$se.fit<-predict(mod, ND, se.fit=T)$se.fit
ND$max<-ND$Pred+1.96*ND$se.fit ; ND$min<-ND$Pred-1.96*ND$se.fit

G_iucn<-ggplot(ND)+
  geom_line(aes(x=IUCN, y=Pred))+
  geom_line(aes(x=IUCN, y=min), linetype="dashed")+
  geom_line(aes(x=IUCN, y=max), linetype="dashed")+
  ggtitle("E) Red List status")+
  ylab("Species sensitivity")+
  xlab("")+
  theme(plot.title=element_text(hjust=0.5, size=12, face="bold"), panel.background=element_rect(fill = "white"), panel.grid=element_line(colour="gray85"))


### Body mass
ND<-data.frame(Primary.Habitat="Forest", specialisation=median(speciesMES$specialisation), Primary.Diet="Invertebrate", Mass=seq(min(speciesMES$Mass, na.rm=T), max(speciesMES$Mass, na.rm=T), length.out=100), IUCN=1, range.km2=median(speciesMES$range.km2, na.rm=T), Order="Passeriformes", Mig="Sedentary", Lat.centr=median(speciesMES$Lat.centr, na.rm=T))
ND$Pred<-predict(mod, ND, se.fit=F)
ND$se.fit<-predict(mod, ND, se.fit=T)$se.fit
ND$max<-ND$Pred+1.96*ND$se.fit ; ND$min<-ND$Pred-1.96*ND$se.fit

G_mass<-ggplot(ND)+
  geom_line(aes(x=Mass, y=Pred))+
  geom_line(aes(x=Mass, y=min), linetype="dashed")+
  geom_line(aes(x=Mass, y=max), linetype="dashed")+
  scale_x_continuous(trans="log10")+
  ggtitle("H) Body mass (g)")+
  ylab("Species sensitivity")+
  xlab("")+
  theme(plot.title=element_text(hjust=0.5, size=12, face="bold"), panel.background=element_rect(fill = "white"), panel.grid=element_line(colour="gray85"))




### Range
ND<-data.frame(Primary.Habitat="Forest", specialisation=median(speciesMES$specialisation, na.rm=T), Primary.Diet="Invertebrate", Mass=median(speciesMES$Mass, na.rm=T), IUCN=1, range.km2=seq(min(speciesMES$range.km2, na.rm=T), max(speciesMES$range.km2, na.rm=T), length.out=1000), Order="Passeriformes", Mig="Sedentary", Lat.centr=median(speciesMES$Lat.centr, na.rm=T))
ND$Pred<-predict(mod, ND, se.fit=F)
ND$se.fit<-predict(mod, ND, se.fit=T)$se.fit
ND$max<-ND$Pred+1.96*ND$se.fit ; ND$min<-ND$Pred-1.96*ND$se.fit

G_range<-ggplot(ND)+
  geom_line(aes(x=range.km2, y=Pred))+
  geom_line(aes(x=range.km2, y=min), linetype="dashed")+
  geom_line(aes(x=range.km2, y=max), linetype="dashed")+
  scale_x_continuous(trans="log10")+
  ggtitle("G) Range (km2)")+
  ylab("Species sensitivity")+
  xlab("")+
  theme(plot.title=element_text(hjust=0.5, size=12, face="bold"), panel.background=element_rect(fill = "white"), panel.grid=element_line(colour="gray85"))


### Specialisation
ND<-data.frame(Primary.Habitat="Forest", specialisation=seq(min(speciesMES$specialisation, na.rm=T), max(speciesMES$specialisation, na.rm=T), length.out=100), Primary.Diet="Invertebrate", Mass=median(speciesMES$Mass, na.rm=T), IUCN=1, range.km2=median(speciesMES$range.km2, na.rm=T), Order="Passeriformes", Mig="Sedentary", Lat.centr=median(speciesMES$Lat.centr, na.rm=T))
ND$Pred<-predict(mod, ND, se.fit=F)
ND$se.fit<-predict(mod, ND, se.fit=T)$se.fit
ND$max<-ND$Pred+1.96*ND$se.fit ; ND$min<-ND$Pred-1.96*ND$se.fit

G_specialisation<-ggplot(ND)+
  geom_line(aes(x=specialisation, y=Pred))+
  geom_line(aes(x=specialisation, y=min), linetype="dashed")+
  geom_line(aes(x=specialisation, y=max), linetype="dashed")+
  ggtitle("F) Specialisation")+
  ylab("Species sensitivity")+
  xlab("")+
  theme(plot.title=element_text(hjust=0.5, size=12, face="bold"), panel.background=element_rect(fill = "white"), panel.grid=element_line(colour="gray85"))



### Migration
ND<-data.frame(Primary.Habitat="Forest", specialisation=median(speciesMES$specialisation), Primary.Diet="Invertebrate", Mass=median(speciesMES$Mass, na.rm=T), IUCN=1, range.km2=median(speciesMES$range.km2, na.rm=T), Order="Passeriformes", Mig=c("Sedentary", "Strict", "Partial"), Lat.centr=median(speciesMES$Lat.centr, na.rm=T))
ND$Pred<-predict(mod, ND, se.fit=F)
ND$se.fit<-predict(mod, ND, se.fit=T)$se.fit
ND$max<-ND$Pred+1.96*ND$se.fit ; ND$min<-ND$Pred-1.96*ND$se.fit

G_migr<-ggplot(ND)+
  geom_pointrange(aes(x=Mig, y=Pred, ymin=min, ymax=max))+
  ylab("Species sensitivity")+
  ggtitle("D) Migration status")+ 
  xlab("")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), plot.title=element_text(hjust=0.5, size=12, face="bold"), panel.background=element_rect(fill = "white"), panel.grid=element_line(colour="gray85"))






### TOTAL PLOT
library(egg)

GR1<-ggarrange(G_main,G_Order, ncol=2, widths=c(4,4))
GR2<-ggarrange(plots=list(G_diet, G_migr, G_iucn), ncol=3, widths=c(4,2,2))
GTOT<-gridExtra::grid.arrange(GR1, GR2,G_specialisation,G_range,G_mass, layout_matrix=matrix(c(1,1,1,1,3, 1,1,1,1,3 ,1,1,1,1,4, 2,2,2,2,4, 2,2,2,2,5, 2,2,2,2,5), ncol=5, byrow=T))
cowplot::save_plot("D:/eBird/HFI.project/Figures/Bird.analyses/Suppl/Drivers.of.sensitivity.png", GTOT, base_aspect_ratio = 1.6, base_height=7)




##########################################
### PREDICT SENSITIVITY TO ALL SPECIES ###
##########################################
species.char$MES.HFI50<-speciesMES$HFI50[match(species.char$species, speciesMES$species)]

## Fill values for those with values not in speciesMES
species.char$Order[droplevels(species.char$Order) %not in% droplevels(speciesMES$Order)]<-Ord_med # C'est l'ordre avec la valeur mediane


species.char$sensi.pred<-predict(mod, newdata=species.char, se.fit=FALSE)
species.char$error<-abs(species.char$sensi.pred-species.char$sensi) # To see species that are badly predicted

Raw<-ggplot(species.char[is.na(species.char$sensi)==F,])+
     geom_point(aes(x=sensi, y=sensi.pred), size=0.7)+
     xlab("Measured sensitivity") + ylab("Predicted sensitivity (uncorrected)")+
     geom_abline(intercept=0, slope=1)+
     scale_x_continuous(lim=c(0,55))+
     scale_y_continuous(lim=c(0,55))

cor.test(species.char$sensi, species.char$sensi.pred)


## Correct the slope

species.char$sensi.cor <- (scale(species.char$sensi.pred)*sd(species.char$sensi, na.rm=T))+mean(species.char$sensi, na.rm=T)
species.char$sensi.cor <- as.numeric(replace(species.char$sensi.cor, species.char$sensi.cor>50, 50))
species.char$sensi.cor <- as.numeric(replace(species.char$sensi.cor, species.char$sensi.cor<0, 0))

Cor<-ggplot(species.char[is.na(species.char$sensi)==F,])+
  geom_point(aes(x=sensi, y=sensi.cor), size=0.9)+
  xlab("Measured sensitivity") + ylab("Predicted sensitivity (corrected)")+
  geom_abline(intercept=0, slope=1)+
  scale_x_continuous(limits=c(min(species.char$sensi.cor, na.rm=T), max(species.char$sensi.cor, na.rm=T)))

cowplot::save_plot("D:/eBird/HFI.project/Figures/Bird.analyses/Suppl/Correlation.measured.corrected.png", gridExtra::grid.arrange(Raw, Cor, ncol=2), base_aspect_ratio = 2, base_height=6)
cor.test(species.char$sensi.cor, species.char$sensi)

# Keep the measured sensitivity for species for which it has been measured
species.char$sensi.cor[is.na(species.char$sensi)==F]<-species.char$sensi[is.na(species.char$sensi)==F]





saveRDS(species.char, "D:/eBird/HFI.project/Analyses/Birds/1.Tables/Species.predicted.sensitivity.K6.rds")





##################################
### PLOT SENSITIVITY HISTOGRAM ###
##################################

G_histo<-ggplot(species.char)+
  geom_histogram(aes(x=sensi.cor, fill=factor(is.na(sensi), c("TRUE", "FALSE"))), show.legend=F)+
  scale_x_continuous(limits=c(0,50))+
  scale_fill_manual(values=c("gray60", "gray20"))+
  theme_minimal()+
  xlab("Species sensitivity")+ylab("")






cowplot::save_plot("D:/eBird/HFI.project/Figures/Bird.analyses/Histograms.for.Fig.1.svg", G_histo, base_aspect_ratio = 1.8, base_height=4)










