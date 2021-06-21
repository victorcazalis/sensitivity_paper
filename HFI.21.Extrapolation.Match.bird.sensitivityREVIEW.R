library(data.table)
setwd("D:/These/eBird/HFI.project/Analyses")
source("H:/VictorRprofile.R")

### Charge RES and merge per species
sensi<-readRDS("Birds/1.Tables/Species.predicted.sensitivity.K6.rds")

RES<-readRDS("Birds/1.Tables/Species.presence.extract.rds")
RES<-subset(RES, sub(" ", ".", RES$Species) %in% sensi$species) # To remove marine and coastal species (all species with distribution in the Americas but not included in this study)

table(sensi$species %in% sub(" ", ".", RES$Species))
table(sub(" ", ".", RES$Species) %in% sensi$species)


RES<-as.data.table(RES)


### Scale by ecoregions
RES$scale<-RES$ECO_ID

res2<-RES[,
          .(Presence=sum(Presence), Origin=paste(Origin, collapse=" "), Season=paste(Season, collapse=" "), N=length(Presence)),
          by=.(scale, Species)]

# Plot to check distribution of individual species in ecoregions: test<-subset(res2, res2$Species=="Quiscalus mexicanus") ; map$SP<-test$Season[match(map$scale, test$scale)] ; ggplot()+geom_sf(data=map, aes(col=is.na(SP), fill=SP))


### Match species sensitivity
res2$sensi<- sensi$sensi.cor[match(sub(" ", ".", res2$Species), sensi$species)]

### Add which species are imputed
sensi$Imputed<-as.numeric(is.na(sensi$sensi))
res2$Imputed<- sensi$Imputed[match(sub(" ", ".", res2$Species), sensi$species)]

### Add all imputed sensitive
sensi$sensi_allSensi<-replace(sensi$sensi.cor, is.na(sensi$sensi)==T, 50)
res2$sensi_allSensi<- sensi$sensi_allSensi[match(sub(" ", ".", res2$Species), sensi$species)]

### Add all imputed non-sensitive
sensi$sensi_noSensi<-replace(sensi$sensi.cor, is.na(sensi$sensi)==T, 0)
res2$sensi_noSensi<- sensi$sensi_noSensi[match(sub(" ", ".", res2$Species), sensi$species)]

### Add threat
res2$threatened<-sensi$IUCN[match(sub(" ", ".", res2$Species), sensi$species)] %in% c(3,4,5)


### Merge per ecoregion
quant25<-as.numeric(quantile(sensi$sensi.cor, 0.75, na.rm=T))
quant75<-as.numeric(quantile(sensi$sensi.cor, 0.25, na.rm=T))

sens.scale<-res2[,
                .(N_spc=length(sensi), 
                  Av.sensib=mean(sensi, na.rm=T), 
                  Med.sensib=median(sensi, na.rm=T), 
                  N25=length(sensi[sensi > quant25]),
                  N25_allSensi=length(sensi_allSensi[sensi_allSensi > quant25]),
                  N25_noSensi=length(sensi_noSensi[sensi_noSensi > quant25]),
                  N25_NotImputed=length(sensi[sensi>quant25 & Imputed==0]),
                  Nthreat=length(sensi[threatened==TRUE]),
                  NImputed=length(sensi[Imputed==1])
                ),
                by=.(scale)
                ]

sens.scale$N_spc<-as.numeric(as.character(sens.scale$N_spc))
sens.scale$N25<-as.numeric(as.character(sens.scale$N25))
sens.scale$N25_allSensi<-as.numeric(as.character(sens.scale$N25_allSensi))
sens.scale$N25_noSensi<-as.numeric(as.character(sens.scale$N25_noSensi))
sens.scale$Nthreat<-as.numeric(as.character(sens.scale$Nthreat))
sens.scale$NImputed<-as.numeric(as.character(sens.scale$NImputed))
sens.scale$N25_NotImputed<-as.numeric(as.character(sens.scale$N25_NotImputed))


sens.scale$P25<-sens.scale$N25/sens.scale$N_spc
sens.scale$P25_allSensi<-sens.scale$N25_allSensi/sens.scale$N_spc
sens.scale$P25_noSensi<-sens.scale$N25_noSensi/sens.scale$N_spc
sens.scale$PThreat<-sens.scale$Nthreat/sens.scale$N_spc
sens.scale$PImputed<-sens.scale$NImputed/sens.scale$N_spc
sens.scale$P25_NotImputed<-sens.scale$N25_NotImputed/(sens.scale$N_spc-sens.scale$NImputed)



### Add values in the map
library(sf)

map<-st_transform(read_sf("D:/These/PA GIS/Ecoregions/ecoregions.Americas.withbiomes.shp"), st_crs("+proj=moll +lon_0=-80 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))
map$scale<-map$ECO_ID


map$Richness<-sens.scale$N_spc[match(map$scale, sens.scale$scale)]
map$Med.sensib<-sens.scale$Med.sensib[match(map$scale, sens.scale$scale)]
map$N25<-sens.scale$N25[match(map$scale, sens.scale$scale)]
map$P25<-sens.scale$P25[match(map$scale, sens.scale$scale)]
map$P25_allSensi<-sens.scale$P25_allSensi[match(map$scale, sens.scale$scale)]
map$P25_noSensi<-sens.scale$P25_noSensi[match(map$scale, sens.scale$scale)]
map$NImputed<-sens.scale$NImputed[match(map$scale, sens.scale$scale)]
map$PImputed<-sens.scale$PImputed[match(map$scale, sens.scale$scale)]
map$P25_NotImputed<-sens.scale$P25_NotImputed[match(map$scale, sens.scale$scale)]





### Map with extreme assumption for imputed species (Fig. S6)

Gcheck1<-ggplot(map)+
  geom_sf(aes(fill=P25), col=NA)+
  scale_fill_gradient(low="white", high="firebrick4", name="")+
  ggtitle("Proportion of high-sensitivity species")+
  theme(legend.position="top", plot.title=element_text(hjust=0.5, size=12, face="bold"), panel.background = element_rect(fill="black"), panel.grid=element_line(colour="gray50"))

Gcheck2<-ggplot(map)+
    geom_sf(aes(fill=P25_noSensi), col=NA)+
    scale_fill_gradient(low="white", high="firebrick4", name="")+
    ggtitle("All data-poor are non sensitive")+
    theme(legend.position="top", plot.title=element_text(hjust=0.5, size=12, face="bold"), panel.background = element_rect(fill="black"), panel.grid=element_line(colour="gray50"))
  
Gcheck3<-ggplot(map)+
  geom_sf(aes(fill=P25_NotImputed), col=NA)+
  scale_fill_gradient(low="white", high="firebrick4", name="")+
  ggtitle("Data-rich only")+
  theme(legend.position="top", plot.title=element_text(hjust=0.5, size=12, face="bold"), panel.background = element_rect(fill="black"), panel.grid=element_line(colour="gray50"))  

Gcheck4<-ggplot(map)+
    geom_sf(aes(fill=P25_allSensi), col=NA)+
    scale_fill_gradient(low="white", high="firebrick4", name="")+
    ggtitle("All data-poor are sensitive")+
    theme(legend.position="top", plot.title=element_text(hjust=0.5, size=12, face="bold"), panel.background = element_rect(fill="black"), panel.grid=element_line(colour="gray50"))
  
Scat1<-ggplot(map)+
    geom_point(aes(x=P25, y=P25_noSensi))+
    xlab("Used metric")+ylab("Alternative metric")+
    theme_minimal()

Scat2<-ggplot(map)+
  geom_point(aes(x=P25, y=P25_NotImputed))+
  xlab("Used metric")+ylab("Alternative metric")+
  theme_minimal()


Scat3<-ggplot(map)+
  geom_point(aes(x=P25, y=P25_allSensi))+
  xlab("Used metric")+ylab("Alternative metric")+
  theme_minimal()


mat<-matrix(c(8,1,8,8,1,8,2,3,4,2,3,4,5,6,7), byrow=T, ncol=3)

G.TOT_CHECK<-gridExtra::grid.arrange(Gcheck1, Gcheck2, Gcheck3, Gcheck4, Scat1, Scat2, Scat3, layout_matrix=mat)

cowplot::save_plot(filename="D:/These/eBird/HFI.project/Figures/Bird.analyses/Suppl/Fig.Maps.predicted.sensitivity.per.ecoregion_CHECK.png", 
                   G.TOT_CHECK, base_height=14, base_width=10)




### SAVE
keep<-as.data.frame(map)[, c("scale", "Med.sensib", "N25", "P25")]
write.csv(keep, paste0("D:/These/eBird/HFI.project/Analyses/Birds/1.Tables/Results.HFI.sensitivity.", SCALE, ".csv"))








