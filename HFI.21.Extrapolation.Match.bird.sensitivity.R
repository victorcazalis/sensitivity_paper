library(data.table)

### Charge RES and merge per species
sensi<-readRDS("D:/eBird/HFI.project/Analyses/Birds/1.Tables/Species.predicted.sensitivity.K6.rds")

RES<-readRDS("D:/eBird/HFI.project/Analyses/Birds/1.Tables/Species.presence.extract.rds")
RES<-subset(RES, sub(" ", ".", RES$Species) %in% sensi$species) # To remove marine and coastal species (all species with distribution in the Americas but not included in this study)

RES<-as.data.table(RES)


### Scale BIOM or ECOR
SCALE<-"ECOR"

if(SCALE=="ECOR"){RES$scale<-RES$ECO_ID}
if(SCALE=="BIOM"){RES$scale<-RES$biome_NS}

res2<-RES[,
          .(Presence=sum(Presence), Origin=paste(Origin, collapse=" "), Season=paste(Season, collapse=" "), N=length(Presence)),
          by=.(scale, Species)]



### Match species sensitivity
res2$sensi<- sensi$sensi.cor[match(sub(" ", ".", res2$Species), sensi$species)]

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
                  SD=sd(sensi, na.rm=T),
                  Nthreat=length(sensi[threatened==TRUE]),
                  Ntolerantes=length(sensi[sensi<quant75])
                  ),
                by=.(scale)
                ]

sens.scale$N_spc<-as.numeric(as.character(sens.scale$N_spc))
sens.scale$N25<-as.numeric(as.character(sens.scale$N25))
sens.scale$SD<-as.numeric(as.character(sens.scale$SD))
sens.scale$Ntolerantes<-as.numeric(as.character(sens.scale$Ntolerantes))
sens.scale$Nthreat<-as.numeric(as.character(sens.scale$Nthreat))

sens.scale$P25<-sens.scale$N25/sens.scale$N_spc
sens.scale$PTol<-sens.scale$Ntolerantes/sens.scale$N_spc
sens.scale$PThreat<-sens.scale$Nthreat/sens.scale$N_spc



### Add values in the map
library(sf)

map<-st_transform(read_sf("D:/PA GIS/Ecoregions/ecoregions.Americas.withbiomes.shp"), st_crs("+proj=moll +lon_0=-80 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))

if(SCALE=="ECOR"){map$scale<-map$ECO_ID}

if(SCALE=="BIOM"){map<-map %>% group_by(biome_NS) %>% summarise()
                  map$scale<-map$biome_NS}

map$Richness<-sens.scale$N_spc[match(map$scale, sens.scale$scale)]
map$Med.sensib<-sens.scale$Med.sensib[match(map$scale, sens.scale$scale)]
map$N25<-sens.scale$N25[match(map$scale, sens.scale$scale)]
map$P25<-sens.scale$P25[match(map$scale, sens.scale$scale)]


### PLOT
G.TOT<-gridExtra::grid.arrange(
ggplot(map)+
  geom_sf(aes(fill=Med.sensib), col=NA)+
  scale_fill_viridis_c(option="plasma", name="")+
  ggtitle("Median sensitivity")+
  theme(legend.position="top", plot.title=element_text(hjust=0.5, size=12, face="bold")),

ggplot(map)+
  geom_sf(aes(fill=N25), col=NA)+
  scale_fill_viridis_c(option="plasma", name="")+
  ggtitle("Number of high-sensitivity species")+
  theme(legend.position="top", plot.title=element_text(hjust=0.5, size=12, face="bold")),

ggplot(map)+
  geom_sf(aes(fill=P25), col=NA)+
  scale_fill_viridis_c(option="plasma", name="")+
  ggtitle("Proportion of high-sensitivity species")+
  theme(legend.position="top", plot.title=element_text(hjust=0.5, size=12, face="bold"))
, ncol=3)

cowplot::save_plot(filename="D:/eBird/HFI.project/Figures/Bird.analyses/Suppl/Fig.Maps.predicted.sensitivity.per.ecoregion.png", 
                                                              G.TOT, base_height=10)




### Map variance in sensitivity
map$SD<-sens.scale$SD[match(map$scale, sens.scale$scale)]

cowplot::save_plot(filename=paste0("D:/eBird/HFI.project/Figures/Bird.analyses/Suppl/Maps.sensitivity SD.", SCALE, ".png"), 
ggplot(map)+
  geom_sf(aes(fill=DescTools::Winsorize(SD, probs=c(0,0.97))), col=NA)+
  scale_fill_viridis_c(option="plasma", name="")+
  ggtitle("Sensitivity SD")+
  theme(legend.position="top", plot.title=element_text(hjust=0.5, size=12, face="bold"))
)




### Map tolerante species distribution
map$PTol<-sens.scale$PTol[match(map$scale, sens.scale$scale)]

cowplot::save_plot(filename=paste0("D:/eBird/HFI.project/Figures/Bird.analyses/Suppl/Maps.tolerant.species.", SCALE, ".png"), 
                   ggplot(map)+
                     geom_sf(aes(fill=PTol), col=NA)+
                     scale_fill_viridis_c(option="plasma", name="")+
                     ggtitle("Sensitivity SD")+
                     theme(legend.position="top", plot.title=element_text(hjust=0.5, size=12, face="bold"))
)




### Map threatened species
map$PThreat<-sens.scale$PThreat[match(map$scale, sens.scale$scale)]
map$NThreat<-sens.scale$Nthreat[match(map$scale, sens.scale$scale)]

Th1<-ggplot(map)+
  geom_sf(aes(fill=NThreat), col=NA)+
  scale_fill_viridis_c(option="plasma", name="")+
  ggtitle("Number of threatened species")+
  theme(legend.position="top", plot.title=element_text(hjust=0.5, size=12, face="bold"))

Th2<-ggplot(map)+
  geom_sf(aes(fill=DescTools::Winsorize(PThreat, probs=c(0,0.95))), col=NA)+
  scale_fill_viridis_c(option="plasma", name="")+
  ggtitle("Proportion of threatened species")+
  theme(legend.position="top", plot.title=element_text(hjust=0.5, size=12, face="bold"))

Th3<-ggplot(map)+
  geom_point(aes(x=NThreat, y=N25))+
  xlab("Number of threatened species")+
  ylab("Number of sensitive species")+
  scale_x_continuous(trans="sqrt")+scale_y_continuous(trans="sqrt")

Th4<-ggplot(map)+
  geom_point(aes(x=PThreat, y=P25))+
  xlab("Proportion of threatened species")+
  ylab("Proportion of sensitive species")+
  scale_x_continuous(trans="sqrt")+scale_y_continuous(trans="sqrt")


cowplot::save_plot(filename=paste0("D:/eBird/HFI.project/Figures/Bird.analyses/Suppl/Maps.threatened.species.", SCALE, ".png"), 
                   gridExtra::grid.arrange(Th1, Th2, Th3, Th4, layout_matrix=matrix(c(1,2,1,2,3,4), ncol=2, byrow=T)),
                   base_height=10
  )




### SAVE
keep<-as.data.frame(map)[, c("scale", "Med.sensib", "N25", "P25", "NThreat", "PThreat", "SD", "PTol")]
write.csv(keep, paste0("D:/eBird/HFI.project/Analyses/Birds/1.Tables/Results.HFI.sensitivity.", SCALE, ".csv"))








