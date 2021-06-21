library(sf)

setwd("D:/These")

### Prepare data
ecoregion<-st_transform(read_sf("PA GIS/Ecoregions/ecoregions.Americas.withbiomes.shp"), st_crs("+proj=moll +lon_0=-80 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))

ecoDF<-read.csv("eBird/HFI.project/Analyses/Habitat/abs.and.trends.per.ecoregion.csv")

sensit<-read.csv("eBird/HFI.project/Analyses/Birds/1.Tables/Results.HFI.sensitivity.ECOR.csv")


ecoregion$absolute<- (-1)*ecoDF$absolute[match(ecoregion$ECO_ID, ecoDF$ECO_ID)]
ecoregion$trends<-ecoDF$trends[match(ecoregion$ECO_ID, ecoDF$ECO_ID)]
ecoregion$PA.extent<-ecoDF$PA.extent[match(ecoregion$ECO_ID, ecoDF$ECO_ID)]
ecoregion$intactPA<-ecoDF$propint[match(ecoregion$ECO_ID, ecoDF$ECO_ID)]
ecoregion$P25<-sensit$P25[match(ecoregion$ECO_ID, sensit$scale)]
ecoregion$N25<-sensit$N25[match(ecoregion$ECO_ID, sensit$scale)]
ecoregion$Med.sensib<-sensit$Med.sensib[match(ecoregion$ECO_ID, sensit$scale)]
ecoregion$N_threat<-sensit$NThreat[match(ecoregion$ECO_ID, sensit$scale)]


### Save table of ecoregions for SI
ecoregion$extent<-ecoDF$extent[match(ecoregion$ECO_ID, ecoDF$ECO_ID)]
ecosave<-as.data.frame(ecoregion[,c("ECO_NAME", "ECO_ID", "extent", "PA.extent", "intactPA", "absolute", "trends", "P25", "N25", "Med.sensib")])
ecosave$geometry<-NULL
ecosave$PA.extent<-round(ecosave$PA.extent,2) ; ecosave$intactPA<-round(ecosave$intactPA,2) ; ecosave$absolute<-round(ecosave$absolute,1) ; ecosave$trends<-round(ecosave$trends,2) ; ecosave$P25<-round(ecosave$P25,2) ; ecosave$Med.sensib<-round(ecosave$Med.sensib,2)
ecosave$trends<-ecosave$trends * (-1) # To transform in trends in intactness and not in HF
ecosave$extent<-scales::scientific(ecosave$extent,3)
ecosave<-subset(ecosave, is.na(ecosave$absolute)==F)
ecosave<-ecosave[order(ecosave$ECO_NAME),]
write.csv(ecosave, "eBird/HFI.project/Analyses/Habitat/Ecoregion.results.for.suppTable.csv", row.names=F)






#########################
### EXTENT INTACT PAs ###
#########################

### Map extent of intact PAs (Fig.2B)
ecoregion$intactPA.cut<-replace(ecoregion$intactPA, ecoregion$intactPA>0.5, 0.5)
Map_intactPA<-ggplot()+
  geom_sf(data=ecoregion, aes(fill=(intactPA.cut)), col=NA, show.legend=F)+
  scale_fill_gradient(low="white", high="darkgreen", trans="sqrt", name="")+
  ggtitle("")+
  theme(legend.text=element_text(angle=90, hjust=0.5, vjust=0.5), panel.background = element_rect(fill="black"), panel.grid=element_line(colour="gray50"), axis.text=element_blank(), axis.ticks=element_blank())


### Match between intact habitat protection and sensitivity (Fig.2D)
Mscale <- data.frame(
  Sensi=rep(c("1Low", "2Med", "3High"),3), 
  Prot=c(rep("1Low",3), rep("2Med",3), rep("3High",3)),
  Fill=rev(c("#684777ff","#435786","#4885C1", "orange3", "peachpuff3", "#89A1C8", "red3", "lightsalmon2", "mistyrose2" 
  ))
)

ecoregion$Fill.cat<-paste0(
  cut(ecoregion$intactPA, breaks=c(0,0.05,0.17,1), labels=c("1Low", "2Med", "3High"), include.lowest=T), 
  cut(ecoregion$P25, breaks=c(quantile(ecoregion$P25, probs=c(0,1/3,2/3,1), na.rm=T)), labels=c("1Low", "2Med", "3High"))
)
ecoregion$Fill.col<-Mscale$Fill[match(ecoregion$Fill.cat, paste0(Mscale$Prot, Mscale$Sensi))]

Fit_Intact<-ggplot(ecoregion)+
  geom_sf(aes(fill=Fill.col), col=NA)+
  scale_fill_identity()+
  ggtitle("")+
  theme(plot.title=element_text(hjust=0.5, size=12, face="bold"), panel.background = element_rect(fill="black"), panel.grid=element_line(colour="gray50"), axis.text=element_blank(), axis.ticks=element_blank())



### Correlation with P25 (Fig.2E)
Correlation<-cor.test(ecoregion$P25, ecoregion$intactPA)

Cor_Intact<-ggplot(ecoregion)+
  geom_hline(yintercept=c(0.05,0.17), linetype="dashed", col="darkgray")+
  geom_vline(xintercept=c(quantile(ecoregion$P25, probs=c(1/3,2/3), na.rm=T)), linetype="dashed", col="darkgray")+
  geom_point(aes(x=P25, y=intactPA, col=Fill.col))+
  scale_colour_identity()+
  scale_y_continuous(trans="sqrt")+
  ylab("Protection intensity")+
  xlab("Proportion of high-sensitivity species")+
  ggtitle("")+
  theme(plot.title=element_text(hjust=0.5, size=12, face="bold"), panel.background = element_rect(fill="black"), panel.grid=element_line(colour="gray50"))







##############
### TRENDS ###
##############
ecoregion$trendsQual<-cut(ecoregion$trends, breaks=c(-10,-2,-0.5,-0.1,0.1,0.5,2,10), labels=c("RECOVERING", "Recovering", "recovering", "stable", "degrading", "Degrading", "DEGRADING"))
ecoregion$trendsQual<-factor(ecoregion$trendsQual, rev(c("RECOVERING", "Recovering", "recovering", "stable", "degrading", "Degrading", "DEGRADING")))

Map_Trends<-ggplot()+
  geom_sf(data=ecoregion, aes(fill=trendsQual), col=NA, show.legend=F)+
  scale_fill_manual(values=c("#8e0152", "#c51b7d", "#f1b6da", "#f7f7f7", "#b8e186", "#4d9221", "#276419"), name=NULL)+  
  ggtitle("")+
  theme(legend.position = "top", plot.title=element_text(hjust=0.5, size=12, face="bold"), panel.background = element_rect(fill="black"), panel.grid=element_line(colour="gray50"), axis.text=element_blank(), axis.ticks=element_blank())



### Correlation with P25
tn <- scales::trans_new("logpeps",
                        function(x) {sign(x)*log(abs(x)+1)},
                        function(x) {sign(x)*(exp(abs(x))-1)})

Correlation.T<-cor.test(ecoregion$P25, ecoregion$trends)

Cor_Trends<-ggplot(ecoregion)+
  geom_hline(yintercept=0, col="darkred")+
  geom_point(aes(x=P25, y=(-1*trends), col=trendsQual), show.legend=FALSE)+
  scale_y_continuous(trans=tn)+
  scale_colour_manual(values=c("#8e0152", "#c51b7d", "#f1b6da", "#f7f7f7", "#b8e186", "#4d9221", "#276419"), name=NULL)+  
  ylab("Trends in PA intactness")+
  xlab("Proportion of high-sensitivity species")+
  ggtitle("")+
  theme(plot.title=element_text(hjust=0.5, size=12, face="bold"), panel.background = element_rect(fill="black"), panel.grid=element_line(colour="gray50"))


  
  

### PLOT PROPORTION OF HIGH-SENSITIVITY SPECIES 
G_sensi<-ggplot(ecoregion)+
  geom_sf(aes(fill=P25), col=NA, show.legend=F)+
  scale_fill_gradient(low="white", high="firebrick4", name="")+
  ggtitle("")+
  theme(legend.position="top", plot.title=element_text(hjust=0.5, size=12, face="bold"), legend.text=element_text(angle=90, hjust=0.5, vjust=0.5), panel.background = element_rect(fill="black"), panel.grid=element_line(colour="gray50"), axis.text=element_blank(), axis.ticks=element_blank())




####################
### Global plots ###
####################
library(gridExtra)

### Figure 2
mat4<-matrix(c(1,2,7,1,2,7,3,4,5,3,6,5), byrow=T, ncol=3)
G_trends<-grid.arrange(G_sensi, Map_intactPA, Fit_Intact, Cor_Intact, Map_Trends, Cor_Trends, layout_matrix=mat4)

cowplot::save_plot(filename="eBird/HFI.project/Figures/Bird.Analyses/raw.FigMaps.png",
                   G_trends,
                   base_height=16, base_width=19.4)



###############
### FIG. S4 ###
###############

G_sensiSUP<-ggplot(ecoregion)+
  geom_sf(aes(fill=P25), col=NA)+
  scale_fill_gradient(low="white", high="firebrick4", name="")+
  ggtitle("Proportion of high-sensitivity species")+
  theme(legend.position="top", plot.title=element_text(hjust=0.5, size=12, face="bold"), legend.text=element_text(angle=90, hjust=0.5, vjust=0.5), panel.background = element_rect(fill="black"), panel.grid=element_line(colour="gray50"), axis.text=element_blank(), axis.ticks=element_blank())

Cor_Sensia<-ggplot(ecoregion)+
  geom_point(aes(x=P25, y=intactPA))+
  scale_y_continuous(trans="sqrt")+
  ylab("Intact habitat protection")+
  xlab("Proportion of high-sensitivity species")+
  ggtitle("")+
  theme_minimal()

G_sensi0<-ggplot(ecoregion)+
  geom_sf(aes(fill=Med.sensib), col=NA)+
  scale_fill_gradient(low="white", high="firebrick4", name="")+
  ggtitle("Median sensitivity")+
  theme(legend.position="top", plot.title=element_text(hjust=0.5, size=12, face="bold"), legend.text=element_text(angle=90, hjust=0.5, vjust=0.5), panel.background = element_rect(fill="black"), panel.grid=element_line(colour="gray50"), axis.text=element_blank(), axis.ticks=element_blank())

Cor_Sensib<-ggplot(ecoregion)+
  geom_point(aes(x=Med.sensib, y=intactPA))+
  scale_y_continuous(trans="sqrt")+
  ylab("Intact habitat protection")+
  xlab("Median sensitivity")+
  ggtitle("")+
  theme_minimal()

G_sensi1<-ggplot(ecoregion)+
  geom_sf(aes(fill=N25), col=NA)+
  scale_fill_gradient(low="white", high="firebrick4", name="")+
  ggtitle("Number of high-sensitivity species")+
  theme(legend.position="top", plot.title=element_text(hjust=0.5, size=12, face="bold"), legend.text=element_text(angle=90, hjust=0.5, vjust=0.5), panel.background = element_rect(fill="black"), panel.grid=element_line(colour="gray50"), axis.text=element_blank(), axis.ticks=element_blank())

Cor_Sensic<-ggplot(ecoregion)+
  geom_point(aes(x=N25, y=intactPA))+
  scale_y_continuous(trans="sqrt")+
  ylab("Intact habitat protection")+
  xlab("Number of high-sensitivity species")+
  ggtitle("")+
  theme_minimal()

library(gridExtra)

mat2<-matrix(c(1,1,1,1,1,2,2,3,3,3,3,3,4,4, 5,5,5,5,5,6,6), ncol=3, byrow=F)
Gannex4<-grid.arrange(G_sensiSUP, Cor_Sensia, G_sensi0, Cor_Sensib, G_sensi1, Cor_Sensic, layout_matrix=mat2)

cowplot::save_plot(filename="D:/These/eBird/HFI.project/Figures/Bird.Analyses/Suppl/Maps.FigS4.sensitivityVariables.png",
                   Gannex4, 
                   base_height=10)



#############
### Fig.3 ###
#############
ecoregion$Area<-st_area(ecoregion)


# Biome barplot
ecoregion$biome<-factor(ecoregion$biome, names(sort((table(ecoregion$biome)), decreasing=T)))
ecoregion$Fill.col<-factor(ecoregion$Fill.col, rev(as.character(Mscale$Fill)[c(3, 6, 2, 5, 1, 9, 8, 4, 7)]))

table(is.na(ecoregion$Fill.col))
Biomes<-ggplot(ecoregion[is.na(ecoregion$Fill.col)==F,])+
  geom_bar(aes(x=biome, fill=Fill.col))+
  geom_bar(aes(x=biome, col=Fill.cat %in% c("1Low3High", "2Med3High", "1Low2Med")), fill=NA, show.legend=F)+
  scale_colour_manual(values=c(NA, "black"))+
  ylab("Number of ecoregions")+
  xlab("")+
  scale_fill_identity()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.background = element_rect(fill="white"), panel.grid=element_line(colour="gray90"))

cowplot::save_plot(filename="D:/eBird/HFI.project/Figures/Bird.Analyses/rawFigure3.png",
                   Biomes,
                   base_height=8, base_width=10)



 
