library(raster) ; library(dplyr) ; library(sf)
`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_)) 



ecoregions<-read_sf("PA GIS/Ecoregions/ecoregions.Americas.withbiomes.shp")
#ecoregions<-read_sf("H:/PhD/Sensitivity paper/Fig.4 TSE/ecoregions.Americas.withbiomes.shp")

hfi13<-raster("PA GIS/Human Footprint Watson/hfp2013_merisINT.tif") %>% crop(., extent(st_transform(ecoregions, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs ")), snap="out")
hfibin<-hfi13<4

shp.PA<-st_transform(read_sf("PA GIS/PA mondiales/VersionJan2020/WDPA_Jan2020-shapefile-polygons.shp"), st_crs(ecoregions))
shp.PA<-st_crop(st_buffer(shp.PA,0), extent(ecoregions))
# Subset PAs
shp.PA<-subset(shp.PA, shp.PA$STATUS %in% c("Designated", "Inscribed", "Established")) # Removes Adopted, Not Reported, Proposed (total = 1700 / 216 000)
shp.PA<-subset(shp.PA, shp.PA$DESIG_ENG != "UNESCO-MAB Biosphere Reserve")
shp.PA<-subset(shp.PA, shp.PA$MARINE %in% c(0,1)) # Removes full marine (keep full terrestrial and mixed)



### Charge distributions
setwd("H:/PhD/Sensitivity paper/Bird.Distributions.Jan2020/BOTW.gdb")
if("distributions" %not in% ls()){distributions <- st_read(dsn = "a00000009.gdbtable")} # Don't charge if already charged

distributions<-subset(distributions, 
                      distributions$PRESENCE %not in% c(4,5) & # Remove extinct and possibly extinct
                        distributions$SEASONAL %in% c(1,2)) # Remove non-breeding grounds (keep only resident and breeding)

sensi<-readRDS("eBird/HFI.project/Analyses/Birds/1.Tables/Species.predicted.sensitivity.K6.rds")
#sensi<-readRDS("H:/PhD/Sensitivity paper/Fig.4 TSE/Species.predicted.sensitivity.K6.rds")
distributions<-subset(distributions, distributions$SCINAME %in% sub("[.]", " ", sensi$species))


###################################################
### CALCULATE SPECIES INTACT PROTECTED HABITATS ###
###################################################
test<-as.data.frame(coordinates(hfibin)) # Extracts all coordinates of the hfiraster
test$Bin<-as.vector(hfibin)

test2<-subset(test, test$Bin==1) # Includes all coordinates of hfiraster with HFI <4
ggplot(test2)+stat_binhex(aes(x,y))
test2$ID<-paste0("IDv", 1:nrow(test2))

test3<-st_as_sf(x = test2, 
                        coords = c("x", "y"),
                        crs = crs(hfibin)) # As test2 (all coords of intact cells) but in shp
test3<-st_transform(test3, st_crs(shp.PA))

test4<-st_join(test3, shp.PA, join=st_intersects) # Calculates whether each intact point was within PAs

test5<-subset(test4, is.na(test4$WDPAID)==F)
test5<-test3[test3$ID %in% test5$ID,] # Take from test3 as test5 has double entries (points within several PAs)


disS<-subset(distributions, sub(" ", ".", distributions$SCINAME) %in% sensi$species[sensi$sensi.cor>as.numeric(quantile(sensi$sensi.cor, 0.75))]) # Subset distributions with only high-sensitivity species
write_sf(disS, "Distributions.test.shp") # To correct some bugs from distributions
disS<-st_read("Distributions.test.shp")
disS3<-st_buffer(disS, 0)
disS4<-st_crop(disS3, extent(ecoregions)) # Restrict to the study region

test6<-st_join(test5, disS4, join=st_intersects) # Cross species data with the distribution of intact protected habitats

test7<-ddply(test6, .(SCINAME), function(x){data.frame(N=nrow(x))})
write.csv(test7, "eBird/HFI.project/Analyses/Birds/1.Tables/Species.access.to.intactPAs.csv", row.names=F)



### Charge results
test7<-read.csv("eBird/HFI.project/Analyses/Birds/1.Tables/Species.access.to.intactPAs.csv")
# test7<-read.csv("H:/PhD/Sensitivity paper/Fig.4 TSE/Species.access.to.intactPAs.csv")
High<-sensi[sensi$sensi.cor > as.numeric(quantile(sensi$sensi.cor, 0.75)),] # Subset high-sensitivity species

## Calculate species theoric threshold
High$threshold<-212.5967-37.53222*log10(High$range.km2) # Calculated from Rodrigues et al. 2004 (Bioscience)
High$threshold[High$range.km2<1000]<-100
High$threshold[High$range.km2>250000]<-10

### Extract intact PA access
High$intactPA<-test7$N[match(High$species, sub(" ", ".", test7$SCINAME))]
High$intactPA[is.na(High$intactPA)==TRUE]<-0 # For species with no access (not included in the file "test7")
High$intactProp<-High$intactPA/High$range.km2*100
High$Access<-High$intactProp>High$threshold ; table(High$Access)


### Species with no / minor cover
High$nocover<-High$intactProp<0.1*High$threshold

### Species of major concern
Pb<-High[High$nocover==T,]
Pb$Imputed<-is.na(Pb$sensi)

# Save table of data-rich species lacking protection (Table S3)
TabSav<-Pb[Pb$Imputed==F, c("species", "sensi.cor", "intactPA", "intactProp", "IUCN")]
TabSav$IUCN<-plyr::revalue(as.character(TabSav$IUCN), c("1"="LC", "2"="NT", "3"="VU", "4"="EN", "5"="CR"))
TabSav$species<-sub("[.]", " ", TabSav$species)
names(TabSav)<-c("Name", "Sensitivity", "Area of intact PAs", "Proportion of intact PAs", "RL status")
TabSav<-TabSav[order(TabSav$Name),]
TabSav$Sensitivity<-round(TabSav$Sensitivity, 1)
TabSav$`Proportion of intact PAs`<-round(TabSav$`Proportion of intact PAs`, 2)
write.csv(TabSav, "eBird/HFI.project/Analyses/Habitat/list.nocover.species.csv", row.names=F)


### Map (Fig.2C)
disINTACT<-subset(distributions, sub(" ", ".",distributions$SCINAME) %in% Pb$species)

ecoregions$Var1<-"A"
ecoDISS<-ecoregions %>% dplyr ::group_by(Var1) %>% dplyr::summarise(N = n())
ecoDISS<-st_transform(ecoDISS, st_crs("+proj=moll +lon_0=-80 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")) # To have consistent projections with other maps
disINTACT<-st_transform(disINTACT, st_crs(ecoDISS)) 

Map<-ggplot()+
  geom_sf(data=ecoDISS, fill="white", col=NA)+
  geom_sf(data=disINTACT, fill="darkred", col=NA, alpha=0.2)+
  theme(panel.background = element_rect(fill="black"), panel.grid=element_line(colour="gray50"), axis.text=element_blank(), axis.ticks=element_blank())+
  xlim((raster::extent(ecoDISS)[1:2])*1.1) 

ggsave("Major.Concern.png",
       plot=Map,
       width=6, height=8, units="in", dpi=300)


disINTACT$Imputed<-Pb$Imputed[match(sub(" ", ".", disINTACT$SCINAME), Pb$species)]
MapSup<-gridExtra::grid.arrange(
  ggplot()+
  geom_sf(data=ecoDISS, fill="white", col=NA)+
  geom_sf(data=disINTACT[disINTACT$Imputed==F,], fill="darkred", col=NA, alpha=0.2)+
  ggtitle("Data-rich species")+
  theme(panel.background = element_rect(fill="black"), panel.grid=element_line(colour="gray50"), axis.text=element_blank(), axis.ticks=element_blank())+
  xlim((raster::extent(ecoDISS)[1:2])*1.1),
  
  ggplot()+
    geom_sf(data=ecoDISS, fill="white", col=NA)+
    geom_sf(data=disINTACT[disINTACT$Imputed==T,], fill="darkred", col=NA, alpha=0.2)+
    ggtitle("Data-poor species")+
    theme(panel.background = element_rect(fill="black"), panel.grid=element_line(colour="gray50"), axis.text=element_blank(), axis.ticks=element_blank())+
    xlim((raster::extent(ecoDISS)[1:2])*1.1),
  
  ncol=2)

ggsave("Major.ConcernSUPP.png",
       plot=MapSup,
       width=12, height=8, units="in", dpi=300)



### Fig. S8 (Increase needed to reach target)
High_inadequately<-High[High$intactProp<High$threshold,]
mean(High_inadequately$threshold-High_inadequately$intactProp)

ggplot(High[High$intactProp<High$threshold,])+
  geom_histogram(aes(x=(threshold-intactProp)))+
  xlab("Increase needed")+
  theme_classic()



