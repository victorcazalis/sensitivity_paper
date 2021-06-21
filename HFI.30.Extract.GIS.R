library(raster) ; library(dplyr) ; library(sf)


ecoregions<-st_transform(read_sf("D:/PA GIS/Ecoregions/ecoregions.Americas.withbiomes.shp"), st_crs("+proj=moll +lon_0=-80 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))
hfi00<-raster("D:/PA GIS/Human Footprint Watson/hfp2000_merisINT.tif") %>% crop(., extent(st_transform(ecoregions, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs ")), snap="out")
hfi13<-raster("D:/PA GIS/Human Footprint Watson/hfp2013_merisINT.tif") %>% crop(., extent(st_transform(ecoregions, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs ")), snap="out")

shp.PA<-st_transform(read_sf("D:/PA GIS/PA mondiales/VersionJan2020/WDPA_Jan2020-shapefile-polygons.shp"), st_crs("+proj=moll +lon_0=-80 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))
shp.PA<-st_crop(st_buffer(shp.PA,0), extent(ecoregions))
# Subset PAs
shp.PA<-subset(shp.PA, shp.PA$STATUS %in% c("Designated", "Inscribed", "Established")) # Removes Adopted, Not Reported, Proposed (total = 1700 / 216 000)
shp.PA<-subset(shp.PA, shp.PA$DESIG_ENG != "UNESCO-MAB Biosphere Reserve")
shp.PA<-subset(shp.PA, shp.PA$MARINE %in% c(0,1)) # Removes full marine (keep full terrestrial and mixed)




ecoPAglob<-st_intersection(ecoregions, shp.PA)
ecoPAglob<-ecoPAglob %>% group_by(ECO_ID) %>% summarise(N=n())


ecoSF<-st_as_sf(as_Spatial(ecoPAglob)) # Necessary because of a bug, not sure why...

### Extract absolute wilderness
library(exactextractr)
ecoSF$absolute<-exactextractr::exact_extract(hfi13, ecoSF, fun=function(value, cov_frac){mean(value[cov_frac>0.5], na.rm=T)})


### Extract trends
diff<-hfi13-hfi00
ecoSF$trends<-exact_extract(diff, ecoSF, fun=function(value, cov_frac){mean(value[cov_frac>0.5], na.rm=T)})




### Extract extent of intact protected habitat
hfibin<-hfi13<4
ecoSF$wildPA<-exact_extract(hfibin, ecoSF, fun=function(value, cov_frac){sum(value[cov_frac>0.1], na.rm=T)})

hfi1<-replace(hfi13, hfi13>=0, 1)
ecoregions$extent1<-exact_extract(hfi1, ecoregions, fun=function(value, cov_frac){sum(value[cov_frac>0.1], na.rm=T)})


### In a DF
ecoDF<-data.frame(ECO_ID=ecoSF$ECO_ID, absolute=ecoSF$absolute, trends=ecoSF$trends, intactPA=ecoSF$wildPA)
ecoDF$extent1<-ecoregions$extent1[match(ecoDF$ECO_ID, ecoregions$ECO_ID)]
ecoDF$propint<-ecoDF$intactPA/ecoDF$extent1

### Add protection proportion
prot<-read.csv("D:/eBird/HFI.project/Analyses/Habitat/area.protected.biome.csv", sep=",")
ecoDF$PA.extent<-(prot$PA_area/prot$area)[match(ecoDF$ECO_ID, prot$ECO_ID)]


### Save 
write.csv(ecoDF, "D:/eBird/HFI.project/Analyses/Habitat/abs.and.trends.per.ecoregion.csv", row.names=F)

