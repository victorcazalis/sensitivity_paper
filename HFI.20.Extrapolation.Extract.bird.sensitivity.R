
library(raster) ; library(exactextractr) ; library(sf)
`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_)) 


### Charge distributions
setwd("D:/PA GIS/Bird.Distributions.Jan2020/BOTW.gdb")
if("distributions" %not in% ls()){distributions <- st_read(dsn = "a00000009.gdbtable")} # Don't charge if already charged

distributions<-subset(distributions, 
                      distributions$PRESENCE %not in% c(4,5) & # Remove extinct and possibly extinct
                        distributions$SEASONAL %in% c(1,2)) # Remove non-breeding grounds (keep only resident and breeding)



###########################
### LIST PER ECOREGIONS ###
###########################
rst_ecoreg<-raster("D:/PA GIS/Ecoregions/ecoregions.americas.rasterized.tif")
ecoregions<-st_transform(read_sf("D:/PA GIS/Ecoregions/ecoregions.Americas.withbiomes.shp"), st_crs(distributions))
attrib<-read.table("D:/PA GIS/Ecoregions/ecoregion.HFI.attributes.txt", header=TRUE)
matrice<-matrix(c(attrib$ID, attrib$ECO_ID), byrow=F, ncol=2)
rst_ecoreg<-reclassify(rst_ecoreg, matrice)

res<-as.data.frame(ecoregions) ; res$geometry<-NULL
res$Presence<-res$Species<-res$Season<-res$Origin<-NA
resTOT<-res[1,]
resTOT[1,]<-NA


## Note: this loop is far from optimal. I first coded a raster of ecoregion to avoid using intersection (which takes time) but then had to add an intersection at the beginning to avoid re-projection issue while reprojection was not mandatory. It's correct but should not be used like that. + I manually changed for the species names of "Phalacrocorax brasilianus" and "Gallinula melanops" because they used a different name in BL distributions

for(SP in 1:nrow(distributions)){
  tryCatch({
  resSP<-res
  
  dist_raw<-distributions[SP,]
  if(as.numeric(st_area(dist_raw))==0){write_sf(dist_raw, "Shp.species.transform.shp") ; dist_raw<-read_sf("Shp.species.transform.shp") %>% st_buffer(., 0)} # I save and charge some shapefiles with issues in the distribution

  distSP<-st_intersection(dist_raw, ecoregions) %>% st_transform(., crs(rst_ecoreg)) %>% st_combine() # First intersect to avoid re-projection issues
  
  
  if(is.null(nrow(distSP))==F){
  if(nrow(distSP)>0){
  Extr<-exact_extract(rst_ecoreg, distSP)
  Extrdf<-as.data.frame(table(Extr[[1]]$value))
  
  resSP$Presence<-Extrdf$Freq[match(resSP$ECO_ID, Extrdf$Var1)]
  
  resSP$Species<-distributions$SCINAME[SP]
  resSP$Season<-distributions$SEASONAL[SP]
  resSP$Origin<-distributions$ORIGIN[SP]

  resSP<-subset(resSP, is.na(resSP$Presence)==FALSE)
  
  resTOT<-rbind(resTOT, resSP)
  }}
  } ,error=function(e){cat(paste0("BUG", SP))})
  
  if(round(SP/1000) == SP/1000){cat(SP) ; saveRDS(resTOT, "D:/resTOT.provisoire.rds") ; beep(1)}
  
  
}


saveRDS(resTOT[-c(1),], "D:/eBird/HFI.project/Analyses/Birds/1.Tables/Species.presence.extract.rds")









