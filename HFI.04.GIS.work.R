library(DescTools) ; library(sf) ; library(sp) ; library(rgdal) ; library(rgeos) ; library(raster) ; library(exactextractr)

chlist<-readRDS(paste0(GBrow, "1.Tables/Chlist.Script03.", QQ, ".DataTable.rds"))



## Apply by site to avoid redundancy in sites
options(digits=10)
chlist$Site<-paste(chlist$lon, chlist$lat, sep="/")
sites<-as.data.frame(table(chlist$Site)) ; names(sites)<-c("Site", "N")
sites$lon<-chlist$lon[match(sites$Site, chlist$Site)]
sites$lat<-chlist$lat[match(sites$Site, chlist$Site)]


################################
### Add biome and ecoregions ###
################################
pts.full<-SpatialPointsDataFrame(coords=data.frame(sites$lon, sites$lat), data=as.data.frame(sites$Site), proj4string=CRS("+init=epsg:4238")) %>%
  spTransform(., CRSobj = CRS("+proj=moll +lon_0=-80 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))

##### Extract ecoregion information through a raster

### CREATE THE RASTER for faster calculation
# I chose the Mollweide projection, putting Central America in the middle
# ecoregion<-st_transform(read_sf("D:/PA GIS/Ecoregions/ecoregions.Americas.withbiomes.shp"), st_crs("+proj=moll +lon_0=-80 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))
# polygs<-spTransform(as_Spatial(ecoregion), CRSobj=crs(ecoregion))
# ras<-raster(crs = crs(ecoregion), res=1000, ext = extent(ecoregion))
# rst1<-rasterize(polygs, ras) 
# rst2<-rasterize(as(polygs, "SpatialLines"), ras) # I combine lines and polygons so that I have every cell interesting covered (as with lines), which is needed for litoral values
# rst_ecoreg<-cover(rst1, rst2) 
# write.table(as.data.frame(rst1@data@attributes), "D:/PA GIS/Ecoregions/ecoregion.HFI.attributes.txt")
# writeRaster(rst_ecoreg, "D:/PA GIS/Ecoregions/ecoregions.americas.rasterized.tif")



### EXTRACT WITH THE RASTER
rst_ecoreg<-raster("D:/PA GIS/Ecoregions/ecoregions.americas.rasterized.tif")
sites$Ecoregion<-extract(rst_ecoreg, pts.full, fun=modal, na.rm=FALSE)  # Gives random number that are stored in attributes (used in following match)
attrib<-read.table("D:/PA GIS/Ecoregions/ecoregion.HFI.attributes.txt", header=TRUE)
sites$ECO_ID<-attrib$ECO_ID[match(sites$Ecoregion, attrib$ID)]
sites$Ecoregion<-NULL


# Subset de chlist et pts dans la zone d'interet uniquement
sites<-subset(sites, is.na(sites$ECO_ID)==FALSE)
pts<-pts.full[pts.full$`sites$Site` %in% sites$Site,]







################################
### Ajout des valeurs raster ###
################################

### Create buffer
bf<-st_transform(st_buffer(st_as_sf(pts), 2500), "+init=epsg:4238")

### Altitude 
alt<-raster("D:/PA GIS/GLOBEelevation.Soum/globe_v1_all/w001001.adf")
bf<-st_transform(bf, crs(alt))
alt<-crop(alt, extent(bf), snap="out") # reduce range
bf$alt<-exact_extract(alt, bf, fun=function(value, cov_frac){mean(value[cov_frac>0.01], na.rm=T)})

### Productivity 
npp<-raster("D:/PA GIS/NPP_2014_2016/NPP_mean_2014_2016.tif")
bf<-st_transform(bf, crs(npp))
npp<-crop(npp, extent(bf), snap="out") # reduce range
bf$npp<-exact_extract(npp, bf, fun=function(value, cov_frac){mean(value[cov_frac>0.01], na.rm=T)})

### Human footprint 
hfp00<-raster("D:/PA GIS/Human Footprint Watson/hfp2000_merisINT.tif")
bf<-st_transform(bf, crs(hfp00))
hfp00<-crop(hfp00, extent(bf), snap=  "out") 
bf$hfp00<-exact_extract(hfp00, bf, fun=function(value, cov_frac){mean(value[cov_frac>0.01], na.rm=T)})

hfp13<-raster("D:/PA GIS/Human Footprint Watson/hfp2013_merisINT.tif")
hfp13<-crop(hfp13, extent(bf), snap=  "out") 
bf$hfp13<-exact_extract(hfp13, bf, fun=function(value, cov_frac){mean(value[cov_frac>0.01], na.rm=T)})

### Add in chlist
chlist$Ecoregion<-sites$ECO_ID[match(chlist$Site, sites$Site)]
chlist$alt<-bf$alt[match(chlist$Site, bf$`sites$Site`)]
chlist$npp<-bf$npp[match(chlist$Site, bf$`sites$Site`)]
chlist$hfp00<-bf$hfp00[match(chlist$Site, bf$`sites$Site`)]
chlist$hfp13<-bf$hfp13[match(chlist$Site, bf$`sites$Site`)]



##########################
### SAVE CHLIST VALUES ###
##########################

### Remove chlist
chlist<-subset(chlist, is.na(chlist$Ecoregion)==FALSE)


### Save
saveRDS(chlist, paste0(GBrow, "1.Tables/Chlist.Script.04.", QQ, ".rds"))





