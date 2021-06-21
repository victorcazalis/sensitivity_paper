library(dplyr) ; library(sf) ; library(raster) ; library(exactextractr) ; library(mgcv)


####################
### PREPARE DATA ###
####################
GBrow<-"D:/eBird/HFI.project/Analyses/Birds/"
`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_)) 

### Charge data
obs<-readRDS(paste0(GBrow, "1.Tables/Obs.Script.08.Breeding.rds"))
names(obs)[names(obs)=="sci_split"]<-"species"
chlist<-readRDS(paste0(GBrow, "1.Tables/Chlist.Script.05.rds"))

setwd("D:/PA GIS/Bird.Distributions.Jan2020/BOTW.gdb")
if("distributions" %not in% ls()){distributions <- st_read(dsn = "a00000009.gdbtable")} # Don't charge if already charged
ecoregions<-st_transform(read_sf("D:/PA GIS/Ecoregions/ecoregions.Americas.withbiomes.shp"), st_crs(distributions))
  

### Create site spatial points
options(digits=10)
sites<-as.data.frame(table(chlist$Site)) ; names(sites)<-c("Site", "N")
sites$lon<-chlist$lon[match(sites$Site, chlist$Site)]
sites$lat<-chlist$lat[match(sites$Site, chlist$Site)]

pts<-st_as_sf(SpatialPointsDataFrame(coords=data.frame(sites$lon, sites$lat), data=as.data.frame(sites$Site), proj4string=CRS("+init=epsg:4238")) %>%
                spTransform(., CRSobj = CRS("+proj=longlat +datum=WGS84 +no_defs")))




############################################
### EXTRACT BREEDING PERIOD PER LATITUDE ###
############################################

### Plot visualisation
obs$day<-chlist$day[match(obs$checklist_id, chlist$Liste)]
obs$lat<-chlist$lat[match(obs$checklist_id, chlist$Liste)]

obs$CutLat<-droplevels(cut(obs$lat, breaks=seq(-90,90, by=10)))

Breed<-subset(obs, is.na(obs$code)==FALSE)

# Distribution of breeding codes
ggplot(Breed)+
  geom_histogram(aes(x=day))+
  facet_wrap(~ factor(as.factor(CutLat), rev(levels(CutLat))), scale="free_y", ncol=2, dir="v")

# ggplot(obs)+
#   geom_histogram(aes(x=day, fill=factor(is.na(code), c("TRUE", "FALSE"))))+
#   scale_y_continuous(trans="sqrt")+
#   facet_wrap(~ factor(as.factor(CutLat), rev(levels(CutLat))), scale="free_y", ncol=2, dir="v")
# 
# # Per taxonomical order
# Breed$Order<-species.char$order[match(Breed$species, species.char$species)]
# ggplot(Breed[Breed$Order !="Passeriformes",])+
#   geom_histogram(aes(x=day, fill=Order))+
#   facet_wrap(~ factor(as.factor(CutLat), rev(levels(CutLat))), scale="free_y", ncol=2, dir="v")
# 
# # Probable and certain breeding only
# Probable<-subset(Breed, Breed$code %in% c("NY", "NE", "ON", "FL", "FY", "CS", "CF", "DD", "PE", "NB", "CN", "T", "C", "N"))
# ggplot(Probable)+
#   geom_histogram(aes(x=day, fill=Order), show.legend=FALSE)+
#   facet_wrap(~ factor(as.factor(CutLat), rev(levels(CutLat))), scale="free_y", ncol=2, dir="v")



### Extract dates of circular quantiles
Breed$Jul.circ<-circular::circular(Breed$day * 360/366, type="angles", units="degrees", zero=0) # Need to be brang to 360 degrees

dates<-read.csv("D:/Dates.TEMP2.csv")
# dates<-data.frame(CutLat=levels(as.factor(Breed$CutLat)), Date_min=NA, Date_max=NA)
# 
# for(i in 1:nrow(dates)){
#   IN<-subset(Breed, Breed$CutLat==dates$CutLat[i])$Jul.circ
#   
#   dates$Date_min[i] <-366/360*as.numeric(circular::quantile.circular(IN, probs=0.025, na.rm=T))
#   dates$Date_max[i]<-366/360*as.numeric(circular::quantile.circular(IN, probs=0.975, na.rm=T))
# }
# 
# Allyear<-c("(0,10]", "(-10,0]")
# dates$Date_min[dates$CutLat %in% Allyear]<-0
# dates$Date_max[dates$CutLat %in% Allyear]<-367
# rm(Allyear)
#
# # Plot
# G_hist<-list()
# 
# for(i in 1:nrow(dates)){
#   
#   if(dates$Date_min[i] < dates$Date_max[i]){D_lim<-c(dates$Date_min[i] : dates$Date_max[i])}
#   if(dates$Date_min[i] > dates$Date_max[i]){D_lim<-c(c(0:dates$Date_max[i]), c(dates$Date_min[i]:366))}
#   
#   DF<-data.frame(Day=Breed$day[Breed$CutLat==dates$CutLat[i]]) ; DF$In<-DF$Day %in% round(D_lim)
#   
#   G_hist[[i]]<-ggplot()+
#     geom_histogram(data=DF, aes(x=Day, fill=factor(In, c("FALSE", "TRUE"))), show.legend=F)+
#     geom_vline(xintercept=dates$Date_min[i], lwd=2)+
#     geom_vline(xintercept=dates$Date_max[i], lwd=2)+
#     scale_fill_manual(values=c("gray50", "darkgreen"), drop=FALSE)+
#     ggtitle(dates$CutLat[i])+
#     scale_x_continuous(lim=c(0,367))
#   
# }
# 
# eval(parse(text= paste0("G_TOT<-gridExtra::grid.arrange(", paste0("G_hist[[", c(length(G_hist):1), "]]", collapse=","), ", layout_matrix=matrix(c(1:nrow(dates)), ncol=2, byrow=F))") ))
# 
# cowplot::save_plot("D:/eBird/HFI.project/Figures/HFI.SI.Figures/Latitude.Breeding.seasons.png", G_TOT, base_width = 7, base_height = 12)     
#      

### Remove data outside these limits in obs
library(plyr)
chlist$Remove_season<-NA
chlist$CutLat<-droplevels(cut(chlist$lat, breaks=seq(-90,90, by=10)))

for(i in 1:nrow(dates)){
  CL<-dates$CutLat[i]
  Min<-dates$Date_min[i] ; Max<-dates$Date_max[i]
  
  if(Max-Min==367){chlist$Remove_season[chlist$CutLat==CL]<-"Keep_all"} # Latitudes for which I'll keep all year round
    else{
  if(Max>Min){chlist$Remove_season[chlist$CutLat==CL]<-revalue(as.character(chlist$day[chlist$CutLat==CL] %in% round(Min:Max)), c("TRUE"="Keep_North", "FALSE"="Rm_North"))} # North part, when the max date is above the min
  
  if(Max<Min){chlist$Remove_season[chlist$CutLat==CL]<-revalue(as.character(chlist$day[chlist$CutLat==CL] %in% round(Max:Min)), c("TRUE"="Rm_South", "FALSE"="Keep_South"))} # South part, when the max is below the min
  
    }
}


table(chlist$CutLat, chlist$Remove_season)
table(is.na(chlist$Remove_season))

ggplot(chlist)+
  geom_histogram(aes(x=day, fill=Remove_season))+
  facet_wrap(~ factor(CutLat, rev(levels(CutLat))), scale="free_y")

chlist<-subset(chlist, chlist$Remove_season %in% c("Keep_all", "Keep_North", "Keep_South"))
chlist$Remove_season<-NULL
obs<-subset(obs, obs$checklist_id %in% chlist$Liste)
pts<-subset(pts, pts$`sites$Site` %in% chlist$Site)





#######################################
### WORK WITH SPECIES DISTRIBUTIONS ###
#######################################
species.list<-read.csv("D:/eBird/HFI.project/Analyses/Birds/1.Tables/Species.data.csv")
species.list$species<-as.factor(sub(" ", ".", species.list$SpeciesBL))
species.list<-subset(species.list, species.list$species %in% obs$species)
obs$Keep<-NA
obs$Site<-chlist$Site[match(obs$checklist_id, chlist$Liste)]
pts$lon<-st_coordinates(pts)[,1]
pts$lat<-st_coordinates(pts)[,2]


### Create chlist.distri where I'll stock results (presence / absence / NA)
library(data.table)
chlist.distri<-data.table(Liste=chlist$Liste, Site=chlist$Site, lon=chlist$lon, lat=chlist$lat)
obs0fill<-data.frame(Liste=NA, Site=NA, lon=NA, lat=NA, species=NA, Ab=NA)

LETT<-LETTERS[20:26] # List of letters
species.listLETT<-subset(species.list, substr(species.list$species,1,1) %in% LETT)
V<-0

for(i in 1:nrow(species.listLETT)){
  tryCatch({
### Prepare data
  SP<-species.listLETT$species[i]
  distSP<-subset(distributions, distributions$SCINAME == sub("[.]", " ", SP) & SEASONAL %in% c(1,2))
  
  tryCatch({
  distSP<-st_crop(distSP, extent(ecoregions)) # Restrict to Americas
  V<-i
  }, error=function(e){cat("Bug distri")})
  
  if(V!=i){
    write_sf(distSP, "D:/SaveAndRead.Species.Distri.Bug.shp")
    distSP<-read_sf("D:/SaveAndRead.Species.Distri.Bug.shp")
    names(distSP)[names(distSP)=="SEASONA"]<-"SEASONAL"
  }
  
  distSP<-distSP %>% dplyr::group_by(SEASONAL) %>% dplyr::summarise(N=n())
  #ggplot(distSP)+geom_sf(aes(fill=as.factor(SEASONAL)))
  
  

### Get SEASONAL for each point (with or without abundance)  
ptsSP<-subset(pts, pts$lat > extent(distSP)@ymin & pts$lat < extent(distSP)@ymax) # Remove points above or below the distribution

JoinSP<-st_join(ptsSP, distSP)
ptsSP$SEASONAL<-JoinSP$SEASONAL[match(ptsSP$`sites$Site`, JoinSP$`sites$Site`)]   # Join for other points, will get the season of points in the breeding range (1 or 2) 
ptsSP<-subset(ptsSP, is.na(ptsSP$SEASONAL)==FALSE)


### Report in chlist.distri
chlist.distri$Keep<-chlist.distri$Site %in% ptsSP$`sites$Site` # Report in chlist.distri which chlist are within breeding grounds and which one aren't

obsSP<-subset(obs, obs$species==SP)
chlist.distri$New<-NA
chlist.distri$New[chlist.distri$Keep=="TRUE"]<-obsSP$Ab[match(chlist.distri$Liste[chlist.distri$Keep=="TRUE"], obsSP$checklist_id)]
chlist.distri$New[chlist.distri$Keep=="TRUE" & is.na(chlist.distri$New)==TRUE]<-"0"

### Check keep
#ggplot(chlist.distri)+stat_summary_2d(aes(x=lon, y=lat, z=is.na(New)), bins=70)

# Write in obs0fill
chlist.species<-subset(chlist.distri, is.na(chlist.distri$New)==FALSE)
chlist.species$Keep<-NULL
chlist.species$species<-as.character(SP)
names(chlist.species)[names(chlist.species)=="New"]<-"Ab"

obs0fill<-rbind(obs0fill, chlist.species)

}, error=function(e){cat("BUG ")})

if(round(i/5)==(i/5)){cat(paste0(i, " "))}

}

table(species.listLETT$species %in% obs0fill$species)


### SOME MANUAL CHECKS
species.listLETT$species[which(species.listLETT$species %not in% obs0fill$species)]
# Check that all species in this list do not breed in the Americas (I fixed some mismatches in BL taxonomy (changes between different data sources I have))


obs0fill2<-obs0fill[2:nrow(obs0fill),]
obs0fill2$Site<-NULL
saveRDS(obs0fill2, paste0("D:/eBird/HFI.project/Analyses/Birds/1.Tables/Obs.0filled.07.", LETT[1], LETT[length(LETT)], ".rds"))



chlist$CutLat<-chlist$Site<-NULL
saveRDS(chlist, paste0(GBrow, "1.Tables/Chlist.Script.07.rds")) # Nothing has changed but it's lighter (no checklists outside breeding season)
     