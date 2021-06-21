library(dplyr) ; library(mgcv) ; library(gtools)
`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_)) 



### Charge data
chlist<-readRDS(paste0(GBrow, "1.Tables/Chlist.Script.05.rds"))
obs<-readRDS(paste0(GBrow, "1.Tables/Obs.Script.05.rds"))
obs$scientific_name<-sub(" ", ".", obs$scientific_name)

iucn<-read.csv("D:/eBird/Data/Expand.Analyses.tables/global/RL status birds.csv", sep=";")   # Je le charge ici pour voir les noms qui ne collent pas
iucn$Scientific.name<-sub(" ", ".", iucn$Scientific.name)

cornell<-read.csv("D:/eBird/Data/Expand.Analyses.tables/global/HBW 3.0-eBird v2018 match.csv", sep=";", header=T)
cornell$Sc_Avibase<-cornell$Sc_HBW<-cornell$Sc_Clem<-NA
for(i in 1:nrow(cornell)){
  cornell$Sc_Avibase[i]<-paste(unlist(strsplit(as.character(cornell$Avibase_latin_name[i])," "))[1], unlist(strsplit(as.character(cornell$Avibase_latin_name[i])," "))[2], sep=".")
  cornell$Sc_HBW[i]<-paste(unlist(strsplit(as.character(cornell$hbw_latin[i])," "))[1], unlist(strsplit(as.character(cornell$hbw_latin[i])," "))[2], sep=".")
  cornell$Sc_Clem[i]<-paste(unlist(strsplit(as.character(cornell$clements_latin[i])," "))[1], unlist(strsplit(as.character(cornell$clements_latin[i])," "))[2], sep=".")
}


### Remove some species
obs<-subset(obs, obs$scientific_name %not in% c("Scytalopus.sp.")) # Maybe need to remove "Megascops.gilesi" as well?


### Report name changes and HBW-lump
nameschange<-data.frame(Nom_eBird=unique(obs$scientific_name[obs$scientific_name %not in% iucn$Scientific.name]))
nameschange$match<-cornell$match[match(nameschange$Nom_eBird, cornell$Sc_Clem)]
nameschange$HBW.Clem<-cornell$HBW.Clem[match(nameschange$Nom_eBird, cornell$Sc_Clem)]
nameschange$Birdlife.HBW<-cornell$Sc_HBW[match(nameschange$Nom_eBird, cornell$Sc_Clem)]
nameschange$match<-replace(as.character(nameschange$match), nameschange$match=="HBW lump", "HBW-lump")







######################################
### APPLY CHANGES FOR NAME CHANGES ###
######################################
table(nameschange$match)
names.1ok<-nameschange[nameschange$match=="1-ok",]

obs$sci_modified<-obs$scientific_name
obs$sci_modified[obs$scientific_name %in% names.1ok$Nom_eBird]<-names.1ok$Birdlife.HBW[match(obs$scientific_name[obs$scientific_name %in% names.1ok$Nom_eBird], names.1ok$Nom_eBird)]

write.csv(names.1ok, paste0(GBrow, "1.Tables/Taxonomy/Taxonomy.control.NameChanges.csv"), row.names=F)




##################################
### APPLY CHANGES FOR HBW-lump ###
##################################

### Import the table of existing changes
lump<-read.csv(paste0(GBrow, "1.Tables/Taxonomy/Taxo.HBWlump.HFI.csv"), sep=";", colClasses = "character")


### Check for new species to include
lump.to.add<-subset(nameschange, nameschange$Nom_eBird %not in% c(lump$Nom_eBird, lump$Nom_eBird2) & nameschange$match != "1-ok")
if(nrow(lump.to.add)>0){
  write.csv(lump.to.add, paste0(GBrow, "1.Tables/Taxonomy/00.HBW.LUMP.TO.ADD.csv"), row.names=F)
  readline("YOU SHOULD ADD NEW SPECIES TO Taxo.HBWlump.HFI.csv; click on Enter afterwards")
  # Reload if I have added names in HBW lump
  lump<-read.csv(paste0(GBrow, "1.Tables/Taxonomy/Taxo.HBWlump.HFI.csv"), sep=";", colClasses = "character")
  lump.to.add<-subset(nameschange, nameschange$Nom_eBird %not in% c(lump$Nom_eBird, lump$Nom_eBird2) & nameschange$match != "1-ok")
  cat(paste0("There are ", nrow(lump.to.add), " species to add in the lump file"))
}


### Isolate obs from species to lump as I'll need to sum there abundance
obs.NOlump<-subset(obs, obs$sci_modified %not in% c(lump$Nom_eBird, lump$Nom_eBird2))
obs.lump<-subset(obs, obs$sci_modified %in% c(lump$Nom_eBird, lump$Nom_eBird2))
cat("Should be TRUE") ; nrow(obs) == (nrow(obs.NOlump) + nrow(obs.lump))


### Apply changes to obs.lump 
obs.lump$sci_modified[obs.lump$sci_modified %in% lump$Nom_eBird]<-lump$Nom_Birdlife[match(obs.lump$sci_modified[obs.lump$sci_modified %in% lump$Nom_eBird], lump$Nom_eBird)]
obs.lump$sci_modified[obs.lump$sci_modified %in% lump$Nom_eBird2]<-lump$Nom_Birdlife[match(obs.lump$sci_modified[obs.lump$sci_modified %in% lump$Nom_eBird2], lump$Nom_eBird2)]


### Sum by checklist*species
obs.lump$Ab<-as.numeric(replace(obs.lump$Ab, obs.lump$Ab=="X", 1000000))

library(data.table)
obs.lump2<-obs.lump[, 
                    .(Ab=sum(Ab), scientific_name=paste0(scientific_name, collapse="_"), N_Lump=length(Ab)), 
                    by=.(as.factor(checklist_id), as.factor(sci_modified))] 

obs.lump2$Ab<-as.character(replace(obs.lump2$Ab, obs.lump2$Ab>=1000000, "X"))
colnames(obs.lump2)[1:2]<-c("checklist_id", "sci_modified")

### Merge obs.NOlump and obs.lump
obs.lumped<-plyr::rbind.fill(obs.lump2, obs.NOlump)


### Save temporary
saveRDS(obs.lumped, paste0(GBrow, "1.Tables/Obs.Script.06.taxoTEMP.rds"))





###################################
### APPLY CHANGES FOR HBW-split ###
###################################


### Prepare datasets
# Charge distributions
library(DescTools) ; library(sf) ; library(sp) ; library(rgdal) ; library(rgeos) ; library(raster); library(cowplot) ; library(dplyr)
setwd("D:/eBird/BirdLife Distributions V7.0")
if("distributions" %not in% ls()){distributions <- st_read(dsn = "a00000009.gdbtable")} # Don't charge if already charged

# Select species to split
split<-subset(cornell, cornell$match=="HBW split" & cornell$Sc_Clem%in% obs.lumped$sci_modified) 

# Add sites in obs.lumped to be able to work by site
obs.lumped$Site<-chlist$Site[match(obs.lumped$checklist_id, chlist$Liste)]

# Create a sf file with sites
sites<-chlist %>% distinct(Site, .keep_all = T)
pts.full<-st_as_sf(spTransform(SpatialPointsDataFrame(coords=data.frame(sites$lon, sites$lat), data=as.data.frame(sites$Site), proj4string=CRS("+init=epsg:4238")), CRSobj = CRS("+proj=longlat +datum=WGS84 +no_defs")))
names(pts.full)[1]<-"Site"

# Charge limits for the plot
limits<-st_transform(read_sf("D:/PA GIS/Administrative boundaries/Limits America HFP.shp"), st_crs("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))






#############################
### LOOP TO SPLIT SPECIES ###
#############################

split$Sc_Clem<-as.factor(split$Sc_Clem)

track<-data.frame(Name_eBird=NA, Nobs_TOT=NA, Nobs_REMOVED=NA, Names_Birdlife=NA, nb_spc=NA)


pdf(paste0("D:/eBird/HFI.project/Figures/Check.figures/Taxonomy/Distance HBW splits HFI project.pdf"), width=15, height=8)

for(i in 1:nlevels(split$Sc_Clem)){ # For each eBird species included in obs that is reported as HBW-split by Cornell
#for(i in 217:236){ # If I need to split it for calculation purposes
  track[i,]<-NA
  
  Name_eBird<-levels(split$Sc_Clem)[i] ; track$Name_eBird<-as.character(track$Name_eBird) ; track$Name_eBird[i]<-as.character(Name_eBird) # Get its eBird name
  Names_Birdlife<-gsub("[.]", " ", cornell$Sc_HBW[cornell$Sc_Clem==levels(split$Sc_Clem)[i]]) # Get its Birdlife names (take in the whole Cornell because some are not written HBW-split for every subspecies, eg. Arizelocichla.milanjensis)
  Names_Birdlife<-unique(c(Names_Birdlife, gsub("[.]", " ", Name_eBird))) # If eBird name not included, (can happen when Cornell table is weird), add it
  Names_Birdlife<-Names_Birdlife[Names_Birdlife != "NA NA"]
  
  Distrib<-distributions[distributions$SCINAME %in% Names_Birdlife,]
  
  
  # Make a plot of species distribution around the hotspot
  G_distr<-ggplot()+
    geom_sf(data=limits, col=NA, fill="thistle")+
    geom_sf(data=Distrib, aes(fill=SCINAME), col=NA)+
    ggtitle("Distribution")
  
  G_distr2<-ggplot()+
    geom_sf(data=st_crop(limits, extent(Distrib)), col=NA, fill="thistle")+
    geom_sf(data=Distrib, aes(fill=SCINAME), col=NA, alpha=0.6)+
    ggtitle("Distribution zoom")
  
  
  # Check distributions do not overlap (I only try for 2 to 4 species species)
  if(nlevels(as.factor(Names_Birdlife))==2){cmd<-"geom_sf(data=st_intersection(st_buffer(Distrib[1,],0), st_buffer(Distrib[2,],0)), fill='darkred', col=NA)"}
  if(nlevels(as.factor(Names_Birdlife))==3){cmd<-"geom_sf(data=st_intersection(st_buffer(Distrib[1,],0), st_buffer(Distrib[2,],0)), fill='darkred', col=NA)   +   geom_sf(data=st_intersection(st_buffer(Distrib[1,],0), st_buffer(Distrib[3,],0)), fill='darkred', col=NA)   +   geom_sf(data=st_intersection(st_buffer(Distrib[2,],0), st_buffer(Distrib[3,],0)), fill='darkred', col=NA)"}
  if(nlevels(as.factor(Names_Birdlife))>=4){cmd<-"geom_sf(data=st_intersection(st_buffer(Distrib[1,],0), st_buffer(Distrib[2,],0)), fill='darkred', col=NA)   +   geom_sf(data=st_intersection(st_buffer(Distrib[1,],0), st_buffer(Distrib[3,],0)), fill='darkred', col=NA)   +   geom_sf(data=st_intersection(st_buffer(Distrib[1,],0), st_buffer(Distrib[4,],0)), fill='darkred', col=NA)   +   geom_sf(data=st_intersection(st_buffer(Distrib[2,],0), st_buffer(Distrib[3,],0)), fill='darkred', col=NA)   +   geom_sf(data=st_intersection(st_buffer(Distrib[2,],0), st_buffer(Distrib[4,],0)), fill='darkred', col=NA)   +   geom_sf(data=st_intersection(st_buffer(Distrib[3,],0), st_buffer(Distrib[4,],0)), fill='darkred', col=NA)"}

  cmd1<-paste0("st_buffer(Distrib[", 1:2, ",],0)", collapse=", ")
  cmd<-paste0("geom_sf(data=st_intersection(",  cmd1 , "), fill='darkred', col=NA)")
  G_overlap<-ggplot()+
    geom_sf(data=Distrib, fill="darksalmon", col=NA)+
    eval(parse(text=cmd))+
    ggtitle("Check distributions do not overlap (darkred)")
  
  
  ##### START WORKING WITH OBSERVATION
  
  # Subset pts
  pts<-subset(pts.full, pts.full$Site %in% obs.lumped$Site[obs.lumped$sci_modified==Name_eBird])
  
  # First use st_join for points within distributions
  pts$SCINAME<-as.character(st_join(pts, st_difference(st_buffer(Distrib,0)), join=st_intersects)$SCINAME) # The st_difference is for the overlaps: choose the one with lowest factor
  
  
  # Choose the closest distribution for NA points
  if("SCINAME" %not in% names(pts)){pts$SCINAME<-NA}
  dist.SF<-st_distance(pts[is.na(pts$SCINAME)==T,], Distrib)
  v<-which(is.na(pts$SCINAME)==T)
  for(n.pt in 1:nrow(dist.SF)){
    NUM<-v[n.pt] # I have only NAs in dist.SF but all points in pts so I need to different indexes
    pts$SCINAME[NUM]<-as.character(Distrib$SCINAME[which(dist.SF[n.pt,]==min(dist.SF[n.pt,]))])
  }
  
  # Add information in track
  track$Names_Birdlife<-as.character(track$Names_Birdlife)
  pts$SCINAME<-as.factor(pts$SCINAME)
  track$Names_Birdlife[i]<-paste(sub(" ", ".", levels(pts$SCINAME)), collapse=" " )
  
  
  # Apply changes in obs
  nb_spc<-nlevels(droplevels(pts$SCINAME)) ; track$nb_spc[i]<-nb_spc
  obs.lumped$sci_split[obs.lumped$sci_modified == Name_eBird]<-as.character(pts$SCINAME)[match(obs.lumped$Site[obs.lumped$sci_modified == Name_eBird], pts$Site)]
  
  
  ## Make a plot to check 
  G_obs<-ggplot()+
    geom_sf(data=Distrib, aes(col=droplevels(SCINAME)), alpha=0.7)+
    xlim(extent(pts)[1:2])+ylim(extent(pts)[3:4])+
    geom_sf(data=pts, aes(col=droplevels(SCINAME)))+    
    scale_colour_discrete(drop = FALSE, name="Split name")+scale_fill_discrete(drop = FALSE)
  

  # Save the plot
  gridExtra ::grid.arrange(G_distr, G_distr2, G_overlap, G_obs, top=paste0("Distribution of ", Name_eBird, " (eBird name)"), layout_matrix=matrix(c(1,2,3, 4,4,4, 4,4,4), byrow=T, ncol=3))
  
  if(i/5==round(i/5)){cat(paste0(i, " ")) ; beep(10)}


}

dev.off()





########################
### SAVE THE NEW OBS ###
########################
obs.lumped$sci_split[is.na(obs.lumped$sci_split)==TRUE]<-obs.lumped$sci_modified[is.na(obs.lumped$sci_split)==TRUE]

obs.lumped$sci_split<-replace(obs.lumped$sci_split, obs.lumped$sci_split=="Calonectris.diomedea", "Calonectris.borealis") # Overlap so I chose one randomly (see file 1-73)
obs.lumped$sci_split<-replace(obs.lumped$sci_split, obs.lumped$sci_split=="Diomedea.sanfordi", "Diomedea.epomophora") # Complete overlap so I chose one randomly (see file 1-73)
obs.lumped$sci_split<-replace(obs.lumped$sci_split, obs.lumped$sci_split=="Trogon.violaceous", "Trogon.violaceus") # Mistake in eBird records?
obs.lumped$sci_split<-replace(obs.lumped$sci_split, obs.lumped$sci_split=="Leucocarbo.bougainvilliorum", "Phalacrocorax.bougainvilliorum") # May have changed since 2017 (careful with the new data)
obs.lumped<-subset(obs.lumped, obs.lumped$sci_split != "Scytalopus.sp.")


saveRDS(obs.lumped, paste0(GBrow, "1.Tables/Obs.Script.06.taxo.rds"))



write.csv(track, paste("D:/eBird/HFI.project/Figures/Check.figures/Taxonomy/Track.Taxonomy.split.csv"), row.names = FALSE)




### Check that all names of obs are included in Birdlife names
cat("\n", "\n", "Should be 100% TRUE", "\n")
table(obs.lumped$sci_split %in% gsub(" ", ".", iucn$Scientific.name))
levels(as.factor(obs.lumped$sci_split))[levels(as.factor(obs.lumped$sci_split)) %not in% iucn$Scientific.name]








