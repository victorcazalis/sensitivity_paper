library(data.table) ; library(reshape)


eb.raw<-fread(paste0(GBrow, "0.Data/1.Exported4Analyses/0.Exported.eBird.dataTABLE.HFI.", QQ, ".csv"))



##########################################################
### Reduce dataset for good quality checklists (quick) ###
##########################################################

# Supprimer les observations sur moins de 30 minutes : aucune donnee sans duree
eb<-eb.raw[duration_minutes>30 & duration_minutes<600]
# Supprimer les Traveling avec plus de 5km : les donnees sans distance (ie protocol stationary) sont conservees
eb<-eb[is.na(eb$effort_distance_km)==TRUE | eb$effort_distance_km<=5]


############################################
### Observation table (keeping X values) ###
############################################
eb$Ab<-as.numeric(replace(eb$observation_count, eb$observation_count=="X", 10000000000))
obs<-eb[, # Subset
                .(Ab=sum(Ab)), #Calculation
                by=.(checklist_id, scientific_name)]# How to group data

obs$Ab<-replace(obs$Ab, obs$Ab>100000000, "X")


### Create chlist table
chlist<-eb[, # Subset
           .(duration=mean(duration_minutes), distance=mean(effort_distance_km), year=mean(year), day=mean(day), lat=mean(latitude), lon=mean(longitude), protocol=protocol_type[1], Observers=paste(unique(unlist(strsplit(observer_id, ","))), collapse=";"), Country=country_code[1]), #Calculation
           by=.(checklist_id)]# How to group data
colnames(chlist)[1]<-"Liste"



# Clean
rm(eb, eb.raw)


### Assign expertise of the more skilled observer + recalculate number of observers (because calculated per observations usually, I calculate it per checklist)
obsqual<-readRDS(paste0(GBrow, "1.Tables/Expertise.scores.table.rds"))
chlist$obsKelling<-chlist$N_obs<-NA
chlist$Cat_obs<-substr(chlist$Liste, 1,1)

### Easy for those with one observer
chlist$obsKelling<-obsqual$obsKelling[match(chlist$Observers, obsqual$observer_id)]
chlist$N_obs[chlist$Cat_obs=="S"]<-1

### Loop for those with several observers
cat(paste0("Total to do: ", table(chlist$Cat_obs)["G"]))
QUANT<-round(quantile(which(chlist$Cat_obs=="G"), seq(0.1, 0.9, 0.05)))
for(i in which(chlist$Cat_obs=="G")){
  OBS<-unique(unlist(strsplit(as.character(chlist$Observers[i]), ";")))
  chlist$obsKelling[i]<-max(obsqual$obsKelling[match(OBS, obsqual$observer_id)], na.rm=TRUE)
  chlist$N_obs[i]<-length(OBS)
  if(i %in% QUANT){cat(paste0(i, " ")) ; beep(10)}
}
chlist<-subset(chlist, chlist$obsKelling!= "-Inf" & is.na(chlist$obsKelling)==F)



###################################################
### Delete checklist too close the same day ~1h ###
###################################################
# Load the function
library(sf)

chlist$suppr<-NA
chlist$dates<-paste(chlist$day, chlist$year)
Dates<-levels(as.factor(chlist$dates))

cat(paste0("Total to do: ", length(Dates)))
for(i in 1:length(Dates)){  
  x<-subset(chlist, chlist$dates==Dates[i]) # Subset per date
  
  # COnvert to sf object
  coords_sf <- st_as_sf(data.frame(x[,c("lon", "lat")]), coords = c("lon", "lat")) ;   st_crs(coords_sf) <- 4326
  # Apply the st_distance function
  Dist <- st_distance(coords_sf)/1000
  # Remove diagonal values and upper matrix
  diag(Dist)<-NA
  Dist[upper.tri(Dist)]<-NA
  Dist<-matrix(as.numeric(Dist), nrow=nrow(Dist), byrow=T)
  
  if(min(Dist, na.rm=T)<5){ # if there are some distances <5km
    
    couples<-as.data.frame(which(Dist < 5, arr.ind=T)) # Show me the pairs closer to 5km
    
    couples$random<-sample(2, nrow(couples), T)
    couples$list_S<-ifelse(couples$random==1, couples$row, couples$col)
    x<-x[sort(unique(couples$list_S)),]
    chlist$suppr[chlist$Liste %in% x$Liste]<-"S"
  }
  if(round(i/100)==i/100){ cat(i, "  ")} # Tell me where you are
}


chlist<-subset(chlist, is.na(chlist$suppr)==T) # Keep only the NA (i.e., the one without "S")
chlist$suppr<-chlist$dates<-NULL

# ### Check that the suppression of checklist the same day at less that 2km has worked
# source("D:/eBird/Data/Expand.Analyses.tables/global/Function.internet.distance.matrix.R")
# ply<-ddply(chlist, .(paste(chlist$year, chlist$day)), function(x){
# 
#   if(nrow(x)==1){Dist<-9999999}else{ # Si il n'y a qu'une ligne : pas de matrice de distance
#     Dist<-round(GeoDistanceInMetresMatrix(x)/1000)
#     diag(Dist)<-NA}
# 
#   data.frame(
#     year=x$year[1],
#     day=x$day[1],
#     Nb_obs=nrow(x),
#     Dist_min=min(Dist, na.rm=T)
#   )
# })
# cat("Should be 100% FALSE")
# table(ply$Dist_min<5)



###################
### Save tables ###
###################
obs<-subset(obs, obs$checklist_id %in% chlist$Liste)
saveRDS(obs, paste0(GBrow, "1.Tables/Obs.Script03.", QQ, ".DataTable.rds"))
saveRDS(chlist, paste0(GBrow, "1.Tables/Chlist.Script03.", QQ, ".DataTable.rds"))


