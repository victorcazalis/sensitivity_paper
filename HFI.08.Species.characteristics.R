library(dplyr) ; library(mgcv) ; library(gtools) ; library(sf) ; library(data.table) ; library(raster) ; library(plyr)
GBrow<-"D:/eBird/HFI.project/Analyses/Birds/"
`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_)) 


obs1<-readRDS(paste0(GBrow, "1.Tables/Obs.0filled.07.AC.rds"))
obs2<-readRDS(paste0(GBrow, "1.Tables/Obs.0filled.07.DE.rds")) ; obs2$Site<-NULL
obs3<-readRDS(paste0(GBrow, "1.Tables/Obs.0filled.07.FK.rds")) ; obs3$Site<-NULL
obs4<-readRDS(paste0(GBrow, "1.Tables/Obs.0filled.07.LN.rds"))
obs5<-readRDS(paste0(GBrow, "1.Tables/Obs.0filled.07.OQ.rds"))
obs6<-readRDS(paste0(GBrow, "1.Tables/Obs.0filled.07.RS.rds"))
obs7<-readRDS(paste0(GBrow, "1.Tables/Obs.0filled.07.TZ.rds"))
obs8<-readRDS(paste0(GBrow, "1.Tables/Obs.0filled.07.Manual.adds.rds"))

obs<-as.data.table(rbind(obs1, obs2, obs3, obs4, obs5, obs6, obs7, obs8))




###############################
### Species list with N.obs ###
###############################
obs$X.counts<-as.numeric(revalue(as.character(obs$Ab=="X"), c("TRUE"="1", "FALSE"="0")))
species<-obs[ , .(N.points=length(Ab), N.obs=length(Ab[Ab>0]), N.X=sum(X.counts)), by=.(species)]
species$prop.X<-species$N.X/species$N.obs


#########################
### Charge Cagan data ###
#########################
char<-read.csv(paste0(GBrow, "1.Tables/Species characteristics data/2020.Species.characteristics.Cagan.csv"), sep=";")
char$species<-replace(as.character(char$Latin), char$Victor.Latin!="", as.character(char$Victor.Latin[char$Victor.Latin!=""])) %>%
  sub(" ", ".", .)


### Calculate habitat specialisation
char$specialisation<-log10(100/(char$HB * char$DB))


### Transform migration
char$Mig[is.na(char$Mig)]<-0
char$Mig<-revalue(as.character(char$Mig), c("1"="Strict", "2"="Partial", "0"="Sedentary"))


### Add information from species table (and fill when 0: species that breeds in the Americas but not in my data)
char$N.points<-species$N.points[match(char$species, species$species)]
char$N.obs<-species$N.obs[match(char$species, species$species)]
char$N.obs[is.na(char$N.obs)]<-0
char$prop.X<-species$prop.X[match(char$species, species$species)]

table(char$N.obs==0)
rm(obs1, obs2, obs3, obs4, obs5, obs6, obs7, obs8, species) ; gc()


### Simplify diet
char$Primary.Diet<-plyr::revalue(char$Primary.Diet, c("invertebrate"="Invertebrate", "Vertebrate"="Carnivore", "Scavenge"="Carnivore", "Herbivore"="Plant"))



#######################
### Red List status ###
#######################
iucn<-read.csv("D:/eBird/HFI.project/Analyses/Birds/1.Tables/Species characteristics data/IUCN Red List 23.01.20.csv", sep=";")  

char$IUCN<-iucn[match(char$Latin, iucn$Scientific.name), "Red.List"]
char$IUCN<-factor(droplevels(char$IUCN), c("CR", "EN", "VU", "NT", "LC", "DD"))

table(is.na(char$IUCN), char$IUCN)



#############################
### Range size + LATITUDE ###
#############################

# Charge distributions if not done yet
setwd("D:/PA GIS/Bird.Distributions.Jan2020/BOTW.gdb")
if("distributions" %not in% ls()){distributions <- st_read(dsn = "a00000009.gdbtable")} # Don't charge if already charged

table(char$Latin %in% distributions$SCINAME)



### Subset distributions
distributions<-subset(distributions, 
                        distributions$PRESENCE %not in% c(4,5) & # Remove extinct and possibly extinct
                        distributions$SEASONAL %in% c(1,2)) # Remove non-breeding grounds (keep only resident and breeding)

# Charge ecoregions to restrict to Americas range
ecoregions<-st_transform(read_sf("D:/PA GIS/Ecoregions/ecoregions.Americas.withbiomes.shp"), st_crs(distributions))

char$range.km2<-char$range.invasive.km2<-char$Lat.centr<-char$Lim.North<-char$Lim.South<-NA

for(i in 1:nrow(char)){
  #tryCatch({
  sp<-subset(distributions, distributions$SCINAME==sub("[.]", " ", as.character(char$species[i])))
  if(NA %in% st_is_valid(sp)){st_write(sp, "D:/Shape.to.del.shp", delete_dsn=TRUE) ; sp<-st_read("D:/Shape.to.del.shp")}
  if(FALSE %in% st_is_valid(sp)){sp<-st_buffer(sp, 0)} # Correct some bugs in the distributions
  sp<-st_intersection(sp, ecoregions)
  sp<-st_transform(sp, st_crs("+proj=moll +lon_0=-80 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))
  char$Lim.North[i]<-extent(sp)[4]
  char$Lim.South[i]<-extent(sp)[3]
  char$range.invasive.km2[i]<-sum(st_area(st_combine(sp[sp$ORIGIN==3,])))/1000000
  sp2<-st_combine(sp)
  char$range.km2[i]<-sum(st_area(sp2))/1000000
  char$Lat.centr[i]<-st_coordinates(st_centroid(sp2))[2]

#} ,error=function(e){cat(paste0(i, " ")) ; species$Lat.centr[i]<-NA})
  if((i/50)==round(i/50)){cat(i)}
}


#################
### FILL GAPS ###
#################

### Calculate stats per family
char.fam<-ddply(char, .(Family), function(x){data.frame(
  Mean_mass=mean(x$Mass, na.rm=T),
  Median_specialisation=mean(x$specialisation, na.rm=T)
)})

### Fill Mass
char$Mass[is.na(char$Mass)==T]<-char.fam$Mean_mass[match(char$Family[is.na(char$Mass)==T], char.fam$Family)]

### Fill specialisation
char$specialisation[is.na(char$specialisation)==T]<-char.fam$Median_specialisation[match(char$Family[is.na(char$specialisation)==T], char.fam$Family)]

char$Primary.Diet[char$Primary.Diet=="Unknown"]<-"Invertebrate"


####################################
### SAVE SPECIES CHARACTERISTICS ###
####################################
saveRDS(char, paste0(GBrow, "1.Tables/Species.characteristics.rds"))

