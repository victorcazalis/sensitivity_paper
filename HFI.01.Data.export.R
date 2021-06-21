
setwd(paste0(GBrow, "0.Data/0.Raw.data/", QQ))

library(auk) ; library(plyr) ; library(readr) ; library(nlme)
`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_)) 



###################
### Charge data ###
###################
files.countries<-list.files(getwd())


###############################################
### EXPORT WITH EXPERTISE RESTRICTIONS ONLY ###
###############################################

### Loop for all countries
for (i in 1:length(files.countries)){
  
  eb<-read_ebd(files.countries[i], rollup=T, unique=F) # I want to rollup but not to remove double counts

  
  ### Reduce dataset
  # Remove useless columns
  eb[,c("state_province", "subnational1_code","subspecies_scientific_name","state","last_edited_date", "subspecies_common_name", "breeding_bird_atlas_category", "effort_area_ha", "county", "subnational2_code", "iba_code", "bcr_code", "usfws_code", "atlas_block", "locality", "locality_id", "locality_type", "first_name", "last_name", "has_media", "reviewed", "x")]<-NULL
  # Remove disapproved observations
  eb<-subset(eb, eb$approved==T)
  # Remove checklist that did not mention every observation
  eb<-subset(eb, eb$all_species_reported==T)
  # Remove domestic species (only Columba livia) 
  eb<-subset(eb, eb$category != "domestic")
  # Remove old observation 
  eb$year<-as.numeric(format(eb$observation_date, "%Y"))
  eb<-subset(eb, eb$year>=2010)
  
  ### Format dates
  eb$year<-as.numeric(format(eb$observation_date, "%Y"))
  eb$month<-as.numeric(format(eb$observation_date, "%m"))
  eb$day<-as.numeric(format(eb$observation_date, "%j"))
  
  
  ### Assign if I can keep the checklist for abundance data (i.e. protocol I like with duration information, distance if Traveling or historical protocols, duplicates removed by auk)
  
  ### eb2 will be used for abundance/occurence analyses: use sampling_event for expertise and checklist_id for abundance/occurence analyses
  eb2<-auk_unique(eb)
  eb2<-subset(eb2, eb2$protocol_type %in% c("Traveling","Stationary","Historical") & is.na(eb2$duration_minutes)==FALSE)
  eb2<-subset(eb2, eb2$protocol_type=="Stationary" | is.na(eb2$effort_distance_km)==FALSE)
  
  
  # Provide name files that is different for CA and US
  if(substr(files.countries[i], nchar(files.countries[i])-20, nchar(files.countries[i])-19)=="US"){NAME<-paste0(substr(files.countries[i], nchar(files.countries[i])-17, nchar(files.countries[i])-16), "_US")}
  if(substr(files.countries[i], nchar(files.countries[i])-20, nchar(files.countries[i])-19)=="CA"){NAME<-paste0(substr(files.countries[i], nchar(files.countries[i])-17, nchar(files.countries[i])-16), "_CA")}
  if(substr(files.countries[i], nchar(files.countries[i])-20, nchar(files.countries[i])-19) %not in% c("CA", "US")){NAME<-substr(files.countries[i], nchar(files.countries[i])-17, nchar(files.countries[i])-16)}
  
  saveRDS(eb, file=paste0(GBrow, "0.Data/1.Exported4Expertise/", QQ, "/Exported4Expertise.", NAME, ".rds"))
  saveRDS(eb2, file=paste0(GBrow, "0.Data/1.Exported4Analyses/", QQ, "/Exported4Analyses.", NAME, ".rds"))
  
  cat(i)
  
}




#####################
### MERGING FILES ###
#####################

### Merge for expertise in script 02

### Merge files for analyses
setwd(paste0(GBrow, "0.Data/1.Exported4Analyses/", QQ))

df<-readRDS(list.files()[1]) # Stock the first data frame in df

for(i in 2:length(list.files())){
  df<-rbind(df, readRDS(list.files()[i])) # Add every table in df
}


library(data.table)
fwrite(df, paste0(GBrow, "0.Data/1.Exported4Analyses/0.Exported.eBird.dataTABLE.HFI.", QQ, ".csv"), row.names=FALSE)








