library(data.table) ; library(reshape)
GBrow<-"D:/eBird/HFI.project/Analyses/Birds/"


### Charge obs
obs<-readRDS(paste0(GBrow, "1.Tables/Obs.Script.06.taxo.rds"))
obs$observation<-paste(obs$checklist_id, obs$scientific_name, sep="/")


### Charge and subset raw data
eb.raw1<-fread(paste0(GBrow, "0.Data/1.Exported4Analyses/0.Exported.eBird.dataTABLE.HFI.Q1.csv"))
eb.raw1<-eb.raw1[,c("checklist_id", "scientific_name", "breeding_bird_atlas_code")]
eb.raw1$observation<-paste(eb.raw1$checklist_id, sub(" ", ".", eb.raw1$scientific_name), sep="/")
eb.raw1b<-subset(eb.raw1, eb.raw1$observation %in% obs$observation)
rm(eb.raw1)

eb.raw2<-fread(paste0(GBrow, "0.Data/1.Exported4Analyses/0.Exported.eBird.dataTABLE.HFI.Q2.csv"))
eb.raw2<-eb.raw2[,c("checklist_id", "scientific_name", "breeding_bird_atlas_code")]
eb.raw2$observation<-paste(eb.raw2$checklist_id, sub(" ", ".", eb.raw2$scientific_name), sep="/")
eb.raw2b<-subset(eb.raw2, eb.raw2$observation %in% obs$observation)
rm(eb.raw2)

eb.raw3<-fread(paste0(GBrow, "0.Data/1.Exported4Analyses/0.Exported.eBird.dataTABLE.HFI.Q3.csv"))
eb.raw3<-eb.raw3[,c("checklist_id", "scientific_name", "breeding_bird_atlas_code")]
eb.raw3$observation<-paste(eb.raw3$checklist_id, sub(" ", ".", eb.raw3$scientific_name), sep="/")
eb.raw3b<-subset(eb.raw3, eb.raw3$observation %in% obs$observation)
rm(eb.raw3)

eb.raw4<-fread(paste0(GBrow, "0.Data/1.Exported4Analyses/0.Exported.eBird.dataTABLE.HFI.Q4.csv"))
eb.raw4<-eb.raw4[,c("checklist_id", "scientific_name", "breeding_bird_atlas_code")]
eb.raw4$observation<-paste(eb.raw4$checklist_id, sub(" ", ".", eb.raw4$scientific_name), sep="/")
eb.raw4b<-subset(eb.raw4, eb.raw4$observation %in% obs$observation)
rm(eb.raw4)

eb.raw5<-fread(paste0(GBrow, "0.Data/1.Exported4Analyses/0.Exported.eBird.dataTABLE.HFI.Q5.csv"))
eb.raw5<-eb.raw5[,c("checklist_id", "scientific_name", "breeding_bird_atlas_code")]
eb.raw5$observation<-paste(eb.raw5$checklist_id, sub(" ", ".", eb.raw5$scientific_name), sep="/")
eb.raw5b<-subset(eb.raw5, eb.raw5$observation %in% obs$observation)
rm(eb.raw5)

eb.raw6<-fread(paste0(GBrow, "0.Data/1.Exported4Analyses/0.Exported.eBird.dataTABLE.HFI.Q6.csv"))
eb.raw6<-eb.raw6[,c("checklist_id", "scientific_name", "breeding_bird_atlas_code")]
eb.raw6$observation<-paste(eb.raw6$checklist_id, sub(" ", ".", eb.raw6$scientific_name), sep="/")
eb.raw6b<-subset(eb.raw6, eb.raw6$observation %in% obs$observation)
rm(eb.raw6)

eb.raw7<-fread(paste0(GBrow, "0.Data/1.Exported4Analyses/0.Exported.eBird.dataTABLE.HFI.Q7.csv"))
eb.raw7<-eb.raw7[,c("checklist_id", "scientific_name", "breeding_bird_atlas_code")]
eb.raw7$observation<-paste(eb.raw7$checklist_id, sub(" ", ".", eb.raw7$scientific_name), sep="/")
eb.raw7b<-subset(eb.raw7, eb.raw7$observation %in% obs$observation)
rm(eb.raw7)


### Match with obs
obs$code<-NA
obs$code[obs$observation %in% eb.raw1b$observation]<-eb.raw1b$breeding_bird_atlas_code[match(obs$observation[obs$observation %in% eb.raw1b$observation], eb.raw1b$observation)]
obs$code[obs$observation %in% eb.raw2b$observation]<-eb.raw2b$breeding_bird_atlas_code[match(obs$observation[obs$observation %in% eb.raw2b$observation], eb.raw2b$observation)]
obs$code[obs$observation %in% eb.raw3b$observation]<-eb.raw3b$breeding_bird_atlas_code[match(obs$observation[obs$observation %in% eb.raw3b$observation], eb.raw3b$observation)]
obs$code[obs$observation %in% eb.raw4b$observation]<-eb.raw4b$breeding_bird_atlas_code[match(obs$observation[obs$observation %in% eb.raw4b$observation], eb.raw4b$observation)]
obs$code[obs$observation %in% eb.raw5b$observation]<-eb.raw5b$breeding_bird_atlas_code[match(obs$observation[obs$observation %in% eb.raw5b$observation], eb.raw5b$observation)]
obs$code[obs$observation %in% eb.raw6b$observation]<-eb.raw6b$breeding_bird_atlas_code[match(obs$observation[obs$observation %in% eb.raw6b$observation], eb.raw6b$observation)]
obs$code[obs$observation %in% eb.raw7b$observation]<-eb.raw7b$breeding_bird_atlas_code[match(obs$observation[obs$observation %in% eb.raw7b$observation], eb.raw7b$observation)]

obs$code<-replace(obs$code, obs$code %in% c("F", "H", ""), NA)

obs$sci_modified<-obs$scientific_name<-obs$observation<-obs$Site<-obs$N_Lump<-NULL


saveRDS(obs, paste0(GBrow, "1.Tables/Obs.Script.08.Breeding.rds"))

