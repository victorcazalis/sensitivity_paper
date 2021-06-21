## Charge

chlist1<-readRDS(paste0(GBrow, "1.Tables/Chlist.Script.04.Q1.rds"))
chlist2<-readRDS(paste0(GBrow, "1.Tables/Chlist.Script.04.Q2.rds"))
chlist3<-readRDS(paste0(GBrow, "1.Tables/Chlist.Script.04.Q3.rds"))
chlist4<-readRDS(paste0(GBrow, "1.Tables/Chlist.Script.04.Q4.rds")) ; chlist4$Cat_obs<-NULL
chlist5<-readRDS(paste0(GBrow, "1.Tables/Chlist.Script.04.Q5.rds")) ; chlist5$Cat_obs<-NULL
chlist6<-readRDS(paste0(GBrow, "1.Tables/Chlist.Script.04.Q6.rds")) ; chlist6$Cat_obs<-NULL
chlist7<-readRDS(paste0(GBrow, "1.Tables/Chlist.Script.04.Q7.rds")) ; chlist7$Cat_obs<-NULL

obs1<-readRDS(paste0(GBrow, "1.Tables/Obs.Script03.Q1.DataTable.rds")) ; obs1<-subset(obs1, obs1$checklist_id %in% chlist1$Liste)
obs2<-readRDS(paste0(GBrow, "1.Tables/Obs.Script03.Q2.DataTable.rds")) ; obs2<-subset(obs2, obs2$checklist_id %in% chlist2$Liste)
obs3<-readRDS(paste0(GBrow, "1.Tables/Obs.Script03.Q3.DataTable.rds")) ; obs3<-subset(obs3, obs3$checklist_id %in% chlist3$Liste)
obs4<-readRDS(paste0(GBrow, "1.Tables/Obs.Script03.Q4.DataTable.rds")) ; obs4<-subset(obs4, obs4$checklist_id %in% chlist4$Liste)
obs5<-readRDS(paste0(GBrow, "1.Tables/Obs.Script03.Q5.DataTable.rds")) ; obs5<-subset(obs5, obs5$checklist_id %in% chlist5$Liste)
obs6<-readRDS(paste0(GBrow, "1.Tables/Obs.Script03.Q6.DataTable.rds")) ; obs6<-subset(obs6, obs6$checklist_id %in% chlist6$Liste)
obs7<-readRDS(paste0(GBrow, "1.Tables/Obs.Script03.Q7.DataTable.rds")) ; obs7<-subset(obs7, obs7$checklist_id %in% chlist7$Liste)

# Merge chlist
chlist<-rbind(chlist1, chlist2, chlist3, chlist4, chlist5, chlist6, chlist7)


# Merge observations
obs<-rbind(obs1, obs2, obs3, obs4, obs5, obs6, obs7)



# Save
saveRDS(chlist, paste0(GBrow, "1.Tables/Chlist.Script.05.rds"))
saveRDS(obs, paste0(GBrow, "1.Tables/Obs.Script.05.rds"))


