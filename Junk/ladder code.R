# ### validate HH wealth Z with subjective status ladder and within-village rank ###
# {
# # import ladder data
# library(RODBC)
# root<-"\\\\files.iem.uzh.ch\\Data\\Institute\\Human_Ecology\\ajaegg\\Private\\My Documents\\UCSB\\Data\\"
# shocks<-odbcDriverConnect(paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",root,"Dificultades_V14_MASTER_AJ_9-21-2014.mdb",sep="")) ## opens connection to database
# ladder<- sqlQuery(shocks, "SELECT * FROM Shocks_Escalera",stringsAsFactors=FALSE) ## use * to select all columns, column names separated by space otherwise
# str(ladder)
# ladder<- ladder[,c("MidPID", "Escalera1Wealth", "Escalera3Status")]
# pid<- sqlQuery(shocks, "SELECT * FROM Shocks_Entrevista",stringsAsFactors=FALSE) ## use * to select all columns, column names separated by space otherwise
# str(pid)
# pid<- pid[,c("MidPID", "Nombre", "Apellido1", "Apellido2")]
# ladder.pid<- merge(ladder, pid, all.x=TRUE)
# str(ladder.pid)
# odbcClose(shocks) ## close connection to save working memory
# 
# # wealth and status hierarchy correlated?
# plot(Escalera1Wealth~Escalera3Status, ladder.pid)
# cor.test(ladder.pid$Escalera1Wealth,ladder.pid$Escalera3Status) # cor=0.44, P=1.507e-11
# 
# # sync PIDs?
# {
# ladder.pid$ActiveMidPID<- ladder.pid$MidPID
# get.csv <- function(target=NA) read.csv(ifelse(is.na(target), file.choose(), target), colClasses="character")
# setwd("D:/Dropbox/PID cleaning")
# reg <- get.csv("population_register_2Feb2014.csv")
# cdm <- get.csv("cambios_de_midpid_2Feb2014.csv")
# 
# ## step 2a: manually change same-name, same-pid collisions
# # (refresh with:)
# key <- paste(cdm$old.id, cdm$nombre, sep="-")
# unique(key[duplicated(key)])
# samename.collisions <- unique(cdm$old.id[key %in% key[duplicated(key)]])
# ladder.pid$ActiveMidPID[which(ladder.pid$ActiveMidPID %in% samename.collisions)]
# 
# # step 2b: auto apply based on exact name matches
# # extract first name of string
# # ladder.pid$Nombre<- ladder.pid$NombreHHHead
# # ladder.pid$Nombre <- sub(" .*","", ladder.pid$Nombre)
# 
# cdm$key <- paste(cdm$old.id, cdm$nombre, sep="-")
# ladder.pid$key <- paste(ladder.pid$ActiveMidPID, ladder.pid$Nombre, sep="-")
# length(cdm$active.id[match(ladder.pid$key[ladder.pid$key %in% cdm$key], cdm$key)])
# ladder.pid$ActiveMidPID[ladder.pid$key %in% cdm$key] <- cdm$active.id[match(ladder.pid$key[ladder.pid$key %in% cdm$key], cdm$key)]
# # 28 updated! 
# 
# cdm <- cdm[,-which(colnames(cdm)=="key")]
# ladder.pid <- ladder.pid[,-which(colnames(ladder.pid)=="key")]
# 
# # step 2c: everyone left
# check <- ladder.pid$ActiveMidPID[which(ladder.pid$ActiveMidPID %in% cdm$old.id)]; length(check)  # 0 left
# 
# i <- 1
# # this code is designed to be copy-pasted over and over to inspect who is being updated
# ladder.pid[which(ladder.pid$ActiveMidPID==check[i]),]
# cdm[cdm$old.id==check[i],]
# i <- i + 1
# 
# ladder.pid$ActiveMidPID[which(ladder.pid$ActiveMidPID==249032)] <- 19900017
# ladder.pid$ActiveMidPID[which(ladder.pid$ActiveMidPID==56047)] <- "58B4"
# ladder.pid$ActiveMidPID[which(ladder.pid$ActiveMidPID==81192)] <- "8KJA"
# 
# # ok, everyone else can be auto-replaced
# 
# length(cdm$active.id[match(ladder.pid$ActiveMidPID[which(ladder.pid$ActiveMidPID %in% cdm$old.id)], cdm$old.id)])
# ladder.pid$ActiveMidPID[which(ladder.pid$ActiveMidPID %in% cdm$old.id)] <- cdm$active.id[match(ladder.pid$ActiveMidPID[which(ladder.pid$ActiveMidPID %in% cdm$old.id)], cdm$old.id)]
# 
# # step 3: anyone not in the register?
# 
# ladder.pid$ActiveMidPID[which(!ladder.pid$ActiveMidPID %in% reg$pid)]  
# # [1] "202113" "292049" "293146" "40315"  "9 X4S"  "VGZE"
# # deal with those later
# }
# 
# 
# # which HH wealth Z best? community, region, whole population
# colnames(ladder.pid)[7]<- "pid"
# ladder.wealth<- merge(ladder.pid, Wealthdata2, all.x=TRUE, by="pid")
# str(ladder.wealth)
# summary(ladder.wealth$HHWealthZ) # 125 NA's...
# 
# cor.test(ladder.wealth$Escalera1Wealth,ladder.wealth$HHWealthZ) # cor=0.29, P=0.0003089
# cor.test(ladder.wealth$Escalera3Status,ladder.wealth$HHWealthZ) # cor=0.17, P=0.03487
# 
# # low correlations with overall wealth Z, what about community level?
# cor.test(ladder.wealth$Escalera1Wealth,ladder.wealth$VillRankAgeCor) # cor=0.21, P=0.008215
# cor.test(ladder.wealth$Escalera3Status,ladder.wealth$VillRankAgeCor) # cor=0.06, P=0.471
# 
# # correlation overall Z and village rank
# cor.test(Wealthdata2$HHWealthZ,Wealthdata2$VillRankAgeCor) # cor=0.8563624, P< 2.2e-16
# }
# # this code doesn't actually work anymore, because I can't get the connection to the access files to work anymore - i think the RODBC package has changed in the meantime
# # i might be able to find the processed object saved somewhere though, i.e. ladder.wealth -> could just provide that? this is anyway tangential to the paper
# 
# 