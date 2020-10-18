#long format for combined model
alldata<-alldata[!is.na(alldata$pid),]
alldata$order<-ave(alldata$Age,alldata$pid,FUN=function(x) rank(x,ties.method="random"))
alldataL<-reshape(alldata[,c("DepZ","ConfZ","LabZ","OthPZ","logCSGZ","sysBPZ","diasBPZ","SaludZ","SumDiagZ","pid","HHId","ComunID","male","AgeZ","GiniZ","HHWealthZ","HHWealthZ.vil","MeanWealthZ","DistZ","SizeZ","order")],varying=c("DepZ","ConfZ","LabZ","OthPZ","logCSGZ","sysBPZ","diasBPZ","SaludZ","SumDiagZ"),v.names="Outcome",idvar=c("pid","order"),times=c("DepZ","ConfZ","LabZ","OthPZ","logCSGZ","sysBPZ","diasBPZ","SaludZ","SumDiagZ"),direction="long")

alldataL<-alldataL[!is.na(alldataL$Outcome),]

combined<- brm(Outcome~GiniZ+male+HHWealthZ.vil+AgeZ+MeanWealthZ+DistZ+SizeZ+(1|ComunID)+(1|HHId)+(1|pid)+(1|time),data=alldataL, family=gaussian(), prior = prior, chains = 3, cores = 3, iter = 5000, warmup = 1000, thin=5, control = list(adapt_delta = 0.99))
summary(combined)