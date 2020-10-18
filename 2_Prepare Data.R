# 		## prepare data ##
# 		##################

inddata<-read.csv("IndividualData.csv")
hhdata<-read.csv("HHWealthWGini.csv")
alldata<-merge(inddata,hhdata,by=c("HHId","Round"))

alldata$AgeZ<- scale(alldata$Age)
alldata$DistZ<- scale(alldata$route.distance.SB)
alldata$SizeZ<- scale(alldata$ComunSize)
alldata$HHSizeZ<- scale(alldata$HHSize)
alldata$GiniZ<- scale(alldata$Gini.AgeCor)
#in original data 1=excellent, 5 =bad, so leave as is so higher is worse
alldata$SaludZ<- scale(alldata$SaludGeneral)
alldata$DepZ<- scale(alldata$DepressionScore)
alldata$ConfZ<- scale(alldata$Conflicts)
#Reverse Code labor partners, so higher is worse
alldata$LabZ<- scale(alldata$LaborPartners) * -1
alldata$OtherProblems<-alldata$SumSystemic-alldata$Conflicts	
alldata$OthPZ<- scale(alldata$OtherProblems)
alldata$logCSGZ<- scale(alldata$logCSG)
alldata$sysBPZ<- scale(alldata$sysBP)
alldata$diasBPZ<- scale(alldata$diasBP)
alldata$SumDiagZ<- scale(alldata$sum.diag)

## center HHWealthZ at village level
alldata$HHWealthZ.vil<- alldata$HHWealthZ-alldata$MeanWealthZ

#just adults >15
alldata2<-alldata[alldata$Age>15,]
write.csv(alldata2,file="alldataAdults.csv",row.names=FALSE)

#just kids <=15
alldata3<-alldata[alldata$Age<=15,]
write.csv(alldata3,file="alldataKids.csv",row.names=FALSE)

