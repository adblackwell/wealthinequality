library(gamlss)
library(ineq)

Wealthdata<-read.csv("HHWealth.csv",stringsAsFactors = FALSE,header=TRUE)

getLMS<-function(M1,newdat){
  m<-predict(M1, what = "mu", newdata =newdat)
  s<-predict(M1, what = "sigma", newdata =newdat)
  n<-try(predict(M1, what = "nu", newdata =newdat))
  if (inherits(n, "try-error")) n<-rep(1,nrow(newdat))
  LMS<-data.frame(Mu=m,Sigma=s,Lambda=n)
  LMS$Sigma<-exp(LMS$Sigma)
  LMS
}


#This function calculates a z-score from a measure and the appropriate LMS values. 
ZfromLMS<-function(measure,la,mu,si){
  zs <- ((measure / mu)^la - 1)/(la * si)
  zs
}

getZ<-function(M1,newdat,yval){
  lms1<-getLMS(M1,newdat)
  Zs<-ZfromLMS(yval,lms1$Lambda,lms1$Mu,lms1$Sigma)
  Zs
}

ZtoVal50<-function(M1,zs){
  LMS<-unlist(getLMS(M1,data.frame(HHAge=50,Round=2))[c("Lambda","Mu","Sigma")])
  o<-as.vector(sapply(zs,function(x) exp(log(x*LMS[1]*LMS[3]+1)/LMS[1]+log(LMS[2])),simplify=TRUE))
  o
}


#Wealth Z-Scores by Age, 
wm<-gamlss(log(Total.value)~pb(HHAge)+Round, sigma.formula=~pb(HHAge)+Round, nu.fo=~pb(HHAge)+Round, family=BCT, data=Wealthdata[,c("Total.value","HHmale","HHAge","Round")],control=gamlss.control(n.cyc = 200))

#plot of centiles. Apparent Jitter on plot is due to the effect of Round
centiles.fan(wm, Wealthdata$HHAge, points=T, pch=16, cex=0.5, col="black", colors="rainbow",cent = c(1, 5, 10, 25, 50, 75, 90, 95, 99), ylab="Material Wealth (Bolivianos)", xlab="Age", main="Tsimane  Wealth by Age",xlim=c(20,85),ylim=c(0,log(60000)),xaxs="i",yaxs="i")

HHWealthZ=getZ(wm, Wealthdata[,c("HHAge","Round")], yval=log(Wealthdata$Total.value))

#Adding 100 doesn't affect z-scores, but allows model to fit without negative values.
wmA<-gamlss(I(log(Animals.value+100))~pb(HHAge)+Round, sigma.formula=~pb(HHAge)+Round, nu.fo=~pb(HHAge)+Round, family=BCT, data=na.omit(Wealthdata[,c("Animals.value","HHAge","Round")]),control=gamlss.control(n.cyc = 200))
centiles.fan(wmA, Wealthdata$HHAge, points=T, pch=16, cex=0.5, col="black", colors="rainbow",cent = c(1, 5, 10, 25, 50, 75, 90, 95, 99), ylab="Animal Wealth (Bolivianos)", xlab="Age", main="Tsimane  Wealth by Age",xlim=c(20,85),ylim=c(0,log(60000)),xaxs="i",yaxs="i")
HHWealthAZ=getZ(wmA, Wealthdata[,c("HHAge","Round")], yval=log(Wealthdata$Animals.value+100))

wmT<-gamlss(I(log(TraditionalAssets.value+100))~pb(HHAge)+Round, sigma.formula=~pb(HHAge)+Round, nu.fo=~pb(HHAge)+Round, family=BCT, data=Wealthdata[,c("TraditionalAssets.value","HHAge","Round")],control=gamlss.control(n.cyc = 200))
centiles.fan(wmT, Wealthdata$HHAge, points=T, pch=16, cex=0.5, col="black", colors="rainbow",cent = c(1, 5, 10, 25, 50, 75, 90, 95, 99), ylab="Material Wealth (Bolivianos)", xlab="Age", main="Tsimane  Wealth by Age",xlim=c(20,85),ylim=c(0,log(60000)),xaxs="i",yaxs="i")
HHWealthTZ=getZ(wmT, Wealthdata[,c("HHAge","Round")], yval=log(Wealthdata$TraditionalAssets.value+100))


wmI<-gamlss(I(log(IndustrialAssets.value+50))~pb(HHAge)+Round, sigma.formula=~pb(HHAge)+Round, nu.fo=~1+Round, family=BCT, data=na.omit(Wealthdata[,c("IndustrialAssets.value","HHAge","Round")]),control=gamlss.control(n.cyc = 50))
centiles.fan(wmI, Wealthdata$HHAge, points=T, pch=16, cex=0.5, col="black", colors="rainbow",cent = c(1, 5, 10, 25, 50, 75, 90, 95, 99), ylab="Material Wealth (Bolivianos)", xlab="Age", main="Tsimane  Wealth by Age",xaxs="i",yaxs="i")
HHWealthIZ=getZ(wmI, Wealthdata[,c("HHAge","Round")], yval=log(Wealthdata$IndustrialAssets.value+200))

HHWealthZs<-data.frame(HHId=Wealthdata$HHId,Round=Wealthdata$Round, HHWealthZ=HHWealthZ, HHWealthAZ=HHWealthAZ, HHWealthTZ=HHWealthTZ,HHWealthIZ=HHWealthIZ)
HHWealthZs[,3:6]<-round(HHWealthZs[,3:6],digits=3)

#back translate age corrected z-score into age corrected wealth
HHWealthZs$AgeCorWealth<-exp(ZtoVal50(wm,HHWealthZ))
HHWealthZs$AgeCorWealthA<-exp(ZtoVal50(wmA,HHWealthAZ))-100
HHWealthZs$AgeCorWealthT<-exp(ZtoVal50(wmT,HHWealthTZ))-100
HHWealthZs$AgeCorWealthI<-exp(ZtoVal50(wmI,HHWealthIZ))-50

Wealthdata<-merge(Wealthdata,HHWealthZs,all.x=TRUE)


#overall ginis by year
Gini2007<-ineq(Wealthdata$Total.value[Wealthdata$Round==2],type="Gini")
Gini2013<-ineq(Wealthdata$Total.value[Wealthdata$Round==3],type="Gini")

#Age Corrected Ginis and means
Gini2007.AgeCor<-ineq(Wealthdata$AgeCorWealth[Wealthdata$Round==2],type="Gini")
Gini2013.AgeCor<-ineq(Wealthdata$AgeCorWealth[Wealthdata$Round==3],type="Gini")

Ginis<-data.frame(Year=c(2007,2013),Gini=c(Gini2007,Gini2013),GiniAgeCor=c(Gini2007.AgeCor,Gini2013.AgeCor))

#by year by village
GiniComRound<-aggregate(list(Gini=Wealthdata$Total.value,Gini.AgeCor=Wealthdata$AgeCorWealth),by=Wealthdata[,c("Round","ComunID")],function(x) ineq(x,type="Gini"))
GiniComRoundCnt<-aggregate(list(WealthCnt=rep(1,nrow(Wealthdata))),by=Wealthdata[,c("Round","ComunID")],sum)
GiniComRound<-merge(GiniComRound,GiniComRoundCnt)
#GiniComRound<-GiniComRound[GiniComRound$WealthCnt>=10,]
GiniComRound2<-reshape(GiniComRound, idvar="ComunID",v.names=c("Gini","Gini.AgeCor","WealthCnt"),timevar="Round",direction="wide")

#Using the average wealth across Round 2 and Round 3 for HH measured in both
Wealthdata1.7<-aggregate(Wealthdata[,c("Total.value","AgeCorWealth","HHWealthZ")],by=Wealthdata[,c("ComunID","HHId")],mean,na.rm=TRUE)

GiniCom<-aggregate(list(Gini=Wealthdata1.7$Total.value,Gini.AgeCor=Wealthdata1.7$AgeCorWealth),by=list(ComunID=Wealthdata1.7$ComunID),function(x) ineq(x,type="Gini"))

ineq(Wealthdata1.7$AgeCorWealth,type="Gini")

MeanCom<-aggregate(list(MeanWealth=Wealthdata1.7$Total.value,MeanWealth.AgeCor=Wealthdata1.7$AgeCorWealth,MeanWealthZ=Wealthdata1.7$HHWealthZ),by=list(ComunID=Wealthdata1.7$ComunID),mean,na.rm=TRUE)
MedianCom<-aggregate(list(MedianWealth=Wealthdata1.7$Total.value,MedianWealth.AgeCor=Wealthdata1.7$AgeCorWealth,MedianWealthZ=Wealthdata1.7$HHWealthZ),by=list(ComunID=Wealthdata1.7$ComunID),median,na.rm=TRUE)

GiniComCnt<-aggregate(list(HHCnt=rep(1,nrow(Wealthdata1.7))),by=list(ComunID=Wealthdata1.7$ComunID),sum)

Ginisummary<-merge(GiniCom,MeanCom)
Ginisummary<-merge(Ginisummary,MedianCom)
Ginisummary<-merge(Ginisummary,GiniComCnt)

Ginisummary<-merge(GiniComRound2,Ginisummary,all.x=TRUE,all.y=TRUE)

vilcount<-unique(Wealthdata[,c("ComunID","ComunSize")])
Ginisummary<-merge(Ginisummary,vilcount)
abline(0,1)

#check representation of households per number of adults
plot(Ginisummary$HHCnt,Ginisummary$ComunSize)
Ginirep<-Ginisummary$HHCnt/Ginisummary$ComunSize
plot(Ginirep,Ginisummary$ComunSize)
#Small villages are all over the place on sampling, large are intermediate
hist(Ginirep)
#range from 6% to 37%. Max expected would be roughly this given 2-3 adults per household.

#check gini by village size and sample size
plot(Gini.AgeCor~ComunSize,Ginisummary)
#a few more extreme values in small villages, but I think we would expect this based on the nature of the Gini coefficient calculation.

plot(Gini.AgeCor~Ginirep,Ginisummary)
cor(Ginisummary$Gini.AgeCor,Ginirep)
cor(Ginisummary$Gini.AgeCor,Ginisummary$HHCnt)
cor(Ginisummary$Gini.AgeCor,Ginisummary$ComunSize)
#doesn't look like much relation with sampling or size. Smaller samples may be more innacurate, but they are unlikely to be biased in a particular direction.

#write HHdata with gini, zscores, and averages
#just using the round 2006 and 2013 rounds,
Wealthdata2<-merge(Wealthdata,Ginisummary,all.x=TRUE)
write.csv(Wealthdata2,"HHWealthWGini.csv",row.names=FALSE)
