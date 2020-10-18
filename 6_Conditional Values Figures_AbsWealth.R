library(wesanderson)
library(brms)
library(rethinking) #devtools::install_github("rmcelreath/rethinking")
#library(bayesplot)
#library(performance)

load("mainmodelsAbsZ.Rdata")


#### Figures ####
#################

## Calculate conditional effects for plotting. (Conditioned on mean of other variables.)
  m.dep<- conditional_effects(dep)
  m.conf<- conditional_effects(conf)
  m.lab<- conditional_effects(lab)
  m.othp<- conditional_effects(othp)
  m.cort<- conditional_effects(cort)
  m.bmi<- conditional_effects(bmi)
  m.sys<- conditional_effects(sys)
  m.dias<- conditional_effects(dias)
  m.salud<- conditional_effects(salud)
  m.sumdiag<- conditional_effects(sumdiag)
  m.infect<- conditional_effects(infect)
  m.resp<- conditional_effects(resp)
  m.gastro<- conditional_effects(gastro)


##Calculate proportion of posterior above zero.
getPP<-function(mod){
  post<-posterior_samples(mod) 
  post$b_absHHWealth<- post$b_HHWealthZ+post$b_MeanWealthZ
  PP<- 0
  for(i in 1:7){
    PP[i]<- sum(post[,i+1]>0)/nrow(post)
  }
  PP[8]<- sum(post[,ncol(post)]>0)/nrow(post)
  names(PP)<- colnames(post[,c(2:8,ncol(post))])
  PP
}

  dep.PP<-getPP(dep)
  conf.PP<-getPP(conf)
  othp.PP<-getPP(othp)
  lab.PP<-getPP(lab)
  cort.PP<-getPP(cort)
  bmi.PP<-getPP(bmi)
  sys.PP<-getPP(sys)
  dias.PP<-getPP(dias)
  salud.PP<-getPP(salud)
  sumdiag.PP<-getPP(sumdiag)
  infect.PP<-getPP(infect)
  resp.PP<-getPP(resp)
  gastro.PP<-getPP(gastro)
  
### relative wealth
tiff("All wealth effects no interactions.ADB.R2.AbsWealth.tiff", compression="lzw", height=9, width=10, units="cm", res=600, pointsize=5)
  par(mfrow=c(3,5), oma=c(3,3,0,0), mar=c(2,2,3,1))
  plot(estimate__~HHWealthZ, m.dep[[3]], type="l", ylim=c(-0.75,0.75), xlim=c(-3,3), main="Depression", col=wes_palettes$Royal2[1], lwd=2, ylab="", xlab="")
  polygon(c(m.dep[[3]]$HHWealthZ, rev(m.dep[[3]]$HHWealthZ)), c(smooth(m.dep[[3]]$lower__), rev(smooth(m.dep[[3]]$upper__))), col=adjustcolor(wes_palettes$Royal2[1], alpha.f=0.5), border=NA)
  text(0,0.7,labels=format(round(1-dep.PP[3],digits=2), nsmall=2), cex=1.5)
  
  mtext(text="Predicted outcome [Z]",side=2,line=2.5)
  plot(estimate__~HHWealthZ, m.conf[[3]], type="l", ylim=c(-0.75,0.75), xlim=c(-3,3), main="Conflicts", col=wes_palettes$Royal2[1], lwd=2, ylab="", xlab="")
  polygon(c(m.conf[[3]]$HHWealthZ, rev(m.conf[[3]]$HHWealthZ)), c(smooth(m.conf[[3]]$lower__), rev(smooth(m.conf[[3]]$upper__))), col=adjustcolor(wes_palettes$Royal2[1], alpha.f=0.5), border=NA)
  text(0,0.7,labels=format(round(1-conf.PP[3],digits=2), nsmall=2), cex=1.5)
  plot(estimate__~HHWealthZ, m.lab[[3]], type="l", ylim=c(-0.75,0.75), xlim=c(-3,3), main="Fewer Labor Partners", col=wes_palettes$Royal2[1], lwd=2, ylab="", xlab="")
  polygon(c(m.lab[[3]]$HHWealthZ, rev(m.lab[[3]]$HHWealthZ)), c(smooth(m.lab[[3]]$lower__), rev(smooth(m.lab[[3]]$upper__))), col=adjustcolor(wes_palettes$Royal2[1], alpha.f=0.5), border=NA)
  text(0,0.7,labels=format(round(1-lab.PP[3],digits=2), nsmall=2), cex=1.5)
  plot(estimate__~HHWealthZ, m.othp[[3]], type="l", ylim=c(-0.75,0.75), xlim=c(-3,3), main="Non-social Problems", col=wes_palettes$Royal2[1], lwd=2, ylab="", xlab="")
  polygon(c(m.othp[[3]]$HHWealthZ, rev(m.othp[[3]]$HHWealthZ)), c(smooth(m.othp[[3]]$lower__), rev(smooth(m.othp[[3]]$upper__))), col=adjustcolor(wes_palettes$Royal2[1], alpha.f=0.5), border=NA)
  text(0,0.7,labels=format(round(1-othp.PP[3],digits=2), nsmall=2), cex=1.5)
  plot(estimate__~HHWealthZ, m.cort[[3]], type="l", ylim=c(-0.75,0.75), xlim=c(-3,3), main="Cortisol", col=wes_palettes$Royal2[1], lwd=2, ylab="", xlab="")
  polygon(c(m.cort[[3]]$HHWealthZ, rev(m.cort[[3]]$HHWealthZ)), c(smooth(m.cort[[3]]$lower__), rev(smooth(m.cort[[3]]$upper__))), col=adjustcolor(wes_palettes$Royal2[1], alpha.f=0.5), border=NA)
  text(0,0.7,labels=format(round(1-cort.PP[3],digits=2), nsmall=2), cex=1.5)
  plot(estimate__~HHWealthZ, m.bmi[[3]], type="l", ylim=c(-0.75,0.75), xlim=c(-3,3), main="Body Mass Index", col=wes_palettes$Royal2[3], lwd=2, ylab="", xlab="")
  polygon(c(m.bmi[[3]]$HHWealthZ, rev(m.bmi[[3]]$HHWealthZ)), c(smooth(m.bmi[[3]]$lower__), rev(smooth(m.bmi[[3]]$upper__))), col=adjustcolor(wes_palettes$Royal2[3], alpha.f=0.5), border=NA)
  text(0,0.7,labels=format(round(1-bmi.PP[3],digits=2), nsmall=2), cex=1.5)
  mtext(text="Predicted outcome [Z]",side=2,line=2.5)
  plot(estimate__~HHWealthZ, m.sys[[3]], type="l", ylim=c(-0.75,0.75), xlim=c(-3,3), main="Systolic BP", col=wes_palettes$Royal2[3], lwd=2, ylab="", xlab="")
  polygon(c(m.sys[[3]]$HHWealthZ, rev(m.sys[[3]]$HHWealthZ)), c(smooth(m.sys[[3]]$lower__), rev(smooth(m.sys[[3]]$upper__))), col=adjustcolor(wes_palettes$Royal2[3], alpha.f=0.5), border=NA)
  text(0,0.7,labels=format(round(1-sys.PP[3],digits=2), nsmall=2), cex=1.5)
  plot(estimate__~HHWealthZ, m.dias[[3]], type="l", ylim=c(-0.75,0.75), xlim=c(-3,3), main="Diastolic BP", col=wes_palettes$Royal2[3], lwd=2, ylab="", xlab="")
  polygon(c(m.dias[[3]]$HHWealthZ, rev(m.dias[[3]]$HHWealthZ)), c(smooth(m.dias[[3]]$lower__), rev(smooth(m.dias[[3]]$upper__))), col=adjustcolor(wes_palettes$Royal2[3], alpha.f=0.5), border=NA)
  text(0,0.7,labels=format(round(1-dias.PP[3],digits=2), nsmall=2), cex=1.5)
  plot(estimate__~HHWealthZ, m.salud[[3]], type="l", ylim=c(-0.75,0.75), xlim=c(-3,3), main="Worse Self-rated Health", col=wes_palettes$Royal2[3], lwd=2, ylab="", xlab="")
  polygon(c(m.salud[[3]]$HHWealthZ, rev(m.salud[[3]]$HHWealthZ)), c(smooth(m.salud[[3]]$lower__), rev(smooth(m.salud[[3]]$upper__))), col=adjustcolor(wes_palettes$Royal2[3], alpha.f=0.5), border=NA)
  text(0,0.7,labels=format(round(1-salud.PP[3],digits=2), nsmall=2), cex=1.5)
  plot(estimate__~HHWealthZ, m.sumdiag[[3]], type="l", ylim=c(-0.75,0.75), xlim=c(-3,3), main="Total Morbidity", col=wes_palettes$Royal2[3], lwd=2, ylab="", xlab="")
  polygon(c(m.sumdiag[[3]]$HHWealthZ, rev(m.sumdiag[[3]]$HHWealthZ)), c(smooth(m.sumdiag[[3]]$lower__), rev(smooth(m.sumdiag[[3]]$upper__))), col=adjustcolor(wes_palettes$Royal2[3], alpha.f=0.5), border=NA)
  text(0,0.7,labels=format(round(1-sumdiag.PP[3],digits=2), nsmall=2), cex=1.5)
  
  frame()
  
  plot(estimate__~HHWealthZ, m.infect[[3]], type="l", ylim=c(0,1), xlim=c(-3,3), main="Infections", col=wes_palettes$Royal2[5], lwd=2, ylab="", xlab="")
  polygon(c(m.infect[[3]]$HHWealthZ, rev(m.infect[[3]]$HHWealthZ)), c(smooth(m.infect[[3]]$lower__), rev(smooth(m.infect[[3]]$upper__))), col=adjustcolor(wes_palettes$Royal2[5], alpha.f=0.5), border=NA)
  text(0,0.9,labels=format(round(1-infect.PP[3],digits=2), nsmall=2), cex=1.5)
  mtext("Predicted Probability",side=2,line=2.5)
  
  plot(estimate__~HHWealthZ, m.resp[[3]], type="l", ylim=c(0,1), xlim=c(-3,3), main="Respiratory", col=wes_palettes$Royal2[5], lwd=2, ylab="", xlab="")
  polygon(c(m.resp[[3]]$HHWealthZ, rev(m.resp[[3]]$HHWealthZ)), c(smooth(m.resp[[3]]$lower__), rev(smooth(m.resp[[3]]$upper__))), col=adjustcolor(wes_palettes$Royal2[5], alpha.f=0.5), border=NA)
  text(0,0.9,labels=format(round(1-resp.PP[3],digits=2), nsmall=2), cex=1.5)
  plot(estimate__~HHWealthZ, m.gastro[[3]], type="l", ylim=c(0,1), xlim=c(-3,3), main="Gastrointestinal", col=wes_palettes$Royal2[5], lwd=2, ylab="", xlab="")
  polygon(c(m.gastro[[3]]$HHWealthZ, rev(m.gastro[[3]]$HHWealthZ)), c(smooth(m.gastro[[3]]$lower__), rev(smooth(m.gastro[[3]]$upper__))), col=adjustcolor(wes_palettes$Royal2[5], alpha.f=0.5), border=NA)
  text(0,0.9,labels=format(round(1-gastro.PP[3],digits=2), nsmall=2), cex=1.5)
  mtext(text="relative Household Wealth [Z]",side=1,line=1,outer=TRUE)
  dev.off()



### inequality

  tiff("All gini effects no interactions.ADB.R2.AbsWealth.tiff", compression="lzw", height=9, width=10, units="cm", res=600, pointsize=5)
  par(mfrow=c(3,5), oma=c(3,3,0,0), mar=c(2,2,3,1))
  plot(estimate__~GiniZ, m.dep[[1]], type="l", ylim=c(-0.75,0.75), xlim=c(-2.5,2.5), main="Depression", col=wes_palettes$Royal2[1], lwd=2, ylab="", xlab="")
  polygon(c(m.dep[[1]]$GiniZ, rev(m.dep[[1]]$GiniZ)), c(smooth(m.dep[[1]]$lower__), rev(smooth(m.dep[[1]]$upper__))), col=adjustcolor(wes_palettes$Royal2[1], alpha.f=0.5), border=NA)
  text(0,0.7,labels=format(round(dep.PP[1],digits=2), nsmall=2), cex=1.5)
  mtext(text="Predicted outcome [Z]",side=2,line=2.5)
  
  plot(estimate__~GiniZ, m.conf[[1]], type="l", ylim=c(-0.75,0.75), xlim=c(-2.5,2.5), main="Conflicts", col=wes_palettes$Royal2[1], lwd=2, ylab="", xlab="")
  polygon(c(m.conf[[1]]$GiniZ, rev(m.conf[[1]]$GiniZ)), c(smooth(m.conf[[1]]$lower__), rev(smooth(m.conf[[1]]$upper__))), col=adjustcolor(wes_palettes$Royal2[1], alpha.f=0.5), border=NA)
  text(0,0.7,labels=format(round(conf.PP[1],digits=2), nsmall=2), cex=1.5)
  plot(estimate__~GiniZ, m.lab[[1]], type="l", ylim=c(-0.75,0.75), xlim=c(-2.5,2.5), main="Fewer Labor Partners", col=wes_palettes$Royal2[1], lwd=2, ylab="", xlab="")
  polygon(c(m.lab[[1]]$GiniZ, rev(m.lab[[1]]$GiniZ)), c(smooth(m.lab[[1]]$lower__), rev(smooth(m.lab[[1]]$upper__))), col=adjustcolor(wes_palettes$Royal2[1], alpha.f=0.5), border=NA)
  text(0,0.7,labels=format(round(lab.PP[1],digits=2), nsmall=2), cex=1.5)
  plot(estimate__~GiniZ, m.othp[[1]], type="l", ylim=c(-0.75,0.75), xlim=c(-2.5,2.5), main="Non-social Problems", col=wes_palettes$Royal2[1], lwd=2, ylab="", xlab="")
  polygon(c(m.othp[[1]]$GiniZ, rev(m.othp[[1]]$GiniZ)), c(smooth(m.othp[[1]]$lower__), rev(smooth(m.othp[[1]]$upper__))), col=adjustcolor(wes_palettes$Royal2[1], alpha.f=0.5), border=NA)
  text(0,0.7,labels=format(round(othp.PP[1],digits=2), nsmall=2), cex=1.5)
  plot(estimate__~GiniZ, m.cort[[1]], type="l", ylim=c(-0.75,0.75), xlim=c(-2.5,2.5), main="Cortisol", col=wes_palettes$Royal2[1], lwd=2, ylab="", xlab="")
  polygon(c(m.cort[[1]]$GiniZ, rev(m.cort[[1]]$GiniZ)), c(smooth(m.cort[[1]]$lower__), rev(smooth(m.cort[[1]]$upper__))), col=adjustcolor(wes_palettes$Royal2[1], alpha.f=0.5), border=NA)
  text(0,0.7,labels=format(round(cort.PP[1],digits=2), nsmall=2), cex=1.5)
  plot(estimate__~GiniZ, m.bmi[[1]], type="l", ylim=c(-0.75,0.75), xlim=c(-2.5,2.5), main="Body Mass Index", col=wes_palettes$Royal2[3], lwd=2, ylab="Predicted outcome [Z]", xlab="")
  polygon(c(m.bmi[[1]]$GiniZ, rev(m.bmi[[1]]$GiniZ)), c(smooth(m.bmi[[1]]$lower__), rev(smooth(m.bmi[[1]]$upper__))), col=adjustcolor(wes_palettes$Royal2[3], alpha.f=0.5), border=NA)
  text(0,0.7,labels=format(round(bmi.PP[1],digits=2), nsmall=2), cex=1.5)
  mtext(text="Predicted outcome [Z]",side=2,line=2.5)
  
  plot(estimate__~GiniZ, m.sys[[1]], type="l", ylim=c(-0.75,0.75), xlim=c(-2.5,2.5), main="Systolic BP", col=wes_palettes$Royal2[3], lwd=2, ylab="", xlab="")
  polygon(c(m.sys[[1]]$GiniZ, rev(m.sys[[1]]$GiniZ)), c(smooth(m.sys[[1]]$lower__), rev(smooth(m.sys[[1]]$upper__))), col=adjustcolor(wes_palettes$Royal2[3], alpha.f=0.5), border=NA)
  text(0,0.7,labels=format(round(sys.PP[1],digits=2), nsmall=2), cex=1.5)
  plot(estimate__~GiniZ, m.dias[[1]], type="l", ylim=c(-0.75,0.75), xlim=c(-2.5,2.5), main="Diastolic BP", col=wes_palettes$Royal2[3], lwd=2, ylab="", xlab="")
  polygon(c(m.dias[[1]]$GiniZ, rev(m.dias[[1]]$GiniZ)), c(smooth(m.dias[[1]]$lower__), rev(smooth(m.dias[[1]]$upper__))), col=adjustcolor(wes_palettes$Royal2[3], alpha.f=0.5), border=NA)
  text(0,0.7,labels=format(round(dias.PP[1],digits=2), nsmall=2), cex=1.5)
  plot(estimate__~GiniZ, m.salud[[1]], type="l", ylim=c(-0.75,0.75), xlim=c(-2.5,2.5), main="Worse Self-rated Health", col=wes_palettes$Royal2[3], lwd=2, ylab="", xlab="Gini [Z]")
  polygon(c(m.salud[[1]]$GiniZ, rev(m.salud[[1]]$GiniZ)), c(smooth(m.salud[[1]]$lower__), rev(smooth(m.salud[[1]]$upper__))), col=adjustcolor(wes_palettes$Royal2[3], alpha.f=0.5), border=NA)
  text(0,0.7,labels=format(round(salud.PP[1],digits=2), nsmall=2), cex=1.5)
  plot(estimate__~GiniZ, m.sumdiag[[1]], type="l", ylim=c(-0.75,0.75), xlim=c(-2.5,2.5), main="Total Morbidity", col=wes_palettes$Royal2[3], lwd=2, ylab="", xlab="Gini [Z]")
  polygon(c(m.sumdiag[[1]]$GiniZ, rev(m.sumdiag[[1]]$GiniZ)), c(smooth(m.sumdiag[[1]]$lower__), rev(smooth(m.sumdiag[[1]]$upper__))), col=adjustcolor(wes_palettes$Royal2[3], alpha.f=0.5), border=NA)
  text(0,0.7,labels=format(round(sumdiag.PP[1],digits=2), nsmall=2), cex=1.5)
  
  frame()
  
  plot(estimate__~GiniZ, m.infect[[1]], type="l", ylim=c(0,1), xlim=c(-2.5,2.5), main="Infections", col=wes_palettes$Royal2[5], lwd=2, ylab="", xlab="Gini [Z]")
  polygon(c(m.infect[[1]]$GiniZ, rev(m.infect[[1]]$GiniZ)), c(smooth(m.infect[[1]]$lower__), rev(smooth(m.infect[[1]]$upper__))), col=adjustcolor(wes_palettes$Royal2[5], alpha.f=0.5), border=NA)
  text(0,0.9,labels=format(round(infect.PP[1],digits=2), nsmall=2), cex=1.5)
  mtext("Predicted Probability",side=2,line=2.5)
  plot(estimate__~GiniZ, m.resp[[1]], type="l", ylim=c(0,1), xlim=c(-2.5,2.5), main="Respiratory", col=wes_palettes$Royal2[5], lwd=2, ylab="", xlab="Gini [Z]")
  polygon(c(m.resp[[1]]$GiniZ, rev(m.resp[[1]]$GiniZ)), c(smooth(m.resp[[1]]$lower__), rev(smooth(m.resp[[1]]$upper__))), col=adjustcolor(wes_palettes$Royal2[5], alpha.f=0.5), border=NA)
  text(0,0.9,labels=format(round(resp.PP[1],digits=2), nsmall=2), cex=1.5)
  plot(estimate__~GiniZ, m.gastro[[1]], type="l", ylim=c(0,1), xlim=c(-2.5,2.5), main="Gastrointestinal", col=wes_palettes$Royal2[5], lwd=2, ylab="", xlab="Gini [Z]")
  polygon(c(m.gastro[[1]]$GiniZ, rev(m.gastro[[1]]$GiniZ)), c(smooth(m.gastro[[1]]$lower__), rev(smooth(m.gastro[[1]]$upper__))), col=adjustcolor(wes_palettes$Royal2[5], alpha.f=0.5), border=NA)
  text(0,0.9,labels=format(round(gastro.PP[1],digits=2), nsmall=2), cex=1.5)
  mtext(text="Gini [Z]",side=1,line=1,outer=TRUE)
  dev.off()


#### Tables ####
################

## Table 1: descriptives
{
  Dep.data<- WealthdataDep$DepressionScore[!is.na(WealthdataDep$DepressionScore) & !is.na(WealthdataDep$GiniZ) & !is.na(WealthdataDep$DistZ)  & !is.na(WealthdataDep$SizeZ)  & !is.na(WealthdataDep$MeanWealthZ)  & !is.na(WealthdataDep$HHWealthZ)  
                                           & !is.na(WealthdataDep$AgeZ)  & !is.na(WealthdataDep$male)]
  Conf.data<- WealthdataDep$Conflicts[!is.na(WealthdataDep$Conflicts) & !is.na(WealthdataDep$GiniZ) & !is.na(WealthdataDep$DistZ)  & !is.na(WealthdataDep$SizeZ)  & !is.na(WealthdataDep$MeanWealthZ)  & !is.na(WealthdataDep$HHWealthZ)  
                                      & !is.na(WealthdataDep$AgeZ)  & !is.na(WealthdataDep$male)]
  Lab.data<- WealthdataDep$LaborPartners[!is.na(WealthdataDep$LaborPartners) & !is.na(WealthdataDep$GiniZ) & !is.na(WealthdataDep$DistZ)  & !is.na(WealthdataDep$SizeZ)  & !is.na(WealthdataDep$MeanWealthZ)  & !is.na(WealthdataDep$HHWealthZ)  
                                         & !is.na(WealthdataDep$AgeZ)  & !is.na(WealthdataDep$male)]
  OthP.data<- WealthdataDep$OtherProblems[!is.na(WealthdataDep$OtherProblems) & !is.na(WealthdataDep$GiniZ) & !is.na(WealthdataDep$DistZ)  & !is.na(WealthdataDep$SizeZ)  & !is.na(WealthdataDep$MeanWealthZ)  & !is.na(WealthdataDep$HHWealthZ)  
                                          & !is.na(WealthdataDep$AgeZ)  & !is.na(WealthdataDep$male)]
  
  Cort.data<- WealthdataCort$C.SG3[!is.na(WealthdataCort$C.SG3) & !is.na(WealthdataCort$GiniZ) & !is.na(WealthdataCort$DistZ)  & !is.na(WealthdataCort$SizeZ)  & !is.na(WealthdataCort$MeanWealthZ)  & !is.na(WealthdataCort$HHWealthZ)  
                                   & !is.na(WealthdataCort$AgeZ)  & !is.na(WealthdataCort$male)]
  
  T.data<- WealthdataT$T_PGSG[!is.na(WealthdataT$T_PGSG) & !is.na(WealthdataT$GiniZ) & !is.na(WealthdataT$DistZ)  & !is.na(WealthdataT$SizeZ)  & !is.na(WealthdataT$MeanWealthZ)  & !is.na(WealthdataT$HHWealthZ)  
                              & !is.na(WealthdataT$AgeZ)  & !is.na(WealthdataT$male)]
  
  sysBP.data<- Wealthdata2_sync$sysBP[!is.na(Wealthdata2_sync$sysBP) & !is.na(Wealthdata2_sync$GiniZ) & !is.na(Wealthdata2_sync$DistZ)  & !is.na(Wealthdata2_sync$SizeZ)  & !is.na(Wealthdata2_sync$MeanWealthZ)  & !is.na(Wealthdata2_sync$HHWealthZ)  
                                      & !is.na(Wealthdata2_sync$AgeZ)  & !is.na(Wealthdata2_sync$male) & Wealthdata2_sync$timediff_vid_sync<2]
  
  diasBP.data<- Wealthdata2_sync$diasBP[!is.na(Wealthdata2_sync$diasBP) & !is.na(Wealthdata2_sync$GiniZ) & !is.na(Wealthdata2_sync$DistZ)  & !is.na(Wealthdata2_sync$SizeZ)  & !is.na(Wealthdata2_sync$MeanWealthZ)  & !is.na(Wealthdata2_sync$HHWealthZ)  
                                        & !is.na(Wealthdata2_sync$AgeZ)  & !is.na(Wealthdata2_sync$male) & Wealthdata2_sync$timediff_vid_sync<2]
  
  BMI.data<- anthropomwealth$BMI.TZ[!is.na(anthropomwealth$BMI.TZ) & !is.na(anthropomwealth$GiniZ) & !is.na(anthropomwealth$DistZ)  & !is.na(anthropomwealth$SizeZ)  & !is.na(anthropomwealth$MeanWealthZ)  & !is.na(anthropomwealth$HHWealthZ)  
                                    & !is.na(anthropomwealth$AgeZ)  & !is.na(anthropomwealth$male) & anthropomwealth$TimeDiff<730 & anthropomwealth$TimeDiff>-730]
  
  Salud.data<- Wealthdata2_sync_subj$SaludGeneral[!is.na(Wealthdata2_sync_subj$SaludGeneral) & !is.na(Wealthdata2_sync_subj$GiniZ) & !is.na(Wealthdata2_sync_subj$DistZ)  & !is.na(Wealthdata2_sync_subj$SizeZ)  & !is.na(Wealthdata2_sync_subj$MeanWealthZ)  & !is.na(Wealthdata2_sync_subj$HHWealthZ)  
                                                  & !is.na(Wealthdata2_sync_subj$AgeZ)  & !is.na(Wealthdata2_sync_subj$male) & Wealthdata2_sync_subj$timediff_vid_sync<2]
  
  Sum.Diag.data<- Wealthdata2_sync$sum.diag[!is.na(Wealthdata2_sync$sum.diag) & !is.na(Wealthdata2_sync$GiniZ) & !is.na(Wealthdata2_sync$DistZ)  & !is.na(Wealthdata2_sync$SizeZ)  & !is.na(Wealthdata2_sync$MeanWealthZ)  & !is.na(Wealthdata2_sync$HHWealthZ)  
                                            & !is.na(Wealthdata2_sync$AgeZ)  & !is.na(Wealthdata2_sync$male) & Wealthdata2_sync$timediff_vid_sync<2]
  
  Infect.data<- Wealthdata2_sync$diag_CCS.LVL.1_1_yes_no[!is.na(Wealthdata2_sync$diag_CCS.LVL.1_1_yes_no) & !is.na(Wealthdata2_sync$GiniZ) & !is.na(Wealthdata2_sync$DistZ)  & !is.na(Wealthdata2_sync$SizeZ)  & !is.na(Wealthdata2_sync$MeanWealthZ)  & !is.na(Wealthdata2_sync$HHWealthZ)  
                                                         & !is.na(Wealthdata2_sync$AgeZ)  & !is.na(Wealthdata2_sync$male) & Wealthdata2_sync$timediff_vid_sync<2]
  
  Resp.data<- Wealthdata2_sync$diag_CCS.LVL.1_8_yes_no[!is.na(Wealthdata2_sync$diag_CCS.LVL.1_8_yes_no) & !is.na(Wealthdata2_sync$GiniZ) & !is.na(Wealthdata2_sync$DistZ)  & !is.na(Wealthdata2_sync$SizeZ)  & !is.na(Wealthdata2_sync$MeanWealthZ)  & !is.na(Wealthdata2_sync$HHWealthZ)  
                                                       & !is.na(Wealthdata2_sync$AgeZ)  & !is.na(Wealthdata2_sync$male) & Wealthdata2_sync$timediff_vid_sync<2]
  
  Gastro.data<- Wealthdata2_sync$diag_CCS.LVL.1_9_yes_no[!is.na(Wealthdata2_sync$diag_CCS.LVL.1_9_yes_no) & !is.na(Wealthdata2_sync$GiniZ) & !is.na(Wealthdata2_sync$DistZ)  & !is.na(Wealthdata2_sync$SizeZ)  & !is.na(Wealthdata2_sync$MeanWealthZ)  & !is.na(Wealthdata2_sync$HHWealthZ)  
                                                         & !is.na(Wealthdata2_sync$AgeZ)  & !is.na(Wealthdata2_sync$male) & Wealthdata2_sync$timediff_vid_sync<2]
  
  Age.data<- na.omit(Wealthdata2$Age)
  
  Sex.data<- na.omit(Wealthdata2$male)
  
  WealthdataHH<- unique(Wealthdata2[,c("HHId", "Total.value")])
  Wealth.data<- WealthdataHH$Total.value 
  
  WealthdataCom<- unique(Wealthdata2[,c("Comunidad", "HHCnt", "route.distance.SB", "MeanWealth.AgeCor", "Gini.AgeCor")])
  Size.data<- na.omit(WealthdataCom$HHCnt)
  Dist.data<- na.omit(WealthdataCom$route.distance.SB)
  MeanWealth.data<- na.omit(WealthdataCom$MeanWealth.AgeCor)
  Gini.data<- na.omit(WealthdataCom$Gini.AgeCor)
  
  
  
  data_summary <- list(Dep.data, Conf.data, Lab.data, OthP.data, Cort.data, T.data, BMI.data, sysBP.data, diasBP.data, Salud.data, Sum.Diag.data, 
                       Infect.data, Resp.data, Gastro.data, Age.data, Sex.data, Wealth.data, Size.data, Dist.data, MeanWealth.data, Gini.data)
  str(data_summary)
  data_summary[[1]]<- as.numeric(data_summary[[1]])
  data_summary[[9]]<- as.numeric(data_summary[[9]])
  data_summary[[10]]<- as.numeric(data_summary[[10]])
  data_summary[[15]]<- as.numeric(data_summary[[15]])
  data_summary[[16]]<- as.numeric(data_summary[[16]])
  data_summary[[17]]<- as.numeric(data_summary[[17]])
  data_summary[[18]]<- as.numeric(data_summary[[18]])
  data_summary[[19]]<- as.numeric(data_summary[[19]])
  data_summary[[20]]<- as.numeric(data_summary[[20]])
  data_summary[[21]]<- as.numeric(data_summary[[21]])
  names(data_summary)<- c("Depression [sum score]", "Conflicts [count]", "Labor Partners [count]", "Non-social Problems [count]", "Cortisol [pg/ml Cr]", 
                          "Testosterone [pg/ml]", "BMI [Z]", "Systolic Blood Pressure [mmHg]", "Diastolic Blood Pressure [mmHg]", "Self-rated Health [scale]", "Total Morbidity [count]", 
                          "Infections [yes/no]", "Respiratory [yes/no]", "Gastrointestinal [yes/no]", "Age [years]", "Gender (51.7% male)", "Household wealth [Bs]", 
                          "Community Size [HH count]", "Distance to town [km]", "Mean Wealth [Bs]", "Wealth inequality [Gini]")
  tab_summarise_vars <- data.frame(
    names(data_summary),
    as.numeric(lapply(data_summary, length)),
    round(as.numeric(lapply(data_summary, median, na.rm = TRUE)),2),
    round(as.numeric(lapply(data_summary, sd, na.rm = TRUE)),2),
    paste(round(as.numeric(unlist(lapply(data_summary, range, na.rm = TRUE))[c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41)]),2),
          round(as.numeric(unlist(lapply(data_summary, range, na.rm = TRUE))[c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42)]),2),
          sep = "-")
  )
  colnames(tab_summarise_vars) <- c("Variable", "N", "Median", "SD", "Range")
  write.csv(tab_summarise_vars, "table1.csv", row.names=FALSE)
  
  # for binary:
  sum(Sex.data)/length(Sex.data)
  sum(Infect.data)/length(Infect.data)
  sum(Resp.data)/length(Resp.data)
  sum(Gastro.data)/length(Gastro.data)
}


## supplementary tables: full model summary
# calculate R2s
{
  depr2<- bayes_R2(dep) # use r2_bayes to get conditional vs marginal R2
  confr2<- bayes_R2(conf) # use r2_bayes to get conditional vs marginal R2
  labr2<- bayes_R2(lab) # use r2_bayes to get conditional vs marginal R2
  othpr2<- bayes_R2(othp) # use r2_bayes to get conditional vs marginal R2
  cortr2<- bayes_R2(cort) # use r2_bayes to get conditional vs marginal R2
  bmir2<- bayes_R2(bmi) # use r2_bayes to get conditional vs marginal R2
  sysr2<- bayes_R2(sys) # use r2_bayes to get conditional vs marginal R2
  diasr2<- bayes_R2(dias) # use r2_bayes to get conditional vs marginal R2
  saludr2<- bayes_R2(salud) # use r2_bayes to get conditional vs marginal R2
  sumdiagr2<- bayes_R2(sumdiag) # use r2_bayes to get conditional vs marginal R2
  infectr2<- bayes_R2(infect) # use r2_bayes to get conditional vs marginal R2
  respr2<- bayes_R2(resp) # use r2_bayes to get conditional vs marginal R2
  gastror2<- bayes_R2(gastro) # use r2_bayes to get conditional vs marginal R2
}
{
  # Dep
  dep.absHHWealth<- cbind(mean(post_dep[,ncol(post_dep)]), HPDI(post_dep[,ncol(post_dep)], prob=0.95)[1], HPDI(post_dep[,ncol(post_dep)], prob=0.95)[2])
  dep.summary<- as.data.frame(rbind(format(round(summary(dep)$fixed[,c(1,3,4)],digits=2),nsmall=2), format(round(dep.absHHWealth,digits=2),nsmall=2), format(round(depr2[c(1,3,4)],digits=2),nsmall=2)))
  write.csv(dep.summary, "dep.summary.csv")
  
  # Conf
  conf.absHHWealth<- cbind(mean(post_conf[,ncol(post_conf)]), HPDI(post_conf[,ncol(post_conf)], prob=0.95)[1], HPDI(post_conf[,ncol(post_conf)], prob=0.95)[2])
  conf.summary<- as.data.frame(rbind(format(round(summary(conf)$fixed[,c(1,3,4)],digits=2),nsmall=2), format(round(conf.absHHWealth,digits=2),nsmall=2), format(round(confr2[c(1,3,4)],digits=2),nsmall=2)))
  write.csv(conf.summary, "conf.summary.csv")
  
  # Lab
  lab.absHHWealth<- cbind(mean(post_lab[,ncol(post_lab)]), HPDI(post_lab[,ncol(post_lab)], prob=0.95)[1], HPDI(post_lab[,ncol(post_lab)], prob=0.95)[2])
  lab.summary<- as.data.frame(rbind(format(round(summary(lab)$fixed[,c(1,3,4)],digits=2),nsmall=2), format(round(lab.absHHWealth,digits=2),nsmall=2), format(round(labr2[c(1,3,4)],digits=2),nsmall=2)))
  write.csv(lab.summary, "lab.summary.csv")
  
  # OthP
  othp.absHHWealth<- cbind(mean(post_othp[,ncol(post_othp)]), HPDI(post_othp[,ncol(post_othp)], prob=0.95)[1], HPDI(post_othp[,ncol(post_othp)], prob=0.95)[2])
  othp.summary<- as.data.frame(rbind(format(round(summary(othp)$fixed[,c(1,3,4)],digits=2),nsmall=2), format(round(othp.absHHWealth,digits=2),nsmall=2), format(round(othpr2[c(1,3,4)],digits=2),nsmall=2)))
  write.csv(othp.summary, "othp.summary.csv")
  
  # Cort
  cort.absHHWealth<- cbind(mean(post_cort[,ncol(post_cort)]), HPDI(post_cort[,ncol(post_cort)], prob=0.95)[1], HPDI(post_cort[,ncol(post_cort)], prob=0.95)[2])
  cort.summary<- as.data.frame(rbind(format(round(summary(cort)$fixed[,c(1,3,4)],digits=2),nsmall=2), format(round(cort.absHHWealth,digits=2),nsmall=2), format(round(cortr2[c(1,3,4)],digits=2),nsmall=2)))
  write.csv(cort.summary, "cort.summary.csv")
  
  # BMI
  bmi.absHHWealth<- cbind(mean(post_bmi[,ncol(post_bmi)]), HPDI(post_bmi[,ncol(post_bmi)], prob=0.95)[1], HPDI(post_bmi[,ncol(post_bmi)], prob=0.95)[2])
  bmi.summary<- as.data.frame(rbind(format(round(summary(bmi)$fixed[,c(1,3,4)],digits=2),nsmall=2), format(round(bmi.absHHWealth,digits=2),nsmall=2), format(round(bmir2[c(1,3,4)],digits=2),nsmall=2)))
  write.csv(bmi.summary, "bmi.summary.csv")
  
  # sys
  sys.absHHWealth<- cbind(mean(post_sys[,ncol(post_sys)]), HPDI(post_sys[,ncol(post_sys)], prob=0.95)[1], HPDI(post_sys[,ncol(post_sys)], prob=0.95)[2])
  sys.summary<- as.data.frame(rbind(format(round(summary(sys)$fixed[,c(1,3,4)],digits=2),nsmall=2), format(round(sys.absHHWealth,digits=2),nsmall=2), format(round(sysr2[c(1,3,4)],digits=2),nsmall=2)))
  write.csv(sys.summary, "sys.summary.csv")
  
  # dias
  dias.absHHWealth<- cbind(mean(post_dias[,ncol(post_dias)]), HPDI(post_dias[,ncol(post_dias)], prob=0.95)[1], HPDI(post_dias[,ncol(post_dias)], prob=0.95)[2])
  dias.summary<- as.data.frame(rbind(format(round(summary(dias)$fixed[,c(1,3,4)],digits=2),nsmall=2), format(round(dias.absHHWealth,digits=2),nsmall=2), format(round(diasr2[c(1,3,4)],digits=2),nsmall=2)))
  write.csv(dias.summary, "dias.summary.csv")
  
  # salud
  salud.absHHWealth<- cbind(mean(post_salud[,ncol(post_salud)]), HPDI(post_salud[,ncol(post_salud)], prob=0.95)[1], HPDI(post_salud[,ncol(post_salud)], prob=0.95)[2])
  salud.summary<- as.data.frame(rbind(format(round(summary(salud)$fixed[,c(1,3,4)],digits=2),nsmall=2), format(round(salud.absHHWealth,digits=2),nsmall=2), format(round(saludr2[c(1,3,4)],digits=2),nsmall=2)))
  write.csv(salud.summary, "salud.summary.csv")
  
  # sumdiag
  sumdiag.absHHWealth<- cbind(mean(post_sumdiag[,ncol(post_sumdiag)]), HPDI(post_sumdiag[,ncol(post_sumdiag)], prob=0.95)[1], HPDI(post_sumdiag[,ncol(post_sumdiag)], prob=0.95)[2])
  sumdiag.summary<- as.data.frame(rbind(format(round(summary(sumdiag)$fixed[,c(1,3,4)],digits=2),nsmall=2), format(round(sumdiag.absHHWealth,digits=2),nsmall=2), format(round(sumdiagr2[c(1,3,4)],digits=2),nsmall=2)))
  write.csv(sumdiag.summary, "sumdiag.summary.csv")
  
  # infect
  infect.absHHWealth<- cbind(mean(post_infect[,ncol(post_infect)]), HPDI(post_infect[,ncol(post_infect)], prob=0.95)[1], HPDI(post_infect[,ncol(post_infect)], prob=0.95)[2])
  infect.summary<- as.data.frame(rbind(format(round(summary(infect)$fixed[,c(1,3,4)],digits=2),nsmall=2), format(round(infect.absHHWealth,digits=2),nsmall=2), format(round(infectr2[c(1,3,4)],digits=2),nsmall=2)))
  write.csv(infect.summary, "infect.summary.csv")
  
  # resp
  resp.absHHWealth<- cbind(mean(post_resp[,ncol(post_resp)]), HPDI(post_resp[,ncol(post_resp)], prob=0.95)[1], HPDI(post_resp[,ncol(post_resp)], prob=0.95)[2])
  resp.summary<- as.data.frame(rbind(format(round(summary(resp)$fixed[,c(1,3,4)],digits=2),nsmall=2), format(round(resp.absHHWealth,digits=2),nsmall=2), format(round(respr2[c(1,3,4)],digits=2),nsmall=2)))
  write.csv(resp.summary, "resp.summary.csv")
  
  # gastro
  gastro.absHHWealth<- cbind(mean(post_gastro[,ncol(post_gastro)]), HPDI(post_gastro[,ncol(post_gastro)], prob=0.95)[1], HPDI(post_gastro[,ncol(post_gastro)], prob=0.95)[2])
  gastro.summary<- as.data.frame(rbind(format(round(summary(gastro)$fixed[,c(1,3,4)],digits=2),nsmall=2), format(round(gastro.absHHWealth,digits=2),nsmall=2), format(round(gastror2[c(1,3,4)],digits=2),nsmall=2)))
  write.csv(gastro.summary, "gastro.summary.csv")
  
  
  
  
  
  
  
}

# same for mediation models
{
  salud.stressr2<- bayes_R2(salud.stress) # use r2_bayes to get conditional vs marginal R2
  sys.stressr2<- bayes_R2(sys.stress) # use r2_bayes to get conditional vs marginal R2
  dias.stressr2<- bayes_R2(dias.stress) # use r2_bayes to get conditional vs marginal R2
  sumdiag.stressr2<- bayes_R2(sumdiag.stress) # use r2_bayes to get conditional vs marginal R2
  infect.stressr2<- bayes_R2(infect.stress) # use r2_bayes to get conditional vs marginal R2
  resp.stressr2<- bayes_R2(resp.stress) # use r2_bayes to get conditional vs marginal R2
  gastro.stressr2<- bayes_R2(gastro.stress) # use r2_bayes to get conditional vs marginal R2
}
{
  # salud
  salud.stress.absHHWealth<- cbind(mean(post_salud.stress[,ncol(post_salud.stress)]), HPDI(post_salud.stress[,ncol(post_salud.stress)], prob=0.95)[1], HPDI(post_salud.stress[,ncol(post_salud.stress)], prob=0.95)[2])
  salud.stress.summary<- as.data.frame(rbind(format(round(summary(salud.stress)$fixed[,c(1,3,4)],digits=2),nsmall=2), format(round(salud.stress.absHHWealth,digits=2),nsmall=2), format(round(salud.stressr2[1,c(1,3,4)],digits=2),nsmall=2)))
  write.csv(salud.stress.summary, "salud.stress.summary.csv")
  
  # sumdiag
  sumdiag.stress.absHHWealth<- cbind(mean(post_sumdiag.stress[,ncol(post_sumdiag.stress)]), HPDI(post_sumdiag.stress[,ncol(post_sumdiag.stress)], prob=0.95)[1], HPDI(post_sumdiag.stress[,ncol(post_sumdiag.stress)], prob=0.95)[2])
  sumdiag.stress.summary<- as.data.frame(rbind(format(round(summary(sumdiag.stress)$fixed[,c(1,3,4)],digits=2),nsmall=2), format(round(sumdiag.stress.absHHWealth,digits=2),nsmall=2), format(round(sumdiag.stressr2[1,c(1,3,4)],digits=2),nsmall=2)))
  write.csv(sumdiag.stress.summary, "sumdiag.stress.summary.csv")
  
  # infect
  infect.stress.absHHWealth<- cbind(mean(post_infect.stress[,ncol(post_infect.stress)]), HPDI(post_infect.stress[,ncol(post_infect.stress)], prob=0.95)[1], HPDI(post_infect.stress[,ncol(post_infect.stress)], prob=0.95)[2])
  infect.stress.summary<- as.data.frame(rbind(format(round(summary(infect.stress)$fixed[,c(1,3,4)],digits=2),nsmall=2), format(round(infect.stress.absHHWealth,digits=2),nsmall=2), format(round(infect.stressr2[1,c(1,3,4)],digits=2),nsmall=2)))
  write.csv(infect.stress.summary, "infect.stress.summary.csv")
  
  # resp
  resp.stress.absHHWealth<- cbind(mean(post_resp.stress[,ncol(post_resp.stress)]), HPDI(post_resp.stress[,ncol(post_resp.stress)], prob=0.95)[1], HPDI(post_resp.stress[,ncol(post_resp.stress)], prob=0.95)[2])
  resp.stress.summary<- as.data.frame(rbind(format(round(summary(resp.stress)$fixed[,c(1,3,4)],digits=2),nsmall=2), format(round(resp.stress.absHHWealth,digits=2),nsmall=2), format(round(resp.stressr2[1,c(1,3,4)],digits=2),nsmall=2)))
  write.csv(resp.stress.summary, "resp.stress.summary.csv")
  
  # gastro
  gastro.stress.absHHWealth<- cbind(mean(post_gastro.stress[,ncol(post_gastro.stress)]), HPDI(post_gastro.stress[,ncol(post_gastro.stress)], prob=0.95)[1], HPDI(post_gastro.stress[,ncol(post_gastro.stress)], prob=0.95)[2])
  gastro.stress.summary<- as.data.frame(rbind(format(round(summary(gastro.stress)$fixed[,c(1,3,4)],digits=2),nsmall=2), format(round(gastro.stress.absHHWealth,digits=2),nsmall=2), format(round(gastro.stressr2[1,c(1,3,4)],digits=2),nsmall=2)))
  write.csv(gastro.stress.summary, "gastro.stress.summary.csv")
  
  # sys
  sys.stress.absHHWealth<- cbind(mean(post_sys.stress[,ncol(post_sys.stress)]), HPDI(post_sys.stress[,ncol(post_sys.stress)], prob=0.95)[1], HPDI(post_sys.stress[,ncol(post_sys.stress)], prob=0.95)[2])
  sys.stress.summary<- as.data.frame(rbind(format(round(summary(sys.stress)$fixed[,c(1,3,4)],digits=2),nsmall=2), format(round(sys.stress.absHHWealth,digits=2),nsmall=2), format(round(sys.stressr2[1,c(1,3,4)],digits=2),nsmall=2)))
  write.csv(sys.stress.summary, "sys.stress.summary.csv")
  
  # dias
  dias.stress.absHHWealth<- cbind(mean(post_dias.stress[,ncol(post_dias.stress)]), HPDI(post_dias.stress[,ncol(post_dias.stress)], prob=0.95)[1], HPDI(post_dias.stress[,ncol(post_dias.stress)], prob=0.95)[2])
  dias.stress.summary<- as.data.frame(rbind(format(round(summary(dias.stress)$fixed[,c(1,3,4)],digits=2),nsmall=2), format(round(dias.stress.absHHWealth,digits=2),nsmall=2), format(round(dias.stressr2[1,c(1,3,4)],digits=2),nsmall=2)))
  write.csv(dias.stress.summary, "dias.stress.summary.csv")
  
}



