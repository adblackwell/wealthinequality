library(wesanderson)
library(brms)
library(rethinking) #devtools::install_github("rmcelreath/rethinking")
#library(bayesplot)
#library(performance)

load("mainmodelsNocov.Rdata")

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
tiff("All wealth effects no interactions.ADB.R2.NoCov.tiff", compression="lzw", height=9, width=10, units="cm", res=600, pointsize=5)
  par(mfrow=c(3,5), oma=c(3,3,0,0), mar=c(2,2,3,1))
  plot(estimate__~HHWealthZ, m.dep[[2]], type="l", ylim=c(-0.75,0.75), xlim=c(-3,3), main="Depression", col=wes_palettes$Royal2[1], lwd=2, ylab="", xlab="")
  polygon(c(m.dep[[2]]$HHWealthZ, rev(m.dep[[2]]$HHWealthZ)), c(smooth(m.dep[[2]]$lower__), rev(smooth(m.dep[[2]]$upper__))), col=adjustcolor(wes_palettes$Royal2[1], alpha.f=0.5), border=NA)
  text(0,0.7,labels=format(round(1-dep.PP[2],digits=2), nsmall=2), cex=1.5)
  
  mtext(text="Predicted outcome [Z]",side=2,line=2.5)
  plot(estimate__~HHWealthZ, m.conf[[2]], type="l", ylim=c(-0.75,0.75), xlim=c(-3,3), main="Conflicts", col=wes_palettes$Royal2[1], lwd=2, ylab="", xlab="")
  polygon(c(m.conf[[2]]$HHWealthZ, rev(m.conf[[2]]$HHWealthZ)), c(smooth(m.conf[[2]]$lower__), rev(smooth(m.conf[[2]]$upper__))), col=adjustcolor(wes_palettes$Royal2[1], alpha.f=0.5), border=NA)
  text(0,0.7,labels=format(round(1-conf.PP[2],digits=2), nsmall=2), cex=1.5)
  plot(estimate__~HHWealthZ, m.lab[[2]], type="l", ylim=c(-0.75,0.75), xlim=c(-3,3), main="Fewer Labor Partners", col=wes_palettes$Royal2[1], lwd=2, ylab="", xlab="")
  polygon(c(m.lab[[2]]$HHWealthZ, rev(m.lab[[2]]$HHWealthZ)), c(smooth(m.lab[[2]]$lower__), rev(smooth(m.lab[[2]]$upper__))), col=adjustcolor(wes_palettes$Royal2[1], alpha.f=0.5), border=NA)
  text(0,0.7,labels=format(round(1-lab.PP[2],digits=2), nsmall=2), cex=1.5)
  plot(estimate__~HHWealthZ, m.othp[[2]], type="l", ylim=c(-0.75,0.75), xlim=c(-3,3), main="Non-social Problems", col=wes_palettes$Royal2[1], lwd=2, ylab="", xlab="")
  polygon(c(m.othp[[2]]$HHWealthZ, rev(m.othp[[2]]$HHWealthZ)), c(smooth(m.othp[[2]]$lower__), rev(smooth(m.othp[[2]]$upper__))), col=adjustcolor(wes_palettes$Royal2[1], alpha.f=0.5), border=NA)
  text(0,0.7,labels=format(round(1-othp.PP[2],digits=2), nsmall=2), cex=1.5)
  plot(estimate__~HHWealthZ, m.cort[[2]], type="l", ylim=c(-0.75,0.75), xlim=c(-3,3), main="Cortisol", col=wes_palettes$Royal2[1], lwd=2, ylab="", xlab="")
  polygon(c(m.cort[[2]]$HHWealthZ, rev(m.cort[[2]]$HHWealthZ)), c(smooth(m.cort[[2]]$lower__), rev(smooth(m.cort[[2]]$upper__))), col=adjustcolor(wes_palettes$Royal2[1], alpha.f=0.5), border=NA)
  text(0,0.7,labels=format(round(1-cort.PP[2],digits=2), nsmall=2), cex=1.5)
  plot(estimate__~HHWealthZ, m.bmi[[2]], type="l", ylim=c(-0.75,0.75), xlim=c(-3,3), main="Body Mass Index", col=wes_palettes$Royal2[2], lwd=2, ylab="", xlab="")
  polygon(c(m.bmi[[2]]$HHWealthZ, rev(m.bmi[[2]]$HHWealthZ)), c(smooth(m.bmi[[2]]$lower__), rev(smooth(m.bmi[[2]]$upper__))), col=adjustcolor(wes_palettes$Royal2[2], alpha.f=0.5), border=NA)
  text(0,0.7,labels=format(round(1-bmi.PP[2],digits=2), nsmall=2), cex=1.5)
  mtext(text="Predicted outcome [Z]",side=2,line=2.5)
  plot(estimate__~HHWealthZ, m.sys[[2]], type="l", ylim=c(-0.75,0.75), xlim=c(-3,3), main="Systolic BP", col=wes_palettes$Royal2[2], lwd=2, ylab="", xlab="")
  polygon(c(m.sys[[2]]$HHWealthZ, rev(m.sys[[2]]$HHWealthZ)), c(smooth(m.sys[[2]]$lower__), rev(smooth(m.sys[[2]]$upper__))), col=adjustcolor(wes_palettes$Royal2[2], alpha.f=0.5), border=NA)
  text(0,0.7,labels=format(round(1-sys.PP[2],digits=2), nsmall=2), cex=1.5)
  plot(estimate__~HHWealthZ, m.dias[[2]], type="l", ylim=c(-0.75,0.75), xlim=c(-3,3), main="Diastolic BP", col=wes_palettes$Royal2[2], lwd=2, ylab="", xlab="")
  polygon(c(m.dias[[2]]$HHWealthZ, rev(m.dias[[2]]$HHWealthZ)), c(smooth(m.dias[[2]]$lower__), rev(smooth(m.dias[[2]]$upper__))), col=adjustcolor(wes_palettes$Royal2[2], alpha.f=0.5), border=NA)
  text(0,0.7,labels=format(round(1-dias.PP[2],digits=2), nsmall=2), cex=1.5)
  plot(estimate__~HHWealthZ, m.salud[[2]], type="l", ylim=c(-0.75,0.75), xlim=c(-3,3), main="Worse Self-rated Health", col=wes_palettes$Royal2[2], lwd=2, ylab="", xlab="")
  polygon(c(m.salud[[2]]$HHWealthZ, rev(m.salud[[2]]$HHWealthZ)), c(smooth(m.salud[[2]]$lower__), rev(smooth(m.salud[[2]]$upper__))), col=adjustcolor(wes_palettes$Royal2[2], alpha.f=0.5), border=NA)
  text(0,0.7,labels=format(round(1-salud.PP[2],digits=2), nsmall=2), cex=1.5)
  plot(estimate__~HHWealthZ, m.sumdiag[[2]], type="l", ylim=c(-0.75,0.75), xlim=c(-3,3), main="Total Morbidity", col=wes_palettes$Royal2[2], lwd=2, ylab="", xlab="")
  polygon(c(m.sumdiag[[2]]$HHWealthZ, rev(m.sumdiag[[2]]$HHWealthZ)), c(smooth(m.sumdiag[[2]]$lower__), rev(smooth(m.sumdiag[[2]]$upper__))), col=adjustcolor(wes_palettes$Royal2[2], alpha.f=0.5), border=NA)
  text(0,0.7,labels=format(round(1-sumdiag.PP[2],digits=2), nsmall=2), cex=1.5)
  
  frame()
  
  plot(estimate__~HHWealthZ, m.infect[[2]], type="l", ylim=c(0,1), xlim=c(-3,3), main="Infections", col=wes_palettes$Royal2[5], lwd=2, ylab="", xlab="")
  polygon(c(m.infect[[2]]$HHWealthZ, rev(m.infect[[2]]$HHWealthZ)), c(smooth(m.infect[[2]]$lower__), rev(smooth(m.infect[[2]]$upper__))), col=adjustcolor(wes_palettes$Royal2[5], alpha.f=0.5), border=NA)
  text(0,0.9,labels=format(round(1-infect.PP[2],digits=2), nsmall=2), cex=1.5)
  mtext("Predicted Probability",side=2,line=2.5)
  
  plot(estimate__~HHWealthZ, m.resp[[2]], type="l", ylim=c(0,1), xlim=c(-3,3), main="Respiratory", col=wes_palettes$Royal2[5], lwd=2, ylab="", xlab="")
  polygon(c(m.resp[[2]]$HHWealthZ, rev(m.resp[[2]]$HHWealthZ)), c(smooth(m.resp[[2]]$lower__), rev(smooth(m.resp[[2]]$upper__))), col=adjustcolor(wes_palettes$Royal2[5], alpha.f=0.5), border=NA)
  text(0,0.9,labels=format(round(1-resp.PP[2],digits=2), nsmall=2), cex=1.5)
  plot(estimate__~HHWealthZ, m.gastro[[2]], type="l", ylim=c(0,1), xlim=c(-3,3), main="Gastrointestinal", col=wes_palettes$Royal2[5], lwd=2, ylab="", xlab="")
  polygon(c(m.gastro[[2]]$HHWealthZ, rev(m.gastro[[2]]$HHWealthZ)), c(smooth(m.gastro[[2]]$lower__), rev(smooth(m.gastro[[2]]$upper__))), col=adjustcolor(wes_palettes$Royal2[5], alpha.f=0.5), border=NA)
  text(0,0.9,labels=format(round(1-gastro.PP[2],digits=2), nsmall=2), cex=1.5)
  mtext(text="relative Household Wealth [Z]",side=1,line=1,outer=TRUE)
  dev.off()



### inequality

  tiff("All gini effects no interactions.ADB.R2.NoCov.tiff", compression="lzw", height=9, width=10, units="cm", res=600, pointsize=5)
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
  plot(estimate__~GiniZ, m.bmi[[1]], type="l", ylim=c(-0.75,0.75), xlim=c(-2.5,2.5), main="Body Mass Index", col=wes_palettes$Royal2[2], lwd=2, ylab="Predicted outcome [Z]", xlab="")
  polygon(c(m.bmi[[1]]$GiniZ, rev(m.bmi[[1]]$GiniZ)), c(smooth(m.bmi[[1]]$lower__), rev(smooth(m.bmi[[1]]$upper__))), col=adjustcolor(wes_palettes$Royal2[2], alpha.f=0.5), border=NA)
  text(0,0.7,labels=format(round(bmi.PP[1],digits=2), nsmall=2), cex=1.5)
  mtext(text="Predicted outcome [Z]",side=2,line=2.5)
  
  plot(estimate__~GiniZ, m.sys[[1]], type="l", ylim=c(-0.75,0.75), xlim=c(-2.5,2.5), main="Systolic BP", col=wes_palettes$Royal2[2], lwd=2, ylab="", xlab="")
  polygon(c(m.sys[[1]]$GiniZ, rev(m.sys[[1]]$GiniZ)), c(smooth(m.sys[[1]]$lower__), rev(smooth(m.sys[[1]]$upper__))), col=adjustcolor(wes_palettes$Royal2[2], alpha.f=0.5), border=NA)
  text(0,0.7,labels=format(round(sys.PP[1],digits=2), nsmall=2), cex=1.5)
  plot(estimate__~GiniZ, m.dias[[1]], type="l", ylim=c(-0.75,0.75), xlim=c(-2.5,2.5), main="Diastolic BP", col=wes_palettes$Royal2[2], lwd=2, ylab="", xlab="")
  polygon(c(m.dias[[1]]$GiniZ, rev(m.dias[[1]]$GiniZ)), c(smooth(m.dias[[1]]$lower__), rev(smooth(m.dias[[1]]$upper__))), col=adjustcolor(wes_palettes$Royal2[2], alpha.f=0.5), border=NA)
  text(0,0.7,labels=format(round(dias.PP[1],digits=2), nsmall=2), cex=1.5)
  plot(estimate__~GiniZ, m.salud[[1]], type="l", ylim=c(-0.75,0.75), xlim=c(-2.5,2.5), main="Worse Self-rated Health", col=wes_palettes$Royal2[2], lwd=2, ylab="", xlab="Gini [Z]")
  polygon(c(m.salud[[1]]$GiniZ, rev(m.salud[[1]]$GiniZ)), c(smooth(m.salud[[1]]$lower__), rev(smooth(m.salud[[1]]$upper__))), col=adjustcolor(wes_palettes$Royal2[2], alpha.f=0.5), border=NA)
  text(0,0.7,labels=format(round(salud.PP[1],digits=2), nsmall=2), cex=1.5)
  plot(estimate__~GiniZ, m.sumdiag[[1]], type="l", ylim=c(-0.75,0.75), xlim=c(-2.5,2.5), main="Total Morbidity", col=wes_palettes$Royal2[2], lwd=2, ylab="", xlab="Gini [Z]")
  polygon(c(m.sumdiag[[1]]$GiniZ, rev(m.sumdiag[[1]]$GiniZ)), c(smooth(m.sumdiag[[1]]$lower__), rev(smooth(m.sumdiag[[1]]$upper__))), col=adjustcolor(wes_palettes$Royal2[2], alpha.f=0.5), border=NA)
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


