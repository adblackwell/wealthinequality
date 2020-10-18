library(wesanderson)
library(brms)
library(rethinking) #devtools::install_github("rmcelreath/rethinking")
#library(bayesplot)
#library(performance)

load("mainmodelsKids.Rdata")
#### Figures ####
#################

## Calculate conditional effects for plotting. (Conditioned on mean of other variables.)
 
  m.bmi<- conditional_effects(bmi)
  m.sumdiag<- conditional_effects(sumdiag)
  m.infect<- conditional_effects(infect)
  m.resp<- conditional_effects(resp)
  m.gastro<- conditional_effects(gastro)


##Calculate proportion of posterior above zero.
getPP<-function(mod){
  post<-posterior_samples(mod) 
  post$b_absHHWealth<- post$b_HHWealthZ.vil+post$b_MeanWealthZ
  PP<- 0
  for(i in 1:7){
    PP[i]<- sum(post[,i+1]>0)/nrow(post)
  }
  PP[8]<- sum(post[,ncol(post)]>0)/nrow(post)
  names(PP)<- colnames(post[,c(2:8,ncol(post))])
  PP
}

 
  bmi.PP<-getPP(bmi)
  sumdiag.PP<-getPP(sumdiag)
  infect.PP<-getPP(infect)
  resp.PP<-getPP(resp)
  gastro.PP<-getPP(gastro)
  
### relative wealth
tiff("All effects kids.ADB.R2.tiff", compression="lzw", height=9, width=10, units="cm", res=600, pointsize=5)
  
  layout(matrix(c(0,1:6,rep(19,7),0,7:12,rep(20,7),0,13:18,rep(21,7)),byrow=TRUE,ncol=7),widths=c(0.3,1,1,0.3,1,1,1),heights=c(1,0.2,1,0.2,1,0.2))
  par(mar=c(2,2,3,1))
  
  plot(estimate__~HHWealthZ.vil, m.bmi[[3]], type="l", ylim=c(-0.75,0.75), xlim=c(-3,3), main="Body Mass Index", col=wes_palettes$Royal2[3], lwd=2, ylab="", xlab="")
  polygon(c(m.bmi[[3]]$HHWealthZ.vil, rev(m.bmi[[3]]$HHWealthZ.vil)), c(smooth(m.bmi[[3]]$lower__), rev(smooth(m.bmi[[3]]$upper__))), col=adjustcolor(wes_palettes$Royal2[3], alpha.f=0.5), border=NA)
  text(0,0.7,labels=format(round(1-bmi.PP[3],digits=2), nsmall=2), cex=1.5)
  mtext(text="Predicted outcome [Z]",side=2,line=2.5)
  
  plot(estimate__~HHWealthZ.vil, m.sumdiag[[3]], type="l", ylim=c(-0.75,0.75), xlim=c(-3,3), main="Total Morbidity", col=wes_palettes$Royal2[3], lwd=2, ylab="", xlab="")
  polygon(c(m.sumdiag[[3]]$HHWealthZ.vil, rev(m.sumdiag[[3]]$HHWealthZ.vil)), c(smooth(m.sumdiag[[3]]$lower__), rev(smooth(m.sumdiag[[3]]$upper__))), col=adjustcolor(wes_palettes$Royal2[3], alpha.f=0.5), border=NA)
  text(0,0.7,labels=format(round(1-sumdiag.PP[3],digits=2), nsmall=2), cex=1.5)
  
  frame()
  
  plot(estimate__~HHWealthZ.vil, m.infect[[3]], type="l", ylim=c(0,1), xlim=c(-3,3), main="Infections", col=wes_palettes$Royal2[5], lwd=2, ylab="", xlab="")
  polygon(c(m.infect[[3]]$HHWealthZ.vil, rev(m.infect[[3]]$HHWealthZ.vil)), c(smooth(m.infect[[3]]$lower__), rev(smooth(m.infect[[3]]$upper__))), col=adjustcolor(wes_palettes$Royal2[5], alpha.f=0.5), border=NA)
  text(0,0.9,labels=format(round(1-infect.PP[3],digits=2), nsmall=2), cex=1.5)
  mtext("Predicted Probability",side=2,line=2.5)
  
  plot(estimate__~HHWealthZ.vil, m.resp[[3]], type="l", ylim=c(0,1), xlim=c(-3,3), main="Respiratory", col=wes_palettes$Royal2[5], lwd=2, ylab="", xlab="")
  polygon(c(m.resp[[3]]$HHWealthZ.vil, rev(m.resp[[3]]$HHWealthZ.vil)), c(smooth(m.resp[[3]]$lower__), rev(smooth(m.resp[[3]]$upper__))), col=adjustcolor(wes_palettes$Royal2[5], alpha.f=0.5), border=NA)
  text(0,0.9,labels=format(round(1-resp.PP[3],digits=2), nsmall=2), cex=1.5)
  plot(estimate__~HHWealthZ.vil, m.gastro[[3]], type="l", ylim=c(0,1), xlim=c(-3,3), main="Gastrointestinal", col=wes_palettes$Royal2[5], lwd=2, ylab="", xlab="")
  polygon(c(m.gastro[[3]]$HHWealthZ.vil, rev(m.gastro[[3]]$HHWealthZ.vil)), c(smooth(m.gastro[[3]]$lower__), rev(smooth(m.gastro[[3]]$upper__))), col=adjustcolor(wes_palettes$Royal2[5], alpha.f=0.5), border=NA)
  text(0,0.9,labels=format(round(1-gastro.PP[3],digits=2), nsmall=2), cex=1.5)
 
#Community Wealth
  plot(estimate__~MeanWealthZ, m.bmi[[5]], type="l", ylim=c(-0.75,0.75), xlim=c(-1,1), main="Body Mass Index", col=wes_palettes$Royal2[3], lwd=2, ylab="", xlab="")
  polygon(c(m.bmi[[5]]$MeanWealthZ, rev(m.bmi[[5]]$MeanWealthZ)), c(smooth(m.bmi[[5]]$lower__), rev(smooth(m.bmi[[5]]$upper__))), col=adjustcolor(wes_palettes$Royal2[3], alpha.f=0.5), border=NA)
  text(0,0.7,labels=format(round(1-bmi.PP[5],digits=2), nsmall=2), cex=1.5)
  mtext(text="Predicted outcome [Z]",side=2,line=2.5)
  
  plot(estimate__~MeanWealthZ, m.sumdiag[[5]], type="l", ylim=c(-0.75,0.75), xlim=c(-1,1), main="Total Morbidity", col=wes_palettes$Royal2[3], lwd=2, ylab="", xlab="")
  polygon(c(m.sumdiag[[5]]$MeanWealthZ, rev(m.sumdiag[[5]]$MeanWealthZ)), c(smooth(m.sumdiag[[5]]$lower__), rev(smooth(m.sumdiag[[5]]$upper__))), col=adjustcolor(wes_palettes$Royal2[3], alpha.f=0.5), border=NA)
  text(0,0.7,labels=format(round(1-sumdiag.PP[5],digits=2), nsmall=2), cex=1.5)
  
  frame()
  
  plot(estimate__~MeanWealthZ, m.infect[[5]], type="l", ylim=c(0,1), xlim=c(-1,1), main="Infections", col=wes_palettes$Royal2[5], lwd=2, ylab="", xlab="")
  polygon(c(m.infect[[5]]$MeanWealthZ, rev(m.infect[[5]]$MeanWealthZ)), c(smooth(m.infect[[5]]$lower__), rev(smooth(m.infect[[5]]$upper__))), col=adjustcolor(wes_palettes$Royal2[5], alpha.f=0.5), border=NA)
  text(0,0.9,labels=format(round(1-infect.PP[5],digits=2), nsmall=2), cex=1.5)
  mtext("Predicted Probability",side=2,line=2.5)
  
  plot(estimate__~MeanWealthZ, m.resp[[5]], type="l", ylim=c(0,1), xlim=c(-1,1), main="Respiratory", col=wes_palettes$Royal2[5], lwd=2, ylab="", xlab="")
  polygon(c(m.resp[[5]]$MeanWealthZ, rev(m.resp[[5]]$MeanWealthZ)), c(smooth(m.resp[[5]]$lower__), rev(smooth(m.resp[[5]]$upper__))), col=adjustcolor(wes_palettes$Royal2[5], alpha.f=0.5), border=NA)
  text(0,0.9,labels=format(round(1-resp.PP[5],digits=2), nsmall=2), cex=1.5)
  
  plot(estimate__~MeanWealthZ, m.gastro[[5]], type="l", ylim=c(0,1), xlim=c(-1,1), main="Gastrointestinal", col=wes_palettes$Royal2[5], lwd=2, ylab="", xlab="")
  polygon(c(m.gastro[[5]]$MeanWealthZ, rev(m.gastro[[5]]$MeanWealthZ)), c(smooth(m.gastro[[5]]$lower__), rev(smooth(m.gastro[[5]]$upper__))), col=adjustcolor(wes_palettes$Royal2[5], alpha.f=0.5), border=NA)
  text(0,0.9,labels=format(round(1-gastro.PP[5],digits=2), nsmall=2), cex=1.5)
  
  
  #Gini
  plot(estimate__~GiniZ, m.bmi[[1]], type="l", ylim=c(-0.75,0.75), xlim=c(-2.5,2.5), main="Body Mass Index", col=wes_palettes$Royal2[3], lwd=2, ylab="Predicted outcome [Z]", xlab="")
  polygon(c(m.bmi[[1]]$GiniZ, rev(m.bmi[[1]]$GiniZ)), c(smooth(m.bmi[[1]]$lower__), rev(smooth(m.bmi[[1]]$upper__))), col=adjustcolor(wes_palettes$Royal2[3], alpha.f=0.5), border=NA)
  text(0,0.7,labels=format(round(bmi.PP[1],digits=2), nsmall=2), cex=1.5)
  mtext(text="Predicted outcome [Z]",side=2,line=2.5)
  
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
  
  par(mar=c(0,0,0,0))
  frame()
  text(0.5,0.5,"Relative Household Wealth [Z]",adj=c(0.5,0.5),cex=1.5)
  frame()
  text(0.5,0.5,"Mean Community Wealth [Z]",adj=c(0.5,0.5),cex=1.5)
  frame()
  text(0.5,0.5,"Inequality [Gini Index Z]",adj=c(0.5,0.5),cex=1.5)
dev.off()
  
  