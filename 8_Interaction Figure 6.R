#use full models for plotting
library(rethinking)

load("fullinteractionmodels.Rdata")

newdata<-expand.grid(male=c(0,1),HHWealthZ.vil=c(-2,2),GiniZ=seq(-2,2,0.5),AgeZ=1,MeanWealthZ=0,DistZ=0,SizeZ=0,HHSizeZ=0)
mp <- newdata$male==1 & newdata$HHWealthZ.vil==-2
mr <- newdata$male==1 & newdata$HHWealthZ.vil==2
fp <- newdata$male==0 & newdata$HHWealthZ.vil==-2
fr <- newdata$male==0 & newdata$HHWealthZ.vil==2

plotmod<-function(mod,lab,ylab=NA){  
  post<-posterior_samples(mod)
  bps<-apply(post[,c("b_GiniZ:HHWealthZ.vil","b_GiniZ:male","b_male:HHWealthZ.vil")],2,function(x) sum(x>0)/length(x))
  p<-t(apply(newdata,1,function(x){
    pp<-post$b_Intercept+post$b_GiniZ*x[3]+post$b_male*x[1]+post$b_HHWealthZ.vil*x[2]+post$`b_GiniZ:HHWealthZ.vil`*x[3]*x[2]+post$`b_GiniZ:male`*x[3]*x[1]+post$`b_male:HHWealthZ.vil`*x[1]*x[2]+post$b_AgeZ*x[4]
    c(mean(pp),HPDI(pp,0.95))
  }))
  shadeplot<-function(x,col="grey"){
    polygon(c(newdata$GiniZ[x],rev(newdata$GiniZ[x])),c(p[x,2],rev(p[x,3])),col=col.alpha(col,0.6))
    lines(newdata$GiniZ[x],p[x,1])
  }
  par(mar=c(0,1,0,0))
  frame()
  text(0.5,0.6,lab,cex=1.2)
  #
  text(0.5,0.25,paste0(c("GxW:","SxG:","SxW:"), formatC(round(bps,2),2,format="f",drop0trailing=FALSE),collapse="  "),cex=1)
  
  par(mar=c(3,2,1,0))
  plot(0,0,xlim=c(-2,2),ylim=range(p),xlab=NA,ylab=NA,main="Males",type="n",xaxt="n")
  shadeplot(mp,"red")
  shadeplot(mr,"blue")
  axis(1,at=c(-2,0,2))
  mtext(ylab,2,2.5,cex=0.8)
  
  par(mar=c(3,1,1,1))
  plot(0,0,xlim=c(-2,2),ylim=range(p),xlab=NA,ylab=NA,main="Females",type="n",xaxt="n",yaxt="n")
  shadeplot(fp,"red")
  shadeplot(fr,"blue")
  axis(1,at=c(-2,0,2))
  
}

tiff("Figure 6 Interactions.tiff", compression="lzw", height=13, width=20, units="cm", res=350, pointsize=8)
layout(matrix(c(0,1,1,4,4,7,7,10,10,13,13,
                0,2,3,5,6,8,9,11,12,14,15,
                0,16,16,19,19,22,22,25,25,28,28,
                0,17,18,20,21,23,24,26,27,29,30,
                0,0,0,31,31,34,34,37,37,41,41,
                0,0,0,32,33,35,36,38,39,41,41,
                0,0,0,40,40,40,40,40,40,41,41)
              ,byrow=TRUE,ncol=11),heights=c(0.3,1,0.3,1,0.3,1,0.2),widths=c(0.2,rep(1,10)))

labs<-c("Depression","Conflicts","Fewer Labor Partners","Non-social Problems","Cortisol","Body Mass Index","Systolic BP","Diastolic BP","Worse Self-Rated Health","Total Morbidity","Infection","Respiratory","Gastrointestinal")
for(i in 1:13){
  ylab<-ifelse(i %in% c(1,6),"Predicted Outcome [Z]",ifelse(i==11,"Log Odds Ratio",NA))
  plotmod(list(dep8,conf8,lab8,othp8,cort8,bmi8,sys8,dias8,salud8,sumdiag8,infect8,resp8,gastro8)[[i]],labs[i],ylab)
}
par(mar=c(0,0,0,0))
frame()
text(0.5,0.5,"Gini [Z]",cex=1.2)
frame()
legend(0.5,0.75,legend=c("Wealth = +2","Wealth = -2"),fill=c("blue","red"),xjust=0.5,yjust=0.5,cex=1.5,bty="n")
dev.off()
