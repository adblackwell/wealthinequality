#forest plot
library(wesanderson)
library(brms)
library(rethinking) #devtools::install_github("rmcelreath/rethinking")
#library(bayesplot)
#library(performance)

load("mainmodels.Rdata")


##Calculate proportion of posterior above zero.
getPP<-function(mod){
  post<-posterior_samples(mod) 
  PP<- 0
  Med<-0
  HPDI.L<-0
  HPDI.H<-0
  HPDI2.L<-0
  HPDI2.H<-0
  for(i in 2:9){
    PP[i-1]<- sum(post[,i]>0)/nrow(post)
    Med[i-1]<- median(post[,i])
    HI<-HPDI(post[,i],prob=0.95)
    HPDI.L[i-1]<-HI[1]
    HPDI.H[i-1]<-HI[2]
    HI2<-HPDI(post[,i],prob=0.75)
    HPDI2.L[i-1]<-HI2[1]
    HPDI2.H[i-1]<-HI2[2]
  }
  out<-rbind(PP,Med,HPDI.L,HPDI.H,HPDI2.L,HPDI2.H)
  colnames(out)<- colnames(post[,2:9])
  out
}

sum1<-list(
dep.PP<-getPP(dep),
conf.PP<-getPP(conf),
lab.PP<-getPP(lab),
othp.PP<-getPP(othp),
cort.PP<-getPP(cort))

sum2<-list(
bmi.PP<-getPP(bmi),
sys.PP<-getPP(sys),
dias.PP<-getPP(dias),
salud.PP<-getPP(salud),
sumdiag.PP<-getPP(sumdiag))

sum3<-list(
infect.PP<-getPP(infect),
resp.PP<-getPP(resp),
gastro.PP<-getPP(gastro))



forplot<-function(posts,col,colm,xlim=c(-1,1),labs=NA){
  plot(0,0,xlim=xlim,ylim=c(-length(posts)-1,0),type="n",yaxt="n",ylab=NA,xlab=NA)
  for(i in 1:length(posts)){
    use<-posts[[i]]
    points(use[2,colm],-i,pch=19,cex=2,col=col)
    lines(c(use[3,colm],use[4,colm]),c(-i,-i),col=col,lwd=2)
    lines(c(use[5,colm],use[6,colm]),c(-i,-i),col=col,lwd=4)
    text(xlim[1],-i,formatC(use[1,colm],2,format="f",drop0trailing=FALSE),adj=c(0,0.5))
  }
  axis(2,at=seq(-length(posts),-1,1),labels = rev(labs),las=1)
  abline(v=0)
}

tiff("Forest Plot covariates.R2.tiff", compression="lzw", height=10, width=16, units="cm", res=600, pointsize=7)

layout(matrix(c(0,16,17,18,19,20, 0,1,2,3,4,5, 0,6,7,8,9,10, 0,21,21,21,21,21, 0,11,12,13,14,15, 0,22,22,22,22,22),byrow=TRUE,ncol=6),widths=c(0.7,1,1,1,1,1),heights=c(0.8,6,6,0.5,4,0.5))
par(mar=c(2,1,1,1))
lab1<-c("Depression","Conflicts","Fewer Labor Partners","Non-social Problems","Cortisol")
lab2<-c("Body Mass Index","Systolic BP","Diastolic BP","Worse Self-Rated Health","Total Morbidity")
lab3<-c("Infection","Respiratory","Gastrointestinal")
forplot(sum1,wes_palettes$Royal2[1],2,c(-1.2,1),lab1)
forplot(sum1,wes_palettes$Royal2[1],4,c(-0.6,0.5))
forplot(sum1,wes_palettes$Royal2[1],6,c(-0.6,0.5))
forplot(sum1,wes_palettes$Royal2[1],7,c(-0.6,0.5))
forplot(sum1,wes_palettes$Royal2[1],8,c(-0.6,0.5))

forplot(sum2,wes_palettes$Royal2[3],2,c(-0.6,0.5),lab2)
forplot(sum2,wes_palettes$Royal2[3],4,c(-0.6,0.5))
forplot(sum2,wes_palettes$Royal2[3],6,c(-0.6,0.5))
forplot(sum2,wes_palettes$Royal2[3],7,c(-0.6,0.5))
forplot(sum2,wes_palettes$Royal2[3],8,c(-0.3,0.3))

forplot(sum3,wes_palettes$Royal2[5],2,c(-1.2,1),lab3)
forplot(sum3,wes_palettes$Royal2[5],4,c(-1.2,1))
forplot(sum3,wes_palettes$Royal2[5],6,c(-1.2,1))
forplot(sum3,wes_palettes$Royal2[5],7,c(-1.2,1))
forplot(sum3,wes_palettes$Royal2[5],8,c(-1.2,1))

par(mar=c(0,0,0,0))
frame()
text(0.5,0.5,"Sex=male",adj=c(0.5,0.5),cex=1)
frame()
text(0.5,0.5,"Age [Z]",adj=c(0.5,0.5),cex=1)
frame()
text(0.5,0.5,"Distance to Town [Z]",adj=c(0.5,0.5),cex=1)
frame()
text(0.5,0.5,"Community Size [Z]",adj=c(0.5,0.5),cex=1)
frame()
text(0.5,0.5,"Household Size [Z]",adj=c(0.5,0.5),cex=1)

frame()
text(0.5,0.5,"Standardized Parameter [sd/sd]",adj=c(0.5,0.75),cex=1)
frame()
text(0.5,0.5,"Logistic Parameter [log odds/sd]",adj=c(0.5,0.75),cex=1)

dev.off()

#Kids

load("mainmodelsKids.Rdata")

sum2<-list(
  bmi.PP<-getPP(bmi),
  sumdiag.PP<-getPP(sumdiag))

sum3<-list(
  infect.PP<-getPP(infect),
  resp.PP<-getPP(resp),
  gastro.PP<-getPP(gastro))


tiff("Forest Plot Kids Covariates.R2.tiff", compression="lzw", height=6, width=11, units="cm", res=600, pointsize=7)

layout(matrix(c(0,11,12,13,14,15, 0,1,2,3,4,5, 0,16,16,16,16,16, 0,6,7,8,9,10, 0,17,17,17,17,17),byrow=TRUE,ncol=6),widths=c(0.7,1,1,1,1,1),heights=c(0.8,3,0.5,4,0.5))
par(mar=c(2,1,1,1))
lab2<-c("Body Mass Index","Total Morbidity")
lab3<-c("Infection","Respiratory","Gastrointestinal")

forplot(sum2,wes_palettes$Royal2[3],2,c(-0.6,0.5),lab2)
forplot(sum2,wes_palettes$Royal2[3],4,c(-0.6,0.5))
forplot(sum2,wes_palettes$Royal2[3],6,c(-0.6,0.5))
forplot(sum2,wes_palettes$Royal2[3],7,c(-0.6,0.5))
forplot(sum2,wes_palettes$Royal2[3],8,c(-0.3,0.3))

forplot(sum3,wes_palettes$Royal2[5],2,c(-1.2,1),lab3)
forplot(sum3,wes_palettes$Royal2[5],4,c(-3.6,3))
forplot(sum3,wes_palettes$Royal2[5],6,c(-1.2,1))
forplot(sum3,wes_palettes$Royal2[5],7,c(-1.2,1))
forplot(sum3,wes_palettes$Royal2[5],8,c(-1.2,1))

par(mar=c(0,0,0,0))
frame()
text(0.5,0.5,"Sex=male",adj=c(0.5,0.5),cex=1)
frame()
text(0.5,0.5,"Age [Z]",adj=c(0.5,0.5),cex=1)
frame()
text(0.5,0.5,"Distance to Town [Z]",adj=c(0.5,0.5),cex=1)
frame()
text(0.5,0.5,"Community Size [Z]",adj=c(0.5,0.5),cex=1)
frame()
text(0.5,0.5,"Household Size [Z]",adj=c(0.5,0.5),cex=1)

frame()
text(0.5,0.5,"Standardized Parameter [sd/sd]",adj=c(0.5,0.75),cex=1)
frame()
text(0.5,0.5,"Logistic Parameter [log odds/sd]",adj=c(0.5,0.75),cex=1)

dev.off()
