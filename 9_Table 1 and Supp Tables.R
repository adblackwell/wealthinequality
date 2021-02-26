library(wesanderson)
library(brms)
library(rethinking) 
library(flextable)
library(officer)
#If rethinking is missing
#devtools::install_github("rmcelreath/rethinking")

#### Tables ####
################
alldatakids<-read.csv("alldataKids.csv")
## Table 1: descriptives
alldatakids$depends<-rowSums(!is.na(alldatakids[,c("BMI.TZ","SumDiagZ","InfectiousDiag","RepiratoryDiag","GIDiag")]))
alldatakids$inds<-rowSums(!is.na(alldatakids[,c("AgeZ","SizeZ","male","Total.value","DistZ","MeanWealthZ","HHSizeZ","ComunID","HHId","pid")]))

#overall descriptive
alldatakids<-alldatakids[alldatakids$depends>0 & alldatakids$inds==10,]
alldatakids3<-aggregate(alldatakids[,c("BMI","sum.diag","InfectiousDiag","RepiratoryDiag","GIDiag","Age","male","AgeCorWealth","ComunSize", "route.distance.SB","MeanWealth.AgeCor","Gini.AgeCor")],by=list(pid=alldatakids$pid),FUN=mean,na.rm=TRUE)

alldataadults<-read.csv("alldataAdults.csv")
alldataadults$depends<-rowSums(!is.na(alldataadults[,c("DepressionScore","Conflicts","LaborPartners","OtherProblems","CSG","BMI","sysBP","diasBP","SaludGeneral","sum.diag","InfectiousDiag","RepiratoryDiag","GIDiag")]))
alldataadults$inds<-rowSums(!is.na(alldataadults[,c("AgeZ","SizeZ","male","HHWealthZ.vil","DistZ","MeanWealthZ","HHSizeZ","ComunID","HHId","pid")]))

#overall descriptive
alldataadults<-alldataadults[alldataadults$depends>0 & alldataadults$inds==10,]
alldataadults3<-aggregate(alldataadults[,c("DepressionScore","Conflicts","LaborPartners","OtherProblems","CSG","BMI","sysBP","diasBP","SaludGeneral","sum.diag","InfectiousDiag","RepiratoryDiag","GIDiag","Age","male")],by=list(pid=alldataadults$pid),FUN=mean,na.rm=TRUE)

hhdata<-read.csv("HHWealthWGini.csv")
hhdata3<-aggregate(hhdata[,c("HHSize","AgeCorWealth")],by=list(pid=hhdata$HHId),FUN=mean,na.rm=TRUE)

comdata<-aggregate(hhdata[,c("ComunSize", "route.distance.SB","MeanWealth.AgeCor","Gini.AgeCor")],by=list(ComunID=hhdata$ComunID,Round=hhdata$Round),FUN=mean,na.rm=TRUE) 
comdata3<-aggregate(hhdata[,c("ComunSize", "route.distance.SB","MeanWealth.AgeCor","Gini.AgeCor")],by=list(ComunID=hhdata$ComunID),FUN=mean,na.rm=TRUE)

desc_stats<-function(x){
  count<-sum(!is.na(x))
  if(length(unique(na.omit(x)))==2) out<-c(count,mean(x,na.rm=TRUE)*100,NA,NA,NA)
  else out<-c(count,median(x,na.rm=TRUE),sd(x,na.rm=TRUE),range(x,na.rm=TRUE))
  out
}

fta<-t(apply(alldataadults[,c("DepressionScore","Conflicts","LaborPartners","OtherProblems","CSG","BMI","sysBP","diasBP","SaludGeneral","sum.diag","InfectiousDiag","RepiratoryDiag","GIDiag","Age","male")],2,desc_stats))
fta2<-t(apply(alldataadults3[,c("DepressionScore","Conflicts","LaborPartners","OtherProblems","CSG","BMI","sysBP","diasBP","SaludGeneral","sum.diag","InfectiousDiag","RepiratoryDiag","GIDiag","Age","male")],2,desc_stats))
fta<-cbind(fta,fta2[,1])
colnames(fta)<-c("Obs","Median","SD","Low","High","N")
dummy<-rep(NA,6)
fta<-rbind(dummy,fta[1:5,],dummy,fta[6:13,],dummy,fta[14:15,])
fta<-data.frame(fta)
fta$Variable<-c("Adult Outcomes: Psychosocial","Depression [sum score]","Conflicts [count]","Labor partners [count] a","Non-social problems [count]","Urinary cortisol [pg/ml]","Adult Outcomes: Health", "BMI [kg/m2] b","Systolic blood pressure [mmHg]","Diastolic blood pressure [mmHg]","Self-rated health [scale] a","Total morbidity [count] c","Infections/parasites [yes/no] c","Respiratory disease [yes/no] c","Gastrointestinal [yes/no] c","Adult Predictors","Age [years]","Sex [0=female, 1=male]")

ftk<-t(apply(alldatakids[,c("BMI","sum.diag","InfectiousDiag","RepiratoryDiag","GIDiag","Age","male")],2,desc_stats))
ftk2<-t(apply(alldatakids3[,c("BMI","sum.diag","InfectiousDiag","RepiratoryDiag","GIDiag","Age","male")],2,desc_stats))
ftk<-cbind(ftk,ftk2[,1])
colnames(ftk)<-c("Obs","Median","SD","Low","High","N")
dummy<-rep(NA,6)
ftk<-rbind(dummy,ftk[1:5,],dummy,ftk[6:7,])
ftk<-data.frame(ftk)
ftk$Variable<-c("Juvenile Outcomes: Health", "BMI [kg/m2] b","Total morbidity [count] c","Infections/parasites [yes/no] c","Respiratory disease [yes/no] c","Gastrointestinal [yes/no] c","Juvenile Predictors","Age [years]","Sex [0=female, 1=male]")

fthh<-t(apply(hhdata[,c("HHSize","AgeCorWealth")],2,desc_stats))
fthh2<-t(apply(hhdata3[,c("HHSize","AgeCorWealth")],2,desc_stats))
fthh<-cbind(fthh,fthh2[,1])
colnames(fthh)<-c("Obs","Median","SD","Low","High","N")
fthh<-rbind(dummy,fthh)
fthh<-data.frame(fthh)
fthh$Variable<-c("Household Predictors","Household size","Household wealth [Bs]")

ftcom<-t(apply(comdata[,c("ComunSize", "route.distance.SB","MeanWealth.AgeCor","Gini.AgeCor")],2,desc_stats))
ftcom2<-t(apply(comdata3[,c("ComunSize", "route.distance.SB","MeanWealth.AgeCor","Gini.AgeCor")],2,desc_stats))
ftcom<-cbind(ftcom,ftcom2[,1])
colnames(ftcom)<-c("Obs","Median","SD","Low","High","N")
ftcom<-rbind(dummy,ftcom)
ftcom<-data.frame(ftcom)

ftcom$Variable<-c("Community Predictors","Community size [Adults >15]","Distance to market town [km]","Mean community wealth [Bs]","Community wealth inequality [Gini]")

ftall<-rbind(fta,ftk,fthh,ftcom)

ft<-flextable(ftall,col_keys = c("Variable","N","Obs","Median","SD","Low","High"))
ft<-colformat_num(ft,j=c("N","Obs"),digits=0) 
ft<-colformat_num(ft,j=c("Median","SD","Low","High"),digits=1)  
ft<-colformat_num(ft,j=c("Median","SD","Low","High"),digits=2,i="Gini.AgeCor")  
ft<-colformat_num(ft,j=c("Median","SD","Low","High"),digits=0,i=c("AgeCorWealth","MeanWealth.AgeCor"))  
ft<-hline(ft,i=(which(is.na(ftall$Obs))-1)[-1], border = fp_border(color="gray"))
ft<-bold(ft,i=which(is.na(ftall$Obs)))
ft<-bold(ft,part="header")
ft<-autofit(ft)
ft<-add_footer_lines(ft,values=c("a Reverse coded in analyses to make higher values worse outcomes.","b Whether higher or lower BMI is better is a bit ambiguous: in high-income countries higher BMI is associated with worse health, lower status and greater inequality, whereas in low-income countries the reverse may be true.","c see Supplementary Table S1 for an overview of the most common morbidities by category."))
ft<-font(ft,fontname="Calibri",part="all")
ft<-fontsize(ft, part="all", size=10)

save_as_docx("Table 1" = ft, path = "Table 1.docx")

#village data
hhdata$PercentSampled<-(hhdata$HHCnt/hhdata$ComunSize)*100
comdata4<-aggregate(hhdata[,c("ComunSize","PercentSampled","route.distance.SB","HHWealthZ","Gini.AgeCor")],by=list(ComunID=hhdata$ComunID),FUN=mean,na.rm=TRUE)
cor(comdata4[,-1])
cor.test(comdata4$HHWealthZ,comdata4$route.distance.SB)
cor.test(comdata4$Gini.AgeCor,comdata4$route.distance.SB)
cor.test(comdata4$Gini.AgeCor,comdata4$HHWealthZ)
cor.test(comdata4$ComunSize ,comdata4$route.distance.SB)
cor.test(comdata4$ComunSize ,comdata4$HHWealthZ)
cor.test(comdata4$ComunSize ,comdata4$Gini.AgeCor)
## supplementary tables: full model summary


modeltable<-function(models,heads=c("Relative Wealth","Absolute Wealth","No Covariates")){
  summs<-lapply(models,function(mod){
    post<-posterior_samples(mod)
    vars<-names(post)[grep("^(b_|sd_)",names(post))]
    summ<-t(apply(post[,vars],2,function(x) c(mean(x),HPDI(x,prob=0.95))))
    summ<- rbind(summ,zR2=bayes_R2(mod)[c(1,3:4)])
    names(summ)<-c("Mean","Lower 95% CI","Upper 95%CI")
    summ<-data.frame(summ)
    summ$Variable<-row.names(summ)
    summ$N<-NA
    summ$N[summ$Variable=="zR2"]<-nrow(mod$data)
    summ$N[summ$Variable=="sd_pid__Intercept"]<-length(grep("^r_pid",names(post)))
    summ$N[summ$Variable=="sd_HHId__Intercept"]<-length(grep("^r_HHId",names(post)))
    summ$N[summ$Variable=="sd_ComunID__Intercept"]<-length(grep("^r_ComunID",names(post)))
    summ
  })

  summs2<-Reduce(function(x,y) merge(x,y,all=TRUE,by="Variable") ,summs)
  variables<-c("b_Intercept","b_GiniZ","b_HHWealthZ.vil","b_HHWealthZ", "b_MeanWealthZ","b_AgeZ","b_male","b_HHSizeZ","b_DistZ","b_SizeZ","sd_ComunID__Intercept","sd_HHId__Intercept","sd_pid__Intercept","zR2")
  summs2<-summs2[na.omit(match(variables,summs2$Variable)),]
  summs2$Variable<-c("Intercept","Gini","Community Relative Wealth a","Sample Relative Wealth b", "Mean Community Wealth c","Age","Sex=male","Household Size","Distance to San Borja","Community Size","sd(Community)","sd(Household)","sd(Individual)","Goodness of fit (R2)")[na.omit(match(summs2$Variable,variables))]
  rEF<-match(c("sd(Community)","sd(Household)","sd(Individual)"),summs2$Variable)
  summs2$Variable[rEF]<-paste0(summs2$Variable[rEF]," N=",summs2$N[rEF])
  N<-summs2$N[summs2$Variable=="Goodness of fit (R2)"]
  summs2<-summs2[,!(names(summs2) %in% c("N.x","N.y","N"))]
  ft2<-flextable(summs2)
  ft2<-autofit(ft2)
  ft2<-set_header_labels(ft2,values=list(Variable=paste0("N=",N,"\nVariable"), V1.x="Mean",X.0.95.x="Lower\n95% CI",X0.95..x="Upper\n95%CI",V1.y="Mean",X.0.95.y="Lower\n95% CI",X0.95..y="Upper\n95%CI",V1="Mean",X.0.95="Lower 95%\nCI",X0.95.="Upper\n95%CI"))
  ft2<-colformat_num(ft2,digits=2) 
  ft2<-vline(ft2,j=c(4,7), border = fp_border(color="gray"))
  ft2<-add_header_row(ft2,values=c("",heads),colwidths=c(1,rep(3,length(heads))))
  ft2<-add_footer_lines(ft2,values=c("a Z-score centered on community mean","b Z-score centered on full sample","c Mean full sample Z-score for the community"))
  ft2<-font(ft2,fontname="Calibri",part="all")
  ft2<-fontsize(ft2, part="all", size=9)
  ft2<-valign(ft2,part="header",valign="bottom")
  ft2<-align(ft2,part="all",align="center",j= -1)
  ft2<-align(ft2,part="all",align="left",j= 1)
  ft2<-width(ft2,j=1,1.8)
  ft2<-width(ft2,j=-1,0.5)
  ft2
}

models<-c("dep", "conf", "lab", "othp", "cort", "sys", "dias", "salud", "sumdiag", "infect", "resp", "gastro", "bmi")
load("mainmodelsAbsZ.Rdata")
for(m in models) assign(paste0(m,"Abs"),get(m))
load("mainmodelsNocov.Rdata")
for(m in models) assign(paste0(m,"NoCov"),get(m))
load("mainmodels.Rdata")

depft<-modeltable(list(dep,depAbs,depNoCov))
confft<-modeltable(list(conf,confAbs,confNoCov))
labft<-modeltable(list(lab,labAbs,labNoCov))
othpft<-modeltable(list(othp,othpAbs,othpNoCov))
cortft<-modeltable(list(cort,cortAbs,cortNoCov))
bmift<-modeltable(list(bmi,bmiAbs,bmiNoCov))
sysft<-modeltable(list(sys,sysAbs,sysNoCov))
diasft<-modeltable(list(dias,diasAbs,diasNoCov))
saludft<-modeltable(list(salud,saludAbs,saludNoCov))
sumdiagft<-modeltable(list(sumdiag,sumdiagAbs,sumdiagNoCov))
infectft<-modeltable(list(infect,infectAbs,infectNoCov))
infectft<-add_footer_lines(infectft,values=c("Note: Model is on the logit scale"))
infectft<-font(infectft,fontname="Calibri",part="all")
infectft<-fontsize(infectft, part="all", size=9)
respft<-modeltable(list(resp,respAbs,respNoCov))
respft<-add_footer_lines(respft,values=c("Note: Model is on the logit scale"))
respft<-font(respft,fontname="Calibri",part="all")
respft<-fontsize(respft, part="all", size=9)
gastroft<-modeltable(list(gastro,gastroAbs,gastroNoCov))
gastroft<-add_footer_lines(gastroft,values=c("Note: Model is on the logit scale"))
gastroft<-font(gastroft,fontname="Calibri",part="all")
gastroft<-fontsize(gastroft, part="all", size=9)

load("mainmodelsKids.Rdata")

#Just use old function and then modify in word so I don't need to write new function. BMI duplicated is just filler
kid1<-modeltable(list(bmi,sumdiag,bmi),c("BMI","Total Morbidity","BMI"))
kid2<-modeltable(list(infect, resp, gastro),c("Infection","Repiratory","Gastrointestinal"))

save_as_docx("Table S2: Model summary – Depression" = depft, "Table S3: Model summary – Social conflicts" = confft,"Table S4: Model summary – Fewer Labor partners" = labft,"Table S5: Model summary – Non-social problems" = othpft,"Table S6: Model summary – Cortisol" = cortft," Table S7: Model summary – BMI" = bmift, "Table S8: Model summary – Systolic blood pressure" = sysft, "Table S9: Model summary – Diastolic blood pressure" = diasft, "Table S10: Model summary – Worse Self-rated health" = saludft, "Table S11: Model summary – Total morbidity" = sumdiagft, "Table S12: Model summary – Infections" = infectft,"Table S13: Model summary – Respiratory illness" = respft, "Table S14: Model summary – Gastrointestinal illness" = gastroft, "Table S15: Gaussian model summaries for juveniles"=kid1,"Table S16: Logistic model summaries for juveniles"=kid2, path = "Supplement Tables.docx")
