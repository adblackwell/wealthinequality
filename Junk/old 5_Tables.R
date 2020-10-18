library(wesanderson)
library(brms)
library(rethinking) 
library(flextable)
library(officer)
#devtools::install_github("rmcelreath/rethinking")

#### Tables ####
################
load("alldata.Rdata")
## Table 1: descriptives
alldata$depends<-rowSums(!is.na(alldata[,c("DepZ","ConfZ","LabZ","OthPZ","logCSGZ","BMI.TZ","sysBPZ","diasBPZ","SaludZ","SumDiagZ","InfectiousDiag","RepiratoryDiag","GIDiag")]))
alldata$inds<-rowSums(!is.na(alldata[,c("AgeZ","SizeZ","male","Total.value","DistZ","MeanWealthZ","ComunID","HHId","pid")]))
  
#overall descriptive
alldata2<-alldata[alldata$depends>0 & alldata$inds==9,]
  
desc_stats<-function(x){
  count<-sum(!is.na(x))
  if(length(unique(na.omit(x)))==2) out<-c(count,mean(x,na.rm=TRUE)*100,NA,NA,NA)
  else out<-c(count,median(x,na.rm=TRUE),sd(x,na.rm=TRUE),range(x,na.rm=TRUE))
  out
  }
  
ft<-t(apply(alldata2[,c("DepressionScore","Conflicts","LaborPartners","OtherProblems","CSG","BMI","sysBP","diasBP","SaludGeneral","sum.diag","InfectiousDiag","RepiratoryDiag","GIDiag","Age","male","AgeCorWealth","ComunSize", "route.distance.SB","MeanWealth.AgeCor","Gini.AgeCor")],2,desc_stats))
colnames(ft)<-c("N","Median","SD","Low","High")
dummy<-rep(NA,5)
ft<-rbind(dummy,ft[1:5,],dummy,ft[6:13,],dummy,ft[14:20,])
ft<-data.frame(ft)
ft$Variable<-c("Outcomes: Psychosocial","Depression [sum score]","Conflicts [count]","Labor Partners [count] a","Non-social Problems [count]","Urinary Cortisol [pg/ml]","Outcomes: Health", "BMI [kg/m2] b","Systolic Blood Pressure [mmHg]","Diastolic Blood Pressure [mmHg]","Self-rated Health [scale] a","Total Morbidity [count] c","Infections/parasites [yes/no] c","Respiratory disease [yes/no] c","Gastrointestinal [yes/no] c","Predictors","Age [years]","Sex [0=female, 1=male]","Household wealth [Bs]","Community Size [Adults >15]","Distance to market town [km]","Mean Community Wealth [Bs]","Community Wealth inequality [Gini]")
ft<-flextable(ft,col_keys = c("Variable","N","Median","SD","Low","High"))
ft<-colformat_num(ft,j="N",digits=0) 
ft<-colformat_num(ft,j=c("Median","SD","Low","High"),digits=1)  
ft<-colformat_num(ft,j=c("Median","SD","Low","High"),digits=2,i="Gini.AgeCor")  
ft<-colformat_num(ft,j=c("Median","SD","Low","High"),digits=0,i=c("CSG","AgeCorWealth","MeanWealth.AgeCor"))  
ft<-hline(ft,i=6, border = fp_border(color="gray"))
ft<-hline(ft,i=15, border = fp_border(color="gray"))
ft<-bold(ft,i=c(1,7,16))
ft<-bold(ft,part="header")
ft<-autofit(ft)
ft<-add_footer_lines(ft,values=c("a Reverse coded in analyses to make higher values worse outcomes.","b Whether higher or lower BMI is better is a bit ambiguous: in high-income countries higher BMI is associated with worse health, lower status and greater inequality, whereas in low-income countries the reverse may be true.","c see Supplementary Table S1 for an overview of the most common morbidities by category."))
ft<-font(ft,fontname="Calibri",part="all")
ft<-fontsize(ft, part="all", size=10)

save_as_docx("Table 1" = ft, path = "Table 1.docx")

## supplementary tables: full model summary
models<-c("dep", "conf", "lab", "othp", "cort", "sys", "dias", "salud", "sumdiag", "infect", "resp", "gastro", "bmi")
load("mainmodelsAbsZ.Rdata")
for(m in models) assign(paste0(m,"Abs"),get(m))
load("mainmodelsNocov.Rdata")
for(m in models) assign(paste0(m,"NoCov"),get(m))
load("mainmodels.Rdata")

#load("mainmodelsStandardized.Rdata")

modeltable<-function(models){
  summs<-lapply(models,function(mod){
    post<-posterior_samples(mod)
    vars<-names(post)[grep("^(b_|sd_)",names(post))]
    summ<-t(apply(post[,vars],2,function(x) c(mean(x),HPDI(x,prob=0.95))))
    summ<- rbind(summ,zR2=bayes_R2(mod)[c(1,3:4)])
    names(summ)<-c("Mean","Lower 95% CI","Upper 95%CI")
    summ<-data.frame(summ)
    summ$Variable<-row.names(summ)
    summ
  })
  summs2<-Reduce(function(x,y) merge(x,y,all=TRUE,by="Variable") ,summs)
  summs2<-summs2[c(6,3,5,4,8,1,7,2,9,10:13),]
  summs2$Variable<-c("Intercept","Gini","Community Relative Wealth a","Sample Relative Wealth b", "Mean Community Wealth c","Age","Sex=male","Distance to San Borja","Community Size","sd(Community)","sd(Household)","sd(Individual)","Goodness of fit (R2)")
  ft2<-flextable(summs2)
  ft2<-autofit(ft2)
  ft2<-set_header_labels(ft2,values=list(V1.x="Mean",X.0.95.x="Lower\n5% CI",X0.95..x="Upper\n95%CI",V1.y="Mean",X.0.95.y="Lower\n95% CI",X0.95..y="Upper\n95%CI",V1="Mean",X.0.95="Lower 95%\nCI",X0.95.="Upper\n95%CI"))
  ft2<-colformat_num(ft2,digits=2) 
  ft2<-vline(ft2,j=c(4,7), border = fp_border(color="gray"))
  ft2<-add_header_row(ft2,values=c("","Relative Wealth","Absolute Wealth","No Covariates"),colwidths=c(1,3,3,3))
  ft2<-add_footer_lines(ft2,values=c("a Z-score centered on community mean","b Z-score centered on full sample","c Mean full sample Z-score for the community"))
  ft2<-font(ft2,fontname="Calibri",part="all")
  ft2<-fontsize(ft2, part="all", size=9)
  ft2<-align(ft2,part="all",align="center",j= -1)
  ft2<-align(ft2,part="all",align="left",j= 1)
  ft2<-width(ft2,j=1,1.8)
  ft2<-width(ft2,j=-1,0.5)
}


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

save_as_docx("Table S2: Model summary – Depression" = depft, "Table S3: Model summary – Social conflicts" = confft,"Table S4: Model summary – Fewer Labor partners" = labft,"Table S5: Model summary – Non-social problems" = othpft,"Table S6: Model summary – Cortisol" = cortft," Table S7: Model summary – BMI" = bmift, "Table S8: Model summary – Systolic blood pressure" = sysft, "Table S9: Model summary – Diastolic blood pressure" = diasft, "Table S10: Model summary – Worse Self-rated health" = saludft, "Table S11: Model summary – Total morbidity" = sumdiagft, "Table S12: Model summary – Infections" = infectft,"Table S13: Model summary – Respiratory illness" = respft, "Table S14: Model summary – Gastrointestinal illness" = gastroft,  path = "Supplement Tables.docx")
