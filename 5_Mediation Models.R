library(brms)
library(sjstats)
library(flextable)
library(officer)
### are wealth-health relationships mediated by psychosocial stress? ###
########################################################################
load("mainmodels.Rdata")
alldata<-read.csv("alldataAdults.csv")


medmodel<-function(dependent,mediator,orig,data=alldata, chains = 3, cores = 3, iter = 3000, warmup = 1000, thin=3, control = list(adapt_delta = 0.96)){
  f1<- bf(as.formula(paste0(dependent,"~GiniZ+male+HHWealthZ.vil+AgeZ+MeanWealthZ+DistZ+SizeZ+HHSizeZ+(1|ComunID)+(1|HHId)+(1|pid)+",mediator)))
  f2<-bf(as.formula(paste0(mediator,"~GiniZ+male+HHWealthZ.vil+AgeZ+MeanWealthZ+DistZ+SizeZ+HHSizeZ+(1|ComunID)+(1|HHId)+(1|pid)")))
  #Since the sample is being reduced from the main models, use the parameters from the main models as priors to help keep the estimates in line
  p<-posterior_samples(get(orig))
  if(dependent=="BMI.TZ") dependent2<-"BMITZ" else dependent2<-dependent
  prior=c(prior_string(paste0("normal(",median(p$b_GiniZ),",",sd(p$b_GiniZ)*2,")"), class="b",coef="GiniZ",resp=dependent2),
          prior_string(paste0("normal(",median(p$b_HHWealthZ.vil),",",sd(p$b_HHWealthZ.vil)*2,")"), class="b",coef="HHWealthZ.vil",resp=dependent2),
          prior_string(paste0("normal(",median(p$b_MeanWealthZ),",",sd(p$b_MeanWealthZ)*2,")"), class="b",coef="MeanWealthZ",resp=dependent2))
  if(length(unique(alldata[,dependent]))==2) family=binomial() else family=gaussian()
  model1<- brm(f1 + f2 + set_rescor(FALSE), data=data, family=family, chains = chains, cores = cores, iter = iter, warmup = warmup, thin=thin, control = control,prior=prior)
  return(model1)
}

Dependents<-c("sysBPZ", "diasBPZ", "SaludZ", "SumDiagZ", "InfectiousDiag", "RepiratoryDiag", "GIDiag", "BMI.TZ")
Mediators<-c("DepZ", "ConfZ", "LabZ", "OthPZ", "logCSGZ")
DependentLab<-c("Systolic\nBlood\nPressure", "Diastolic\nBlood\nPressure", "Worse\nSelf-Rated\nHealth", "Total\nMorbidity", "Infectious\nIllness", "Respiratory\nIllness", "GI\nIllness", "BMI")
MediatorLab<-c("Depression", "Conflicts", "Fewer Labor Partners", "Other Problems", "Cortisol")
models<-expand.grid(Dependents=Dependents,Mediators=Mediators,stringsAsFactors = FALSE)
models$orig<- rep(c("sys","dias","salud","sumdiag","infect","resp","gastro","bmi"),length(Mediators))

#Run all models
Medmodels<-list()
for(i in 1:nrow(models)){
  Medmodels[[i]]<-medmodel(models[i,1],models[i,2],models[i,3])
  #save each round so can pick up if something gets interrupted
  save(Medmodels,file="mediationModels.Rdata")
}

#12, 14, 15, 29, 30, 31
#Make Tables for output
##Summarize mediation
MedSummary<-function(mod,treat,mediator,prob=0.95){
    m<-mediation(mod, treat=treat, mediator = mediator,prob=prob)
    m$treat<-treat
    m$mediator<-mediator
    m$dependent<-attr(m, "response")
    m$N<-nrow(mod$data)
    m
}
##Make Tables
Dependents[Dependents=="BMI.TZ"]<-"BMITZ"
MediationTable<-function(treat,treatname){
  tab<-data.frame(effect=NA,value=NA,hdi.low=NA,hdi.high=NA,treat=NA,mediator=NA,dependent=NA,N=NA)
  for(i in 1:40){
    ms<-MedSummary(Medmodels[[i]],treat=treat,models$Mediators[i],0.9)
    tab<-rbind(tab,ms)
  }
  tab<-na.omit(tab)
  tab$out<-paste0(formatC(tab$value,2,format="f")," [",formatC(tab$hdi.low,2,format="f"),";",formatC(tab$hdi.high,2,format="f"),"]")
  
  tab$out[tab$effect=="proportion mediated"]<-paste0(formatC(tab$value[tab$effect=="proportion mediated"]*100,0,format="f"),"% [",formatC(tab$hdi.low[tab$effect=="proportion mediated"]*100,0,format="f"),"%;",formatC(tab$hdi.high[tab$effect=="proportion mediated"]*100,0,format="f"),"%]")
  
  tabr<-reshape(tab,v.names="out",timevar="effect",idvar=c("dependent","mediator"),direction="wide",drop=c("treat","value","hdi.low","hdi.high"))
  tabr$dependent<-DependentLab[match(tabr$dependent,Dependents)]
  tabr$mediator<-MediatorLab[match(tabr$mediator,Mediators)]
  tabr<-tabr[order(tabr$dependent),c(2,1,3,4,5,7,6,8)]
  ft<-flextable(tabr)
  ft<-set_header_labels(ft,values=list(dependent="Dependent",mediator="Mediator",N="N",out.direct=paste0(treatname,"\nDirect Effect"),out.indirect=paste0(treatname,"\nIndirect Effect"),out.mediator="Mediator\nEffect",out.total=paste0(treatname,"\nTotal Effect"),"out.proportion mediated"="Proportion\nMediated"))
  ft<-merge_v(ft,1)
  ft<-hline(ft,i=seq(length(Mediators),nrow(tabr)-length(Mediators),length(Mediators)), border = fp_border(color="gray"))
  ft<-fix_border_issues(ft)
  ft<-font(ft,fontname="Calibri",part="all")
  ft<-fontsize(ft, part="all", size=8)
  ft<-valign(ft,part="all",valign="top")
  ft<-autofit(ft)
  ft<-width(ft,j=1,0.7)
  ft<-width(ft,j=2,1.12)
  ft<-width(ft,j=3,0.39)
  ft<-width(ft,j=4:7,1)
  ft<-width(ft,j=8,1.1)
  ft
}

Ginift<-MediationTable("GiniZ","Gini")
Wealthft<-MediationTable("HHWealthZ.vil","Wealth")
MeanWealthft<-MediationTable("MeanWealthZ","Mean Wealth")

save_as_docx("Table S18: Mediation of Gini effects" = Ginift, "Table S19: Mediation of wealth effects" = Wealthft, "Table S20: Mediation of mean community wealth effects" = MeanWealthft, path = "Mediation tables.docx")

#bayesps for mediator
bps<-Reduce(rbind,lapply(Medmodels, function(x) {
  post<-posterior_samples(x)
  c(median(post[,11]),sum(post[,11]>0)/length(post[,11]))
}))

models$medeffect<-bps[,1]
models$Pg0<-bps[,2]
models[models$Pg0<0.3 | models$Pg0>0.8,]
