library(brms)
library(rethinking)

## load models
load("mainmodels.Rdata")

# standard deviation for bernoulli models (for gaussian models, it's sigma)
logitsd<-pi/sqrt(3)

# dep
post.dep<- posterior_samples(dep)[,c("b_GiniZ", "b_male", "b_HHWealthZ.vil", "b_AgeZ", "b_MeanWealthZ", "b_DistZ", "b_SizeZ", "sigma")]
dep.d<- data.frame(effect=c("b_GiniZ", "b_male", "b_HHWealthZ.vil", "b_AgeZ", "b_MeanWealthZ", "b_DistZ", "b_SizeZ"), d_mean=NA, d_median=NA, d_mad=NA, d_lwr.hpdi=NA, d_upr.hpdi=NA)
dep.d$d_mean<-apply(post.dep[,1:7], 2, function(x) mean(x/post.dep[,"sigma"])) 
dep.d$d_median<-apply(post.dep[,1:7], 2, function(x) median(x/post.dep[,"sigma"])) 
dep.d$d_mad<-apply(post.dep[,1:7], 2, function(x) mad(x/post.dep[,"sigma"])) 
dep.d$d_lwr.hpdi<-apply(post.dep[,1:7], 2, function(x) HPDI(x/post.dep[,"sigma"], prob=0.95))[1,]
dep.d$d_upr.hpdi<-apply(post.dep[,1:7], 2, function(x) HPDI(x/post.dep[,"sigma"], prob=0.95))[2,]

# conf
post.conf<- posterior_samples(conf)[,c("b_GiniZ", "b_male", "b_HHWealthZ.vil", "b_AgeZ", "b_MeanWealthZ", "b_DistZ", "b_SizeZ", "sigma")]
conf.d<- data.frame(effect=c("b_GiniZ", "b_male", "b_HHWealthZ.vil", "b_AgeZ", "b_MeanWealthZ", "b_DistZ", "b_SizeZ"), d_mean=NA, d_median=NA, d_mad=NA, d_lwr.hpdi=NA, d_upr.hpdi=NA)
conf.d$d_mean<-apply(post.conf[,1:7], 2, function(x) mean(x/post.conf[,"sigma"])) 
conf.d$d_median<-apply(post.conf[,1:7], 2, function(x) median(x/post.conf[,"sigma"])) 
conf.d$d_mad<-apply(post.conf[,1:7], 2, function(x) mad(x/post.conf[,"sigma"])) 
conf.d$d_lwr.hpdi<-apply(post.conf[,1:7], 2, function(x) HPDI(x/post.conf[,"sigma"], prob=0.95))[1,]
conf.d$d_upr.hpdi<-apply(post.conf[,1:7], 2, function(x) HPDI(x/post.conf[,"sigma"], prob=0.95))[2,]

# lab
post.lab<- posterior_samples(lab)[,c("b_GiniZ", "b_male", "b_HHWealthZ.vil", "b_AgeZ", "b_MeanWealthZ", "b_DistZ", "b_SizeZ", "sigma")]
lab.d<- data.frame(effect=c("b_GiniZ", "b_male", "b_HHWealthZ.vil", "b_AgeZ", "b_MeanWealthZ", "b_DistZ", "b_SizeZ"), d_mean=NA, d_median=NA, d_mad=NA, d_lwr.hpdi=NA, d_upr.hpdi=NA)
lab.d$d_mean<-apply(post.lab[,1:7], 2, function(x) mean(x/post.lab[,"sigma"])) 
lab.d$d_median<-apply(post.lab[,1:7], 2, function(x) median(x/post.lab[,"sigma"])) 
lab.d$d_mad<-apply(post.lab[,1:7], 2, function(x) mad(x/post.lab[,"sigma"])) 
lab.d$d_lwr.hpdi<-apply(post.lab[,1:7], 2, function(x) HPDI(x/post.lab[,"sigma"], prob=0.95))[1,]
lab.d$d_upr.hpdi<-apply(post.lab[,1:7], 2, function(x) HPDI(x/post.lab[,"sigma"], prob=0.95))[2,]

# othp
post.othp<- posterior_samples(othp)[,c("b_GiniZ", "b_male", "b_HHWealthZ.vil", "b_AgeZ", "b_MeanWealthZ", "b_DistZ", "b_SizeZ", "sigma")]
othp.d<- data.frame(effect=c("b_GiniZ", "b_male", "b_HHWealthZ.vil", "b_AgeZ", "b_MeanWealthZ", "b_DistZ", "b_SizeZ"), d_mean=NA, d_median=NA, d_mad=NA, d_lwr.hpdi=NA, d_upr.hpdi=NA)
othp.d$d_mean<-apply(post.othp[,1:7], 2, function(x) mean(x/post.othp[,"sigma"])) 
othp.d$d_median<-apply(post.othp[,1:7], 2, function(x) median(x/post.othp[,"sigma"])) 
othp.d$d_mad<-apply(post.othp[,1:7], 2, function(x) mad(x/post.othp[,"sigma"])) 
othp.d$d_lwr.hpdi<-apply(post.othp[,1:7], 2, function(x) HPDI(x/post.othp[,"sigma"], prob=0.95))[1,]
othp.d$d_upr.hpdi<-apply(post.othp[,1:7], 2, function(x) HPDI(x/post.othp[,"sigma"], prob=0.95))[2,]

# cort
post.cort<- posterior_samples(cort)[,c("b_GiniZ", "b_male", "b_HHWealthZ.vil", "b_AgeZ", "b_MeanWealthZ", "b_DistZ", "b_SizeZ", "sigma")]
cort.d<- data.frame(effect=c("b_GiniZ", "b_male", "b_HHWealthZ.vil", "b_AgeZ", "b_MeanWealthZ", "b_DistZ", "b_SizeZ"), d_mean=NA, d_median=NA, d_mad=NA, d_lwr.hpdi=NA, d_upr.hpdi=NA)
cort.d$d_mean<-apply(post.cort[,1:7], 2, function(x) mean(x/post.cort[,"sigma"])) 
cort.d$d_median<-apply(post.cort[,1:7], 2, function(x) median(x/post.cort[,"sigma"])) 
cort.d$d_mad<-apply(post.cort[,1:7], 2, function(x) mad(x/post.cort[,"sigma"])) 
cort.d$d_lwr.hpdi<-apply(post.cort[,1:7], 2, function(x) HPDI(x/post.cort[,"sigma"], prob=0.95))[1,]
cort.d$d_upr.hpdi<-apply(post.cort[,1:7], 2, function(x) HPDI(x/post.cort[,"sigma"], prob=0.95))[2,]

# sys
post.sys<- posterior_samples(sys)[,c("b_GiniZ", "b_male", "b_HHWealthZ.vil", "b_AgeZ", "b_MeanWealthZ", "b_DistZ", "b_SizeZ", "sigma")]
sys.d<- data.frame(effect=c("b_GiniZ", "b_male", "b_HHWealthZ.vil", "b_AgeZ", "b_MeanWealthZ", "b_DistZ", "b_SizeZ"), d_mean=NA, d_median=NA, d_mad=NA, d_lwr.hpdi=NA, d_upr.hpdi=NA)
sys.d$d_mean<-apply(post.sys[,1:7], 2, function(x) mean(x/post.sys[,"sigma"])) 
sys.d$d_median<-apply(post.sys[,1:7], 2, function(x) median(x/post.sys[,"sigma"])) 
sys.d$d_mad<-apply(post.sys[,1:7], 2, function(x) mad(x/post.sys[,"sigma"])) 
sys.d$d_lwr.hpdi<-apply(post.sys[,1:7], 2, function(x) HPDI(x/post.sys[,"sigma"], prob=0.95))[1,]
sys.d$d_upr.hpdi<-apply(post.sys[,1:7], 2, function(x) HPDI(x/post.sys[,"sigma"], prob=0.95))[2,]

# dias
post.dias<- posterior_samples(dias)[,c("b_GiniZ", "b_male", "b_HHWealthZ.vil", "b_AgeZ", "b_MeanWealthZ", "b_DistZ", "b_SizeZ", "sigma")]
dias.d<- data.frame(effect=c("b_GiniZ", "b_male", "b_HHWealthZ.vil", "b_AgeZ", "b_MeanWealthZ", "b_DistZ", "b_SizeZ"), d_mean=NA, d_median=NA, d_mad=NA, d_lwr.hpdi=NA, d_upr.hpdi=NA)
dias.d$d_mean<-apply(post.dias[,1:7], 2, function(x) mean(x/post.dias[,"sigma"])) 
dias.d$d_median<-apply(post.dias[,1:7], 2, function(x) median(x/post.dias[,"sigma"])) 
dias.d$d_mad<-apply(post.dias[,1:7], 2, function(x) mad(x/post.dias[,"sigma"])) 
dias.d$d_lwr.hpdi<-apply(post.dias[,1:7], 2, function(x) HPDI(x/post.dias[,"sigma"], prob=0.95))[1,]
dias.d$d_upr.hpdi<-apply(post.dias[,1:7], 2, function(x) HPDI(x/post.dias[,"sigma"], prob=0.95))[2,]

# salud
post.salud<- posterior_samples(salud)[,c("b_GiniZ", "b_male", "b_HHWealthZ.vil", "b_AgeZ", "b_MeanWealthZ", "b_DistZ", "b_SizeZ", "sigma")]
salud.d<- data.frame(effect=c("b_GiniZ", "b_male", "b_HHWealthZ.vil", "b_AgeZ", "b_MeanWealthZ", "b_DistZ", "b_SizeZ"), d_mean=NA, d_median=NA, d_mad=NA, d_lwr.hpdi=NA, d_upr.hpdi=NA)
salud.d$d_mean<-apply(post.salud[,1:7], 2, function(x) mean(x/post.salud[,"sigma"])) 
salud.d$d_median<-apply(post.salud[,1:7], 2, function(x) median(x/post.salud[,"sigma"])) 
salud.d$d_mad<-apply(post.salud[,1:7], 2, function(x) mad(x/post.salud[,"sigma"])) 
salud.d$d_lwr.hpdi<-apply(post.salud[,1:7], 2, function(x) HPDI(x/post.salud[,"sigma"], prob=0.95))[1,]
salud.d$d_upr.hpdi<-apply(post.salud[,1:7], 2, function(x) HPDI(x/post.salud[,"sigma"], prob=0.95))[2,]

# sumdiag
post.sumdiag<- posterior_samples(sumdiag)[,c("b_GiniZ", "b_male", "b_HHWealthZ.vil", "b_AgeZ", "b_MeanWealthZ", "b_DistZ", "b_SizeZ", "sigma")]
sumdiag.d<- data.frame(effect=c("b_GiniZ", "b_male", "b_HHWealthZ.vil", "b_AgeZ", "b_MeanWealthZ", "b_DistZ", "b_SizeZ"), d_mean=NA, d_median=NA, d_mad=NA, d_lwr.hpdi=NA, d_upr.hpdi=NA)
sumdiag.d$d_mean<-apply(post.sumdiag[,1:7], 2, function(x) mean(x/post.sumdiag[,"sigma"])) 
sumdiag.d$d_median<-apply(post.sumdiag[,1:7], 2, function(x) median(x/post.sumdiag[,"sigma"])) 
sumdiag.d$d_mad<-apply(post.sumdiag[,1:7], 2, function(x) mad(x/post.sumdiag[,"sigma"])) 
sumdiag.d$d_lwr.hpdi<-apply(post.sumdiag[,1:7], 2, function(x) HPDI(x/post.sumdiag[,"sigma"], prob=0.95))[1,]
sumdiag.d$d_upr.hpdi<-apply(post.sumdiag[,1:7], 2, function(x) HPDI(x/post.sumdiag[,"sigma"], prob=0.95))[2,]

# bmi
post.bmi<- posterior_samples(bmi)[,c("b_GiniZ", "b_male", "b_HHWealthZ.vil", "b_AgeZ", "b_MeanWealthZ", "b_DistZ", "b_SizeZ", "sigma")]
bmi.d<- data.frame(effect=c("b_GiniZ", "b_male", "b_HHWealthZ.vil", "b_AgeZ", "b_MeanWealthZ", "b_DistZ", "b_SizeZ"), d_mean=NA, d_median=NA, d_mad=NA, d_lwr.hpdi=NA, d_upr.hpdi=NA)
bmi.d$d_mean<-apply(post.bmi[,1:7], 2, function(x) mean(x/post.bmi[,"sigma"])) 
bmi.d$d_median<-apply(post.bmi[,1:7], 2, function(x) median(x/post.bmi[,"sigma"])) 
bmi.d$d_mad<-apply(post.bmi[,1:7], 2, function(x) mad(x/post.bmi[,"sigma"])) 
bmi.d$d_lwr.hpdi<-apply(post.bmi[,1:7], 2, function(x) HPDI(x/post.bmi[,"sigma"], prob=0.95))[1,]
bmi.d$d_upr.hpdi<-apply(post.bmi[,1:7], 2, function(x) HPDI(x/post.bmi[,"sigma"], prob=0.95))[2,]

# infect
post.infect<- posterior_samples(infect)[,c("b_GiniZ", "b_male", "b_HHWealthZ.vil", "b_AgeZ", "b_MeanWealthZ", "b_DistZ", "b_SizeZ")]
infect.d<- data.frame(effect=c("b_GiniZ", "b_male", "b_HHWealthZ.vil", "b_AgeZ", "b_MeanWealthZ", "b_DistZ", "b_SizeZ"), d_mean=NA, d_median=NA, d_mad=NA, d_lwr.hpdi=NA, d_upr.hpdi=NA)
infect.d$d_mean<-apply(post.infect[,1:7], 2, function(x) mean(x/logitsd)) 
infect.d$d_median<-apply(post.infect[,1:7], 2, function(x) median(x/logitsd)) 
infect.d$d_mad<-apply(post.infect[,1:7], 2, function(x) mad(x/logitsd)) 
infect.d$d_lwr.hpdi<-apply(post.infect[,1:7], 2, function(x) HPDI(x/logitsd, prob=0.95))[1,]
infect.d$d_upr.hpdi<-apply(post.infect[,1:7], 2, function(x) HPDI(x/logitsd, prob=0.95))[2,]

# resp
post.resp<- posterior_samples(resp)[,c("b_GiniZ", "b_male", "b_HHWealthZ.vil", "b_AgeZ", "b_MeanWealthZ", "b_DistZ", "b_SizeZ")]
resp.d<- data.frame(effect=c("b_GiniZ", "b_male", "b_HHWealthZ.vil", "b_AgeZ", "b_MeanWealthZ", "b_DistZ", "b_SizeZ"), d_mean=NA, d_median=NA, d_mad=NA, d_lwr.hpdi=NA, d_upr.hpdi=NA)
resp.d$d_mean<-apply(post.resp[,1:7], 2, function(x) mean(x/logitsd)) 
resp.d$d_median<-apply(post.resp[,1:7], 2, function(x) median(x/logitsd)) 
resp.d$d_mad<-apply(post.resp[,1:7], 2, function(x) mad(x/logitsd)) 
resp.d$d_lwr.hpdi<-apply(post.resp[,1:7], 2, function(x) HPDI(x/logitsd, prob=0.95))[1,]
resp.d$d_upr.hpdi<-apply(post.resp[,1:7], 2, function(x) HPDI(x/logitsd, prob=0.95))[2,]

# gastro
post.gastro<- posterior_samples(gastro)[,c("b_GiniZ", "b_male", "b_HHWealthZ.vil", "b_AgeZ", "b_MeanWealthZ", "b_DistZ", "b_SizeZ")]
gastro.d<- data.frame(effect=c("b_GiniZ", "b_male", "b_HHWealthZ.vil", "b_AgeZ", "b_MeanWealthZ", "b_DistZ", "b_SizeZ"), d_mean=NA, d_median=NA, d_mad=NA, d_lwr.hpdi=NA, d_upr.hpdi=NA)
gastro.d$d_mean<-apply(post.gastro[,1:7], 2, function(x) mean(x/logitsd)) 
gastro.d$d_median<-apply(post.gastro[,1:7], 2, function(x) median(x/logitsd)) 
gastro.d$d_mad<-apply(post.gastro[,1:7], 2, function(x) mad(x/logitsd)) 
gastro.d$d_lwr.hpdi<-apply(post.gastro[,1:7], 2, function(x) HPDI(x/logitsd, prob=0.95))[1,]
gastro.d$d_upr.hpdi<-apply(post.gastro[,1:7], 2, function(x) HPDI(x/logitsd, prob=0.95))[2,]

save(dep.d, conf.d, lab.d, othp.d, cort.d, sys.d, dias.d, salud.d, sumdiag.d, infect.d, resp.d, gastro.d, bmi.d,file="mainmodels_cohensd.Rdata")
