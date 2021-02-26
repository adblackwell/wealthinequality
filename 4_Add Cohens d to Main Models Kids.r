library(brms)
library(rethinking)

## load models
load("mainmodelsKids.Rdata")

# standard deviation for bernoulli models (for gaussian models, it's sigma)
logitsd<-pi/sqrt(3)

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

save(sumdiag.d, infect.d, resp.d, gastro.d, bmi.d,file="mainmodelsKids_cohensd.Rdata")
