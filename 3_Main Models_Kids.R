#Main models run for juveniles

library(brms)
# stan settings
rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


alldata<-read.csv("alldataKids.csv")

## run analyses ##
##################

## set prior
prior=c(prior(normal(0,5), class=Intercept), 
prior(cauchy(0,2), class=sd), 
prior(normal(0,1), class=b))


### psychosocial outcomes ###
#############################

##psychosocial data not available for kids

### health outcomes ###
#######################

## objective health (diagnoses) ##
## sum of all diagnoses (n=1506)
sumdiag<- brm(SumDiagZ~GiniZ+male+HHWealthZ.vil+AgeZ+MeanWealthZ+DistZ+SizeZ+HHSizeZ+(1|ComunID)+(1|HHId)+(1|pid),
data=alldata, family=gaussian(), prior = prior, chains = 3, cores = 3, iter = 3000, warmup = 500, thin=5, control = list(adapt_delta = 0.99))
summary(sumdiag)	# looks good

## 1: Infectious and parasitic disease (n=1506)
infect<- brm(InfectiousDiag~GiniZ+male+HHWealthZ.vil+AgeZ+MeanWealthZ+DistZ+SizeZ+HHSizeZ+(1|ComunID)+(1|HHId)+(1|pid),
data=alldata, family=bernoulli(), prior = prior, chains = 3, cores = 3, iter = 3000, warmup = 500, thin=5, control = list(adapt_delta = 0.99))
summary(infect)	# looks good

## 8: Respiratory disease (n=1506)
resp<- brm(RepiratoryDiag~GiniZ+male+HHWealthZ.vil+AgeZ+MeanWealthZ+DistZ+SizeZ+HHSizeZ+(1|ComunID)+(1|HHId)+(1|pid),
data=alldata, family=bernoulli(), prior = prior, chains = 3, cores = 3, iter = 3000, warmup = 500, thin=5, control = list(adapt_delta = 0.99))
summary(resp)	# looks good

## 9: GI disease (n=1506)
gastro<- brm(GIDiag~GiniZ+male+HHWealthZ.vil+AgeZ+MeanWealthZ+DistZ+SizeZ+HHSizeZ+(1|ComunID)+(1|HHId)+(1|pid),
data=alldata, family=bernoulli(), prior = prior, chains = 3, cores = 3, iter = 3000, warmup = 500, thin=5, control = list(adapt_delta = 0.99))
summary(gastro)	# looks good

### BMI ###  (n=)
bmi<- brm(BMI.TZ~GiniZ+male+HHWealthZ.vil+AgeZ+MeanWealthZ+DistZ+SizeZ+HHSizeZ+(1|ComunID)+(1|HHId)+(1|pid),
data=alldata, family=gaussian(), prior = prior, chains = 3, cores = 3, iter = 5000, warmup = 1000, thin=5, control = list(adapt_delta = 0.99))
summary(bmi)	# looks good

save(sumdiag, infect, resp, gastro, bmi,file="mainmodelsKids.Rdata")





