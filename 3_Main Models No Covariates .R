#Main models with no covariates

library(brms)

# stan settings
rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

alldata<-read.csv("alldataAdults.csv")

## run analyses ##
##################

## set prior
prior=c(prior(normal(0,5), class=Intercept), 
prior(cauchy(0,2), class=sd), 
prior(normal(0,1), class=b))


### psychosocial outcomes ###
#############################

### WealthdataDep ###
## Depression Score (n=670)
## base model
dep<- brm(DepZ~GiniZ+HHWealthZ+(1|ComunID)+(1|HHId)+(1|pid),
data=alldata, family=gaussian(), prior = prior, chains = 3, cores = 3, iter = 3000, warmup = 500, thin=5, control = list(adapt_delta = 0.95))
summary(dep)	# looks good (n=672)

## Conflicts (n=401)
## base model
conf<- brm(ConfZ~GiniZ+HHWealthZ+(1|ComunID)+(1|HHId)+(1|pid),
data=alldata, family=gaussian(), prior = prior, chains = 3, cores = 3, iter = 3000, warmup = 500, thin=5, control = list(adapt_delta = 0.95))
summary(conf)	# looks good

## Labor partners (n=399)
## base model
lab<- brm(LabZ~GiniZ+HHWealthZ+(1|ComunID)+(1|HHId)+(1|pid),
data=alldata, family=gaussian(), prior = prior, chains = 3, cores = 3, iter = 3000, warmup = 500, thin=5, control = list(adapt_delta = 0.95))
summary(lab)	# looks good

## Other problems (n=398)
## base model
othp<- brm(OthPZ~GiniZ+HHWealthZ+(1|ComunID)+(1|HHId)+(1|pid),
data=alldata, family=gaussian(), prior = prior, chains = 3, cores = 3, iter = 3000, warmup = 500, thin=5, control = list(adapt_delta = 0.95))
summary(othp)	# looks good

### cortisol (n=811) ###
## base model
cort<- brm(logCSGZ~GiniZ+HHWealthZ+(1|ComunID)+(1|HHId)+(1|pid),
data=alldata, family=gaussian(), prior = prior, chains = 3, cores = 3, iter = 3000, warmup = 500, thin=5, control = list(adapt_delta = 0.95))
summary(cort)	# looks good

### blood pressure ###
## systolic (n=3195)
## base model
sys<- brm(sysBPZ~GiniZ+HHWealthZ+(1|ComunID)+(1|HHId)+(1|pid),
data=alldata, family=gaussian(), prior = prior, chains = 3, cores = 3, iter = 3000, warmup = 500, thin=5, control = list(adapt_delta = 0.95))
summary(sys)	# looks good

## diastolic (n=3195)
## base model
dias<- brm(diasBPZ~GiniZ+HHWealthZ+(1|ComunID)+(1|HHId)+(1|pid),
data=alldata, family=gaussian(), prior = prior, chains = 3, cores = 3, iter = 3000, warmup = 500, thin=5, control = list(adapt_delta = 0.95))
summary(dias)	# looks good

### health outcomes ###
#######################

## subjective health ## n = 2523
## base model
salud<- brm(SaludZ~GiniZ+HHWealthZ+(1|ComunID)+(1|HHId)+(1|pid),
data=alldata, family=gaussian(), prior = prior, chains = 3, cores = 3, iter = 3000, warmup = 500, thin=5, control = list(adapt_delta = 0.95))
summary(salud)	# looks good

## objective health (diagnoses) ##
## sum of all diagnoses (n=1542)
## base model
sumdiag<- brm(SumDiagZ~GiniZ+HHWealthZ+(1|ComunID)+(1|HHId)+(1|pid),
data=alldata, family=gaussian(), prior = prior, chains = 3, cores = 3, iter = 3000, warmup = 500, thin=5, control = list(adapt_delta = 0.95))
summary(sumdiag)	# looks good

## 1: Infectious and parasitic disease (n=1542)
## base model
infect<- brm(InfectiousDiag~GiniZ+HHWealthZ+(1|ComunID)+(1|HHId)+(1|pid),
data=alldata, family=bernoulli(), prior = prior, chains = 3, cores = 3, iter = 3000, warmup = 500, thin=5, control = list(adapt_delta = 0.95))
summary(infect)	# looks good

## 8: Respiratory disease (n=1542)
## base model
resp<- brm(RepiratoryDiag~GiniZ+HHWealthZ+(1|ComunID)+(1|HHId)+(1|pid),
data=alldata, family=bernoulli(), prior = prior, chains = 3, cores = 3, iter = 3000, warmup = 500, thin=5, control = list(adapt_delta = 0.95))
summary(resp)	# looks good

## 9: GI disease (n=1542)
## base model
gastro<- brm(GIDiag~GiniZ+HHWealthZ+(1|ComunID)+(1|HHId)+(1|pid),
data=alldata, family=bernoulli(), prior = prior, chains = 3, cores = 3, iter = 3000, warmup = 500, thin=5, control = list(adapt_delta = 0.95))
summary(gastro)	# looks good

### BMI ###  (n=5179)
## base model
bmi<- brm(BMI.TZ~GiniZ+HHWealthZ+(1|ComunID)+(1|HHId)+(1|pid),
data=alldata, family=gaussian(), prior = prior, chains = 3, cores = 3, iter = 6000, warmup = 2000, thin=5, control = list(adapt_delta = 0.95))
summary(bmi)	# looks good

save(dep, conf, lab, othp, cort, sys, dias, salud, sumdiag, infect, resp, gastro, bmi,file="mainmodelsNocov.Rdata")


