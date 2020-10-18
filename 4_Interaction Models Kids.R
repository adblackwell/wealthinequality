library(brms)
load("mainmodelsKids.Rdata")

#Interaction Models
  ## stepwise selection of interaction terms
  sumdiag2<- update(sumdiag,formula. = ~ . + GiniZ:male,cores=3)
  sumdiag3<- update(sumdiag,formula. = ~ . + male:HHWealthZ.vil,cores=3)
  sumdiag4<- update(sumdiag,formula. = ~ . + GiniZ:HHWealthZ.vil,cores=3)
  sumdiag5<- update(sumdiag,formula. = ~ . + GiniZ:male + male:HHWealthZ.vil,cores=3)
  sumdiag6<- update(sumdiag,formula. = ~ . + GiniZ:HHWealthZ.vil + male:HHWealthZ.vil,cores=3)
  sumdiagloo<-loo(sumdiag, sumdiag2, sumdiag3, sumdiag4, sumdiag5, sumdiag6, reloo=TRUE) 
  save(sumdiag, sumdiag2, sumdiag3, sumdiag4, sumdiag5, sumdiag6, sumdiagloo,file="sumdiagKids.Rdata")
  
  ## stepwise selection of interaction terms
  infect2<- update(infect,formula. = ~ . + GiniZ:male,cores=3)
  infect3<- update(infect,formula. = ~ . + male:HHWealthZ.vil,cores=3)
  infect4<- update(infect,formula. = ~ . + GiniZ:HHWealthZ.vil,cores=3)
  infect5<- update(infect,formula. = ~ . + GiniZ:male + male:HHWealthZ.vil,cores=3)
  infect6<- update(infect,formula. = ~ . + GiniZ:HHWealthZ.vil + male:HHWealthZ.vil,cores=3)
  infectloo<-loo(infect, infect2, infect3, infect4, infect5, infect6, reloo=TRUE) 
  save(infect, infect2, infect3, infect4, infect5, infect6, infectloo,file="infectKids.Rdata")
  
  ## stepwise selection of interaction terms
  resp2<- update(resp,formula. = ~ . + GiniZ:male,cores=3)
  resp3<- update(resp,formula. = ~ . + male:HHWealthZ.vil,cores=3)
  resp4<- update(resp,formula. = ~ . + GiniZ:HHWealthZ.vil,cores=3)
  resp5<- update(resp,formula. = ~ . + GiniZ:male + male:HHWealthZ.vil,cores=3)
  resp6<- update(resp,formula. = ~ . + GiniZ:HHWealthZ.vil + male:HHWealthZ.vil,cores=3)
  
  resploo<-loo(resp, resp2, resp3, resp4, resp5, resp6, reloo=TRUE) 
  save(resp, resp2, resp3, resp4, resp5, resp6, resploo,file="respKids.Rdata")
  
  
  ## stepwise selection of interaction terms
  gastro2<- update(gastro,formula. = ~ . + GiniZ:male,cores=3)
  gastro3<- update(gastro,formula. = ~ . + male:HHWealthZ.vil,cores=3)
  gastro4<- update(gastro,formula. = ~ . + GiniZ:HHWealthZ.vil,cores=3)
  gastro5<- update(gastro,formula. = ~ . + GiniZ:male + male:HHWealthZ.vil,cores=3)
  gastro6<- update(gastro,formula. = ~ . + GiniZ:HHWealthZ.vil + male:HHWealthZ.vil,cores=3)
  gastroloo<-loo(gastro, gastro2, gastro3, gastro4, gastro5, gastro6, reloo=TRUE) 
  save(gastro, gastro2, gastro3, gastro4, gastro5, gastro6, gastroloo,file="gastroKids.Rdata")
  
  
  ##No interaction models for BMI?
  bmi2<- update(bmi,formula. = ~ . + GiniZ:male,cores=3)
  bmi3<- update(bmi,formula. = ~ . + male:HHWealthZ.vil,cores=3)
  bmi4<- update(bmi,formula. = ~ . + GiniZ:HHWealthZ.vil,cores=3)
  bmi5<- update(bmi,formula. = ~ . + GiniZ:male + male:HHWealthZ.vil,cores=3)
  bmi6<- update(bmi,formula. = ~ . + GiniZ:HHWealthZ.vil + male:HHWealthZ.vil,cores=3)
  #Too many bad pareto-k to reloo. Take with grain of salt
  bmiloo<-loo(bmi, bmi2, bmi3, bmi4, bmi5, bmi6, reloo=FALSE) 
  save(bmi, bmi2, bmi3, bmi4, bmi5, bmi6, bmiloo,file="bmiKids.Rdata")

