library(brms)
load("mainmodels.Rdata")

#Interaction Models
  ## stepwise selection of interaction terms
  dep2<- update(dep,formula. = ~ . + GiniZ:male,cores=3)
  dep3<- update(dep,formula. = ~ . + male:HHWealthZ.vil,cores=3)
  dep4<- update(dep,formula. = ~ . + GiniZ:HHWealthZ.vil,cores=3)
  dep5<- update(dep,formula. = ~ . + GiniZ:male + male:HHWealthZ.vil,cores=3)
  dep6<- update(dep,formula. = ~ . + GiniZ:HHWealthZ.vil + male:HHWealthZ.vil,cores=3)
  dep7<- update(dep,formula. = ~ . + GiniZ:HHWealthZ.vil + GiniZ:male,cores=3)
  dep8<- update(dep,formula. = ~ . + GiniZ:HHWealthZ.vil + GiniZ:male + male:HHWealthZ.vil,cores=3)
  deploo<-loo(dep, dep2, dep3, dep4, dep5, dep6,dep7,dep8, reloo=TRUE) 
  save(dep, dep2, dep3, dep4, dep5, dep6,dep7,dep8, deploo,file="dep.Rdata")
  # Model comparisons:
  #   elpd_diff se_diff
  # dep3  0.0       0.0   
  # dep5 -0.1       0.9   
  # dep6 -0.4       0.6   
  # dep8 -1.6       0.9   
  # dep4 -1.9       2.1   
  # dep  -2.1       2.1   
  # dep2 -2.2       2.2   
  # dep7 -3.6       2.1   
  
  
  
  ## stepwise selection of interaction terms
  conf2<- update(conf,formula. = ~ . + GiniZ:male,cores=3)
  conf3<- update(conf,formula. = ~ . + male:HHWealthZ.vil,cores=3)
  conf4<- update(conf,formula. = ~ . + GiniZ:HHWealthZ.vil,cores=3)
  conf5<- update(conf,formula. = ~ . + GiniZ:male + male:HHWealthZ.vil,cores=3)
  conf6<- update(conf,formula. = ~ . + GiniZ:HHWealthZ.vil + male:HHWealthZ.vil,cores=3)
  conf7<- update(conf,formula. = ~ . + GiniZ:HHWealthZ.vil + GiniZ:male,cores=3)
  conf8<- update(conf,formula. = ~ . + GiniZ:HHWealthZ.vil + GiniZ:male + male:HHWealthZ.vil,cores=3)
  confloo<-loo(conf, conf2, conf3, conf4, conf5, conf6, conf7,conf8, reloo=TRUE) 
  save(conf, conf2, conf3, conf4, conf5, conf6, conf7,conf8,confloo,file="conf.Rdata")
  # Model comparisons:
  #   elpd_diff se_diff
  # conf3  0.0       0.0   
  # conf  -0.1       1.5   
  # conf4 -0.1       1.6   
  # conf6 -0.1       1.0   
  # conf2 -0.7       1.5   
  # conf5 -2.0       0.9
  
  ## stepwise selection of interaction terms
  lab2<- update(lab,formula. = ~ . + GiniZ:male,cores=3)
  lab3<- update(lab,formula. = ~ . + male:HHWealthZ.vil,cores=3)
  lab4<- update(lab,formula. = ~ . + GiniZ:HHWealthZ.vil,cores=3)
  lab5<- update(lab,formula. = ~ . + GiniZ:male + male:HHWealthZ.vil,cores=3)
  lab6<- update(lab,formula. = ~ . + GiniZ:HHWealthZ.vil + male:HHWealthZ.vil,cores=3)
  lab7<- update(lab,formula. = ~ . + GiniZ:HHWealthZ.vil + GiniZ:male,cores=3)
  lab8<- update(lab,formula. = ~ . + GiniZ:HHWealthZ.vil + GiniZ:male + male:HHWealthZ.vil,cores=3)
  labloo<-loo(lab, lab2, lab3, lab4, lab5, lab6, reloo=TRUE) 
  save(lab, lab2, lab3, lab4, lab5, lab6, lab7,lab8,labloo,file="lab.Rdata")
  # Model comparisons:
  # elpd_diff se_diff
  # lab4   0.0       0.0  
  # lab6  -3.6       2.8  
  # lab   -6.9       8.9  
  # lab2  -8.1       7.9  
  # lab3  -8.2       7.8  
  # lab5 -12.9       8.7
  
  ## stepwise selection of interaction terms
  othp2<- update(othp,formula. = ~ . + GiniZ:male,cores=3)
  othp3<- update(othp,formula. = ~ . + male:HHWealthZ.vil,cores=3)
  othp4<- update(othp,formula. = ~ . + GiniZ:HHWealthZ.vil,cores=3)
  othp5<- update(othp,formula. = ~ . + GiniZ:male + male:HHWealthZ.vil,cores=3)
  othp6<- update(othp,formula. = ~ . + GiniZ:HHWealthZ.vil + male:HHWealthZ.vil,cores=3)
  othp7<- update(othp,formula. = ~ . + GiniZ:HHWealthZ.vil + GiniZ:male,cores=3)
  othp8<- update(othp,formula. = ~ . + GiniZ:HHWealthZ.vil + GiniZ:male + male:HHWealthZ.vil,cores=3)
  othploo<-loo(othp, othp2, othp3, othp4, othp5, othp6, othp7, othp8, reloo=TRUE) 
  save(othp, othp2, othp3, othp4, othp5, othp6, othp7, othp8, othploo,file="othp.Rdata")
  # Model comparisons:
  #   elpd_diff se_diff
  # othp3  0.0       0.0   
  # othp4 -0.6       1.7   
  # othp  -0.7       1.7   
  # othp2 -1.2       2.1   
  # othp5 -1.5       1.1   
  # othp7 -1.5       2.1   
  # othp6 -1.6       0.7   
  # othp8 -2.1       1.2 
  
  ## stepwise selection of interaction terms
  cort2<- update(cort,formula. = ~ . + GiniZ:male,cores=3)
  cort3<- update(cort,formula. = ~ . + male:HHWealthZ.vil,cores=3)
  cort4<- update(cort,formula. = ~ . + GiniZ:HHWealthZ.vil,cores=3)
  cort5<- update(cort,formula. = ~ . + GiniZ:male + male:HHWealthZ.vil,cores=3)
  cort6<- update(cort,formula. = ~ . + GiniZ:HHWealthZ.vil + male:HHWealthZ.vil,cores=3)
  cort7<- update(cort,formula. = ~ . + GiniZ:HHWealthZ.vil + GiniZ:male,cores=3)
  cort8<- update(cort,formula. = ~ . + GiniZ:HHWealthZ.vil + GiniZ:male + male:HHWealthZ.vil,cores=3)
  cortloo<-loo(cort, cort2, cort3, cort4, cort5, cort6, cort7,cort8, reloo=TRUE) 
  save(cort, cort2, cort3, cort4, cort5, cort6, cort7,cort8,cortloo,file="cort.Rdata")
  # Model comparisons:
  # elpd_diff se_diff
  # cort2  0.0       0.0   
  # cort7 -0.4       1.1   
  # cort  -0.6       1.9   
  # cort4 -0.8       2.0   
  # cort3 -0.9       1.9   
  # cort5 -1.5       1.0   
  # cort6 -2.7       2.1   
  # cort8 -3.0       1.3   
  
  ## stepwise selection of interaction terms
  sys2<- update(sys,formula. = ~ . + GiniZ:male,cores=3)
  sys3<- update(sys,formula. = ~ . + male:HHWealthZ.vil,cores=3)
  sys4<- update(sys,formula. = ~ . + GiniZ:HHWealthZ.vil,cores=3)
  sys5<- update(sys,formula. = ~ . + GiniZ:male + male:HHWealthZ.vil,cores=3)
  sys6<- update(sys,formula. = ~ . + GiniZ:HHWealthZ.vil + male:HHWealthZ.vil,cores=3)
  sys7<- update(sys,formula. = ~ . + GiniZ:HHWealthZ.vil + GiniZ:male,cores=3)
  sys8<- update(sys,formula. = ~ . + GiniZ:HHWealthZ.vil + GiniZ:male + male:HHWealthZ.vil,cores=3)
  sysloo<-loo(sys, sys2, sys3, sys4, sys5, sys6, reloo=TRUE) 
  save(sys, sys2, sys3, sys4, sys5, sys6, sys7,sys8,sysloo,file="sys.Rdata")
  # Model comparisons:
  # elpd_diff se_diff
  # sys6  0.0       0.0   
  # sys3  0.0       1.6   
  # sys4 -0.4       1.9   
  # sys  -1.0       1.8   
  # sys2 -1.1       1.8   
  # sys5 -2.7       1.7 
  
  ## stepwise selection of interaction terms
  dias2<- update(dias,formula. = ~ . + GiniZ:male,cores=3)
  dias3<- update(dias,formula. = ~ . + male:HHWealthZ.vil,cores=3)
  dias4<- update(dias,formula. = ~ . + GiniZ:HHWealthZ.vil,cores=3)
  dias5<- update(dias,formula. = ~ . + GiniZ:male + male:HHWealthZ.vil,cores=3)
  dias6<- update(dias,formula. = ~ . + GiniZ:HHWealthZ.vil + male:HHWealthZ.vil,cores=3)
  dias7<- update(dias,formula. = ~ . + GiniZ:male + GiniZ:HHWealthZ.vil + male:HHWealthZ.vil,cores=3)
  dias8<- update(dias,formula. = ~ . + GiniZ:HHWealthZ.vil + GiniZ:male + male:HHWealthZ.vil,cores=3)
  diasloo<-loo(dias, dias2, dias3, dias4, dias5, dias6, dias7, reloo=TRUE) 
  save(dias, dias2, dias3, dias4, dias5, dias6, dias7, dias8,diasloo,file="dias.Rdata")
  # Model comparisons:
  # elpd_diff se_diff
  # dias2  0.0       0.0   
  # dias3  0.0       2.8   
  # dias  -0.3       2.1   
  # dias7 -0.4       2.1   
  # dias5 -0.9       2.0   
  # dias6 -1.3       2.9   
  # dias4 -3.5       2.2
  #unclear why 5 isn't preferred, since both 2 and 3 are
  
  ## stepwise selection of interaction terms
  salud2<- update(salud,formula. = ~ . + GiniZ:male,cores=3)
  salud3<- update(salud,formula. = ~ . + male:HHWealthZ.vil,cores=3)
  salud4<- update(salud,formula. = ~ . + GiniZ:HHWealthZ.vil,cores=3)
  salud5<- update(salud,formula. = ~ . + GiniZ:male + male:HHWealthZ.vil,cores=3)
  salud6<- update(salud,formula. = ~ . + GiniZ:HHWealthZ.vil + male:HHWealthZ.vil,cores=3)
  salud7<- update(salud,formula. = ~ . + GiniZ:male + GiniZ:HHWealthZ.vil + male:HHWealthZ.vil,cores=3)
  salud8<- update(salud,formula. = ~ . + GiniZ:HHWealthZ.vil + GiniZ:male + male:HHWealthZ.vil,cores=3)
  saludloo<-loo(salud, salud2, salud3, salud4, salud5, salud6, reloo=TRUE) 
  save(salud, salud2, salud3, salud4, salud5, salud6, salud7,salud8,saludloo,file="salud.Rdata")
  # Model comparisons:
  #   elpd_diff se_diff
  # salud3  0.0       0.0   
  # salud4 -1.4       1.7   
  # salud  -1.4       1.3   
  # salud5 -2.5       1.4   
  # salud2 -3.4       1.4   
  # salud6 -4.3       1.5 
  
  ## stepwise selection of interaction terms
  sumdiag2<- update(sumdiag,formula. = ~ . + GiniZ:male,cores=3)
  sumdiag3<- update(sumdiag,formula. = ~ . + male:HHWealthZ.vil,cores=3)
  sumdiag4<- update(sumdiag,formula. = ~ . + GiniZ:HHWealthZ.vil,cores=3)
  sumdiag5<- update(sumdiag,formula. = ~ . + GiniZ:male + male:HHWealthZ.vil,cores=3)
  sumdiag6<- update(sumdiag,formula. = ~ . + GiniZ:HHWealthZ.vil + male:HHWealthZ.vil,cores=3)
  sumdiag7<- update(sumdiag,formula. = ~ . + GiniZ:HHWealthZ.vil + GiniZ:male,cores=3)
  sumdiag8<- update(sumdiag,formula. = ~ . + GiniZ:male + GiniZ:HHWealthZ.vil + male:HHWealthZ.vil,cores=3)
  sumdiagloo<-loo(sumdiag, sumdiag2, sumdiag3, sumdiag4, sumdiag5, sumdiag6,sumdiag7,sumdiag8,reloo=TRUE) 
  save(sumdiag, sumdiag2, sumdiag3, sumdiag4, sumdiag5, sumdiag6,sumdiag7,sumdiag8, sumdiagloo,file="sumdiag.Rdata")
  # Model comparisons:
  # elpd_diff se_diff
  # sumdiag   0.0       0.0   
  # sumdiag2 -0.6       1.7   
  # sumdiag6 -0.7       1.8   
  # sumdiag4 -1.3       1.6   
  # sumdiag5 -1.7       1.7   
  # sumdiag7 -1.8       2.2   
  # sumdiag3 -1.8       1.0   
  # sumdiag8 -2.3       2.3  
  
  ## stepwise selection of interaction terms
  infect2<- update(infect,formula. = ~ . + GiniZ:male,cores=3)
  infect3<- update(infect,formula. = ~ . + male:HHWealthZ.vil,cores=3)
  infect4<- update(infect,formula. = ~ . + GiniZ:HHWealthZ.vil,cores=3)
  infect5<- update(infect,formula. = ~ . + GiniZ:male + male:HHWealthZ.vil,cores=3)
  infect6<- update(infect,formula. = ~ . + GiniZ:HHWealthZ.vil + male:HHWealthZ.vil,cores=3)
  infect7<- update(infect,formula. = ~ . + GiniZ:HHWealthZ.vil + GiniZ:male,cores=3)
  infect8<- update(infect,formula. = ~ . + GiniZ:male + GiniZ:HHWealthZ.vil + male:HHWealthZ.vil,cores=3)
  infectloo<-loo(infect, infect2, infect3, infect4, infect5, infect6, infect7, infect8,reloo=TRUE) 
  save(infect, infect2, infect3, infect4, infect5, infect6,infect7,infect8, infectloo,file="infect.Rdata")
  
  # Model comparisons:
  #   elpd_diff se_diff
  # infect4  0.0       0.0   
  # infect  -0.3       0.7   
  # infect3 -0.5       1.3   
  # infect2 -0.6       1.0   
  # infect6 -1.5       1.2   
  # infect5 -1.6       1.5 

  #Parameter in infect4 is essentially no effect.
  
  ## stepwise selection of interaction terms
  resp2<- update(resp,formula. = ~ . + GiniZ:male,cores=3,save_all_pars=TRUE)
  resp3<- update(resp,formula. = ~ . + male:HHWealthZ.vil,cores=3)
  resp4<- update(resp,formula. = ~ . + GiniZ:HHWealthZ.vil,cores=3)
  resp5<- update(resp,formula. = ~ . + GiniZ:male + male:HHWealthZ.vil,cores=3)
  resp6<- update(resp,formula. = ~ . + GiniZ:HHWealthZ.vil + male:HHWealthZ.vil,cores=3)
  resp7<- update(resp,formula. = ~ . + GiniZ:HHWealthZ.vil + GiniZ:male,cores=3)
  resp8<- update(resp,formula. = ~ . + GiniZ:male + GiniZ:HHWealthZ.vil + male:HHWealthZ.vil,cores=3)
  resploo<-loo(resp, resp2, resp3, resp4, resp5, resp6, resp7,resp8, reloo=TRUE) 
  save(resp, resp2, resp3, resp4, resp5, resp6, resp7,resp8, resploo,file="resp.Rdata")
  
  # Model comparisons:
  #   elpd_diff se_diff
  # resp7  0.0       0.0   
  # resp3 -0.6       2.6   
  # resp  -0.9       2.4   
  # resp6 -1.2       2.0   
  # resp8 -1.3       1.4   
  # resp4 -1.4       1.7   
  # resp5 -2.0       2.2   
  # resp2 -2.7       1.9 
  
  ## stepwise selection of interaction terms
  gastro2<- update(gastro,formula. = ~ . + GiniZ:male,cores=3)
  gastro3<- update(gastro,formula. = ~ . + male:HHWealthZ.vil,cores=3)
  gastro4<- update(gastro,formula. = ~ . + GiniZ:HHWealthZ.vil,cores=3)
  gastro5<- update(gastro,formula. = ~ . + GiniZ:male + male:HHWealthZ.vil,cores=3)
  gastro6<- update(gastro,formula. = ~ . + GiniZ:HHWealthZ.vil + male:HHWealthZ.vil,cores=3)
  gastro7<- update(gastro,formula. = ~ . + GiniZ:male + GiniZ:HHWealthZ.vil + male:HHWealthZ.vil,cores=3)
  gastro8<- update(gastro,formula. = ~ . + GiniZ:HHWealthZ.vil + GiniZ:male + male:HHWealthZ.vil,cores=3)
  gastroloo<-loo(gastro, gastro2, gastro3, gastro4, gastro5, gastro6, reloo=TRUE) 
  save(gastro, gastro2, gastro3, gastro4, gastro5, gastro6,gastro7,gastro8, gastroloo,file="gastro.Rdata")
  
  # #Model comparisons:
  # elpd_diff se_diff
  # gastro   0.0       0.0   
  # gastro2 -0.5       1.2   
  # gastro3 -0.8       1.3   
  # gastro5 -0.9       1.7   
  # gastro4 -0.9       1.2   
  # gastro6 -1.0       1.6 
  
  ##No interaction models for BMI?
  bmi2<- update(bmi,formula. = ~ . + GiniZ:male,cores=3)
  bmi3<- update(bmi,formula. = ~ . + male:HHWealthZ.vil,cores=3)
  bmi4<- update(bmi,formula. = ~ . + GiniZ:HHWealthZ.vil,cores=3)
  bmi5<- update(bmi,formula. = ~ . + GiniZ:male + male:HHWealthZ.vil,cores=3)
  bmi6<- update(bmi,formula. = ~ . + GiniZ:HHWealthZ.vil + male:HHWealthZ.vil,cores=3)
  bmi7<- update(bmi,formula. = ~ . + GiniZ:male + GiniZ:HHWealthZ.vil + male:HHWealthZ.vil,cores=3)
  bmi8<- update(bmi,formula. = ~ . + GiniZ:HHWealthZ.vil + GiniZ:male + male:HHWealthZ.vil,cores=3)
  #Too many bad pareto-k to reloo. Take with grain of salt
  bmiloo<-loo(bmi, bmi2, bmi3, bmi4, bmi5, bmi6, reloo=FALSE) 
  save(bmi, bmi2, bmi3, bmi4, bmi5, bmi6,bmi7,bmi8, bmiloo,file="bmi.Rdata")
  # Model comparisons:
  #   elpd_diff se_diff
  # bmi    0.0       0.0  
  # bmi5  -5.7       6.3  
  # bmi6 -10.0       6.6  
  # bmi2 -12.5       7.1  
  # bmi3 -16.0       6.7  
  # bmi4 -16.3       6.8 

  
#Save the full models together for plotting
save(dep8,conf8,lab8,othp8,cort8,bmi8,sys8,dias8,salud8,sumdiag8,infect8,resp8,gastro8, file="fullinteractionmodels.Rdata")
