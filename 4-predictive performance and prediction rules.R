#############################################################################
## Stage 4:  Predictive Performance assessments and prediction rulesmining
## Codes by Valery Fuh-ngwa. Correspondence to valeryfuh.ngwa@utas.edu.au
###############################################################################

##Start with Lasso

load("VariableSelected.RData")
load("LassoEnsemble.RData")
##uncomment these lines to get performance for SIS and Enet
#load("SISEnsemble.RData")
#load("EnetEnsemble.RData")

perMEgbm<-matrix(0, 10, 7)
colnames(perMEgbm)<-c("visit", "tAUC", "tLB", "tUB", "vAUC", "vLB", "vUB")
perMEgbm[1:10,1]<-1:10

##Visit=1
pred.bin   <- predict(res.bin.trn1, newdata= dat.tvDB[dat.tvDB$visit==1,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tvDB[dat.tvDB$visit==1, resp.vars]) ##Training performance
perMEgbm[1,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn1, newdata= dat.tst[dat.tst$visit==1,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tst[dat.tst$visit==1, resp.vars]) ##Training performance
perMEgbm[1,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])
##Visit=2
pred.bin   <- predict(res.bin.trn1, newdata= dat.tvDB[dat.tvDB$visit==2,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tvDB[dat.tvDB$visit==2, resp.vars]) ##Training performance
perMEgbm[2,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn1, newdata= dat.tst[dat.tst$visit==2,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tst[dat.tst$visit==2, resp.vars]) ##Training performance
perMEgbm[2,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])
##Visit=3
pred.bin   <- predict(res.bin.trn1, newdata= dat.tvDB[dat.tvDB$visit==3,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tvDB[dat.tvDB$visit==3, resp.vars]) ##Training performance
perMEgbm[3,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn1, newdata= dat.tst[dat.tst$visit==3,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tst[dat.tst$visit==3, resp.vars]) ##Training performance
perMEgbm[3,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

##Visit=4
pred.bin   <- predict(res.bin.trn1, newdata= dat.tvDB[dat.tvDB$visit==4,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tvDB[dat.tvDB$visit==4, resp.vars]) ##Training performance
perMEgbm[4,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn1, newdata= dat.tst[dat.tst$visit==4,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tst[dat.tst$visit==4, resp.vars]) ##Training performance
perMEgbm[4,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

##Visit=5
pred.bin   <- predict(res.bin.trn1, newdata= dat.tvDB[dat.tvDB$visit==5,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tvDB[dat.tvDB$visit==5, resp.vars]) ##Training performance
perMEgbm[5,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn1, newdata= dat.tst[dat.tst$visit==5,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tst[dat.tst$visit==5, resp.vars]) ##Training performance
perMEgbm[5,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

##Visit=6
pred.bin   <- predict(res.bin.trn1, newdata= dat.tvDB[dat.tvDB$visit==6,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tvDB[dat.tvDB$visit==6, resp.vars]) ##Training performance
perMEgbm[6,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn1, newdata= dat.tst[dat.tst$visit==6,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tst[dat.tst$visit==6, resp.vars]) ##Training performance
perMEgbm[6,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

##Visit=7
pred.bin   <- predict(res.bin.trn1, newdata= dat.tvDB[dat.tvDB$visit==7,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tvDB[dat.tvDB$visit==7, resp.vars]) ##Training performance
perMEgbm[7,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn1, newdata= dat.tst[dat.tst$visit==7,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tst[dat.tst$visit==7, resp.vars]) ##Training performance
perMEgbm[7,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

##Visit=8
pred.bin   <- predict(res.bin.trn1, newdata= dat.tvDB[dat.tvDB$visit==8,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tvDB[dat.tvDB$visit==8, resp.vars]) ##Training performance
perMEgbm[8,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn1, newdata= dat.tst[dat.tst$visit==8,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tst[dat.tst$visit==8, resp.vars]) ##Training performance
perMEgbm[8,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

##Visit=9
pred.bin   <- predict(res.bin.trn1, newdata= dat.tvDB[dat.tvDB$visit==9,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tvDB[dat.tvDB$visit==9, resp.vars]) ##Training performance
perMEgbm[9,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn1, newdata= dat.tst[dat.tst$visit==9,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tst[dat.tst$visit==9, resp.vars]) ##Training performance
perMEgbm[9,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

##Visit=10
pred.bin   <- predict(res.bin.trn1, newdata= dat.tvDB[dat.tvDB$visit==10,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tvDB[dat.tvDB$visit==10, resp.vars]) ##Training performance
perMEgbm[10,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn1, newdata= dat.tst[dat.tst$visit==10,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tst[dat.tst$visit==10, resp.vars]) ##Training performance
perMEgbm[10,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])
perMEgbm


##MErf
######

perMErf<-matrix(0, 10, 7)
colnames(perMErf)<-c("visit", "tAUC", "tLB", "tUB", "vAUC", "vLB", "vUB")
perMErf[1:10,1]<-1:10
##Visit=1
pred.bin   <- predict(res.bin.trn2, newdata= dat.tvDB[dat.tvDB$visit==1,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tvDB[dat.tvDB$visit==1, resp.vars]) ##Training performance
perMErf[1,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn2, newdata= dat.tst[dat.tst$visit==1,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tst[dat.tst$visit==1, resp.vars]) ##Training performance
perMErf[1,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])
##Visit=2
pred.bin   <- predict(res.bin.trn2, newdata= dat.tvDB[dat.tvDB$visit==2,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tvDB[dat.tvDB$visit==2, resp.vars]) ##Training performance
perMErf[2,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn2, newdata= dat.tst[dat.tst$visit==2,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tst[dat.tst$visit==2, resp.vars]) ##Training performance
perMErf[2,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])
##Visit=3
pred.bin   <- predict(res.bin.trn2, newdata= dat.tvDB[dat.tvDB$visit==3,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tvDB[dat.tvDB$visit==3, resp.vars]) ##Training performance
perMErf[3,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn2, newdata= dat.tst[dat.tst$visit==3,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tst[dat.tst$visit==3, resp.vars]) ##Training performance
perMErf[3,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

##Visit=4
pred.bin   <- predict(res.bin.trn2, newdata= dat.tvDB[dat.tvDB$visit==4,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tvDB[dat.tvDB$visit==4, resp.vars]) ##Training performance
perMErf[4,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn2, newdata= dat.tst[dat.tst$visit==4,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tst[dat.tst$visit==4, resp.vars]) ##Training performance
perMErf[4,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

##Visit=5
pred.bin   <- predict(res.bin.trn2, newdata= dat.tvDB[dat.tvDB$visit==5,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tvDB[dat.tvDB$visit==5, resp.vars]) ##Training performance
perMErf[5,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn2, newdata= dat.tst[dat.tst$visit==5,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tst[dat.tst$visit==5, resp.vars]) ##Training performance
perMErf[5,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

##Visit=6
pred.bin   <- predict(res.bin.trn2, newdata= dat.tvDB[dat.tvDB$visit==6,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tvDB[dat.tvDB$visit==6, resp.vars]) ##Training performance
perMErf[6,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn2, newdata= dat.tst[dat.tst$visit==6,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tst[dat.tst$visit==6, resp.vars]) ##Training performance
perMErf[6,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

##Visit=7
pred.bin   <- predict(res.bin.trn2, newdata= dat.tvDB[dat.tvDB$visit==7,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tvDB[dat.tvDB$visit==7, resp.vars]) ##Training performance
perMErf[7,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn2, newdata= dat.tst[dat.tst$visit==7,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tst[dat.tst$visit==7, resp.vars]) ##Training performance
perMErf[7,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

##Visit=8
pred.bin   <- predict(res.bin.trn2, newdata= dat.tvDB[dat.tvDB$visit==8,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tvDB[dat.tvDB$visit==8, resp.vars]) ##Training performance
perMErf[8,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn2, newdata= dat.tst[dat.tst$visit==8,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tst[dat.tst$visit==8, resp.vars]) ##Training performance
perMErf[8,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

##Visit=9
pred.bin   <- predict(res.bin.trn2, newdata= dat.tvDB[dat.tvDB$visit==9,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tvDB[dat.tvDB$visit==9, resp.vars]) ##Training performance
perMErf[9,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn2, newdata= dat.tst[dat.tst$visit==9,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tst[dat.tst$visit==9, resp.vars]) ##Training performance
perMErf[9,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

##Visit=10
pred.bin   <- predict(res.bin.trn2, newdata= dat.tvDB[dat.tvDB$visit==10,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tvDB[dat.tvDB$visit==10, resp.vars]) ##Training performance
perMErf[10,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn2, newdata= dat.tst[dat.tst$visit==10,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$Y.star, dat.tst[dat.tst$visit==10, resp.vars]) ##Training performance
perMErf[10,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])
perMErf

##GBM
#####
perGBM<-matrix(0, 10,7)
colnames(perGBM)<-c("visit", "tAUC", "tLB", "tUB", "vAUC", "vLB", "vUB")
perGBM[1:10,1]<-1:10

##Visit=1
pred.bin   <- predict(res.bin.trn3, newdata= dat.tvDB[dat.tvDB$visit==1,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tvDB[dat.tvDB$visit==1, resp.vars]) ##Training performance
perGBM[1,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn3, newdata= dat.tst[dat.tst$visit==1,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tst[dat.tst$visit==1, resp.vars]) ##Training performance
perGBM[1,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])
##Visit=2
pred.bin   <- predict(res.bin.trn3, newdata= dat.tvDB[dat.tvDB$visit==2,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tvDB[dat.tvDB$visit==2, resp.vars]) ##Training performance
perGBM[2,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn3, newdata= dat.tst[dat.tst$visit==2,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tst[dat.tst$visit==2, resp.vars]) ##Training performance
perGBM[2,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])
##Visit=3
pred.bin   <- predict(res.bin.trn3, newdata= dat.tvDB[dat.tvDB$visit==3,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tvDB[dat.tvDB$visit==3, resp.vars]) ##Training performance
perGBM[3,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn3, newdata= dat.tst[dat.tst$visit==3,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tst[dat.tst$visit==3, resp.vars]) ##Training performance
perGBM[3,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

##Visit=4
pred.bin   <- predict(res.bin.trn3, newdata= dat.tvDB[dat.tvDB$visit==4,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tvDB[dat.tvDB$visit==4, resp.vars]) ##Training performance
perGBM[4,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn3, newdata= dat.tst[dat.tst$visit==4,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tst[dat.tst$visit==4, resp.vars]) ##Training performance
perGBM[4,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

##Visit=5
pred.bin   <- predict(res.bin.trn3, newdata= dat.tvDB[dat.tvDB$visit==5,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tvDB[dat.tvDB$visit==5, resp.vars]) ##Training performance
perGBM[5,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn3, newdata= dat.tst[dat.tst$visit==5,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tst[dat.tst$visit==5, resp.vars]) ##Training performance
perGBM[5,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

##Visit=6
pred.bin   <- predict(res.bin.trn3, newdata= dat.tvDB[dat.tvDB$visit==6,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tvDB[dat.tvDB$visit==6, resp.vars]) ##Training performance
perGBM[6,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn3, newdata= dat.tst[dat.tst$visit==6,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tst[dat.tst$visit==6, resp.vars]) ##Training performance
perGBM[6,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

##Visit=7
pred.bin   <- predict(res.bin.trn3, newdata= dat.tvDB[dat.tvDB$visit==7,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tvDB[dat.tvDB$visit==7, resp.vars]) ##Training performance
perGBM[7,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn3, newdata= dat.tst[dat.tst$visit==7,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tst[dat.tst$visit==7, resp.vars]) ##Training performance
perGBM[7,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

##Visit=8
pred.bin   <- predict(res.bin.trn3, newdata= dat.tvDB[dat.tvDB$visit==8,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tvDB[dat.tvDB$visit==8, resp.vars]) ##Training performance
perGBM[8,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn3, newdata= dat.tst[dat.tst$visit==8,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tst[dat.tst$visit==8, resp.vars]) ##Training performance
perGBM[8,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

##Visit=9
pred.bin   <- predict(res.bin.trn3, newdata= dat.tvDB[dat.tvDB$visit==9,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tvDB[dat.tvDB$visit==9, resp.vars]) ##Training performance
perGBM[9,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn3, newdata= dat.tst[dat.tst$visit==9,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tst[dat.tst$visit==9, resp.vars]) ##Training performance
perGBM[9,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

##Visit=10
pred.bin   <- predict(res.bin.trn3, newdata= dat.tvDB[dat.tvDB$visit==10,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tvDB[dat.tvDB$visit==10, resp.vars]) ##Training performance
perGBM[10,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn3, newdata= dat.tst[dat.tst$visit==10,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tst[dat.tst$visit==10, resp.vars]) ##Training performance
perGBM[10,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])
perGBM

##RF
####
perRF<-matrix(0, 10, 7)
colnames(perRF)<-c("visit", "tAUC", "tLB", "tUB", "vAUC", "vLB", "vUB")
perRF[1:10,1]<-1:10
##Visit=1
pred.bin   <- predict(res.bin.trn4, newdata= dat.tvDB[dat.tvDB$visit==1,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tvDB[dat.tvDB$visit==1, resp.vars]) ##Training performance
perRF[1,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn4, newdata= dat.tst[dat.tst$visit==1,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tst[dat.tst$visit==1, resp.vars]) ##Training performance
perRF[1,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])
##Visit=2
pred.bin   <- predict(res.bin.trn4, newdata= dat.tvDB[dat.tvDB$visit==2,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tvDB[dat.tvDB$visit==2, resp.vars]) ##Training performance
perRF[2,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn4, newdata= dat.tst[dat.tst$visit==2,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tst[dat.tst$visit==2, resp.vars]) ##Training performance
perRF[2,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])
##Visit=3
pred.bin   <- predict(res.bin.trn4, newdata= dat.tvDB[dat.tvDB$visit==3,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tvDB[dat.tvDB$visit==3, resp.vars]) ##Training performance
perRF[3,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn4, newdata= dat.tst[dat.tst$visit==3,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tst[dat.tst$visit==3, resp.vars]) ##Training performance
perRF[3,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

##Visit=4
pred.bin   <- predict(res.bin.trn4, newdata= dat.tvDB[dat.tvDB$visit==4,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tvDB[dat.tvDB$visit==4, resp.vars]) ##Training performance
perRF[4,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn4, newdata= dat.tst[dat.tst$visit==4,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tst[dat.tst$visit==4, resp.vars]) ##Training performance
perRF[4,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

##Visit=5
pred.bin   <- predict(res.bin.trn4, newdata= dat.tvDB[dat.tvDB$visit==5,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tvDB[dat.tvDB$visit==5, resp.vars]) ##Training performance
perRF[5,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn4, newdata= dat.tst[dat.tst$visit==5,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tst[dat.tst$visit==5, resp.vars]) ##Training performance
perRF[5,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

##Visit=6
pred.bin   <- predict(res.bin.trn4, newdata= dat.tvDB[dat.tvDB$visit==6,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tvDB[dat.tvDB$visit==6, resp.vars]) ##Training performance
perRF[6,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn4, newdata= dat.tst[dat.tst$visit==6,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tst[dat.tst$visit==6, resp.vars]) ##Training performance
perRF[6,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

##Visit=7
pred.bin   <- predict(res.bin.trn4, newdata= dat.tvDB[dat.tvDB$visit==7,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tvDB[dat.tvDB$visit==7, resp.vars]) ##Training performance
perRF[7,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn4, newdata= dat.tst[dat.tst$visit==7,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tst[dat.tst$visit==7, resp.vars]) ##Training performance
perRF[7,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

##Visit=8
pred.bin   <- predict(res.bin.trn4, newdata= dat.tvDB[dat.tvDB$visit==8,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tvDB[dat.tvDB$visit==8, resp.vars]) ##Training performance
perRF[8,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn4, newdata= dat.tst[dat.tst$visit==8,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tst[dat.tst$visit==8, resp.vars]) ##Training performance
perRF[8,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

##Visit=9
pred.bin   <- predict(res.bin.trn4, newdata= dat.tvDB[dat.tvDB$visit==9,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tvDB[dat.tvDB$visit==9, resp.vars]) ##Training performance
perRF[9,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn4, newdata= dat.tst[dat.tst$visit==9,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tst[dat.tst$visit==9, resp.vars]) ##Training performance
perRF[9,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

##Visit=10
pred.bin   <- predict(res.bin.trn4, newdata= dat.tvDB[dat.tvDB$visit==10,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tvDB[dat.tvDB$visit==10, resp.vars]) ##Training performance
perRF[10,2:4]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])

pred.bin   <- predict(res.bin.trn4, newdata= dat.tst[dat.tst$visit==10,], type="prob",  allow.new.levels = TRUE)
perf<-Performance.measures(pred.bin$model[,2], dat.tst[dat.tst$visit==10, resp.vars]) ##Training performance
perRF[10,5:7]<-c(perf[[3]], perf[[3]]-1.95*perf[[4]],perf[[3]]+1.95*perf[[4]])
perRF
save(res.bin.trn1,res.bin.trn2, res.bin.trn3, res.bin.trn4,
     perMEgbm, perMErf, perGBM, perRF, file="EnsembleFitPerformance.RData")
##Save results

load("EnsembleFitPerformance.RData")
library(xtable)

xtable(perGBM)
xtable(perRF)
xtable(perMEgbm)
xtable(perMErf)

load("EnsembleFitPerformance.RData")

####################################################################
## Evaluating individual SNPs as a prognostic tool for measuring####
## disability progression, accounting for previous EDSS states######
####################################################################

library(ggpubr)
library(ggplot2)
library(directlabels)
library(grid)
library(gridExtra)
library(lemon)

dat<-dat.tDB
rhs.vars<-keep
resp.vars <- "response" 
rand.vars= c("Tstop","trans")                               ## random effect variables 
pred.time<-c(2.7, 4.97, 7.5, 9.5, 10.1) # This is the median time between two consecutive visits 
keep<-unique(keep)
RFscore<-matrix(0, length(keep), 10)
colnames(RFscore)<-c("v1","v2","v3","v4","v5","v6","v7","v8","v9", "v10")
RFscore<-data.matrix(RFscore)
GBMscore<-RFscore
auc<-matrix(0, length(keep), 10)
colnames(auc)<-colnames(GBMscore)<-colnames(RFscore)
avscore<-auc
p<-length(keep)
n <- length(unique(dat.tvDB$id))
##Train Lasso-Ensemble models
for (i in 1:10){
  for (q in 1:1){
    print(c("visit:", i))
    set.seed(2020 + q)
    n_new <- sample(dat.tvDB$id, n, replace=FALSE)
    n_new <- sort(n_new, decreasing = FALSE)
    xtrain <- NULL
    k <- 1
    for (j in 1:length(n_new)){                                   ##Generate bootstrap samples for each time point
      n_len <- length(dat.tvDB$id[dat.tvDB$id %in% n_new[j]])     
      newdata <- dat.tvDB[dat.tvDB$id %in% n_new[j],]
      newdata$id <- as.numeric(newdata$id)
      newdata$id <- k
      xtrain <- rbind(xtrain, newdata)
      k <- k + 1
    }
  }
  rhs.vars <-c(keep)  ## fixed effect variables 
  res1<- MEml2(method= "RF", data = xtrain[xtrain$visit<=i,],     id=id,  resp.vars= resp.vars, rhs.vars= rhs.vars, rand.vars=rand.vars, para=para)
  res2<- MEml2(method= "RF", data = dat.tvDB[dat.tvDB$visit<=i,], id=id,  resp.vars= resp.vars, rhs.vars= rhs.vars, rand.vars=rand.vars, para=para)
  vim1<-1-t(varImp(res1$model, scale = TRUE, useModel=TRUE)$importance[,1])/100
  vim2<-1-t(varImp(res2$model, scale = TRUE, useModel=TRUE)$importance[,1])/100
  vim<-pmax(vim1,vim2)
  RFscore[1:p,i]<-vim
  res1<- MEml2(method= "GBM", data = xtrain[xtrain$visit<=i,],     id=id,  resp.vars= resp.vars, rhs.vars= rhs.vars, rand.vars=rand.vars, para=para)
  res2<- MEml2(method= "GBM", data = dat.tvDB[dat.tvDB$visit<=i,], id=id,  resp.vars= resp.vars, rhs.vars= rhs.vars, rand.vars=rand.vars, para=para)
  vim1<-1-t(varImp(res1$model, scale = TRUE, useModel=TRUE)$importance[,1])/100
  vim2<-1-t(varImp(res2$model, scale = TRUE, useModel=TRUE)$importance[,1])/100
  vim<-pmax(vim1,vim2)
  GBMscore[1:p,i]<-vim
}
##save results
save(GBMscore,res1, RFscore, file = "VIMP.RData") ## res1 is for us to right order of snps importance in the RFScore and GBM score
#################################################

##Plot VIMP Data

load("VIMP.RData")
score<-GBMscore
#score<-RFscore
score<-data.frame(score)
score$snp<-rownames(varImp(res1$model, scale = TRUE, useModel=TRUE)$importance)
score<-score[!score$snp=="edssprev_7",] #eliminate previous edss as we are interested in snp effects only
score=score[order(score$v10, decreasing = T),]
score$snp<-factor(score$snp, levels = c(score$snp))
df=melt(score, id=11)
colnames(df)=c("variable", "visit", "score")
df$time<-0
df$time[df$visit=="v1"]=1
df$time[df$visit=="v2"]=2
df$time[df$visit=="v3"]=3
df$time[df$visit=="v4"]=4
df$time[df$visit=="v5"]=5
df$time[df$visit=="v6"]=6
df$time[df$visit=="v7"]=7
df$time[df$visit=="v8"]=8
df$time[df$visit=="v9"]=9
df$time[df$visit=="v10"]=10
save(score, df, file="ImportanceScoresGBM.RData")
#save(score, df, file="ImportanceScoresRF.RData")
ggplot(df, aes(x = time, y = score)) + 
  geom_line(aes(color = variable), size = 1.01)+
  scale_y_continuous( n.breaks = 15, breaks = waiver())+
  scale_x_continuous(n.breaks = 10, breaks = waiver())+
  theme(legend.key.height= unit(0.677, 'cm'),
        legend.key.width= unit(0.4, 'cm'),
        legend.direction="vertical",
        legend.justification=c(-0.01, 0),
        legend.position=c(-0.01, 0),
        axis.title.x = element_text(size=10, color="black", face="bold",angle=0),
        axis.title.y = element_text(size=10, color="black", face="bold",angle=90),
        legend.title=element_text(face="bold.italic", color="black",size=9),
        legend.text=element_text(face="bold.italic",  color="black",size=8),
        legend.background = element_rect(fill='transparent'))+
  guides(col = guide_legend("Genetic markers",nrow=28))


###Make Volcanoe plots of log2vimpScores versus -log10 Pvalue
form1<-as.formula(paste("Surv(Tstop, status)", paste(c(keep,"visit", "(1|mstype/id)"),  collapse=" + "), sep=" ~"))
form2<-as.formula(paste("Surv(Tstop, status)", paste(c(keep, "frailty.gaussian(mstype)"),  collapse=" + "), sep=" ~"))
form3<-as.formula(paste("Surv(Tstop, status)", paste(c(keep, "cluster(mstype)"),  collapse=" + "), sep=" ~"))

DF<-msdat
DF[keep]<-scale(DF[keep])
summary(CoxFit <- coxme(form1, data=DF,x=TRUE))      ##No time effect
#summary(CoxFit<-coxph(form2, method = "efron", x=TRUE, data = DF))
summary(CoxFit<-coxph(form3, method = "efron", x=TRUE, data = DF))
xtable(summary(CoxFit)$coef[,c(1,6)], digits = 128)
genDF<-data.frame(snp=as.character(score[,11]), vimp=rowMeans(data.matrix(score[,1:10])))
beta=log2(as.numeric(summary(CoxFit)$coefficients[,2]))
pval=-log10(as.numeric(summary(CoxFit)$coefficients[,6]))
ind<-which(pval>=8)
volDF<-data.frame(snp=rownames(summary(CoxFit)$coefficients),
                  beta=beta,pval=pval)

comDF<-merge(genDF, volDF, by="snp")
comDF$group=NA
comDF$group[comDF$beta<=0]="Negative"
comDF$group[comDF$beta>0]="Positive"
comDF$group<-as.factor(comDF$group)
library(ggrepel)
options(ggrepel.max.overlaps = Inf)
ggplot(data=comDF, aes(x=beta, y=pval, col=group, label=snp)) +
  xlab("log2(Hazard ratio)") + ylab("-log10(P-value)")+
  geom_point() + 
  theme_minimal() +
  geom_text_repel()+
  #scale_y_continuous(breaks=seq(0.5, 8, 1), limits=c(0.5, 8))+
  scale_x_continuous(breaks=seq(-0.2, 0.4, 0.1), limits=c(-0.2, 0.22))+
  scale_color_manual(values=c("blue",  "red")) +
  geom_vline(xintercept=c(-0.1, 0.1), col=c("blue",  "red"), size=1) +
  geom_hline(yintercept=-log10(0.05), col=8, size=1, lty=2)+
  geom_hline(yintercept=-log10(0.05/28), col=8, size=1, lty=1)+
  theme(axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        legend.position=c(0.50, 0.87))+
  guides(col = guide_legend("Association",nrow=2))


##Enhanced Volcanoe plot
library(EnhancedVolcano)


###Venn diagram of SNPs
set.seed(20190708)
x <- list(
  ENET = L12.snps, 
  LASSO = L1.snps, 
  SIS = sis.snp
)
#if (!require(devtools)) install.packages("devtools")
#devtools::install_github("yanlinlin82/ggvenn")
library(ggvenn)
ggvenn(
  x, 
  fill_color = c("#0073C2FF", "#EFC000FF", "#868686FF"),
  stroke_size = 0.75, set_name_size = 6,
  fill_alpha = 0.75,
)

forest_model(CoxFit)

################################
## Entracting Predicting Rules##
################################
load("Keep.RData")
DF<-msdat; DF$edssprev<-NA
DF$edssprev[DF$edssprev_1==1]=0
DF$edssprev[DF$edssprev_2==1]=1
DF$edssprev[DF$edssprev_3==1]=2
DF$edssprev[DF$edssprev_4==1]=3
DF$edssprev[DF$edssprev_5==1]=4
DF$edssprev[DF$edssprev_6==1]=5
DF$edssprev[DF$edssprev_7==1]=6
DF$edssprev[DF$edssprev_8==1]=7
DF$edssprev[DF$edssprev_9==1]=8
DF$edssprev[DF$edssprev_10==1]=9

resp.vars <- "status"                                       ## response variable
rand.vars= c("Tstop","trans")                               ## random effect variables 
rhs.vars <-c("visit","trans","Tstop","mstype", keep)
order.vars<-"visit"
id <- "id"

dat.trn<-cbind.data.frame(dat.trn[,1:8], dat.trn[,keep]) ##All Training set
dat.tst<-cbind.data.frame(dat.tst[,1:8], dat.tst[,keep]) ##Indepdent Test set
DB<-LongiLagSplit(dat.trn, id, rhs.vars, resp.vars, order.vars = "visit",lag = 1)
dat.tDB<-DB$train
dat.vDB<-DB$test
dat.tvDB<-rbind(dat.tDB,dat.vDB) ##training plus validation data

tvDB<-subset(DF,DF$id%in%dat.tvDB$id)
vDB <-subset(DF, DF$id%in%dat.tst$id)
rhs.vars <-c("edssprev", "mstype", keep)
form<-as.formula(paste("Surv(Tstop, status)", paste(rhs.vars,  collapse=" + "), sep=" ~"))
library(xtable)
gbmFit <- gbm(form, data=DF, n.tree = 1000,interaction.depth = 10,distribution="coxph")
for (i in c(1,2,3,4)){
  set.seed(1235)
  X1       <- tvDB[tvDB$visit==i, rhs.vars]
  targetT  <- abs(log(tvDB$time[tvDB$visit==i]+0.05))
  targetS  <- tvDB$status[tvDB$visit==i]
  treeList <- GBM2List(gbmFit,X1)
  ruleExec <- extractRules(treeList,X1)
  ruleExec <- unique(ruleExec)
  ruleMetric <- getRuleMetric(ruleExec,X1,targetT)
  ruleMetric <- pruneRule(ruleMetric,X1,targetT, maxDecay = 0.05, typeDecay = 2)
  ruleMetric <- unique(ruleMetric)
  learner  <- buildLearner(ruleMetric,X1,targetT, minFreq = 0.001)  ##
  X2       <- vDB[vDB$visit==i, rhs.vars]
  targetvT <- abs(log(vDB$time[vDB$visit==i]+0.05))
  targetvS <- vDB$status[vDB$visit==i]
  ruleT <- selectRuleRRF(learner,  X1, targetT) ##Get relevant rules using regularized RF
  ruleT <- selectRuleRRF(ruleT, X2, targetvT)   ##Validate pred rule on outcome Time
  ruleS <- getRuleMetric(ruleT,X1,targetS)      ##extract pred rule on training outcome status
  ruleS <- selectRuleRRF(ruleS,  X1, targetS)   ##compute Importance of pred rule on training outcome status
  ruleS <- presentRules(ruleS,colnames(X1))
  ruleT <- presentRules(ruleT,colnames(X1))
  ruleT <- data.frame(ruleT)
  ruleS <- data.frame(ruleS) 
  ruleT$predT   <- ruleT$pred
  ruleT$predS   <- ruleS$pred
  ruleT$impRRFT <- ruleT$impRRF
  ruleT$impRRFV <- ruleT$impRRF.1
  ruleT         <- ruleT[,-c(1:3,5:7)]
  readableLearner <- ruleT
  readableLearner[,2] <- round(as.numeric(as.character(readableLearner[,2])), 2)
  readableLearner[,4] <- round(as.numeric(as.character(readableLearner[,4])), 2)
  readableLearner[,5] <- round(as.numeric(as.character(readableLearner[,5])), 2)
  print(xtable(readableLearner), include.rownames=FALSE)
}


###### Correlation measure
###### Correlation measure
para$glmer.Control = glmerControl(optimizer = "bobyqa")
para$family = "binomial"

##Re-define Random and Fix components
dat<-dat.tDB
resp.vars <- "response" 
rand.vars= c("Tstop","trans")                               ## random effect variables 
rhs.vars <-c("trans","Tstop","mstype", keep)  ## fixed effect variables 
res.trn<- MEml2(method= "MEgbm", data = dat.tDB, id=id,  resp.vars= resp.vars, rhs.vars= rhs.vars, rand.vars=rand.vars, para=para)
res.tst<- MEml2(method= "MEgbm", data = dat.tst, id=id,  resp.vars= resp.vars, rhs.vars= rhs.vars, rand.vars=rand.vars, para=para)
res1<- MEml2(method= "MEgbm", data = dat.tvDB, id=id,  resp.vars= resp.vars, rhs.vars= rhs.vars, rand.vars=rand.vars, para=para)

pred1   <- predict(res.trn, newdata= dat.tvDB, type="prob",  allow.new.levels = TRUE)
pred2   <- predict(res1, newdata= dat.tst, type="prob",  allow.new.levels = TRUE)


para$glmer.Control = lmerControl(optimizer = "bobyqa")
para$family = "gaussian"
dat.tst$Y.star<-abs(pred2$Y.star)
resp.vars <- "Y.star" 

res.tst2<- MEml2(method= "MEgbm", data = dat.tst, id=id,  resp.vars= resp.vars, rhs.vars= rhs.vars, rand.vars=rand.vars, para=para)
pred3   <- predict(res.tst2, newdata= dat.tst, type="prob",  allow.new.levels = TRUE)


##Test set agreement
pdat<-cbind.data.frame(lscore=plogis(abs(pred2$Y.star)),label="Observed")
adat<-cbind.data.frame(lscore=plogis(abs(pred3$Y.star)),label="Predicted")
dat1<-cbind.data.frame(predicted=pdat$lscore,Observed=adat$lscore, label="Test cohort", group=dat.tst$mstype)
##Train set agreement
pdat<-cbind.data.frame(lscore=plogis(res1$Y.star),label="Observed")
adat<-cbind.data.frame(lscore=plogis(pred1$Y.star),label="Predicted")
dat2<-cbind.data.frame(predicted=pdat$lscore,Observed=adat$lscore, label="Training cohort", group=dat.tvDB$mstype)

library(ggpubr)
ldat<-rbind(dat2, dat1)
save(ldat, file = "CorrPlot.RData")
load("corrPlot.Rdata")
ggscatter(ldat[ldat$label=="Test cohort",], x = "Observed", y = "predicted",size=3.5, color = "black",facet.by = "group",
          add = "reg.line", conf.int = F, fill = "lightgray",shape=21,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Observed probability of worsening", ylab = "Predicted probability of worsening")
ggscatter(ldat[ldat$label=="Training cohort",], x = "Observed", y = "predicted",
          size=3.5, color = "black",facet.by = "group",add = "reg.line", conf.int = F, 
          fill = "lightgray",shape=21,cor.coef = TRUE, 
          cor.method = "pearson",point = TRUE,
          xlab = "Observed probability of worsening", ylab = "Predicted probability of worsening")

###
ggscatter(ldat[ldat$label=="Test cohort",], x = "Observed", y = "predicted",size=3.5, color = "black",facet.by = "label",
          add = "reg.line", conf.int = F, fill = "lightgray",shape=21,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Observed probability of worsening", ylab = "Predicted probability of worsening")
ggscatter(ldat[ldat$label=="Training cohort",], x = "Observed", y = "predicted",
          size=3.5, color = "black",facet.by = "label",add = "reg.line", conf.int = F, 
          fill = "lightgray",shape=21,cor.coef = TRUE, 
          cor.method = "pearson",point = TRUE,
          xlab = "Observed probability of worsening", ylab = "Predicted probability of worsening")

