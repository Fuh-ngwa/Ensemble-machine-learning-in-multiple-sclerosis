##############################################################################33333
## Stage 3:  Mixed effect machine learning on the selected variables from stage 2   
## Codes by Valery Fuh-ngwa. Correspondence to valeryfuh.ngwa@utas.edu.au
###############################################################################

library(MEml)

##load the results from stage 2
load("msdat3.RDATA")
load("VariableSelected.RData")

##Define parameters for the mixed effects model
seed = 112355
para <- list(
  method = "cv", # internal cross-validation method for parameter tuning. See caret package
  tuneLength=3, # grid size for parameter search 
  number = 3,  # number of internal cross-validation
  n.trees=300,   # number of trees in gbm 
  ntree = 400,   # number of trees in random forest
  interaction.depth=5,
  shrinkage=0.05,
  n.minobsinnode=10,
  opt.para= TRUE, # perform parameter tuning through internal cross-validation 
  include.RE = TRUE,  ## to include estimated random effect as a predictor in the machine learning model
  max.iter = 10, ## maximum number of iterations for the "expectation maximization" like step  
  alpha=0.05, 
  minsize=20,
  maxdepth=30,
  family = "gaussian", 
  glmer.Control = lmerControl(optimizer = "bobyqa"), #glmerControl 
  likelihoodCheck = TRUE, 
  nAGQ=0, 
  decay = 0.05, 
  K = 3, 
  tol= 1e-5,
  seed = seed,
  importance=TRUE
)

dat<-msdat
msdat$id <- as.numeric(msdat$id)  ## random effect grouping variables
id <- "id"

para$glmer.Control = glmerControl(optimizer = "bobyqa")
para$family = "binomial"                          
resp.vars <- "status"

############################################
###Classification using Lasso-Selected SNPs#
############################################

L1dat.trn<-cbind.data.frame(dat.trn[,1:8], dat.trn[,L1.snps[-c(1:4)]])
L1dat.tst<-cbind.data.frame(dat.tst[,1:8], dat.tst[,L1.snps[-c(1:4)]])

##Declare Random and Fix components
rand.vars= c("visit", "trans", "mstype")  ## random effect variables 
                                          ##trans = EDSS transitions e.g., 1-2, 1-3, 1-4, 1-5, 1-6, 1-7, 1-8, 1-9, 2-1, 2-3, 2-4 etc. etc.
                                          ##"mstype" contained levels of MS phenotypes: CIS, RRMS, PPMS, SPMS
##declare fixed-effects components 
rhs.vars1 <-c("Tstop", "visit", L1.snps[-c(1:4)]) ## fixed effect variables 
rhs.vars2 <-L1.snps[-c(1:4)]   ## fixed effect variables 

##Train Lasso-Ensemble models. Here used SNPS selected by Lasso and predict disability progression "status" using Mixed-efects GBM RF, and classical RF and GBM

res.bin.trn1  <- MEml2(method= "MEgbm", data = L1dat.trn, id=id,  resp.vars= resp.vars, rhs.vars= rhs.vars1, rand.vars=rand.vars, para=para)
res.bin.trn2  <- MEml2(method= "MErf", data = L1dat.trn, id=id,  resp.vars= resp.vars, rhs.vars= rhs.vars1, rand.vars=rand.vars, para=para)
res.bin.trn3  <- MEml2(method= "GBM", data = L1dat.trn, id=id,  resp.vars= resp.vars, rhs.vars= rhs.vars2, rand.vars=NULL, para=para)
res.bin.trn4  <- MEml2(method= "RF", data = L1dat.trn, id=id,  resp.vars= resp.vars, rhs.vars= rhs.vars2, rand.vars=NULL, para=para)

##compute L-score on test set
res.bin.tst <- MEml2(method= "MEgbm", data = L1dat.tst, id=id,  resp.vars= resp.vars, rhs.vars= rhs.vars1, rand.vars=rand.vars, para=para)
res.all     <- MEml2(method= "MEgbm", data = msdat, id=id,  resp.vars= resp.vars, rhs.vars= rhs.vars1, rand.vars=rand.vars, para=para)

##Save lasso-ensemble results

save(res.bin.trn1,res.bin.trn2, res.bin.trn3, res.bin.trn4,res.bin.tst, res.all, file="LassoEnsemble.RData")


#############################################
###Classification using Enet-Selected SNPs ##
#############################################

##Training and Validation data
L12dat.trn<-cbind.data.frame(dat.trn[,1:8], dat.trn[,L12.snps[-c(1:4)]])
L12dat.tst<-cbind.data.frame(dat.tst[,1:8], dat.tst[,L12.snps[-c(1:4)]])

##Declare Random and Fix components
rand.vars= c("visit", "trans")  ## random effect variables 
rhs.vars1 <-c("Tstop", "mstype","visit", L12.snps[-c(1:4)]) ## fixed effect variables 
rhs.vars2 <-c(L12.snps[-c(1:4)])   ## fixed effect variables 

###Train Enet-Ensemble models
res.bin.trn1  <- MEml2(method= "MEgbm", data = L12dat.trn, id=id,  resp.vars= resp.vars, rhs.vars= rhs.vars1, rand.vars=rand.vars, para=para)
res.bin.trn2  <- MEml2(method= "MErf", data = L12dat.trn, id=id,  resp.vars= resp.vars, rhs.vars= rhs.vars1, rand.vars=rand.vars, para=para)
res.bin.trn3  <- MEml2(method= "GBM", data = L12dat.trn, id=id,  resp.vars= resp.vars, rhs.vars= rhs.vars2, rand.vars=NULL, para=para)
res.bin.trn4  <- MEml2(method= "RF", data = L12dat.trn, id=id,  resp.vars= resp.vars, rhs.vars= rhs.vars2, rand.vars=NULL, para=para)

##compute L-score on test set using MEgbm
res.bin.tst <- MEml2(method= "MEgbm", data = L12dat.tst, id=id,  resp.vars= resp.vars, rhs.vars= rhs.vars1, rand.vars=rand.vars, para=para)
res.all     <- MEml2(method= "MEgbm", data = msdat, id=id,  resp.vars= resp.vars, rhs.vars= rhs.vars1, rand.vars=rand.vars, para=para)

##Save enet-ensemble results
save(res.bin.trn1,res.bin.trn2, res.bin.trn3, res.bin.trn4,res.bin.tst,res.all, file="EnetEnsemble.RData")



#############################################
###Classification using NNG-SIS -Selected SNPs ##
#############################################

##Training and Validation data
sisdat.trn<-cbind.data.frame(dat.trn[,1:8], dat.trn[,sis.snps[-c(1:4)]])
sisdat.tst<-cbind.data.frame(dat.tst[,1:8], dat.tst[,sis.snps[-c(1:4)]])

##Declare Random and Fix components
rand.vars= c("visit", "trans")  ## random effect variables 
rhs.vars1 <-c("Tstop", "mstype","visit", sis.snps[-c(1:4)]) ## fixed effect variables 
rhs.vars2 <-c(sis.snps[-c(1:4)])   ## fixed effect variables 

###Train Enet-Ensemble models
res.bin.trn1  <- MEml2(method= "MEgbm", data = sisdat.trn, id=id,  resp.vars= resp.vars, rhs.vars= rhs.vars1, rand.vars=rand.vars, para=para)
res.bin.trn2  <- MEml2(method= "MErf", data = sisdat.trn, id=id,  resp.vars= resp.vars, rhs.vars= rhs.vars1, rand.vars=rand.vars, para=para)
res.bin.trn3  <- MEml2(method= "GBM", data = sisdat.trn, id=id,  resp.vars= resp.vars, rhs.vars= rhs.vars2, rand.vars=NULL, para=para)
res.bin.trn4  <- MEml2(method= "RF", data = sisdat.trn, id=id,  resp.vars= resp.vars, rhs.vars= rhs.vars2, rand.vars=NULL, para=para)

##compute L-score on test set using MEgbm
res.bin.tst <- MEml2(method= "MEgbm", data = sisdat.tst, id=id,  resp.vars= resp.vars, rhs.vars= rhs.vars1, rand.vars=rand.vars, para=para)
res.all     <- MEml2(method= "MEgbm", data = msdat, id=id,  resp.vars= resp.vars, rhs.vars= rhs.vars1, rand.vars=rand.vars, para=para)

##Save SIS-ensemble results
save(res.bin.trn1,res.bin.trn2, res.bin.trn3, res.bin.trn4,res.bin.tst,res.all, file="SISEnsemble.RData")
