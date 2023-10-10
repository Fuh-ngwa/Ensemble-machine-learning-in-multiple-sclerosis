##############################################################################
## Stage 2: Variable selection using penalised Lasso, Elastic net, and NNG-SIS   
## Codes by Valery Fuh-ngwa
## Correspondence to valeryfuh.ngwa@utas.edu.au
###############################################################################

setwd("C:/Users/Owner/Documents/Survival_Analysis/DynPred_JointModels/DynPreds_JM/Study 3")

##Load data from stage 1
load("msdat2.RDATA")

##convert edss and edssprev toi numeric variabls and create the response status
msdat$edss<-as.numeric(as.character(msdat$edss))
msdat$edssprev<-as.numeric(as.character(msdat$edssprev))
msdat$status<-ifelse(msdat$edss>msdat$edssprev, 1, 0) #change in edss from previous value by at least one point

##read in ms genotype data
xmat<-read.delim("~/Survival_Analysis/trainG_N272.txt")  #read in 199 SNPs
geno<-subset(xmat, ausid%in%zmat$id)
xmat=data.frame(geno[,-1])
geno=cbind.data.frame(id=geno$ausid,xmat )

##merge clinical and genotype data
msdat<-merge(msdat,geno, by="id")
indx<-which(colnames(msdat)%in%colnames(geno[,-1]))
genoX<-msdat[,indx]
covx<-colnames(geno[,-1]) #names of genetic variants
prev.names<-sort(names(msdat)[grep("edssprev_",colnames(msdat))])[-1] ##names of previous edss variables in the ms data

##Create all possible pairwise interaction between genetic variants and edssprev 

xz.vars<-c("status", prev.names, covx)
form <- Y ~ .^2
R> model.matrix(form, data = dat)
###create interactins
fmla <- as.formula(paste("status~ .^2"))
mat=data.frame(model.matrix(fmla, data=msdat[, xz.vars]))
mat<-mat[,-1]
colnames(mat)[1:7]<-prev.names
cov<-colnames(mat) #get names of all variables including interactions
msdat<-cbind.data.frame(msdat[, 1:28],mat)
pmat<-msdat[, 1:28] ##clinical data
fmat<-msdat[, cov]  ##genetic data including interactions with edssprev
pdata<-msdat

##Split data into training and testing set
set.seed(3456)
nodup<-pdata[!duplicated(pdata$id),]
ix <- createDataPartition(nodup$status, p = .75,list = FALSE, times = 1) #75% for training and 25% for testing
head(ix)
nodup.trn<-nodup[ix,]
nodup.tst<-nodup[-ix,]
id.trn<-nodup.trn
dat.trn <- pdata[pdata$id%in%nodup.trn$id, ] #retain only ids from nodup.trn
dat.tst <- pdata[pdata$id%in%nodup.tst$id, ] #retain only ids from nodup.tst
adj<-c("time", "mstype", "reltime", "trans")
ztrain<-data.matrix(dat.trn[, adj])
xtrain<-dat.trn[,x]
ytrain<-as.factor(dat.trn$status)
dat.trn<-data.frame(dat.trn)

##Fit Lasso using 10-fold cv, get opt lmabda on training set
cvfit1<-cvl(ytrain,  xtrain , ztrain , lambda1=1,   data=dat.trn,model = "logistic", fold=10)
optL1 <-optL1(ytrain,xtrain , ztrain, data=dat.trn, model = "logistic",  fold=cvfit1$fold)

##Refit Lasso model on full data using optimal lambda
xmat<-msdat[, cov]
zmat<-data.matrix(msdat[, c("time", "mstype", "reltime", "trans")])
ymat<-as.factor(msdat$status)
lassoFit <-penalized(ymat, xmat ,zmat, lambda1=optL1$lambda, data=msdat, model = "logistic")
L1.snps<-names(coefficients(lassoFit)) ##126 non-zero coefficients

##Fit Elastic Net using 10-fold cv, get opt lmabda on training set
##10-fold cv, get opt lmabda on training set
cvfit2<-cvl(ytrain,  xtrain , ztrain , lambda1=2, data=dat.trn,model = "logistic", fold=10)
cvfit3<-cvl(ytrain,  xtrain , ztrain , lambda2=2,  data=dat.trn,model = "logistic", fold=10)
optL2.1 <-optL1(ytrain,xtrain , ztrain, data=dat.trn, model = "logistic",  fold=cvfit2$fold)
optL3.1 <-optL1(ytrain,xtrain , ztrain, data=dat.trn, model = "logistic",  fold=cvfit3$fold)

##Refit Elastic net model on full data using optimal L1 and L2 penalties 
enetFit <-penalized(ymat, xmat ,zmat, lambda1=optL2.1$lambda, lambda2=optL3.1$lambda, data=msdat,model = "logistic")
L12.snps<-names(coefficients(enetFit)) ##127 non-zero coefficients


##Fit Sure NNG-Independent Screening on training data 
library(SIS)
Z = msdat$Tstop; ind = msdat$status
Y = survival::Surv(Z,ind)
X<-data.matrix(msdat[, c(adj,cov)])
sisFit=SIS(X, Y, family='cox', penalty='lasso', tune='aic', varISIS='aggr', seed=41, iter=FALSE)
ind<-sisFit$ix
##Extract SNPs
length(sisFit$ix)
sis.snp<-cov[ind]
interSNP<-intersect(intersect(L1.snps,L12.snps), sis.snp)  ##common SNPs selected across all models

##save data
save(msdat, file="msdat3.RDATA")

##Save the results to be used in stage 3 (Mixed-effect machine learning)
save(lassoFit, L1.snps, enetFit, L12.snps, sisFit, sis.snp,interSNP, file = "VariableSelected.RData") 

######################### END VARIABLE SELECTION STAGE 1 #####################################
