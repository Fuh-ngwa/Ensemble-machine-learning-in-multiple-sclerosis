#################################################################
## Stage 1: Imputation of EDSS   
## Codes by Valery Fuh-ngwa. Correspondence to valeryfuh.ngwa@utas.edu.au
#################################################################

##Libraries required for imputation of EDSS
library("mice")
library("JointAI")
library("ggplot2")
library("corrplot")
library("VIM")
library("naniar")
library("visdat")
library(splines)
library(SparseM)
library(quantreg)
library(mvtnorm)
library(AER)
library(fastDummies)
library(Hmisc)
library(multistateutils)
library(mstate)
library(msm)
library(JM)

setwd("C:/Users/Owner/Documents/Survival_Analysis/DynPred_JointModels/DynPreds_JM/Study 3")

###read in MS longitudinal data. This dataframe containds information on clinical and demographics factors, and genetic variants

msdat<-read.delim("MasterUltimateLongSurv.txt")

##create previous edss and dummies for previous edss

msdat$edss=ceiling(as.numeric(as.character(msdat$edss)))
msdat$edss=msdat$edss+1 #do this to combined levels of EDSS e.g. EDSS 0 becomes EDSS 1, 1.5 becomes 2, 2.5 becomes 3, 3.5 become 4 and so on and so

##create lag variable for edss: edssprev by id
msdat <- msdat %>%                            
  group_by(id) %>%
  dplyr::mutate(edssprev = lag(edss, n = 1, default = NA)) ##ensure the first value for edssprev for each id is NA so that it is not used in the analysis

msdat$edssprev <-as.factor(msdat$edssprev) #convert to factor

#create dummy variables for edssprev
msdat <- dummy_cols(msdat, select_columns = 'edssprev',remove_selected_columns = TRUE)  
msdat<-msdat[,-29]
msdat[,19:28]<-lapply(msdat[,19:28], factor) #convert all previous edss to factor variables
msdat$edss=as.factor(msdat$edss)
msdat$edss=relevel(msdat$edss, ref = "1")

##Imputation of missing edss variables using partial proportional odds model

#create a visit variable
msdat$visit=1
msdat$visit <- ave(msdat$visit,msdat$id, FUN=cumsum)
freq=data.frame(table(msdat$id))
id<-as.character(freq$Var1[freq$Freq==1])
msdat<-msdat[-which(msdat$id%in%id),]
length(unique(msdat$id))


##Fit the Partial proportional odds model
clmm <- clmm_imp(edss~ 
                   #These are the main analysis variables used to impute edss. 
                   # The previous edss is also used to impute the current edss
                   
                   sex + age  + bmi + cdms + ebv + hads + time + edssprev_2 + edssprev_3+
                   edssprev_4 + edssprev_5 + edssprev_6 + edssprev_7+
                   edssprev_8 + edssprev_9 + edssprev_10,    
                 
                 #These are the auxiliary variables used to inform the imputation 
                 #of the main analysis variables if they are missing
                 auxvars = ~ relapse+vitd+smoker+latexp+mstype,    
                 data = msdat,
                 random=~1+time|id,                  #random effects components
                 n.chains = 3,                       #Number of machine chains
                 thin=1,                             #the thinning factor. Keep every 1 sampled observation
                 nonprop =list(edss~sex+bmi),        #non proportional odds for sex and bmi
                 refcats = "first",                  #The first category of edss is used as reference i.e, EDSS 1
                 n.adapt = 600,                      #The Markov's chains  adapts after 600 iterations
                 n.iter = 10000,                     #Sample 10000 values from the posterior distribution of edss
                 rev ="edss",                        #reverse model where the risk accumulates at higher ordered level of edss
                 shrinkage = 'ridge',                # A ridge regression shrinkage factor
                 ppc = TRUE, seed = 1355,            #set seed for reporducibility
                 monitor_params = c(imps = TRUE,     #monitor all analsysis variables parameters
                                    analysis_main = TRUE))
save(clmm, file="CLMM_EDSS_IMP.RData")   #save the results

##Get summary of the imputation results. See supplementary results of Fuh-ngwa et al.,
summary(clmm)
plot_all(clmm)
GR_crit(clmm)
MC_error(clmm)
par(mar = c(3, 5, 0.5, 0.5), mgp = c(2, 0.6, 0), mfrow = c(1, 2))
plot(MC_error(clmm))  # left panel: all iterations 101:600
plot(MC_error(mod13a, end = 250))  # right panel: iterations 101:250
densplot(clmm, subset = list(analysis_main = FALSE,
                             other = c('beta[1]', 'beta[5]')), nrow = 1)

###Extract 10 imputed datasets

imp_msdat <- get_MIdat(clmm, m = 10, seed = 2018)


imp1<-imp_msdat$edss[imp_msdat$Imputation_==1]
imp2<-imp_msdat$edss[imp_msdat$Imputation_==2]
imp3<-imp_msdat$edss[imp_msdat$Imputation_==3]
imp4<-imp_msdat$edss[imp_msdat$Imputation_==4]
imp5<-imp_msdat$edss[imp_msdat$Imputation_==5]
imp6<-imp_msdat$edss[imp_msdat$Imputation_==6]
imp7<-imp_msdat$edss[imp_msdat$Imputation_==7]
imp8<-imp_msdat$edss[imp_msdat$Imputation_==8]
imp9<-imp_msdat$edss[imp_msdat$Imputation_==9]
imp10<-imp_msdat$edss[imp_msdat$Imputation_==10]

imdat<-cbind.data.frame(x1=imp1,x2=imp2,x3=imp3,x4=imp4,x5=imp5,x6=imp6,x7=imp7,x8=imp8,x9=imp9,x10=imp10)

##Get maximum posterior value of EDSS across 10 data sets
msdat$edss_imp<-as.numeric(sapply(1:nrow(imdat), function(idx){
  t.max <- max(as.numeric(as.character(t(imdat[idx, ]))))}))

###Table 1 of Fuh-ngwa et al.,
kmat.obs<-statetable.msm(msdat$edss, id, data=msdat) #get Q-matrix
kmat.imp<-statetable.msm(msdat$edss_imp, id, data=msdat) #get Q-matrix

rsum.obs<-rowSums(kmat.obs)
rsum.imp<-colSums(kmat.imp)

prob.obs<-(kmat.obs/rsum.obs)*100
prob.imp<-(kmat.imp/rsum.imp)*100
prob.obs
prob.imp

library(xtable)

xtable(prob.obs, digits = 1)
xtable(prob.imp, digits = 1)

save(msdat, file="msdat2.RDATA")  #Save data for use in next stahge of analysis


## END #
