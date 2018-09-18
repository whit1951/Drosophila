#' Random forest analysis for Drosophila simulations: Experiment 1
#' Lauren White
#' September 19, 2018

#Load libraries
library(party)

#Load data
setwd("~/Drosophila/Drosophila")
Exp1<-read.csv("Experiment1Summary.csv")
Exp1<-Exp1[,-1] #get rid of X.1 column
Exp2<-read.csv("Experiment2Summary.csv")
Exp2<-Exp2[,-1]  #get rid of X.1 column
Exp3<-read.csv("Experiment3Summary.csv")
Exp2<-Exp2[,-1]  #get rid of X.1 column


#Is an outbreak successful or not?
Exp1$outbreak<-ifelse(Exp1$max_inf>1, 1,0)
Exp2$outbreak<-ifelse(Exp2$max_inf>1, 1,0)
Exp3$outbreak<-ifelse(Exp3$max_inf>1, 1,0)


set.seed(213)

tic=Sys.time()
fit.cf1 <- cforest(outbreak ~ line+sex+radius+beta+scale_i, data=Exp1, controls=cforest_unbiased(ntree=100))
v1 <- varimp(fit.cf1, conditional= TRUE)
v1<-v1[order(v1)]
write.csv(v1, "Exp1RF_logit100.csv")

print(difftime(Sys.time(),tic,units="mins"))


#' If outbreak is successful...
#' What determines outbreak size?
#' What determines outbreak duration?
Exp1_success<-Exp1[which(Exp1$outbreak==1),]
Exp2_success<-Exp1[which(Exp1$outbreak==1),]
Exp3_success<-Exp1[which(Exp1$outbreak==1),]


tic=Sys.time()
fit.cf2 <- cforest(max_inf ~ line+sex+radius+beta+scale_i, data=Exp1_success, controls=cforest_unbiased(ntree=100))
v2 <- varimp(fit.cf2, conditional= TRUE)
v2<-v2[order(v2)]
write.csv(v2, "Exp1RF_maxI100.csv")

print(difftime(Sys.time(),tic,units="mins"))

tic=Sys.time()
fit.cf3 <- cforest(duration ~ line+sex+radius+beta+scale_i, data=Exp1_success, controls=cforest_unbiased(ntree=100))
v3 <- varimp(fit.cf3, conditional= TRUE)
v3<-v3[order(v3)]
write.csv(v3, "Exp1RF_dur100.csv")

print(difftime(Sys.time(),tic,units="mins"))


rm(list=ls(all=T)) #clear workspace



