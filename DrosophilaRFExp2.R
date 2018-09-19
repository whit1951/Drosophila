#' Random forest analysis for Drosophila simulations: Exp2
#' Lauren White
#' September 19, 2018

#Load libraries
library(party)

#Load data
setwd("~/Drosophila/Drosophila")

Exp2<-read.csv("Experiment2Summary.csv")
Exp2<-Exp2[,-1]  #get rid of X.1 column

#Is an outbreak successful or not?
Exp2$outbreak<-ifelse(Exp2$max_inf>1, 1,0)

set.seed(213)

tic=Sys.time()
fit.cf1 <- cforest(outbreak ~ index_line+index_sex+radius+beta+scale_i, data=Exp2, controls=cforest_unbiased(ntree=1000))
v1 <- varimp(fit.cf1, conditional= TRUE)
v1<-v1[order(v1)]
write.csv(v1, "Exp2RF_logit1000.csv")

print(difftime(Sys.time(),tic,units="mins"))


#' If outbreak is successful...
#' What determines outbreak size?
#' What determines outbreak duration?
#' What determines R0?
Exp2_success<-Exp2[which(Exp2$outbreak==1),]


tic=Sys.time()
fit.cf2 <- cforest(max_inf ~ index_line+index_sex+radius+beta+scale_i, data=Exp2_success, controls=cforest_unbiased(ntree=1000))
v2 <- varimp(fit.cf2, conditional= TRUE)
v2<-v2[order(v2)]
write.csv(v2, "Exp2RF_maxI1000.csv")

print(difftime(Sys.time(),tic,units="mins"))

tic=Sys.time()
fit.cf3 <- cforest(duration ~ index_line+index_sex+radius+beta+scale_i, data=Exp2_success, controls=cforest_unbiased(ntree=1000))
v3 <- varimp(fit.cf3, conditional= TRUE)
v3<-v3[order(v3)]
write.csv(v3, "Exp2RF_dur1000.csv")

print(difftime(Sys.time(),tic,units="mins"))

tic=Sys.time()
fit.cf4 <- cforest(R0 ~ index_line+index_sex+radius+beta+scale_i, data=Exp2_success, controls=cforest_unbiased(ntree=1000))
v4 <- varimp(fit.cf4, conditional= TRUE)
v4<-v4[order(v4)]
write.csv(v4, "Exp2RF_R0_1000.csv")

print(difftime(Sys.time(),tic,units="mins"))


rm(list=ls(all=T)) #clear workspace



