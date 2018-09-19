#' Random forest analysis for Drosophila simulations: Exp2
#' Lauren White
#' September 19, 2018

#Load libraries
library(party)

#Load data
setwd("~/Drosophila/Drosophila")

Exp3<-read.csv("Experiment3Summary.csv")
Exp3<-Exp3[,-1]  #get rid of X.1 column

#Is an outbreak successful or not?
Exp3$outbreak<-ifelse(Exp3$max_inf>1, 1,0)

set.seed(213)

tic=Sys.time()
fit.cf1 <- cforest(outbreak ~ vary_agg + vary_dur+ vary_infect + radius+beta+scale_i, data=Exp3, controls=cforest_unbiased(ntree=1000))
v1 <- varimp(fit.cf1, conditional= TRUE)
v1<-v1[order(v1)]
write.csv(v1, "Exp3RF_logit1000.csv")

print(difftime(Sys.time(),tic,units="mins"))


#' If outbreak is successful...
#' What determines outbreak size?
#' What determines outbreak duration?
#' What determines R0?
Exp3_success<-Exp3[which(Exp3$outbreak==1),]


tic=Sys.time()
fit.cf2 <- cforest(max_inf ~ vary_agg + vary_dur+ vary_infect + radius+beta+scale_i, data=Exp3_success, controls=cforest_unbiased(ntree=1000))
v2 <- varimp(fit.cf2, conditional= TRUE)
v2<-v2[order(v2)]
write.csv(v2, "Exp3RF_maxI1000.csv")

print(difftime(Sys.time(),tic,units="mins"))

tic=Sys.time()
fit.cf3 <- cforest(duration ~ vary_agg + vary_dur+ vary_infect + radius+beta+scale_i, data=Exp3_success, controls=cforest_unbiased(ntree=1000))
v3 <- varimp(fit.cf3, conditional= TRUE)
v3<-v3[order(v3)]
write.csv(v3, "Exp3RF_dur1000.csv")

print(difftime(Sys.time(),tic,units="mins"))

tic=Sys.time()
fit.cf4 <- cforest(R0 ~ vary_agg + vary_dur+ vary_infect + radius+beta+scale_i, data=Exp3_success, controls=cforest_unbiased(ntree=1000))
v4 <- varimp(fit.cf4, conditional= TRUE)
v4<-v4[order(v4)]
write.csv(v4, "Exp3RF_R0_1000.csv")

print(difftime(Sys.time(),tic,units="mins"))


rm(list=ls(all=T)) #clear workspace



