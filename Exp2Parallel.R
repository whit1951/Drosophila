
#' Run Experiment 2 in parallel via clusters
#' Lauren White, whit1951@umn.edu
#' May 27, 2018

#load libraries
library(parallel)
library(foreach)
library(doParallel)
library(SDMTools)
library(igraph)

#load functions
#source('C:/Users/law2y/OneDrive/R Code/Drosophila/MSI Code/Exp2functions.R')
source('/home/forester/whit1951/Exp2functions.R')

#load datasets
agg<-read.csv("Dmel Fixed Radius Social Aggregation.csv")
agg<-agg[which(agg$Infection=="Control"),] #Don't include sick flies in estimation of social aggregation
infect<-read.csv("Dmel DCV Load and Shed.csv")
duration<-read.csv("Dmel Infection Duration and Lethal Load.csv")

beta<-c(0.1, 0.5, 1)
index_line<-unique(agg$Line)
index_sex<-c("Female", "Male")
radius<-c(10,15,20)
nnodes<-1000
max.time<-1000
n.sims<-500
scale_i<-c(1,2)
subp<-length(index_line)*length(index_sex)

params<-expand.grid(beta=beta, index_line=index_line, index_sex=index_sex, radius=radius, nnodes=nnodes, max.time=max.time, n.sims=n.sims, scale_i=scale_i)



#test<-sim_networks(agg=agg, infect=infect, duration=duration, line=line_numbers[i], sex=sex[j], nnodes=1000, radius=20, beta=1, max.time=100, n.sims=100, scale_i=2)

## Apply the declared function in parallel
#setwd("C:/Users/law2y/OneDrive/R Code/Drosophila/TestParallel2")

ncores <- parallel::detectCores()
doParallel::registerDoParallel(ncores)
tic=Sys.time()
#(agg, infect, duration, radius, nnodes, subp, scale_i, beta, max.time, n.sims, index_line, index_sex){
foreach(n = 1:nrow(params)) %dopar% sim_networks_prop_parallel(agg=agg, infect=infect, duration=duration, radius=params[n,]$radius, nnodes=params[n,]$nnodes, subp=subp, scale_i=params[n,]$scale_i, beta=params[n,]$beta,  max.time=params[n,]$max.time, n.sims=params[n,]$n.sims, index_line=params[n,]$index_line, index_sex=params[n,]$index_sex)  
print(difftime(Sys.time(),tic,units="mins"))

#save(summary_data, file="move_sim1.RData")
#save(params, file="parameters_sim1.RData")

rm(list=ls(all=T)) #clear workspace