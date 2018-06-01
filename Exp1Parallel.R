
#' Run Experiment 1 in parallel via clusters
#' Lauren White, whit1951@umn.edu
#' May 27, 2018

#load libraries
library(parallel)
library(foreach)
library(doParallel)
library(SDMTools)
library(igraph)

#load functions
#source('C:/Users/law2y/OneDrive/R Code/Drosophila/MSI Code/Exp1functions.R')
source('/home/forester/whit1951/Exp1functions.R')

#load datasets
agg<-read.csv("Dmel Fixed Radius Social Aggregation.csv")
agg<-agg[which(agg$Infection=="Control"),] #Don't include sick flies in estimation of social aggregation
infect<-read.csv("Dmel DCV Load and Shed.csv")
duration<-read.csv("Dmel Infection Duration and Lethal Load.csv")

beta<-c(0.1, 0.5, 1)
line_numbers<-unique(agg$Line)
sex<-c("Female", "Male")
radius<-c(10,15,20)
nnodes<-1000
max.time<-1000
n.sims<-500
scale_i<-c(1,2)

params<-expand.grid(beta=beta, line_numbers=line_numbers, sex=sex, radius=radius, nnodes=nnodes, max.time=max.time, n.sims=n.sims, scale_i=scale_i)



#test<-sim_networks(agg=agg, infect=infect, duration=duration, line=line_numbers[i], sex=sex[j], nnodes=1000, radius=20, beta=1, max.time=100, n.sims=100, scale_i=2)

## Apply the declared function in parallel
#setwd("C:/Users/law2y/OneDrive/R Code/Drosophila/TestParallel")

ncores <- parallel::detectCores()
doParallel::registerDoParallel(ncores)
tic=Sys.time()
#(agg, infect, duration, line, sex, radius, nnodes, beta, max.time, n.sims, scale_i){
foreach(n = 1:nrow(params)) %dopar% sim_networks_parallel(agg=agg, infect=infect, duration=duration, line=params[n,]$line_numbers, sex=params[n,]$sex, radius=params[n,]$radius, nnodes=params[n,]$nnodes, beta=params[n,]$beta, max.time=params[n,]$max.time, n.sims=params[n,]$n.sims, scale_i=params[n,]$scale_i)
print(difftime(Sys.time(),tic,units="mins"))

#save(summary_data, file="move_sim1.RData")
#save(params, file="parameters_sim1.RData")

rm(list=ls(all=T)) #clear workspace