
#' Run Experiment 3 in parallel via clusters
#' Lauren White, whit1951@umn.edu
#' May 27, 2018

#load libraries
library(parallel)
library(foreach)
library(doParallel)
library(SDMTools)
library(igraph)

#load functions
#source('C:/Users/law2y/OneDrive/R Code/Drosophila/MSI Code/Exp3functions.R')
source('/home/forester/whit1951/Exp3functions.R')

#load datasets
agg<-read.csv("Dmel Fixed Radius Social Aggregation.csv")
agg<-agg[which(agg$Infection=="Control"),] #Don't include sick flies in estimation of social aggregation
infect<-read.csv("Dmel DCV Load and Shed.csv")
duration<-read.csv("Dmel Infection Duration and Lethal Load.csv")

beta<-c(0.1, 0.5, 1)
radius<-c(10,15,20)
nnodes<-1000
max.time<-1000
n.sims<-500
scale_i<-c(1,2)
vary_agg<-c(TRUE, FALSE)
vary_dur<-c(TRUE, FALSE)
vary_infect<-c(TRUE, FALSE)

params<-expand.grid(beta=beta, radius=radius, nnodes=nnodes, max.time=max.time, n.sims=n.sims, scale_i=scale_i, vary_agg=vary_agg, vary_infect=vary_infect, vary_dur=vary_dur)


## Apply the declared function in parallel
#setwd("C:/Users/law2y/OneDrive/R Code/Drosophila/TestParallel3")

ncores <- parallel::detectCores()
doParallel::registerDoParallel(ncores)
tic=Sys.time()
#sim_networks_pan_parallel<-function(agg, infect, duration, nnodes, scale_i, radius, beta, max.time, n.sims, vary_agg, vary_infect, vary_dur){
  foreach(n = 1:nrow(params)) %dopar% sim_networks_pan_parallel(agg=agg, infect=infect, duration=duration, nnodes=params[n,]$nnodes, scale_i=params[n,]$scale_i, radius=params[n,]$radius,  beta=params[n,]$beta, max.time=params[n,]$max.time, n.sims=params[n,]$n.sims, vary_agg=params[n,]$vary_agg, vary_infect=params[n,]$vary_infect, vary_dur=params[n,]$vary_dur)
print(difftime(Sys.time(),tic,units="mins"))

#save(summary_data, file="move_sim1.RData")
#save(params, file="parameters_sim1.RData")

rm(list=ls(all=T)) #clear workspace