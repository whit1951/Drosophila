
#'---
#'title: "Experiment 3: Panmictic"
#'author: "Lauren White"
#'date: "May 27, 2018"
#'---

## Experiment #3 as outlined in shared Google Doc

#' @function scaleupdd_pan
#' @author Lauren White
#' @param `df` - dataframe containing all social aggregation data
#' @param `radius` - desired fix radius measurement to use (10 mm, 15 mm, or 20 mm)
#' @param `nnodes`- number of desired nodes for simulated network
#' @returns as list: `new_g` - new network based on empirical degree distribution for given line/sex combo and threshold radius; `emp_density`- empricial network density, `emp_meandegree`- empirical mean degree of network; `sim_density`- simulated network density; `sim_meandegree` - simulated mean degree
#' @requires `igraph` library
#' @description - Function to pull social aggregation data for a given sex/line combo of fruit flies and generate a new, scaled-up network based on empirical degree distribution

scaleupdd_pan<-function(df, radius, nnodes){
  library(igraph)
  #Determine degree distribution (dd) for target radius
  if(radius==20){
  dd<-df$FliesWithin20mm
  } else if(radius==15){
  dd<-df$FliesWithin15mm
  } else if(radius==10) {
  dd<-df$FliesWithin10mm
  } else {
    print("Invalid target radius")
    break()
  }

  #Check to see if degree distribution sums to be even, if odd break function
  sumdd<- sum(dd, na.rm=TRUE) #is sum of degrees even or odd?
  if(sumdd %% 2==1)
  {print("Degree distribution sums to odd number")
   break()
  }

  #generate network from experimental data
  g<-igraph::sample_degseq(out.deg = dd, method = "simple.no.multiple")
  #plot(g)
  emp_density<-igraph::edge_density(g)
  emp_meandegree<-mean(igraph::degree(g))

  h<-hist(dd, breaks=seq(from=0, to=max(dd)+1, by=1), right=FALSE, plot=FALSE)
  counts<-h$counts
  k<-seq(from=0, to=max(dd), by=1)
  pmass<-counts/sum(counts) #probabilty mass of degree distribution

  #Scale up degree distribution by sampling from degree distribution
  sum<-1
  while(sum %% 2==1){
  test<-sample(k, nnodes, replace=TRUE, prob=pmass)#scale up to nnodes individuals
  sum<-sum(test) #must be even to assign edges
  }
  
  h<-hist(test, breaks=seq(from=0, to=max(test)+1, by=1), right=FALSE, plot=FALSE)
  new_g<-igraph::sample_degseq(out.deg = test, method = "simple.no.multiple")
  #plot(new_g)
  #summary(new_g)
  sim_density<-igraph::edge_density(new_g) #check network statistics of new network
  sim_meandegree<-mean(igraph::degree(new_g))
  
  output<-list(new_g,emp_density,emp_meandegree, sim_density, sim_meandegree)
  return(output)
}



## Function describing the "constant" mean degree of the entire  panmictic system
#' @function scaleupdd_constant
#' @author Lauren White
#' @param `df` - dataframe containing all social aggregation data
#' @param `radius` - desired fix radius measurement to use (10 mm, 15 mm, or 20 mm)
#' @param `nnodes`- number of desired nodes for simulated network
#' @returns as list: `g` - new network based on empirical degree distribution for given threshold radius; `mdeg`- empirical mean degree of network; `sim_density`- simulated network density; `sim_meandegree` - simulated mean degree
#' @requires `igraph` library
#' @description - Function to generate network for mean degree of social aggregation data for a given threshold radius

scaleupdd_constant<-function(df, radius, nnodes){
  #Determine degree distribution (dd) for target radius
  library(igraph)
  if(radius==20){
  dd<-df$FliesWithin20mm
  } else if(radius==15){
  dd<-df$FliesWithin15mm
  } else if(radius==10) {
  dd<-df$FliesWithin10mm
  } else {
    print("Invalid target radius")
    break()
  }

  #Check to see if degree distribution sums to be even, if odd break function
  sumdd<- sum(dd, na.rm=TRUE) #is sum of degrees even or odd?
  if(sumdd %% 2==1)
  {print("Degree distribution sums to odd number")
   break()
  }
  mdeg<-round(mean(dd)) #what is the mean degree for the entire collective data set?
  
  #generate network from experimental data
  g<-igraph::sample_degseq(out.deg = rep(mdeg, times=nnodes), method = "simple.no.multiple")
  #plot(g)

  sim_density<-igraph::edge_density(g) #check network statistics of new network
  sim_meandegree<-mean(igraph::degree(g))
  
  output<-list(g,mdeg, sim_density, sim_meandegree)
  return(output)
}


## Function to get scaled infectiousness across all flies n a panmictic population
#' @function infectious_pan
#' @author Lauren White
#' @param `df` - dataframe containing all viral shedding load data
#' @param `g`- simulated network of flies 
#' @param `scale_i`- scale the infectiousness; a scale_i=1 would give infectiousness values ranging from [0,1]
#' @return `g` -updated network with assigned infectiousness values
#' @requires `igraph` library
#' @description - Function to pull infectiousness data for a given sex/line combo of fruit flies and updated simulated network based on those values; assigns attribute V(g)$infectiousness to nodes in network

infectious_pan<-function(df, g, scale_i){

df<-df[!is.na(df$Virus.Shed.per.Fly),] #remove flies with NA values from dataset 
# df$scaled_infect<-df$Virus.Shed.per.Fly/max(df$Virus.Shed.per.Fly, na.rm=TRUE)
# df[df$Virus.Shed.per.Fly>0,]

df$scaled_infect<-log(df$Virus.Shed.per.Fly)/log(max(df$Virus.Shed.per.Fly)) #scale on log scale
df$scaled_infect[df$scaled_infect<0]<-0 #flies with original values of 0, rescaled to zero

#hist(df$scaled_infect)

h<-hist(df$scaled_infect, breaks=seq(from=0, to=max(df$scaled_infect)+0.001, by=0.001), right=TRUE, plot=FALSE)
counts<-h$counts
infect_scale<-seq(from=0, to=max(df$scaled_infect), by=0.001)
pmass_infect<-counts/sum(counts)

sim_infect<-sample(x=infect_scale, size=vcount(g), replace=TRUE, pmass_infect)
#hist(sim_infect)
sim_infect<-sim_infect*scale_i

V(g)$infectiousness<-sim_infect
return(g)
}


## Function to provide constant/average infectiousness across all flies n a panmictic population
#' @function `infectious_constant`
#' @param `df` - dataframe containing all viral shedding load data
#' @param `g`- simulated network of flies 
#' @return `g` -updated network with assigned infectiousness values
#' @param `scale_i`- scale the infectiousness; a scale_i=1 would give infectiousness values ranging from [0,1] (right now, this is just multiplying the mean)
#' @requires `igraph` library
#' @description - Function to pull infectiousness data for all fruit flies and updated simulated network based on those values; assigns AVERAGE/MEAN attribute V(g)$infectiousness to all nodes in network

infectious_constant<-function(df, g, scale_i){

df<-df[!is.na(df$Virus.Shed.per.Fly),] #remove flies with NA values from dataset 
# df$scaled_infect<-df$Virus.Shed.per.Fly/max(df$Virus.Shed.per.Fly, na.rm=TRUE)
# df[df$Virus.Shed.per.Fly>0,]

df$scaled_infect<-log(df$Virus.Shed.per.Fly)/log(max(df$Virus.Shed.per.Fly)) #scale on log scale
df$scaled_infect[df$scaled_infect<0]<-0 #flies with original values of 0, rescaled to zero

mean_infect<-mean(df$scaled_infect)*scale_i

V(g)$infectiousness<-mean_infect
return(g)
}

## Function to get infection duration for panmictic population of flies
#' @function `inf_dur_pan`
#' @param `df` - dataframe containing all viral shedding load data
#' @param `g`- simulated network of flies for given line/sex combo
#' @return `g` -updated network with assigned infectiousness values
#' @requires `igraph` library
#' @description - Function to pull infection duration data for a given sex/line combo of fruit flies and update simulated network based on those values; assigns attribute V(g)$infectiousness to nodes in network

inf_dur_pan<-function(df, g){

  duration<-df$Lifespan
  
  h<-hist(duration, breaks=seq(from=0, to=max(duration, na.rm=TRUE)+1, by=1), right=FALSE, plot=FALSE)
  counts<-h$counts
  dur<-seq(from=0, to=max(duration, na.rm=TRUE), by=1)
  pmass_dur<-counts/sum(counts)
  sim_dur<-sample(dur, size=1000, replace=TRUE, pmass_dur)
  #hist(sim_dur)
  mort_rate<-1/sim_dur #mortality rate is the inverse of lifespan
  #hist(mort_rate) #This should be preassigned to each individual
  
  #Assign mortality rate to igraph object
  V(g)$mort_rate<-mort_rate
  #V(g)$mort_rate
  return(g)
}


## Function to get AVERAGE/MEAN infection duration for panmictic population of flies
#' @function `inf_dur_constant`
#' @param `df` - dataframe containing all viral shedding load data
#' @param `g`- simulated network of flies for given line/sex combo
#' @return `g` -updated network with assigned infectiousness values
#' @requires `igraph` library
#' @description - Function to pull infection duration data for all fruit flies and update simulated network based on those values; assigns AVERAGE/MEAN attribute of V(g)$infectiousness to nodes in network

inf_dur_constant<-function(df, g){

  duration<-df$Lifespan
  mean_dur<-mean(duration)
  
  mort_rate<-1/mean_dur #mortality rate is the inverse of lifespan

  #Assign mortality rate to igraph object
  V(g)$mort_rate<-mort_rate
  #V(g)$mort_rate
  return(g)
}



## Function to calculate discordant edgelist for a given network `nw`
#' @author Lauren White
#' @parameter nw: igraph object
#' @parameter edgelist: edgelist for igraph object
#' Calculates discordant dyads in network, e.g., S/I pairs; Adapted from EpiModel package function
discordant<-function (nw, edgelist) 
{
    status <- V(nw)$inf_status 
    el <- edgelist
    del <- NULL
    if (nrow(el) > 0) {
        el <- el[sample(1:nrow(el)), , drop = FALSE]
        stat <- matrix(status[el], ncol = 2)
        isInf <- matrix(stat %in% "I", ncol = 2)
        isSus <- matrix(stat %in% "S", ncol = 2)
        SIpairs <- el[isSus[, 1] * isInf[, 2] == 1, , drop = FALSE]
        ISpairs <- el[isSus[, 2] * isInf[, 1] == 1, , drop = FALSE]
        pairs <- rbind(SIpairs, ISpairs[, 2:1])
        if (nrow(pairs) > 0) {
            sus <- pairs[, 1]
            inf <- pairs[, 2]
            del <- data.frame(sus, inf)
        }
    }
    return(del)
}

## Function to simulate disease spread on simulated networks
#' @param sim_network
#' @param beta- transmission efficiency of the pathogen
#' @param max.time- maximum duration to run simulations
#' @param g- network on which to simulate disease spread
#' @return N- timecourse of prevelance in population (S,I,R)
sim_network<-function(beta, max.time, g){

#define the network
n<-vcount(g) #size of network

#Randomly select who starts off infected
index_case <- sample(1:n, 1) #just one infected individual to start
g<-set_vertex_attr(g, "inf_status", value="S")
g<-set_vertex_attr(g, "inf_status", index=index_case, value="I")
V(g)$inf_status

#Set up matrices to record disease/prevalence output
N <-data.frame(t=NaN, S=NaN, I=NaN, R=NaN)
infIds<-which(V(g)$inf_status=="I")
susIds<-which(V(g)$inf_status=="S")
N[1,] <- c(1, length(susIds), length(infIds), 0)

#Create edge list for network
edges<-as_edgelist(g, names = TRUE)
R0<-0 #set R0 to zero

#Loop until maximum run time
for (t in 2:max.time) {
  #Determine discordant edges in network (if any)
del<-discordant(g, edges)

#If there are discordant edges, test to see if infection occurs between discordant pairs (e.g., flip a coin weighted by beta)
if(!is.null(del)){
  finalProb<-beta*V(g)$infectiousness[del$inf] #final transmission probability= transmission efficiency of pathogen* the scaled infectiousness of the individual infected fly appearing in discordant edge list
  finalProb[finalProb>1]<-1
  transmit <- rbinom(nrow(del), 1, finalProb)
  
  #Update infection status of affected pairs
  newI<-del$sus[which(transmit==1)]
  oldI<-del$inf[which(transmit==1)]
  
  #If one of the successful transmission cases resulted from index case (e.g., infIds)
  if(is.element(index_case, oldI)){
    R0<-R0+ length(del$inf[which(transmit==1)]==index_case)
  }
  
  #Update network/graph attributes
  g<-set_vertex_attr(g, "inf_status", index=newI, value= "I")
}

if(length(infIds>0)){
    infIds<-which(V(g)$inf_status=="I")
    dieoff <- rbinom(length(infIds), 1, V(g)$mort_rate[infIds])
    newR<-infIds[which(dieoff==1)]
    g<-set_vertex_attr(g, "inf_status", index=newR, value= "R")
}

#Record prevalence at current time step
numinfIds<-length(which(V(g)$inf_status=="I"))
numsusIds<-length(which(V(g)$inf_status=="S"))
numremIds<-length(which(V(g)$inf_status=="R"))
N[t,] <- c(t, numsusIds, numinfIds, numremIds)
if(numinfIds==0)
{
  for(j in (t+1):max.time)
  {
    N[j,] <- c(j, numsusIds, numinfIds, numremIds)
  }
  break
}

}
sim_results<-list("timecourse"=N, "R0"=R0)
return(sim_results)
}


## Test `scaledupdd_pan`, `infectiousness_pan`, `inf_dur_pan`, and `sim_network` functions
# library(igraph)
# 
# agg<-read.csv("Dmel Fixed Radius Social Aggregation.csv")
# agg<-agg[which(agg$Infection=="Control"),] #Don't include sick flies in estimation of social aggregation
# infect<-read.csv("Dmel DCV Load and Shed.csv")
# duration<-read.csv("Dmel Infection Duration and Lethal Load.csv")
# 
# line_numbers<-unique(agg$Line)
# line_numbers
# 
# test<-scaleupdd_pan(df=agg, radius=20, nnodes=1000)
# g<-test[[1]]
# 
# g<-infectious_pan(df=infect, g=g, scale_i=1)
# V(g)$infectiousness
# 
# g<-inf_dur_pan(df=duration, g=g)
# V(g)$mort_rate
# 
# N<-sim_network(beta=1, max.time=100, g=g)
# 
# 
# ## Scenario #1
# ### Infectiousness varies (Hold aggregation and infection duration constant)
# 
# test<-scaleupdd_constant(agg, radius=20, nnodes=1000)
# g<-test[[1]]
# 
# g<-infectious_pan(df=infect, g=g, scale_i=1)
# V(g)$infectiousness
# 
# g<-inf_dur_constant(df=duration, g=g)
# V(g)$mort_rate
# 
# 
# 
# ## Scenario #2
# ### Social aggregation varies (Hold infectiousness and infection duration constant)
# 
# 
# test<-scaleupdd_pan(df=agg, radius=20, nnodes=1000)
# g<-test[[1]]
# 
# g<-infectious_constant(df=infect, g=g, scale_i=1)
# V(g)$infectiousness
# 
# g<-inf_dur_constant(df=duration, g=g)
# V(g)$mort_rate
# 
# ## Scenario #3
# ### Infection duration varies (Hold infectiousness and social aggregation constant)
# test<-scaleupdd_constant(agg, radius=20, nnodes=1000)
# g<-test[[1]]
# 
# g<-infectious_constant(df=infect, g=g, scale_i=2)
# V(g)$infectiousness
# 
# g<-inf_dur_pan(df=duration, g=g)
# V(g)$mort_rate



## Function to allow given component to vary and to simulate disease spread across network for `nsim` number of simulations
#' @function `sim_networks_pan`
#' @param `agg`- social aggregation dataframe
#' @param `infect`- infectiouness dataframe
#' @param `duration`- infection duration dataframe
#' @param `radius`- target radius to test (10, 15, or 20 mm)
#' @param `nnodes`- the number of desired nodes in the network
#' @param `scale_i`- how to scale infectiousness (default of 1 would scale values from [0,1])
#' @param `beta`- transmission efficiency of the pathogen [0,1]
#' @param `max.time`- how many timesteps to run simulation
#' @param `n.sims`- the number of simulations to run
#' @param `vary_agg` logical value- does social aggregation vary?
#' @param `vary_infect` logical value- does infectiousnes vary?
#' @param `vary_dur` logical value- does infection duration vary?

sim_networks_pan<-function(agg, infect, duration, radius, nnodes, scale_i, beta, max.time, n.sims, vary_agg, vary_infect, vary_dur){
  output<-list()
  I_dat<-matrix(data=numeric(), nrow=max.time, ncol=n.sims)
  
  for(i in 1:n.sims){
    if (vary_agg==TRUE){
     test<-scaleupdd_pan(df=agg, radius=radius, nnodes)
     g<-test[[1]]
    } else{
      test<-scaleupdd_constant(agg, radius=radius, nnodes=nnodes)
      g<-test[[1]]
    }
    
    if(vary_infect==TRUE){
      g<-infectious_pan(df=infect, g=g, scale_i=scale_i)
    }else{
      g<-infectious_constant(df=infect, g=g, scale_i=scale_i)
    }
    if(vary_dur==TRUE){
        g<-inf_dur_pan(df=duration, g=g)
    }else{
        g<-inf_dur_constant(df=duration, g=g)
    }

  N<-sim_network(beta=beta, max.time=max.time, g=g)
  output$graph[[i]]<-g
  output$N[[i]]<-N
  output$vary_agg<-vary_agg
  output$vary_dur<-vary_dur
  output$vary_infect<-vary_infect
  output$scale_i<-scale_i
  output$radius<-radius
  output$beta<-beta
  I_dat[,i]<-N$timecourse$I
  }
  write.csv(I_dat, file=paste("Exp3TimecourseAgg", vary_agg, "Infect", vary_infect, "Dur", vary_dur, "beta", beta, "radius", radius, "scale_i", scale_i, ".csv", sep=""))
  return(output)
}

## Function to generate summary stats and plot output
#' @function `summarystats_pan`
#' @param `test`- list object output resulting from `sim_networks_pan`
#' @return both a .csv file of summary statistics and a .tiff image file of simulated timecourses

summarystats_pan<-function(test, nnodes, max.time){
#Generate some summary stats
L<-length(test$N)
sum_stats<-data.frame(vary_agg=logical(L), vary_dur=logical(L), vary_infect=logical(L), beta=numeric(L), radius=numeric(L), scale_i=numeric(L), max_inf=numeric(L), time_max=numeric(L), duration=numeric(L), fadeout=logical(L), R0=numeric(L))

for(i in 1:L){
  timecourse<-test$N[[i]]$timecourse
  sum_stats$max_inf[i]<-max(timecourse$I)
  sum_stats$time_max[i]<-which.max(timecourse$I)
  sum_stats$duration[i]<-which.min(timecourse$I)
  sum_stats$fadeout[i]<-max(timecourse$I)==1 #does the simulation spread beyond initally infected individual
  sum_stats$R0[i]<-test$N[[i]]$R0
  sum_stats$vary_agg[i]<-test$vary_agg
  sum_stats$vary_dur[i]<-test$vary_dur
  sum_stats$vary_infect[i]<-test$vary_infect
  sum_stats$scale_i[i]<-test$scale_i
  sum_stats$radius[i]<-test$radius
  sum_stats$beta[i]<-test$beta
  #lines(timecourse$t, y=timecourse$I)
}

write.csv(sum_stats, file=paste("Exp3SumAgg", test$vary_agg, "Infect", test$vary_infect, "Dur", test$vary_dur, "beta", test$beta, "radius", test$radius, "scale_i", test$scale_i, ".csv", sep=""))

#Plot time course data
# tiff(filename = paste("Exp3Agg", test$vary_agg, "Infect", test$vary_infect, "Dur", test$vary_dur, "beta", test$beta, "radius", test$radius, "scale_i", test$scale_i, ".tiff", sep=""))
# maxI<-max(sum_stats$max_inf)
# plot(1, type="n", xlab="Time", ylab="Number of infected individuals", xlim=c(0, max.time), ylim=c(0, nnodes+1), main="Vary Social Aggregation")
# for(i in 1:L){
#   timecourse<-test$N[[i]]$timecourse
#   lines(timecourse$t, y=timecourse$I)
# }
# dev.off()
}


## Scenario 1: Vary Social Aggregation
#load library
# library(igraph)
# 
# #load datasets
# agg<-read.csv("Dmel Fixed Radius Social Aggregation.csv")
# agg<-agg[which(agg$Infection=="Control"),] #Don't include sick flies in estimation of social aggregation
# infect<-read.csv("Dmel DCV Load and Shed.csv")
# duration<-read.csv("Dmel Infection Duration and Lethal Load.csv")
# 
# test<-sim_networks_pan(agg=agg, infect=infect, duration=duration, nnodes=1000, scale_i=1, radius=20, beta=1, max.time=100, n.sims=10, vary_agg=TRUE, vary_infect=FALSE, vary_dur=FALSE)
# summarystats_pan(test, nnodes=1000, max.time=100)


# Function: sim_networks_pan_parallel -------------------------------------

sim_networks_pan_parallel<-function(agg, infect, duration, nnodes, scale_i, radius, beta, max.time, n.sims, vary_agg, vary_infect, vary_dur){
  test<-sim_networks_pan(agg=agg, infect=infect, duration=duration, nnodes=nnodes, scale_i=scale_i, radius=radius, beta=beta, max.time=max.time, n.sims=n.sims, vary_agg=vary_agg, vary_infect=vary_infect, vary_dur=vary_dur)
  summarystats_pan(test, nnodes=nnodes, max.time=max.time)
}

# ## Scenario 2: Vary Infectiousness
# test2<-sim_networks_pan(agg=agg, infect=infect, duration=duration, nnodes=1000, radius=20, beta=1, max.time=100, n.sims=10, vary_agg=FALSE, vary_infect=TRUE, vary_dur=FALSE)
# summarystats_pan(test2, nnodes=1000, max.time=100)
# 
# 
# ## Scenario 3: Vary Infection Duration
# 
# test3<-sim_networks_pan(agg=agg, infect=infect, duration=duration, nnodes=1000, radius=20, beta=1, max.time=100, n.sims=10, vary_agg=FALSE, vary_infect=FALSE, vary_dur=TRUE)
# 
# summarystats_pan(test3, nnodes=1000, max.time=100)
# 
# 
# ## Scenario 4: Vary Social Aggregation + Infectiousness
# test4<-sim_networks_pan(agg=agg, infect=infect, duration=duration, nnodes=1000, radius=20, beta=1, max.time=100, n.sims=10, vary_agg=TRUE, vary_infect=TRUE, vary_dur=FALSE)
# summarystats_pan(test4, nnodes=1000, max.time=100)
# 
# 
# 
# ## Scenario 5: Vary Social Aggregation + Infection Duration
# test5<-sim_networks_pan(agg=agg, infect=infect, duration=duration, nnodes=1000, radius=20, beta=1, max.time=100, n.sims=10, vary_agg=TRUE, vary_infect=FALSE, vary_dur=TRUE)
# summarystats_pan(test5, nnodes=1000, max.time=100)
# 
# 
# ## Scenario 6: Vary Infectiousness + Infection Duration
# test6<-sim_networks_pan(agg=agg, infect=infect, duration=duration, nnodes=1000, radius=20, beta=1, max.time=100, n.sims=10, vary_agg=FALSE, vary_infect=TRUE, vary_dur=TRUE)
# summarystats_pan(test6, nnodes=1000, max.time=100)
# 
# 
# ## Scenario 7: Vary None
# 
# test7<-sim_networks_pan(agg=agg, infect=infect, duration=duration, nnodes=1000, radius=20, beta=1, max.time=100, n.sims=10, vary_agg=FALSE, vary_infect=FALSE, vary_dur=FALSE)
# summarystats_pan(test7, nnodes=1000, max.time=100)
