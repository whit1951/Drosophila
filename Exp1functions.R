#' Experiment 1 functions needed to run in parallel on supercomputer
#' May 25, 2018
#' Lauren White, whit1951@umn.edu


# Function: scaleupdd -----------------------------------------------------
#' @function scaleupdd
#' @param `df` - dataframe containing all social aggregation data
#' @param `line` - genetic line number
#' @param `sex` - sex of fruit flies, either "Female" or "Male"
#' @param `radius` - desired fix radius measurement to use (10 mm, 15 mm, or 20 mm)
#' @param `nnodes`- number of desired nodes in simulated network
#' @returns as list: `new_g` - new network based on empirical degree distribution for given line/sex combo and threshold radius; `emp_density`- empricial network density, `emp_meandegree`- empirical mean degree of network; `sim_density`- simulated network density; `sim_meandegree` - simulated mean degree
#' @requires `igraph` library
#' @description - Function to pull social aggregation data for a given sex/line combo of fruit flies and generate a new, scaled-up network based on empirical degree distribution

scaleupdd<-function(df, line, sex, radius, nnodes){
  library(igraph)
  
  target_line<-df[which(df$Line==line),] #data for just target line
  
  #Narrow down target line by sex
  if(sex=="Female"){
    target_line<-target_line[which(target_line$Sex=="Female"),]
  } else if(sex== "Male"){
    target_line<-target_line[which(target_line$Sex=="Male"),]
  } else {
    print("Invalid Sex")
    break()
  }
  
  #Determine degree distribution (dd) for target radius
  if(radius==20){
    dd<-target_line$FliesWithin20mm
  } else if(radius==15){
    dd<-target_line$FliesWithin15mm
  } else if(radius==10) {
    dd<-target_line$FliesWithin10mm
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
  emp_density<-edge_density(g)
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
  sim_density<-edge_density(new_g) #check network statistics of new network
  sim_meandegree<-mean(igraph::degree(new_g))
  
  output<-list(new_g,emp_density,emp_meandegree, sim_density, sim_meandegree)
  return(output)
}



# Function: infectious ----------------------------------------------------
#' Function to get scaled infectiousness for given line/sex combo of flies
#' @param `df` - dataframe containing all viral shedding load data
#' @param `line` - genetic line number
#' @param `sex` - sex of fruit flies, either "f" or "m"
#' @param `g`- simulated network of flies for given line/sex combo
#' @param `scale_i`- scale the infectiousness; a scale_i=1 would give infectiousness values ranging from [0,1]
#' @return `g` -updated network with assigned infectiousness values
#' @requires `igraph` library
#' @description - Function to pull infectiousness data for a given sex/line combo of fruit flies and updated simulated network based on those values; assigns attribute V(g)$infectiousness to nodes in network

infectious<-function(df, line, sex, g, scale_i){
  sex<-ifelse(sex=="Female", "f", "m")  #adjust for naming in this data set
  #Scale infectiousness by maximum viral load in total (mixed) population (exlcude NA values)
  df<-df[!is.na(df$Virus.Shed.per.Fly),] #remove flies with NA values from dataset 
  # df$scaled_infect<-df$Virus.Shed.per.Fly/max(df$Virus.Shed.per.Fly, na.rm=TRUE)
  # df[df$Virus.Shed.per.Fly>0,]
  
  df$scaled_infect<-log(df$Virus.Shed.per.Fly)/log(max(df$Virus.Shed.per.Fly)) #scale on log scale
  df$scaled_infect[df$scaled_infect<0]<-0 #flies with original values of 0, rescaled to zero
  
  #hist(df$scaled_infect)
  
  #Isolate scaled infectiousness of target line
  scaled_infect<-df$scaled_infect[which(df$Line==line & df$Sex==sex)]
  
  h<-hist(scaled_infect, breaks=seq(from=0, to=max(scaled_infect)+0.001, by=0.001), right=TRUE, plot=FALSE)
  counts<-h$counts
  infect_scale<-seq(from=0, to=max(scaled_infect), by=0.001)
  pmass_infect<-counts/sum(counts)
  
  sim_infect<-sample(x=infect_scale, size=vcount(g), replace=TRUE, pmass_infect)
  sim_infect<-sim_infect*scale_i
  #hist(sim_infect)
  
  V(g)$infectiousness<-sim_infect
  return(g)
}



# Function: inf_dur -------------------------------------------------------
#' Function to get infection duration for given line/sex combo of flies
#' @param `df` - dataframe containing all viral shedding load data
#' @param `line` - genetic line number
#' @param `sex` - sex of fruit flies, either "Female" or "Male"
#' @param `g`- simulated network of flies for given line/sex combo
#' @return `g` -updated network with assigned infectiousness values
#' @requires `igraph` library
#' @description - Function to pull infection duration data for a given sex/line combo of fruit flies and update simulated network based on those values; assigns attribute V(g)$infectiousness to nodes in network

inf_dur<-function(df, line, sex, g){
  
  #Check to make sure that parameters match requirements of data set
  line_numbers<-unique(df$Line)
  if(!line %in% line_numbers){
    print("Invalid line number")
    break()
  }
  if(sex %in% c("Male", "Female")){
    
    duration<-df$Lifespan[which(df$Line==line & df$Sex==sex)]
    
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
    V(g)$mort_rate
    return(g)
    
  } else{
    print("Invalid sex")
    break()
  }
}



# Function: discordant ----------------------------------------------------
#' Function to calculate discordant edgelist for a given network `nw`
#' @author Lauren White
#' @parameter nw: igraph object
#' @parameter edgelist: edgelist for igraph object
#' @description Calculates discordant dyads in network, e.g., S/I pairs; Adapted from EpiModel package function

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


# Function: sim_network ---------------------------------------------------
#' Function to simulate disease spread on simulated networks
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
  #V(g)$inf_status
  
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



# Function: sim_networks --------------------------------------------------
#' Function to generate network and run `sim_network` in desired loop including all functions
#' @param `agg`- social aggregation dataframe
#' @param `infect`- infectiouness dataframe
#' @param `duration`- infection duration dataframe
#' @param `line`- desired line to test
#' @param `sex`- target sex ("Male" or "Female") to test
#' @param `radius`- target radius to test (10, 15, or 20 mm)
#' @param `nnodes`- the number of desired nodes in the network
#' @param `beta`- transmission efficiency of the pathogen [0,1]
#' @param `max.time`- how many timesteps to run simulation
#' @param `n.sims`- the number of simulations to run
#' @param `scale_i`- how to scale infectiousness (default of 1 would scale values from [0,1])

sim_networks<-function(agg, infect, duration, line, sex, radius, nnodes, beta, max.time, n.sims, scale_i){
  output<-list()
  I_dat<-matrix(data=numeric(), nrow=max.time, ncol=n.sims)
  for(i in 1:n.sims){
    test<-scaleupdd(df=agg,line=line, sex=sex, radius=radius, nnodes)
    g<-test[[1]]
    g<-infectious(df=infect, line=line, sex=sex, g=g, scale_i=scale_i)
    g<-inf_dur(df=duration, line=line, sex=sex, g=g)
    N<-sim_network(beta=beta, max.time=max.time, g=g)
    output$graph[[i]]<-g
    output$N[[i]]<-N
    output$scale_i<-scale_i
    output$line<-line
    output$sex<-sex
    output$radius<-radius
    output$beta<-beta
    I_dat[,i]<-N$timecourse$I
  }
  write.csv(I_dat, file=paste("Exp1Timecourse", line, sex, "R", radius, "beta", beta,"scale", scale_i, ".csv", sep=""))
  #summarystats(output, max.time, nnodes)
  return(output)
}


# Function: summary_stats -------------------------------------------------
#' A function that produces summary stats and timecourse plots for the output list produced by the `sim_networks` function
#' @function `summarystats`
#' @param `test`- list object output resulting from `sim_networks_pan`
#' @return both a .csv file of summary statistics and a .tiff image file of simulated timecourses

summarystats<-function(test, max.time, nnodes){
  L<-length(test$N)
  
  sum_stats<-data.frame(line=numeric(L), sex=character(L),  radius=numeric(L), beta=numeric(L), scale_i=numeric(L),  max_inf=numeric(L), time_max=numeric(L), duration=numeric(L), fadeout=logical(L), R0=numeric(L), stringsAsFactors = FALSE)
  for(i in 1:L){
    timecourse<-test$N[[i]]$timecourse
    sum_stats$max_inf[i]<-max(timecourse$I)
    sum_stats$time_max[i]<-which.max(timecourse$I)
    sum_stats$duration[i]<-which.min(timecourse$I)
    sum_stats$fadeout[i]<-max(timecourse$I)==1 #does the simulation spread beyond initally infected individual?
    sum_stats$R0[i]<-test$N[[i]]$R0
    sum_stats$line[i]<-test$line
    sum_stats$sex[i]<-test$sex
    sum_stats$radius[i]<-test$radius
    sum_stats$beta[i]<-test$beta
    sum_stats$scale_i[i]<-test$scale_i
  }
  
  write.csv(sum_stats, file=paste("Exp1Sum",test$line, test$sex, "R", test$radius, "beta", test$beta,"scale", test$scale_i, ".csv", sep=""))
  #save(test, file=paste(test$line, test$sex, "R", test$radius, "beta", test$beta,"scale", test$scale_i, ".RData", sep=""))
  
  #Plot time course data
  tiff(filename = paste("Exp1Sum", test$line, test$sex, "R", test$radius, "beta", test$beta, "scale", test$scale_i, ".tiff", sep=""))
  #Plot time course data
  maxI<-max(sum_stats$max_inf)
  plot(1, type="n", xlab="Time", ylab="Number of infected individuals", xlim=c(0, max.time), ylim=c(0, nnodes), main=paste(test$line, test$sex, sep= " "))
  for(i in 1:L){
    timecourse<-test$N[[i]]$timecourse
    lines(timecourse$t, y=timecourse$I)
  }
  dev.off()
  
  return(sum_stats)
}


# Test sim_networks & summarystats functions ------------------------------
#load library
# library(igraph)
# 
#load datasets
# agg<-read.csv("Dmel Fixed Radius Social Aggregation.csv")
# agg<-agg[which(agg$Infection=="Control"),] #Don't include sick flies in estimation of social aggregation
# infect<-read.csv("Dmel DCV Load and Shed.csv")
# duration<-read.csv("Dmel Infection Duration and Lethal Load.csv")
# 
# 
# 
# #use proc.time() to check how long it takes to run for n.sims
# ptm <- proc.time()
# test<-sim_networks(agg=agg, infect=infect, duration=duration, line=59, sex="Female", nnodes=1000, radius=20, beta=1, max.time=1000, n.sims=10, scale_i=1)
# #save(test, file="test.RData")
# stats<-summarystats(test=test, max.time=1000, nnodes=1000)
# proc.time()-ptm


# Function: sim_networks_parallel -----------------------------------------
#' function to sim_networks in parallel
#' 

sim_networks_parallel<-function(agg, infect, duration, line, sex, radius, nnodes, beta, max.time, n.sims, scale_i){
  print(paste(line, sex, "R", radius, "beta", beta,"scale", scale_i, sep=""))
  test<-sim_networks(agg=agg, infect=infect, duration=duration, line=line, sex=sex, nnodes=nnodes, radius=radius, beta=beta, max.time=max.time, n.sims=n.sims, scale_i=scale_i)
  stats<-summarystats(test=test, max.time=max.time, nnodes=nnodes)
  }



