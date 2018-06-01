#' Functions need to run Experiment 2 in parallel
#' @author: Lauren White, whit1951@umn.edu
#' @date: May 26, 2018

#' @function `dd_sub`
#' @param `df` - dataframe containing all social aggregation data
#' @param `line` - genetic line number
#' @param `sex` - sex of fruit flies, either "Female" or "Male"
#' @param `radius` - desired fix radius measurement to use (10 mm, 15 mm, or 20 mm)
#' @param `nnodes`- number of desired nodes in simulated network
#' @returns: `out.deg` - vector of degrees for `nnodes` in network
#' @requires `igraph` library
#' @description - Function to pull social aggregation data for a given sex/line combo of fruit flies and return a vector of expected degrees for `nnodes`number of nodes

dd_sub<-function(df, line, sex, radius, nnodes){
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
  
  out.deg<-test

  return(out.deg)
}




## Function to get scaled infectiousness for given line/sex combo of flies
#' @function infectious_prop
#' @param `df` - dataframe containing all viral shedding load data
#' @param `line` - genetic line number
#' @param `sex` - sex of fruit flies, either "f" or "m"
#' @param `g`- simulated network of flies for given line/sex combo
#' @param `scale_i` - scale the infectiousness; e.g., a value of `scale_i=1` would give infectiousness values ranging from [0,1]
#' @return `infect` -vector of infectiousness values
#' @requires `igraph` library
#' @description - Function to pull infectiousness data for a given sex/line combo of fruit flies 

infectious_prop<-function(df, line, sex, nnodes, scale_i){
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

sim_infect<-sample(x=infect_scale, size=nnodes, replace=TRUE, pmass_infect)
#hist(sim_infect)
sim_infect<-sim_infect*scale_i

#sim_infect
return(sim_infect)
}


## Function to get infection duration for given line/sex combo of flies
#' @function `inf_dur_prop`
#' @param `df` - dataframe containing all viral shedding load data
#' @param `line` - genetic line number
#' @param `sex` - sex of fruit flies, either "Female" or "Male"
#' @param `nnodes`- desired number of infection duration values generated
#' @return `mort_rate` -vector of expected mortality rates for `nnodes` number of nodes
#' @requires `igraph` library
#' @description - Function to pull infection duration data for a given sex/line combo of fruit flies 

inf_dur_prop<-function(df, line, sex, nnodes){

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
  sim_dur<-sample(dur, size=nnodes, replace=TRUE, pmass_dur)
  #hist(sim_dur)
  mort_rate<-1/sim_dur #mortality rate is the inverse of lifespan
  #hist(mort_rate) #This should be preassigned to each individual
  
  return(mort_rate)
  
  } else{
     print("Invalid sex")
      break()
  }
}



## Test updated functions independently

agg<-read.csv("Dmel Fixed Radius Social Aggregation.csv")
agg<-agg[which(agg$Infection=="Control"),] #Don't include sick flies in estimation of social aggregation
infect<-read.csv("Dmel DCV Load and Shed.csv")
duration<-read.csv("Dmel Infection Duration and Lethal Load.csv")

nnodes<-1000 #number of desired nodes in network/flys in population
subp<-nnodes/20 #number of nodes in sub-population for specific line/sex combo
r<-20 #radius

dd<-dd_sub(df=agg, line=59, sex="Female", radius=20, nnodes=subp)
infectious<-infectious_prop(df=infect, line=59, sex="Female", nnodes=subp, scale_i=1)
mort_rate<-inf_dur_prop(df=duration, line=59, sex="Female", nnodes=subp)


## Put it all together for a network of `nnodes` size, divided into `subp` populations for a given radius `r`
#' @function deg.dist.df
#' @param `agg`- social aggregation dataframe
#' @param `infect`- infectiouness dataframe
#' @param `duration`- infection duration dataframe
#' @param `radius`- target radius to test (10, 15, or 20 mm)
#' @param `nnodes`- the number of desired nodes in the network
#' @param `scale_i`- how to scale infectiousness (default of 1 would scale values from [0,1])
#' @param `subp`- number of nodes in sub-population for specific line/sex combo
#' @return `g`- an igraph network
#' @description generates a network (with infectiousness, mortality rate traits, sex & line traits) that has been created by sampling equally from each of the 20 sex/line combos

nnodes<-1000 #number of desired nodes in network/flys in population
subp<-nnodes/20 #number of nodes in sub-population for specific line/sex combo
radius<-20 #radius

equalprop<-function(agg, infect, duration, radius, nnodes, subp, scale_i=scale_i){
  
deg.dist.df<-data.frame(line=NULL, sex=NULL, dd=NULL, infectiousness=NULL, mort_rate=NULL)

line_numbers<-unique(agg$Line)
sexes<-c("Female", "Male")
for (i in 1:length(line_numbers)) #line numbers
{
  line<-line_numbers[i]
  for (j in 1:2){ #sexes
    sex<-sexes[j]
    dd<-dd_sub(df=agg, line=line, sex=sex, radius= radius, nnodes=subp)
    infectiousness<-infectious_prop(df=infect, line=line, sex=sex, nnodes=subp, scale_i=scale_i)
    mort_rate<-inf_dur_prop(df=duration, line=line, sex=sex, nnodes=subp)
    line_col<-rep(line, subp)
    sex_col<-rep(sex, subp)
    sub_df<-data.frame(line=line_col, sex=sex_col, dd=dd, infectiousness=infectiousness, mort_rate=mort_rate)
    deg.dist.df<-rbind(sub_df, deg.dist.df)
  }
}

g<-igraph::sample_degseq(out.deg = deg.dist.df$dd, method = "simple.no.multiple")

#attach remaining attribues to randomly generated graph
V(g)$line<-deg.dist.df$line
V(g)$sex<-as.character(deg.dist.df$sex)
V(g)$infectiousness<-(deg.dist.df$infectiousness)
V(g)$mort_rate<-deg.dist.df$mort_rate

return(g)
}

g<-equalprop(agg=agg, infect=infect, duration=duration, radius=radius, nnodes=nnodes, subp=subp, scale_i=1)



## Function to calculate discordant edgelist for a given network `nw`
#' @function discordant
#' @author Lauren White
#' @parameter `nw`: igraph object
#' @parameter `edgelist`: edgelist for igraph object
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

## Function to simulate disease spread on simulated networks with a given index case of a particular line/sex
#' @function `sim_network_index`
#' @param `beta`- transmission efficiency of the pathogen
#' @param `max.time`- maximum duration to run simulations
#' @param `g`- network on which to simulate disease spread
#' @param `line`- index line
#' @param `sex`- index sex
#' @return `N`- timecourse of prevelance in population (S,I,R)

sim_network_index<-function(beta, max.time, g, line, sex){
sub<-V(g)[V(g)$line==line & V(g)$sex==sex]
#define the network
n<-length(sub) #size of subset

#Randomly select who starts off infected
index_case <- sample(sub, size=1) #just one infected individual to start
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


## Simulate spread of pathogen in a for loop for `n.sims` number of simulations
#' @function `sim_networks_prop`
#' @param `agg`- social aggregation dataframe
#' @param `infect`- infectiouness dataframe
#' @param `duration`- infection duration dataframe
#' @param `radius`- target radius to test (10, 15, or 20 mm)
#' @param `nnodes`- the number of desired nodes in the network
#' @param `subp`- number of nodes in sub-population for specific line/sex combo
#' @param `scale_i`- how to scale infectiousness (default of 1 would scale values from [0,1])
#' @param `beta`- transmission efficiency of the pathogen
#' @param `max.time`- maximum duration to run simulations
#' @param `n.sims`- number of simulations to run
#' @param `index_line`- target line to use as index case
#' @param `index_sex`-target sex to use as index case
#' @return `g`- an igraph network
#' @description simulates networks (with infectiousness, mortality rate traits, sex & line traits) that have been created by sampling equally from each of the 20 sex/line combos for `n.sims` number of simulations

sim_networks_prop<-function(agg, infect, duration, radius, nnodes, subp, scale_i, beta, max.time, n.sims, index_line, index_sex){
  
  output<-list()
  I_dat<-matrix(data=numeric(), nrow=max.time, ncol=n.sims)
  
  for(i in 1:n.sims){
    
  g<-equalprop(agg=agg, infect=infect, duration=duration, radius=radius, nnodes=nnodes, subp=subp, scale_i=scale_i)
  N<-sim_network_index(beta=beta, max.time=max.time, g=g, line=index_line, sex=index_sex)
  output$graph[[i]]<-g
  output$N[[i]]<-N
  output$radius<-radius
  output$beta<-beta
  output$index_line<-index_line
  output$index_sex<-index_sex
  output$scale_i<-scale_i
  I_dat[,i]<-N$timecourse$I
  }
  
  write.csv(I_dat, file=paste("Exp2Timecourse", index_line, index_sex, "R", radius, "beta", beta,"scale", scale_i, ".csv", sep=""))
  return(output)
}

## Function to plot results and generate summary stats
#' @function summarystats
#' @param `test`- list object output resulting from `sim_networks_prop`
#' @return both a .csv file of summary statistics and a .tiff image file of simulated timecourses

#Generate some summary stats
summarystats_prop<-function(test, nnodes, max.time){
L<-length(test$N)
sum_stats<-data.frame(index_line=numeric(L), index_sex=character(L), beta=numeric(L), radius=numeric(L), scale_i=numeric(L), max_inf=numeric(L), time_max=numeric(L), duration=numeric(L), fadeout=logical(L), R0=numeric(L), stringsAsFactors = FALSE)

for(i in 1:L){
  timecourse<-test$N[[i]]$timecourse
  sum_stats$max_inf[i]<-max(timecourse$I)
  sum_stats$time_max[i]<-which.max(timecourse$I)
  sum_stats$duration[i]<-which.min(timecourse$I)
  sum_stats$fadeout[i]<-max(timecourse$I)==1 #does the simulation spread beyond initally infected individual
  sum_stats$R0[i]<-test$N[[i]]$R0
  sum_stats$index_line[i]<-test$index_line
  sum_stats$index_sex[i]<-test$index_sex
  sum_stats$scale_i[i]<-test$scale_i
  sum_stats$beta[i]<-test$beta
  sum_stats$radius[i]<-test$radius
}

write.csv(sum_stats, file=paste("Exp2Index", test$index_line, test$index_sex,"beta", test$beta, "radius", test$radius, "scale_i", test$scale_i, ".csv", sep=""))

#Plot time course data
tiff(filename = paste("Exp2Index", test$index_line, test$index_sex,"beta", test$beta, "radius", test$radius, "scale_i", test$scale_i, ".tiff", sep=""))
maxI<-max(sum_stats$max_inf)
plot(1, type="n", xlab="Time", ylab="Number of infected individuals", xlim=c(0, max.time), ylim=c(0, nnodes+1), main=paste("Index case=", test$index_line, test$index_sex, sep=""))
for(i in 1:L){
  timecourse<-test$N[[i]]$timecourse
  lines(timecourse$t, y=timecourse$I)
}
dev.off()
}

# ## Test `sim_networks_prop` function and `summarystats_prop`function (run the simulation and record/plot the results)
# #load library
# library(igraph)
# 
# #load datasets
# agg<-read.csv("Dmel Fixed Radius Social Aggregation.csv")
# agg<-agg[which(agg$Infection=="Control"),] #Don't include sick flies in estimation of social aggregation
# infect<-read.csv("Dmel DCV Load and Shed.csv")
# duration<-read.csv("Dmel Infection Duration and Lethal Load.csv")
# 
# nnodes<-1000 #number of desired nodes in network/flys in population
# subp<-nnodes/20 #number of nodes in sub-population for specific line/sex combo
# radius<-20 #radius
# 
# test<-sim_networks_prop(agg=agg, infect=infect, duration=duration, nnodes=nnodes, subp=subp, scale_i=1, radius=radius, beta=1, max.time=100, n.sims=2, index_line = 59, index_sex="Male")
# 
# summarystats_prop(test, max.time=100, nnodes=1000)


# Function: sim_networks_parallel -----------------------------------------
#' function to sim_networks in parallel
#' 

sim_networks_prop_parallel<-function(agg, infect, duration, radius, nnodes, subp, scale_i, beta, max.time, n.sims, index_line, index_sex){
  test<-sim_networks_prop(agg=agg, infect=infect, duration=duration, radius=radius, nnodes=nnodes, subp=subp, scale_i=scale_i, beta=beta, max.time=max.time, n.sims=n.sims, index_line=index_line, index_sex=index_sex)
  stats<-summarystats_prop(test=test, max.time=max.time, nnodes=nnodes)
}


## Let's briefly look at all the combos of line/sex index cases
# line_numbers<-unique(agg$Line)
# sex<-c("Female", "Male")
# 
# setwd("C:/Users/law2y/OneDrive/R Code/Drosophila/Experiment2Results")
# 
# for(i in 1:length(line_numbers)){
#   for (j in 1:length(sex)){
#       test<-sim_networks_prop(agg=agg, infect=infect, duration=duration, nnodes=nnodes, subp=subp, scale_i=1, radius=radius, beta=1, max.time=100, n.sims=10, index_line=line_numbers[i], index_sex=sex[j])
#       summarystats_prop(test)
#   }
# }