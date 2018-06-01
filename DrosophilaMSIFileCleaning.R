#' Check and combine MSI output
#' Date: May 29, 2018
#' Author: Lauren White

# Read in file names from MSI Results directory ---------------------------
#' Experiment 1
#' Expecting 720 total= 360 summaries + 360 timecourses
filenames <- list.files(path = "C:/Users/law2y/OneDrive/R Code/Drosophila/MSI Results/Experiment1/")
summaries<-filenames[grep("Sum", filenames)] #summary data
infecteds<-filenames[grep("Timecourse", filenames)] #I data
setwd("C:/Users/law2y/OneDrive/R Code/Drosophila/MSI Results/Experiment1/")
merged_data1<- do.call("rbind", lapply(summaries, read.csv, header = TRUE))
merged_data1$sex[merged_data1$sex == 1] <- "Female"
merged_data1$sex[merged_data1$sex == 2] <- "Male"
setwd("C:/Users/law2y/OneDrive/R Code/Drosophila/MSI Results/")
write.csv(merged_data1, "Experiment1Summary.csv")

#' Experiment 2
#' Expecting 720 total= 360 summaries + 360 timecourses
filenames2 <- list.files(path = "C:/Users/law2y/OneDrive/R Code/Drosophila/MSI Results/Experiment2/")
summaries2<-filenames2[grep("Index", filenames2)] #summary data
infecteds2<-filenames2[grep("Timecourse", filenames2)] #I data
setwd("C:/Users/law2y/OneDrive/R Code/Drosophila/MSI Results/Experiment2/")
merged_data2<- do.call("rbind", lapply(summaries2, read.csv, header = TRUE))
merged_data2$index_sex[merged_data2$index_sex == 1] <- "Female"
merged_data2$index_sex[merged_data2$index_sex == 2] <- "Male"


setwd("C:/Users/law2y/OneDrive/R Code/Drosophila/MSI Results/")
write.csv(merged_data2, "Experiment2Summary.csv")

#' Experiment 3
#' Expecting 288 total= 144 summaries + 144 timecourses
filenames3 <- list.files(path = "C:/Users/law2y/OneDrive/R Code/Drosophila/MSI Results/Experiment3/")
summaries3<-filenames3[grep("Sum", filenames3)] #summary data
infecteds3<-filenames3[grep("Timecourse", filenames3)] #I data
setwd("C:/Users/law2y/OneDrive/R Code/Drosophila/MSI Results/Experiment3/")
merged_data3<- do.call("rbind", lapply(summaries3, read.csv, header = TRUE))
setwd("C:/Users/law2y/OneDrive/R Code/Drosophila/MSI Results/")
write.csv(merged_data3, "Experiment3Summary.csv")


##Check to make sure that the expected number of runs has been completed for each experiment
AggFALSE_sum<-summaries3[grep("AggFALSE", summaries3, fixed=TRUE)] 
AggTRUE_sum<-summaries3[grep("AggTRUE", summaries3, fixed=TRUE)]                  

DurFALSE_sum<-summaries3[grep("DurFALSE", summaries3, fixed=TRUE)] 
DurTRUE_sum<-summaries3[grep("DurTRUE", summaries3, fixed=TRUE)]

InfectFALSE_sum<-summaries3[grep("InfectFALSE", summaries3, fixed=TRUE)] 
InfectTRUE_sum<-summaries3[grep("InfectTRUE", summaries3, fixed=TRUE)]  

radius10<-summaries3[grep("radius10", summaries3, fixed=TRUE)]
radius15<-summaries3[grep("radius15", summaries3, fixed=TRUE)]
radius20<-summaries3[grep("radius20", summaries3, fixed=TRUE)]

beta0.1<-summaries3[grep("beta0.1", summaries3, fixed=TRUE)]
beta0.5<-summaries3[grep("beta0.5", summaries3, fixed=TRUE)]
beta1<-summaries3[grep("beta1", summaries3, fixed=TRUE)]

scale_i1<-summaries3[grep("scale_i1", summaries3, fixed=TRUE)]
scale_i2<-summaries3[grep("scale_i2", summaries3, fixed=TRUE)]
