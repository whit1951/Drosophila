#' Drosophila data analysis 
#' Author: Lauren White
#' Date: September 17, 2018

# Load libraries
library(doBy)
library(ggplot2)
library(viridis)
library(ggthemes)
library(scales)
library(lsr)

# Load data
#setwd("C:/Users/law2y/OneDrive/R Code/Drosophila/MSI Results/")
# Exp1<-read.csv("Experiment1Summary.csv")
# Exp1<-Exp1[,-1] #get rid of X.1 column
# Exp2<-read.csv("Experiment2Summary.csv")
# Exp2<-Exp2[,-1]  #get rid of X.1 column
# Exp3<-read.csv("Experiment3Summary.csv")
# Exp2<-Exp2[,-1]  #get rid of X.1 column

#' # Read data file from Network File Storage
Exp1 <- read.csv('/nfs/lwhite-data/Drosophila/Experiment1Summary.csv')
Exp2 <- read.csv('/nfs/lwhite-data/Drosophila/Experiment2Summary.csv')
Exp3 <- read.csv('/nfs/lwhite-data/Drosophila/Experiment3Summary.csv')

#'Get rid of extra columns in dfs
Exp1$X <- NULL
Exp1$X.1 <- NULL

Exp2$X <- NULL
Exp2$X.1 <- NULL

Exp3$X <- NULL
Exp3$X.1 <- NULL

#'Subdivide experiment 1 by beta
Exp1beta1<-Exp1[which(Exp1$beta==1),]
Exp1beta0.5<-Exp1[which(Exp1$beta==0.5),]
Exp1beta0.1<-Exp1[which(Exp1$beta==0.1),]

# Box plots --------------------------------------------

tiff("boxplot_maxI.tiff", height = 5.3, width = 5.8, units = "in", compression = "lzw", res = 300)
gg<- ggplot(data=Exp1beta1, aes(x=as.factor(line), y=max_inf)) #, fill=covariation)
gg<- gg+ geom_boxplot(stat="boxplot", position=position_dodge())
gg<- gg + facet_grid(sex ~ as.factor(scale_i))
gg<- gg+ labs(x = "Line", y = "Maximum number of infected flies")
#gg<-gg +theme_grey()
#gg<-gg + theme(plot.title = element_text(hjust = 0.5))
gg<- gg+ theme(axis.title = element_text(face="bold", size=19))
gg<- gg + theme(axis.text.x = element_text(angle=90, hjust= 0, vjust=0.5, size=16))
gg<- gg + theme(axis.text.y = element_text(vjust=0.5, size=16)) 
gg<- gg + theme(strip.text = element_text(vjust=0.5, size=16)) 
#gg <- gg+ theme(legend.position="none")
#gg<-gg+  theme( axis.ticks.length=unit(-0.1, "cm")) #, axis.ticks.margin=unit(0.5, "cm"))
#gg<-gg+ theme(axis.ticks.x=element_blank())
#gg<-gg +theme (axis.text.x = element_text(margin=unit(c(0.15,0.15,0.15,0.15), "cm")), axis.text.y = element_text(margin=unit(c(0.15,0.15,0.15,0.15), "cm")))
gg
dev.off()

tiff("boxplot_dur.tiff", height = 5.3, width = 5.8, units = "in", compression = "lzw", res = 300)
gg<- ggplot(data=Exp1beta1, aes(x=as.factor(line), y=time_max)) #, fill=covariation)
gg<- gg+ geom_boxplot(stat="boxplot", position=position_dodge())
gg<- gg + facet_grid(sex ~ as.factor(scale_i))
gg<- gg+ labs(x = "Line", y = "Duration")
#gg<-gg +theme_grey()
#gg<-gg + theme(plot.title = element_text(hjust = 0.5))
gg<- gg+ theme(axis.title = element_text(face="bold", size=19))
gg<- gg + theme(axis.text.x = element_text(angle=90, hjust= 0, vjust=0.5, size=16))
gg<- gg + theme(axis.text.y = element_text(vjust=0.5, size=16)) 
gg<- gg + theme(strip.text = element_text(vjust=0.5, size=16)) 
#gg <- gg+ theme(legend.position="none")
#gg<-gg+  theme( axis.ticks.length=unit(-0.1, "cm")) #, axis.ticks.margin=unit(0.5, "cm"))
#gg<-gg+ theme(axis.ticks.x=element_blank())
#gg<-gg +theme (axis.text.x = element_text(margin=unit(c(0.15,0.15,0.15,0.15), "cm")), axis.text.y = element_text(margin=unit(c(0.15,0.15,0.15,0.15), "cm")))
gg
dev.off()


# Heat maps ---------------------------------------------------------------
#Heatmap maximum number of infected individuals
tiff("Exp1beta1_maxI.tiff", height = 7, width = 13, units = "in", compression = "lzw", res = 300)
sdf <- summaryBy(max_inf~line+ sex+radius+ scale_i, data=Exp1beta1, FUN=mean)
gg <- ggplot(sdf, aes(x=as.factor(line), y=as.factor(sex), fill=max_inf.mean))
#sdf$betas <- ordered(sdf$betas, levels = c("0 -6", "0 -3", "0 0", "3 -6", "3 -3", "3 0", "6 -6", "6 -3", "6 0"))
gg <- gg + geom_tile(color="white", size=0.1)
gg <- gg + scale_fill_viridis(name="Maximum number of \n infected flies")
gg <- gg + coord_equal()
gg <- gg + facet_grid(radius ~ scale_i)
gg<- gg+ labs(x="Line", y="Sex")
gg<- gg+ theme(axis.title = element_text(face="bold", size=24))
gg<- gg + theme(axis.text.x = element_text(angle=90, hjust=0, vjust=0.5, size=16))
gg<- gg + theme(axis.text.y = element_text(vjust=0.5, size=16)) 
gg<- gg + theme(strip.text = element_text(vjust=0.5, size=18))
gg<- gg + theme(strip.text.x = element_text(angle = 90))
gg<- gg + theme(legend.text = element_text(vjust=0.5, size=14), legend.title=element_text(vjust=0.5, size=20))
gg
dev.off()

#Heatmap duration
tiff("Exp1beta1_dur.tiff", height = 7, width = 13, units = "in", compression = "lzw", res = 300)
sdf <- summaryBy(duration~line+ sex+radius+ scale_i, data=Exp1beta1, FUN=mean)
gg <- ggplot(sdf, aes(x=as.factor(line), y=as.factor(sex), fill=duration.mean))
#sdf$betas <- ordered(sdf$betas, levels = c("0 -6", "0 -3", "0 0", "3 -6", "3 -3", "3 0", "6 -6", "6 -3", "6 0"))
gg <- gg + geom_tile(color="white", size=0.1)
gg <- gg + scale_fill_viridis(name="Duration")
gg <- gg + coord_equal()
gg <- gg + facet_grid(radius ~ scale_i)
gg<- gg+ labs(x="Line", y="Sex")
gg<- gg+ theme(axis.title = element_text(face="bold", size=24))
gg<- gg + theme(axis.text.x = element_text(angle=90, hjust=0, vjust=0.5, size=16))
gg<- gg + theme(axis.text.y = element_text(vjust=0.5, size=16)) 
gg<- gg + theme(strip.text = element_text(vjust=0.5, size=18))
gg<- gg + theme(strip.text.x = element_text(angle = 90))
gg<- gg + theme(legend.text = element_text(vjust=0.5, size=14), legend.title=element_text(vjust=0.5, size=20))
gg
dev.off()


tiff("Exp1beta0.1_maxI.tiff", height = 7, width = 13, units = "in", compression = "lzw", res = 300)
sdf <- summaryBy(max_inf~line+ sex+radius+ scale_i, data=Exp1beta0.1, FUN=mean)
gg <- ggplot(sdf, aes(x=as.factor(line), y=as.factor(sex), fill=max_inf.mean))
#sdf$betas <- ordered(sdf$betas, levels = c("0 -6", "0 -3", "0 0", "3 -6", "3 -3", "3 0", "6 -6", "6 -3", "6 0"))
gg <- gg + geom_tile(color="white", size=0.1)
gg <- gg + scale_fill_viridis(name="Maximum number of \n infected flies")
gg <- gg + coord_equal()
gg <- gg + facet_grid(radius ~ scale_i)
gg<- gg+ labs(x="Line", y="Sex")
gg<- gg+ theme(axis.title = element_text(face="bold", size=24))
gg<- gg + theme(axis.text.x = element_text(angle=90, hjust=0, vjust=0.5, size=16))
gg<- gg + theme(axis.text.y = element_text(vjust=0.5, size=16)) 
gg<- gg + theme(strip.text = element_text(vjust=0.5, size=18))
gg<- gg + theme(strip.text.x = element_text(angle = 90))
gg<- gg + theme(legend.text = element_text(vjust=0.5, size=14), legend.title=element_text(vjust=0.5, size=20))
gg
dev.off()

tiff("Exp1beta0.1_dur.tiff", height = 7, width = 13, units = "in", compression = "lzw", res = 300)
sdf <- summaryBy(duration~line+ sex+radius+ scale_i, data=Exp1beta0.1, FUN=mean)
gg <- ggplot(sdf, aes(x=as.factor(line), y=as.factor(sex), fill=duration.mean))
#sdf$betas <- ordered(sdf$betas, levels = c("0 -6", "0 -3", "0 0", "3 -6", "3 -3", "3 0", "6 -6", "6 -3", "6 0"))
gg <- gg + geom_tile(color="white", size=0.1)
gg <- gg + scale_fill_viridis(name="Duration")
gg <- gg + coord_equal()
gg <- gg + facet_grid(radius ~ scale_i)
gg<- gg+ labs(x="Line", y="Sex")
gg<- gg+ theme(axis.title = element_text(face="bold", size=24))
gg<- gg + theme(axis.text.x = element_text(angle=90, hjust=0, vjust=0.5, size=16))
gg<- gg + theme(axis.text.y = element_text(vjust=0.5, size=16)) 
gg<- gg + theme(strip.text = element_text(vjust=0.5, size=18))
gg<- gg + theme(strip.text.x = element_text(angle = 90))
gg<- gg + theme(legend.text = element_text(vjust=0.5, size=14), legend.title=element_text(vjust=0.5, size=20))
gg
dev.off()

tiff("Exp1beta0.5_maxI.tiff", height = 7, width = 13, units = "in", compression = "lzw", res = 300)
sdf <- summaryBy(max_inf~line+ sex+radius+ scale_i, data=Exp1beta0.5, FUN=mean)
gg <- ggplot(sdf, aes(x=as.factor(line), y=as.factor(sex), fill=max_inf.mean))
#sdf$betas <- ordered(sdf$betas, levels = c("0 -6", "0 -3", "0 0", "3 -6", "3 -3", "3 0", "6 -6", "6 -3", "6 0"))
gg <- gg + geom_tile(color="white", size=0.1)
gg <- gg + scale_fill_viridis(name="Maximum number of \n infected flies")
gg <- gg + coord_equal()
gg <- gg + facet_grid(radius ~ scale_i)
gg<- gg+ labs(x="Line", y="Sex")
gg<- gg+ theme(axis.title = element_text(face="bold", size=24))
gg<- gg + theme(axis.text.x = element_text(angle=90, hjust=0, vjust=0.5, size=16))
gg<- gg + theme(axis.text.y = element_text(vjust=0.5, size=16)) 
gg<- gg + theme(strip.text = element_text(vjust=0.5, size=18))
gg<- gg + theme(strip.text.x = element_text(angle = 90))
gg<- gg + theme(legend.text = element_text(vjust=0.5, size=14), legend.title=element_text(vjust=0.5, size=20))
gg
dev.off()

tiff("Exp1beta0.5_dur.tiff", height = 7, width = 13, units = "in", compression = "lzw", res = 300)
sdf <- summaryBy(duration~line+ sex+radius+ scale_i, data=Exp1beta0.1, FUN=mean)
gg <- ggplot(sdf, aes(x=as.factor(line), y=as.factor(sex), fill=duration.mean))
#sdf$betas <- ordered(sdf$betas, levels = c("0 -6", "0 -3", "0 0", "3 -6", "3 -3", "3 0", "6 -6", "6 -3", "6 0"))
gg <- gg + geom_tile(color="white", size=0.1)
gg <- gg + scale_fill_viridis(name="Duration")
gg <- gg + coord_equal()
gg <- gg + facet_grid(radius ~ scale_i)
gg<- gg+ labs(x="Line", y="Sex")
gg<- gg+ theme(axis.title = element_text(face="bold", size=24))
gg<- gg + theme(axis.text.x = element_text(angle=90, hjust=0, vjust=0.5, size=16))
gg<- gg + theme(axis.text.y = element_text(vjust=0.5, size=16)) 
gg<- gg + theme(strip.text = element_text(vjust=0.5, size=18))
gg<- gg + theme(strip.text.x = element_text(angle = 90))
gg<- gg + theme(legend.text = element_text(vjust=0.5, size=14), legend.title=element_text(vjust=0.5, size=20))
gg
dev.off()

#' ------------------------------------------------------------------------
# Exp. 2 ------------------------------------------------------------------
#' ------------------------------------------------------------------------

#' Subdivide experiment 2 by beta
Exp2beta1<-Exp2[which(Exp2$beta==1),]
Exp2beta0.5<-Exp2[which(Exp2$beta==0.5),]
Exp2beta0.1<-Exp2[which(Exp2$beta==0.1),]

#' Beta= 0.1

tiff("Exp2beta0.1_maxI.tiff", height = 7, width = 13, units = "in", compression = "lzw", res = 300)
sdf <- summaryBy(max_inf~index_line+ index_sex+radius+ scale_i, data=Exp2beta0.1, FUN=mean)
gg <- ggplot(sdf, aes(x=as.factor(index_line), y=as.factor(index_sex), fill=max_inf.mean))
#sdf$betas <- ordered(sdf$betas, levels = c("0 -6", "0 -3", "0 0", "3 -6", "3 -3", "3 0", "6 -6", "6 -3", "6 0"))
gg <- gg + geom_tile(color="white", size=0.1)
gg <- gg + scale_fill_viridis(name="Maximum number of \n infected flies")
gg <- gg + coord_equal()
gg <- gg + facet_grid(radius ~ scale_i)
gg<- gg+ labs(x="Line", y="Sex")
gg<- gg+ theme(axis.title = element_text(face="bold", size=24))
gg<- gg + theme(axis.text.x = element_text(angle=90, hjust=0, vjust=0.5, size=16))
gg<- gg + theme(axis.text.y = element_text(vjust=0.5, size=16)) 
gg<- gg + theme(strip.text = element_text(vjust=0.5, size=18))
gg<- gg + theme(strip.text.x = element_text(angle = 90))
gg<- gg + theme(legend.text = element_text(vjust=0.5, size=14), legend.title=element_text(vjust=0.5, size=20))
gg
dev.off()

tiff("Exp2beta0.1_dur.tiff", height = 7, width = 13, units = "in", compression = "lzw", res = 300)
sdf <- summaryBy(duration~index_line+ index_sex+radius+ scale_i, data=Exp2beta0.1, FUN=mean)
gg <- ggplot(sdf, aes(x=as.factor(index_line), y=as.factor(index_sex), fill=duration.mean))
#sdf$betas <- ordered(sdf$betas, levels = c("0 -6", "0 -3", "0 0", "3 -6", "3 -3", "3 0", "6 -6", "6 -3", "6 0"))
gg <- gg + geom_tile(color="white", size=0.1)
gg <- gg + scale_fill_viridis(name="Duration")
gg <- gg + coord_equal()
gg <- gg + facet_grid(radius ~ scale_i)
gg<- gg+ labs(x="Line", y="Sex")
gg<- gg+ theme(axis.title = element_text(face="bold", size=24))
gg<- gg + theme(axis.text.x = element_text(angle=90, hjust=0, vjust=0.5, size=16))
gg<- gg + theme(axis.text.y = element_text(vjust=0.5, size=16)) 
gg<- gg + theme(strip.text = element_text(vjust=0.5, size=18))
gg<- gg + theme(strip.text.x = element_text(angle = 90))
gg<- gg + theme(legend.text = element_text(vjust=0.5, size=14), legend.title=element_text(vjust=0.5, size=20))
gg
dev.off()

tiff("Exp2beta0.1_R0.tiff", height = 7, width = 13, units = "in", compression = "lzw", res = 300)
sdf <- summaryBy(R0~index_line+ index_sex+radius+ scale_i, data=Exp2beta0.1, FUN=mean)
gg <- ggplot(sdf, aes(x=as.factor(index_line), y=as.factor(index_sex), fill=R0.mean))
#sdf$betas <- ordered(sdf$betas, levels = c("0 -6", "0 -3", "0 0", "3 -6", "3 -3", "3 0", "6 -6", "6 -3", "6 0"))
gg <- gg + geom_tile(color="white", size=0.1)
gg <- gg + scale_fill_viridis(name="R0")
gg <- gg + coord_equal()
gg <- gg + facet_grid(radius ~ scale_i)
gg<- gg+ labs(x="Line", y="Sex")
gg<- gg+ theme(axis.title = element_text(face="bold", size=24))
gg<- gg + theme(axis.text.x = element_text(angle=90, hjust=0, vjust=0.5, size=16))
gg<- gg + theme(axis.text.y = element_text(vjust=0.5, size=16)) 
gg<- gg + theme(strip.text = element_text(vjust=0.5, size=18))
gg<- gg + theme(strip.text.x = element_text(angle = 90))
gg<- gg + theme(legend.text = element_text(vjust=0.5, size=14), legend.title=element_text(vjust=0.5, size=20))
gg
dev.off()

#' Beta= 0.5

tiff("Exp2beta0.5_maxI.tiff", height = 7, width = 13, units = "in", compression = "lzw", res = 300)
sdf <- summaryBy(max_inf~index_line+ index_sex+radius+ scale_i, data=Exp2beta0.5, FUN=mean)
gg <- ggplot(sdf, aes(x=as.factor(index_line), y=as.factor(index_sex), fill=max_inf.mean))
#sdf$betas <- ordered(sdf$betas, levels = c("0 -6", "0 -3", "0 0", "3 -6", "3 -3", "3 0", "6 -6", "6 -3", "6 0"))
gg <- gg + geom_tile(color="white", size=0.1)
gg <- gg + scale_fill_viridis(name="Maximum number of \n infected flies")
gg <- gg + coord_equal()
gg <- gg + facet_grid(radius ~ scale_i)
gg<- gg+ labs(x="Line", y="Sex")
gg<- gg+ theme(axis.title = element_text(face="bold", size=24))
gg<- gg + theme(axis.text.x = element_text(angle=90, hjust=0, vjust=0.5, size=16))
gg<- gg + theme(axis.text.y = element_text(vjust=0.5, size=16)) 
gg<- gg + theme(strip.text = element_text(vjust=0.5, size=18))
gg<- gg + theme(strip.text.x = element_text(angle = 90))
gg<- gg + theme(legend.text = element_text(vjust=0.5, size=14), legend.title=element_text(vjust=0.5, size=20))
gg
dev.off()

tiff("Exp2beta0.5_dur.tiff", height = 7, width = 13, units = "in", compression = "lzw", res = 300)
sdf <- summaryBy(duration~index_line+ index_sex+radius+ scale_i, data=Exp2beta0.5, FUN=mean)
gg <- ggplot(sdf, aes(x=as.factor(index_line), y=as.factor(index_sex), fill=duration.mean))
#sdf$betas <- ordered(sdf$betas, levels = c("0 -6", "0 -3", "0 0", "3 -6", "3 -3", "3 0", "6 -6", "6 -3", "6 0"))
gg <- gg + geom_tile(color="white", size=0.1)
gg <- gg + scale_fill_viridis(name="Duration")
gg <- gg + coord_equal()
gg <- gg + facet_grid(radius ~ scale_i)
gg<- gg+ labs(x="Line", y="Sex")
gg<- gg+ theme(axis.title = element_text(face="bold", size=24))
gg<- gg + theme(axis.text.x = element_text(angle=90, hjust=0, vjust=0.5, size=16))
gg<- gg + theme(axis.text.y = element_text(vjust=0.5, size=16)) 
gg<- gg + theme(strip.text = element_text(vjust=0.5, size=18))
gg<- gg + theme(strip.text.x = element_text(angle = 90))
gg<- gg + theme(legend.text = element_text(vjust=0.5, size=14), legend.title=element_text(vjust=0.5, size=20))
gg
dev.off()

tiff("Exp2beta0.5_R0.tiff", height = 7, width = 13, units = "in", compression = "lzw", res = 300)
sdf <- summaryBy(R0~index_line+ index_sex+radius+ scale_i, data=Exp2beta0.5, FUN=mean)
gg <- ggplot(sdf, aes(x=as.factor(index_line), y=as.factor(index_sex), fill=R0.mean))
#sdf$betas <- ordered(sdf$betas, levels = c("0 -6", "0 -3", "0 0", "3 -6", "3 -3", "3 0", "6 -6", "6 -3", "6 0"))
gg <- gg + geom_tile(color="white", size=0.1)
gg <- gg + scale_fill_viridis(name="R0")
gg <- gg + coord_equal()
gg <- gg + facet_grid(radius ~ scale_i)
gg<- gg+ labs(x="Line", y="Sex")
gg<- gg+ theme(axis.title = element_text(face="bold", size=24))
gg<- gg + theme(axis.text.x = element_text(angle=90, hjust=0, vjust=0.5, size=16))
gg<- gg + theme(axis.text.y = element_text(vjust=0.5, size=16)) 
gg<- gg + theme(strip.text = element_text(vjust=0.5, size=18))
gg<- gg + theme(strip.text.x = element_text(angle = 90))
gg<- gg + theme(legend.text = element_text(vjust=0.5, size=14), legend.title=element_text(vjust=0.5, size=20))
gg
dev.off()

#' Beta= 1

tiff("Exp2beta1_maxI.tiff", height = 7, width = 13, units = "in", compression = "lzw", res = 300)
sdf <- summaryBy(max_inf~index_line+ index_sex+radius+ scale_i, data=Exp2beta1, FUN=mean)
gg <- ggplot(sdf, aes(x=as.factor(index_line), y=as.factor(index_sex), fill=max_inf.mean))
#sdf$betas <- ordered(sdf$betas, levels = c("0 -6", "0 -3", "0 0", "3 -6", "3 -3", "3 0", "6 -6", "6 -3", "6 0"))
gg <- gg + geom_tile(color="white", size=0.1)
gg <- gg + scale_fill_viridis(name="Maximum number of \n infected flies")
gg <- gg + coord_equal()
gg <- gg + facet_grid(radius ~ scale_i)
gg<- gg+ labs(x="Line", y="Sex")
gg<- gg+ theme(axis.title = element_text(face="bold", size=24))
gg<- gg + theme(axis.text.x = element_text(angle=90, hjust=0, vjust=0.5, size=16))
gg<- gg + theme(axis.text.y = element_text(vjust=0.5, size=16)) 
gg<- gg + theme(strip.text = element_text(vjust=0.5, size=18))
gg<- gg + theme(strip.text.x = element_text(angle = 90))
gg<- gg + theme(legend.text = element_text(vjust=0.5, size=14), legend.title=element_text(vjust=0.5, size=20))
gg
dev.off()

tiff("Exp2beta1_dur.tiff", height = 7, width = 13, units = "in", compression = "lzw", res = 300)
sdf <- summaryBy(duration~index_line+ index_sex+radius+ scale_i, data=Exp2beta1, FUN=mean)
gg <- ggplot(sdf, aes(x=as.factor(index_line), y=as.factor(index_sex), fill=duration.mean))
#sdf$betas <- ordered(sdf$betas, levels = c("0 -6", "0 -3", "0 0", "3 -6", "3 -3", "3 0", "6 -6", "6 -3", "6 0"))
gg <- gg + geom_tile(color="white", size=0.1)
gg <- gg + scale_fill_viridis(name="Duration")
gg <- gg + coord_equal()
gg <- gg + facet_grid(radius ~ scale_i)
gg<- gg+ labs(x="Line", y="Sex")
gg<- gg+ theme(axis.title = element_text(face="bold", size=24))
gg<- gg + theme(axis.text.x = element_text(angle=90, hjust=0, vjust=0.5, size=16))
gg<- gg + theme(axis.text.y = element_text(vjust=0.5, size=16)) 
gg<- gg + theme(strip.text = element_text(vjust=0.5, size=18))
gg<- gg + theme(strip.text.x = element_text(angle = 90))
gg<- gg + theme(legend.text = element_text(vjust=0.5, size=14), legend.title=element_text(vjust=0.5, size=20))
gg
dev.off()

tiff("Exp2beta1_R0.tiff", height = 7, width = 13, units = "in", compression = "lzw", res = 300)
sdf <- summaryBy(R0~index_line+ index_sex+radius+ scale_i, data=Exp2beta1, FUN=mean)
gg <- ggplot(sdf, aes(x=as.factor(index_line), y=as.factor(index_sex), fill=R0.mean))
#sdf$betas <- ordered(sdf$betas, levels = c("0 -6", "0 -3", "0 0", "3 -6", "3 -3", "3 0", "6 -6", "6 -3", "6 0"))
gg <- gg + geom_tile(color="white", size=0.1)
gg <- gg + scale_fill_viridis(name="R0")
gg <- gg + coord_equal()
gg <- gg + facet_grid(radius ~ scale_i)
gg<- gg+ labs(x="Line", y="Sex")
gg<- gg+ theme(axis.title = element_text(face="bold", size=24))
gg<- gg + theme(axis.text.x = element_text(angle=90, hjust=0, vjust=0.5, size=16))
gg<- gg + theme(axis.text.y = element_text(vjust=0.5, size=16)) 
gg<- gg + theme(strip.text = element_text(vjust=0.5, size=18))
gg<- gg + theme(strip.text.x = element_text(angle = 90))
gg<- gg + theme(legend.text = element_text(vjust=0.5, size=14), legend.title=element_text(vjust=0.5, size=20))
gg
dev.off()


#' ------------------------------------------------------------------------
# Exp. 3 ------------------------------------------------------------------
#' ------------------------------------------------------------------------

#' Subdivide experiment 3 by betas & scaled infectiousness
Exp3beta1<-Exp3[which(Exp3$beta==1 & Exp3$scale_i==2),]
Exp3beta0.5<-Exp3[which(Exp3$beta==0.5 & Exp3$scale_i==2),]
Exp3beta0.1<-Exp3[which(Exp3$beta==0.1 & Exp3$scale_i==2),]

#' Beta= 0.1

tiff("Exp3beta0.1_maxI.tiff", height = 7, width = 13, units = "in", compression = "lzw", res = 300)
sdf <- summaryBy(max_inf~ vary_agg+ vary_dur +vary_infect + radius, data=Exp3beta0.1, FUN=mean)
gg <- ggplot(sdf, aes(x=as.factor(vary_agg), y=as.factor(vary_dur), fill=max_inf.mean))
#sdf$betas <- ordered(sdf$betas, levels = c("0 -6", "0 -3", "0 0", "3 -6", "3 -3", "3 0", "6 -6", "6 -3", "6 0"))
gg <- gg + geom_tile(color="white", size=0.1)
gg <- gg + scale_fill_viridis(name="Maximum number of \n infected flies")
gg <- gg + coord_equal()
gg <- gg + facet_grid(radius ~ vary_infect )
gg<- gg+ labs(x="Vary Aggregation", y="Vary Duration")
gg<- gg+ theme(axis.title = element_text(face="bold", size=24))
gg<- gg + theme(axis.text.x = element_text(angle=90, hjust=0, vjust=0.5, size=16))
gg<- gg + theme(axis.text.y = element_text(vjust=0.5, size=16)) 
gg<- gg + theme(strip.text = element_text(vjust=0.5, size=18))
gg<- gg + theme(strip.text.x = element_text(angle = 90))
gg<- gg + theme(legend.text = element_text(vjust=0.5, size=14), legend.title=element_text(vjust=0.5, size=20))
gg
dev.off()

tiff("Exp3beta0.1_dur.tiff", height = 7, width = 13, units = "in", compression = "lzw", res = 300)
sdf <- summaryBy(duration~ vary_agg+ vary_dur +vary_infect + radius, data=Exp3beta0.1, FUN=mean)
gg <- ggplot(sdf, aes(x=as.factor(vary_agg), y=as.factor(vary_dur), fill=duration.mean))
#sdf$betas <- ordered(sdf$betas, levels = c("0 -6", "0 -3", "0 0", "3 -6", "3 -3", "3 0", "6 -6", "6 -3", "6 0"))
gg <- gg + geom_tile(color="white", size=0.1)
gg <- gg + scale_fill_viridis(name="Duration")
gg <- gg + coord_equal()
gg <- gg + facet_grid(radius ~ vary_infect )
gg<- gg+ labs(x="Vary Aggregation", y="Vary Duration")
gg<- gg+ theme(axis.title = element_text(face="bold", size=24))
gg<- gg + theme(axis.text.x = element_text(angle=90, hjust=0, vjust=0.5, size=16))
gg<- gg + theme(axis.text.y = element_text(vjust=0.5, size=16)) 
gg<- gg + theme(strip.text = element_text(vjust=0.5, size=18))
gg<- gg + theme(strip.text.x = element_text(angle = 90))
gg<- gg + theme(legend.text = element_text(vjust=0.5, size=14), legend.title=element_text(vjust=0.5, size=20))
gg
dev.off()


#' Beta= 0.5

tiff("Exp3beta0.5_maxI.tiff", height = 7, width = 13, units = "in", compression = "lzw", res = 300)
sdf <- summaryBy(max_inf~ vary_agg+ vary_dur +vary_infect + radius, data=Exp3beta0.5, FUN=mean)
gg <- ggplot(sdf, aes(x=as.factor(vary_agg), y=as.factor(vary_dur), fill=max_inf.mean))
#sdf$betas <- ordered(sdf$betas, levels = c("0 -6", "0 -3", "0 0", "3 -6", "3 -3", "3 0", "6 -6", "6 -3", "6 0"))
gg <- gg + geom_tile(color="white", size=0.1)
gg <- gg + scale_fill_viridis(name="Maximum number of \n infected flies")
gg <- gg + coord_equal()
gg <- gg + facet_grid(radius ~ vary_infect )
gg<- gg+ labs(x="Vary Aggregation", y="Vary Duration")
gg<- gg+ theme(axis.title = element_text(face="bold", size=24))
gg<- gg + theme(axis.text.x = element_text(angle=90, hjust=0, vjust=0.5, size=16))
gg<- gg + theme(axis.text.y = element_text(vjust=0.5, size=16)) 
gg<- gg + theme(strip.text = element_text(vjust=0.5, size=18))
gg<- gg + theme(strip.text.x = element_text(angle = 90))
gg<- gg + theme(legend.text = element_text(vjust=0.5, size=14), legend.title=element_text(vjust=0.5, size=20))
gg
dev.off()

tiff("Exp3beta0.5_dur.tiff", height = 7, width = 13, units = "in", compression = "lzw", res = 300)
sdf <- summaryBy(duration~ vary_agg+ vary_dur +vary_infect + radius, data=Exp3beta0.5, FUN=mean)
gg <- ggplot(sdf, aes(x=as.factor(vary_agg), y=as.factor(vary_dur), fill=duration.mean))
#sdf$betas <- ordered(sdf$betas, levels = c("0 -6", "0 -3", "0 0", "3 -6", "3 -3", "3 0", "6 -6", "6 -3", "6 0"))
gg <- gg + geom_tile(color="white", size=0.1)
gg <- gg + scale_fill_viridis(name="Duration")
gg <- gg + coord_equal()
gg <- gg + facet_grid(radius ~ vary_infect )
gg<- gg+ labs(x="Vary Aggregation", y="Vary Duration")
gg<- gg+ theme(axis.title = element_text(face="bold", size=24))
gg<- gg + theme(axis.text.x = element_text(angle=90, hjust=0, vjust=0.5, size=16))
gg<- gg + theme(axis.text.y = element_text(vjust=0.5, size=16)) 
gg<- gg + theme(strip.text = element_text(vjust=0.5, size=18))
gg<- gg + theme(strip.text.x = element_text(angle = 90))
gg<- gg + theme(legend.text = element_text(vjust=0.5, size=14), legend.title=element_text(vjust=0.5, size=20))
gg
dev.off()

#' Beta= 1

tiff("Exp3beta1_maxI.tiff", height = 7, width = 13, units = "in", compression = "lzw", res = 300)
sdf <- summaryBy(max_inf~ vary_agg+ vary_dur +vary_infect + radius, data=Exp3beta1, FUN=mean)
gg <- ggplot(sdf, aes(x=as.factor(vary_agg), y=as.factor(vary_dur), fill=max_inf.mean))
#sdf$betas <- ordered(sdf$betas, levels = c("0 -6", "0 -3", "0 0", "3 -6", "3 -3", "3 0", "6 -6", "6 -3", "6 0"))
gg <- gg + geom_tile(color="white", size=0.1)
gg <- gg + scale_fill_viridis(name="Maximum number of \n infected flies")
gg <- gg + coord_equal()
gg <- gg + facet_grid(radius ~ vary_infect )
gg<- gg+ labs(x="Vary Aggregation", y="Vary Duration")
gg<- gg+ theme(axis.title = element_text(face="bold", size=24))
gg<- gg + theme(axis.text.x = element_text(angle=90, hjust=0, vjust=0.5, size=16))
gg<- gg + theme(axis.text.y = element_text(vjust=0.5, size=16)) 
gg<- gg + theme(strip.text = element_text(vjust=0.5, size=18))
gg<- gg + theme(strip.text.x = element_text(angle = 90))
gg<- gg + theme(legend.text = element_text(vjust=0.5, size=14), legend.title=element_text(vjust=0.5, size=20))
gg
dev.off()

tiff("Exp3beta1_dur.tiff", height = 7, width = 13, units = "in", compression = "lzw", res = 300)
sdf <- summaryBy(duration~ vary_agg+ vary_dur +vary_infect + radius, data=Exp3beta1, FUN=mean)
gg <- ggplot(sdf, aes(x=as.factor(vary_agg), y=as.factor(vary_dur), fill=duration.mean))
#sdf$betas <- ordered(sdf$betas, levels = c("0 -6", "0 -3", "0 0", "3 -6", "3 -3", "3 0", "6 -6", "6 -3", "6 0"))
gg <- gg + geom_tile(color="white", size=0.1)
gg <- gg + scale_fill_viridis(name="Duration")
gg <- gg + coord_equal()
gg <- gg + facet_grid(radius ~ vary_infect )
gg<- gg+ labs(x="Vary Aggregation", y="Vary Duration")
gg<- gg+ theme(axis.title = element_text(face="bold", size=24))
gg<- gg + theme(axis.text.x = element_text(angle=90, hjust=0, vjust=0.5, size=16))
gg<- gg + theme(axis.text.y = element_text(vjust=0.5, size=16)) 
gg<- gg + theme(strip.text = element_text(vjust=0.5, size=18))
gg<- gg + theme(strip.text.x = element_text(angle = 90))
gg<- gg + theme(legend.text = element_text(vjust=0.5, size=14), legend.title=element_text(vjust=0.5, size=20))
gg
dev.off()
