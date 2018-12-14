#' Plot variable importance results
#' Lauren White
#' September 19, 2018


#' Load libraries
library(doBy)
library(ggplot2)
library(viridis)
library(ggthemes)
library(scales)
library(lsr)
library(SDMTools)
library(cowplot)



# Experiment 1 ------------------------------------------------------------

#Load importance values (not scaled by SD)
imp_logit<-read.csv("Exp1RF_logit1000.csv")
imp_logitprev<-read.csv("Exp1RF_maxI1000.csv")
imp_logitdur<-read.csv("Exp1RF_dur1000.csv")


#imp_logit$SD<-rep(0.005, times=nrow(imp_logit))
A<-ggplot(imp_logit, aes(x=reorder(X, x), y=x)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  # geom_errorbar(aes(ymin=X.IncMSE-SD, ymax=X.IncMSE+SD),
  #               size=.3,    # Thinner lines
  #               width=.2,
  #               position=position_dodge(.9)) +
  scale_x_discrete(element_blank(), labels = c("scale_i" = "scaled infectiousness", "beta" = "transmission efficiency",
                                               "radius" =  "radius","sex" = "sex", "line" = "genetic background")) +
  ylab("MDA") +
  #ylab("")+
  theme_bw()+ theme(plot.margin = unit(c(0.1, 0, 0, 0), "cm"))+
  #ggtitle("(A)") + #" Variable Importance for Epidemic \nSuccess") +
  theme(axis.text=element_text(size=6), plot.title = element_text(size = 8), axis.title=element_text(size=6), axis.text.x=element_text(angle=270, hjust=0, vjust=0.5)) 
#theme(plot.margin=unit(c(0.1,0.1,0,0), "cm"))
# theme_bw(axis.text=element_text(size=14),    axis.title=element_text(size=14,face="bold"), plot.title = element_text(size = 40))
A

#imp_logitprev$SD<-rep(0.0000001, times=nrow(imp_logitprev))
B<-ggplot(imp_logitprev, aes(x=reorder(X, x), y=x)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  # geom_errorbar(aes(ymin=X.IncMSE-SD, ymax=X.IncMSE+SD),
  #               size=.3,    # Thinner lines
  #               width=.2,
  #               position=position_dodge(.9)) +
  scale_x_discrete(element_blank(), labels = c("scale_i" = "scaled infectiousness", "beta" = "transmission efficiency",
                                               "radius" =  "radius","sex" = "sex", "line" = "genetic background")) +
  ylab(NULL)+
  theme_bw()+ theme(plot.margin = unit(c(0.25, 0, 0, 0), "cm"))+
  #ggtitle("(B)") + #" Variable Importance for Maximum \nPrevalence|Success") +
  theme(axis.text=element_text(size=6), plot.title = element_text(size = 8), axis.title=element_text(size=6), axis.text.x=element_text(angle=270, hjust=0, vjust=0.5)) 

#theme(plot.margin=unit(c(-0.25,0.1,0,0), "cm"))
#coord_flip()
B

#imp_logitdur$SD<-rep(100, times=nrow(imp_logitdur))
C<-ggplot(imp_logitdur, aes(x=reorder(X, x), y=x)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  # geom_errorbar(aes(ymin=X.IncMSE-SD, ymax=X.IncMSE+SD),
  #               size=.3,    # Thinner lines
  #               width=.2,
  #               position=position_dodge(.9)) +
  scale_x_discrete(element_blank(), labels = c("scale_i" = "scaled infectiousness", "beta" = "transmission efficiency",
                                               "radius" =  "radius","sex" = "sex", "line" = "genetic background")) +
  ylab(NULL)+
  theme_bw()+ theme(plot.margin = unit(c(0.1, 0, 0, 0), "cm"))+
  #ggtitle("(C)") + #" Variable Importance for Epidemic \nDuration|Success") +
  
  theme(axis.text=element_text(size=6), plot.title = element_text(size = 8), axis.title=element_text(size=6), axis.text.x=element_text(angle=270, hjust=0, vjust=0.5)) 

#theme(plot.margin=unit(c(-0.25,0.1,0,0), "cm"))
#coord_flip()
C

prow <- plot_grid(A, B, C,
                  align = 'vh',
                  labels = c("(A)", "(B)", "(C)"),
                  hjust = -0.60,
                  ncol = 3, label_size=6
)
#paxis <- axis_canvas(prow, axis = "y") 
#ggdraw(insert_yaxis_grob(prow, paxis, grid::unit(.25, "null")))

#setwd()
tiff("Exp1RF.tiff", height =4.5, width =8.7, units = "cm", compression = "lzw", res = 1200)
#multiplot(A, B, C, cols=3)
prow
dev.off()


# Experiment 2 ------------------------------------------------------------

#Load importance values (not scaled by SD)
imp_logit<-read.csv("Exp2RF_logit1000.csv")
imp_logitprev<-read.csv("Exp2RF_maxI1000.csv")
imp_logitdur<-read.csv("Exp2RF_dur1000.csv")
imp_logitR0<-read.csv("Exp2RF_R0_1000.csv")

#imp_logit$SD<-rep(0.005, times=nrow(imp_logit))
A<-ggplot(imp_logit, aes(x=reorder(X, x), y=x)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  # geom_errorbar(aes(ymin=X.IncMSE-SD, ymax=X.IncMSE+SD),
  #               size=.3,    # Thinner lines
  #               width=.2,
  #               position=position_dodge(.9)) +
  scale_x_discrete(element_blank(), labels = c("scale_i" = "scaled infectiousness",
                                               "radius" =  "radius","index_sex" = "index sex", "index_line" = "index line")) +
  ylab("MDA") +
  #ylab("")+
  theme_bw()+ theme(plot.margin = unit(c(0.1, 0, 0, 0), "cm"))+
  #ggtitle("(A)") + #" Variable Importance for Epidemic \nSuccess") +
  theme(axis.text=element_text(size=6), plot.title = element_text(size = 8), axis.title=element_text(size=6), axis.text.x=element_text(angle=270, hjust=0, vjust=0.5)) 
#theme(plot.margin=unit(c(0.1,0.1,0,0), "cm"))
# theme_bw(axis.text=element_text(size=14),    axis.title=element_text(size=14,face="bold"), plot.title = element_text(size = 40))
A

#imp_logitprev$SD<-rep(0.0000001, times=nrow(imp_logitprev))
B<-ggplot(imp_logitprev, aes(x=reorder(X, x), y=x)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  # geom_errorbar(aes(ymin=X.IncMSE-SD, ymax=X.IncMSE+SD),
  #               size=.3,    # Thinner lines
  #               width=.2,
  #               position=position_dodge(.9)) +
  scale_x_discrete(element_blank(), labels = c("scale_i" = "scaled infectiousness",
                                               "radius" =  "radius","index_sex" = "index sex", "index_line" = "index line")) +
  #ylab("Mean Decrease in Accuracy") +
  ylab(NULL)+
  theme_bw()+ theme(plot.margin = unit(c(0.25, 0, 0, 0), "cm"))+
  #ggtitle("(B)") + #" Variable Importance for Maximum \nPrevalence|Success") +
  theme(axis.text=element_text(size=6), plot.title = element_text(size = 8), axis.title=element_text(size=6), axis.text.x=element_text(angle=270, hjust=0, vjust=0.5)) 

#theme(plot.margin=unit(c(-0.25,0.1,0,0), "cm"))
#coord_flip()
B

#imp_logitdur$SD<-rep(100, times=nrow(imp_logitdur))
C<-ggplot(imp_logitdur, aes(x=reorder(X, x), y=x)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  # geom_errorbar(aes(ymin=X.IncMSE-SD, ymax=X.IncMSE+SD),
  #               size=.3,    # Thinner lines
  #               width=.2,
  #               position=position_dodge(.9)) +
  scale_x_discrete(element_blank(), labels = c("scale_i" = "scaled infectiousness",
                                               "radius" =  "radius","index_sex" = "index sex", "index_line" = "index line")) +
  #ylab("Mean Decrease in Accuracy") +
  ylab(NULL)+
  theme_bw()+ theme(plot.margin = unit(c(0.1, 0, 0, 0), "cm"))+
  #ggtitle("(C)") + #" Variable Importance for Epidemic \nDuration|Success") +
  
  theme(axis.text=element_text(size=6), plot.title = element_text(size = 8), axis.title=element_text(size=6), axis.text.x=element_text(angle=270, hjust=0, vjust=0.5)) 

#theme(plot.margin=unit(c(-0.25,0.1,0,0), "cm"))
#coord_flip()
C

#imp_logitdur$SD<-rep(100, times=nrow(imp_logitdur))
D<-ggplot(imp_logitR0, aes(x=reorder(X, x), y=x)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  # geom_errorbar(aes(ymin=X.IncMSE-SD, ymax=X.IncMSE+SD),
  #               size=.3,    # Thinner lines
  #               width=.2,
  #               position=position_dodge(.9)) +
  scale_x_discrete(element_blank(), labels = c("scale_i" = "scaled infectiousness",
                                               "radius" =  "radius","index_sex" = "index sex", "index_line" = "index line")) +
  #ylab("Mean Decrease in Accuracy") +
  ylab(NULL)+
  theme_bw()+ theme(plot.margin = unit(c(0.1, 0, 0, 0), "cm"))+
  #ggtitle("(C)") + #" Variable Importance for Epidemic \nDuration|Success") +
  
  theme(axis.text=element_text(size=6), plot.title = element_text(size = 8), axis.title=element_text(size=6), axis.text.x=element_text(angle=270, hjust=0, vjust=0.5)) 

#theme(plot.margin=unit(c(-0.25,0.1,0,0), "cm"))
#coord_flip()
D

prow <- plot_grid(A, B, C, D,
                  align = 'vh',
                  labels = c("(A)", "(B)", "(C)", "(D)"),
                  hjust = -0.75,
                  ncol = 2, label_size=7
)
#paxis <- axis_canvas(prow, axis = "y") 
#ggdraw(insert_yaxis_grob(prow, paxis, grid::unit(.25, "null")))

#setwd()
tiff("Exp2RF.tiff", height =8.7, width =8.7, units = "cm", compression = "lzw", res = 1200)
#multiplot(A, B, C, cols=3)
prow
dev.off()

# Experiment 3 ------------------------------------------------------------

#Load importance values (not scaled by SD)
imp_logit<-read.csv("Exp3RF_logit1000.csv")
imp_logitprev<-read.csv("Exp3RF_maxI1000.csv")
imp_logitdur<-read.csv("Exp3RF_dur1000.csv")
imp_logitR0<-read.csv("Exp3RF_R0_1000.csv")

#imp_logit$SD<-rep(0.005, times=nrow(imp_logit))
A<-ggplot(imp_logit, aes(x=reorder(X, x), y=x)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  # geom_errorbar(aes(ymin=X.IncMSE-SD, ymax=X.IncMSE+SD),
  #               size=.3,    # Thinner lines
  #               width=.2,
  #               position=position_dodge(.9)) +
  scale_x_discrete(element_blank(), labels = c("scale_i" = "scaled\n infectiousness",
                                               "radius" =  "radius", "beta" = "transmission\n efficiency", "vary_agg"= "aggregation\n varied", "vary_infect"="infectiousness\n varied", "vary_dur"="duration varied")) +
  ylab("MDA") +
  #ylab("")+
  theme_bw()+ theme(plot.margin = unit(c(0.1, 0, 0, 0), "cm"))+
  #ggtitle("(A)") + #" Variable Importance for Epidemic \nSuccess") +
  theme(axis.text=element_text(size=6), plot.title = element_text(size = 8), axis.title=element_text(size=6), axis.text.x=element_text(angle=270, hjust=0, vjust=0.5)) 
#theme(plot.margin=unit(c(0.1,0.1,0,0), "cm"))
# theme_bw(axis.text=element_text(size=14),    axis.title=element_text(size=14,face="bold"), plot.title = element_text(size = 40))
A

#imp_logitprev$SD<-rep(0.0000001, times=nrow(imp_logitprev))
B<-ggplot(imp_logitprev, aes(x=reorder(X, x), y=x)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  # geom_errorbar(aes(ymin=X.IncMSE-SD, ymax=X.IncMSE+SD),
  #               size=.3,    # Thinner lines
  #               width=.2,
  #               position=position_dodge(.9)) +
  scale_x_discrete(element_blank(), labels = c("scale_i" = "scaled\n infectiousness",
                                               "radius" =  "radius", "beta" = "transmission\n efficiency", "vary_agg"= "aggregation\n varied", "vary_infect"="infectiousness\n varied", "vary_dur"="duration varied")) +
  #ylab("Mean Decrease in Accuracy") +
  ylab(NULL)+
  theme_bw()+ theme(plot.margin = unit(c(0.25, 0, 0, 0), "cm"))+
  #ggtitle("(B)") + #" Variable Importance for Maximum \nPrevalence|Success") +
  theme(axis.text=element_text(size=6), plot.title = element_text(size = 8), axis.title=element_text(size=6), axis.text.x=element_text(angle=270, hjust=0, vjust=0.5)) 

#theme(plot.margin=unit(c(-0.25,0.1,0,0), "cm"))
#coord_flip()
B

#imp_logitdur$SD<-rep(100, times=nrow(imp_logitdur))
C<-ggplot(imp_logitdur, aes(x=reorder(X, x), y=x)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  # geom_errorbar(aes(ymin=X.IncMSE-SD, ymax=X.IncMSE+SD),
  #               size=.3,    # Thinner lines
  #               width=.2,
  #               position=position_dodge(.9)) +
  scale_x_discrete(element_blank(), labels = c("scale_i" = "scaled\n infectiousness",
                                               "radius" =  "radius", "beta" = "transmission\n efficiency", "vary_agg"= "aggregation\n varied", "vary_infect"="infectiousness\n varied", "vary_dur"="duration varied")) +
  #ylab("Mean Decrease in Accuracy") +
  ylab(NULL)+
  theme_bw()+ theme(plot.margin = unit(c(0.1, 0, 0, 0), "cm"))+
  #ggtitle("(C)") + #" Variable Importance for Epidemic \nDuration|Success") +
  
  theme(axis.text=element_text(size=6), plot.title = element_text(size = 8), axis.title=element_text(size=6), axis.text.x=element_text(angle=270, hjust=0, vjust=0.5)) 

#theme(plot.margin=unit(c(-0.25,0.1,0,0), "cm"))
#coord_flip()
C

#imp_logitdur$SD<-rep(100, times=nrow(imp_logitdur))
D<-ggplot(imp_logitR0, aes(x=reorder(X, x), y=x)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  # geom_errorbar(aes(ymin=X.IncMSE-SD, ymax=X.IncMSE+SD),
  #               size=.3,    # Thinner lines
  #               width=.2,
  #               position=position_dodge(.9)) +
  scale_x_discrete(element_blank(), labels = c("scale_i" = "scaled\n infectiousness",
                                               "radius" =  "radius", "beta" = "transmission\n efficiency", "vary_agg"= "aggregation\n varied", "vary_infect"="infectiousness\n varied", "vary_dur"="duration varied")) +
  #ylab("Mean Decrease in Accuracy") +
  ylab(NULL)+
  theme_bw()+ theme(plot.margin = unit(c(0.1, 0, 0, 0), "cm"))+
  #ggtitle("(C)") + #" Variable Importance for Epidemic \nDuration|Success") +
  
  theme(axis.text=element_text(size=6), plot.title = element_text(size = 8), axis.title=element_text(size=6), axis.text.x=element_text(angle=270, hjust=0, vjust=0.5)) 

#theme(plot.margin=unit(c(-0.25,0.1,0,0), "cm"))
#coord_flip()
D

prow <- plot_grid(A, B, C, D,
                  align = 'vh',
                  labels = c("(A)", "(B)", "(C)", "(D)"),
                  hjust = -0.75,
                  ncol = 2, label_size=7
)
#paxis <- axis_canvas(prow, axis = "y") 
#ggdraw(insert_yaxis_grob(prow, paxis, grid::unit(.25, "null")))

#setwd()
tiff("Exp3RF.tiff", height =8.7, width =8.7, units = "cm", compression = "lzw", res = 1200)
#multiplot(A, B, C, cols=3)
prow
dev.off()

