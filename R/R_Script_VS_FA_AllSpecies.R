library(ggplot2)
library(grid)
library(plyr)


## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This is does the summary; it's not easy to understand...
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun= function(xx, col, na.rm) {
                   c( N    = length2(xx[,col], na.rm=na.rm),
                      mean = mean   (xx[,col], na.rm=na.rm),
                      sd   = sd     (xx[,col], na.rm=na.rm),
                      max  = max   (xx[,col], na.rm=na.rm),
                      min  = min   (xx[,col], na.rm=na.rm)
                      )
                 },
                 measurevar,
                 na.rm
                 )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean"=measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)


#setwd("C:/Users/MBiber/Documents/...")
data <- read.delim("VideoData_FA_FB.csv", sep=",",header=T)


#Create a new theme just for Andy... White with black lines, no bar fill, font sizes set
theme_update( panel.grid.minor= theme_blank(),
              panel.grid.major= theme_blank(),
              panel.background= theme_blank(),
              axis.title.x= theme_text( family= "serif", angle= 0 , size= 12),
              axis.text.x= theme_text( family= "serif", angle= 0 , size= 11),
              axis.text.y= theme_text( family= "serif" , size= 11),
              axis.title.y= theme_text( family= "serif", angle= 90 , size= 12))
theme_map <- theme_get()
theme_set( theme_bw())


#Belgia Mound

#Subset data for Belgica Mound
subbelgica <- subset(data, Area == "Belgica Mound")

#Kruskall-Wallis Test, non-paramteric rank test
kw_LepidionFA_belgica<-kruskal.test(LepidionFA~CoralNoCoral, data=subbelgica)
kw_LepidionFA_belgica$star <- "" 
kw_LepidionFA_belgica$star[kw_LepidionFA_belgica$p.value <= .05]  <- "*" 
kw_LepidionFA_belgica$star[kw_LepidionFA_belgica$p.value <= .01]  <- "**" 
kw_LepidionFA_belgica$star[kw_LepidionFA_belgica$p.value <= .001] <- "***"

#Summary LepidionFA for Belgica Mound
summarybelgicaLepidionFA <- summarySE(subbelgica, measurevar="LepidionFA", groupvars="CoralNoCoral")

#Plot LepidionFA for Belgica Mound
plotbelgicaLepidionFA <- ggplot(summarybelgicaLepidionFA, aes(x=CoralNoCoral, y=LepidionFA)) +
    geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
    geom_errorbar(aes(ymin=LepidionFA-se, ymax=LepidionFA+se),
                  width=.1,                    # Width of the error bars
                  position=position_dodge(.9)) +
    opts(title="Belgica Mound") +
    xlab("Coral framework") +
    ylab(expression(paste(italic("Lepidion eques")))) +
    ylim(0,400) +
    geom_text(aes(label=kw_LepidionFA_belgica$star), colour="black", vjust=-1, size=10) +
    #scale_y_continuous(breaks=-1:20000*1000) +
    #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
    geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
    theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_SynaphobranchusFA_belgica<-kruskal.test(SynaphobranchusFA~CoralNoCoral, data=subbelgica)
kw_SynaphobranchusFA_belgica$star <- "" 
kw_SynaphobranchusFA_belgica$star[kw_SynaphobranchusFA_belgica$p.value <= .05]  <- "*" 
kw_SynaphobranchusFA_belgica$star[kw_SynaphobranchusFA_belgica$p.value <= .01]  <- "**" 
kw_SynaphobranchusFA_belgica$star[kw_SynaphobranchusFA_belgica$p.value <= .001] <- "***"

#Plot SynaphobranchusFA for Belgica Mound
summarybelgicaSynaphobranchusFA <- summarySE(subbelgica, measurevar="SynaphobranchusFA", groupvars="CoralNoCoral")

plotbelgicaSynaphobranchusFA <- ggplot(summarybelgicaSynaphobranchusFA, aes(x=CoralNoCoral, y=SynaphobranchusFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=SynaphobranchusFA-se, ymax=SynaphobranchusFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="Belgica Mound") +
                  xlab("Coral framework") +
                  ylab(expression(paste(italic("Synaphobranchus kaupii")))) +
                  ylim(0,120) +
                  geom_text(aes(label=kw_SynaphobranchusFA_belgica$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_SigmopsFA_belgica<-kruskal.test(SigmopsFA~CoralNoCoral, data=subbelgica)
kw_SigmopsFA_belgica$star <- "" 
kw_SigmopsFA_belgica$star[kw_SigmopsFA_belgica$p.value <= .05]  <- "*" 
kw_SigmopsFA_belgica$star[kw_SigmopsFA_belgica$p.value <= .01]  <- "**" 
kw_SigmopsFA_belgica$star[kw_SigmopsFA_belgica$p.value <= .001] <- "***"

#Plot SigmopsFA for Belgica Mound
summarybelgicaSigmopsFA <- summarySE(subbelgica, measurevar="SigmopsFA", groupvars="CoralNoCoral")

plotbelgicaSigmopsFA <- ggplot(summarybelgicaSigmopsFA, aes(x=CoralNoCoral, y=SigmopsFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=SigmopsFA-se, ymax=SigmopsFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="Belgica Mound") +
                  xlab("Coral framework") +
                  ylab(expression(paste(italic("Sigmops bathyphilus")))) +
                  ylim(0,100) +
                  geom_text(aes(label=kw_SigmopsFA_belgica$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_MoraFA_belgica<-kruskal.test(MoraFA~CoralNoCoral, data=subbelgica)
kw_MoraFA_belgica$star <- "" 
kw_MoraFA_belgica$star[kw_MoraFA_belgica$p.value <= .05]  <- "*" 
kw_MoraFA_belgica$star[kw_MoraFA_belgica$p.value <= .01]  <- "**" 
kw_MoraFA_belgica$star[kw_MoraFA_belgica$p.value <= .001] <- "***"

#Plot MoraFA for Belgica Mound
summarybelgicaMoraFA <- summarySE(subbelgica, measurevar="MoraFA", groupvars="CoralNoCoral")

plotbelgicaMoraFA <- ggplot(summarybelgicaMoraFA, aes(x=CoralNoCoral, y=MoraFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MoraFA-se, ymax=MoraFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="Belgica Mound") +
                  xlab("Coral framework") +
                  ylab(expression(paste(italic("Mora moro")))) +
                  ylim(0,25) +
                  geom_text(aes(label=kw_MoraFA_belgica$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_ChimaeraFA_belgica<-kruskal.test(ChimaeraFA~CoralNoCoral, data=subbelgica)
kw_ChimaeraFA_belgica$star <- "" 
kw_ChimaeraFA_belgica$star[kw_ChimaeraFA_belgica$p.value <= .05]  <- "*" 
kw_ChimaeraFA_belgica$star[kw_ChimaeraFA_belgica$p.value <= .01]  <- "**" 
kw_ChimaeraFA_belgica$star[kw_ChimaeraFA_belgica$p.value <= .001] <- "***"


#Plot ChimaeraFA for Belgica Mound
summarybelgicaChimaeraFA <- summarySE(subbelgica, measurevar="ChimaeraFA", groupvars="CoralNoCoral")

plotbelgicaChimaeraFA <- ggplot(summarybelgicaChimaeraFA, aes(x=CoralNoCoral, y=ChimaeraFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=ChimaeraFA-se, ymax=ChimaeraFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="Belgica Mound") +
                  xlab("Coral framework") +
                  ylab(expression(paste(italic("Chimaera monstrosa")))) +
                  ylim(0,50) +
                  geom_text(aes(label=kw_ChimaeraFA_belgica$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_HelicolenusFA_belgica<-kruskal.test(HelicolenusFA~CoralNoCoral, data=subbelgica)
kw_HelicolenusFA_belgica$star <- "" 
kw_HelicolenusFA_belgica$star[kw_HelicolenusFA_belgica$p.value <= .05]  <- "*" 
kw_HelicolenusFA_belgica$star[kw_HelicolenusFA_belgica$p.value <= .01]  <- "**" 
kw_HelicolenusFA_belgica$star[kw_HelicolenusFA_belgica$p.value <= .001] <- "***"

#Plot HelicolenusFA for Belgica Mound
summarybelgicaHelicolenusFA <- summarySE(subbelgica, measurevar="HelicolenusFA", groupvars="CoralNoCoral")

plotbelgicaHelicolenusFA <- ggplot(summarybelgicaHelicolenusFA, aes(x=CoralNoCoral, y=HelicolenusFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=HelicolenusFA-se, ymax=HelicolenusFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="Belgica Mound") +
                  xlab("Coral framework") +
                  ylab(expression(paste(italic("Helicolenus dactylopterus")))) +
                  ylim(0,200) +
                  geom_text(aes(label=kw_HelicolenusFA_belgica$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_LophiusFA_belgica<-kruskal.test(LophiusFA~CoralNoCoral, data=subbelgica)
kw_LophiusFA_belgica$star <- "" 
kw_LophiusFA_belgica$star[kw_LophiusFA_belgica$p.value <= .05]  <- "*" 
kw_LophiusFA_belgica$star[kw_LophiusFA_belgica$p.value <= .01]  <- "**" 
kw_LophiusFA_belgica$star[kw_LophiusFA_belgica$p.value <= .001] <- "***"

#Plot LophiusFA for Belgica Mound
summarybelgicaLophiusFA <- summarySE(subbelgica, measurevar="LophiusFA", groupvars="CoralNoCoral")

plotbelgicaLophiusFA <- ggplot(summarybelgicaLophiusFA, aes(x=CoralNoCoral, y=LophiusFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=LophiusFA-se, ymax=LophiusFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="Belgica Mound") +
                  xlab("Coral framework") +
                  ylab(expression(paste(italic("Lophius piscatorius")))) +
                  ylim(0,10) +
                  geom_text(aes(label=kw_LophiusFA_belgica$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_MolvaFA_belgica<-kruskal.test(MolvaFA~CoralNoCoral, data=subbelgica)
kw_MolvaFA_belgica$star <- "" 
kw_MolvaFA_belgica$star[kw_MolvaFA_belgica$p.value <= .05]  <- "*" 
kw_MolvaFA_belgica$star[kw_MolvaFA_belgica$p.value <= .01]  <- "**" 
kw_MolvaFA_belgica$star[kw_MolvaFA_belgica$p.value <= .001] <- "***"

#Plot MolvaFA for Belgica Mound
summarybelgicaMolvaFA <- summarySE(subbelgica, measurevar="MolvaFA", groupvars="CoralNoCoral")

plotbelgicaMolvaFA <- ggplot(summarybelgicaMolvaFA, aes(x=CoralNoCoral, y=MolvaFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MolvaFA-se, ymax=MolvaFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="Belgica Mound") +
                  xlab("Coral framework") +
                  ylab(expression(paste(italic("Molva dypterygia")))) +
                  ylim(0,5) +
                  geom_text(aes(label=kw_MolvaFA_belgica$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Plot BrosmeFA for Belgica Mound
summarybelgicaBrosmeFA <- summarySE(subbelgica, measurevar="BrosmeFA", groupvars="CoralNoCoral")

plotbelgicaBrosmeFA <- ggplot(summarybelgicaBrosmeFA, aes(x=CoralNoCoral, y=BrosmeFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=BrosmeFA-se, ymax=BrosmeFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("Coral framework") +
                  ylab("") +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Plot HoplostethusFA for Belgica Mound
summarybelgicaHoplostethusFA <- summarySE(subbelgica, measurevar="HoplostethusFA", groupvars="CoralNoCoral")

plotbelgicaHoplostethusFA <- ggplot(summarybelgicaHoplostethusFA, aes(x=CoralNoCoral, y=HoplostethusFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=HoplostethusFA-se, ymax=HoplostethusFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("Coral framework") +
                  ylab("") +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Plot PhycisFA for Belgica Mound
summarybelgicaPhycisFA <- summarySE(subbelgica, measurevar="PhycisFA", groupvars="CoralNoCoral")

plotbelgicaPhycisFA <- ggplot(summarybelgicaPhycisFA, aes(x=CoralNoCoral, y=PhycisFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=PhycisFA-se, ymax=PhycisFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("Coral framework") +
                  ylab("") +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Plot PseudotriakisFA for Belgica Mound
summarybelgicaPseudotriakisFA <- summarySE(subbelgica, measurevar="PseudotriakisFA", groupvars="CoralNoCoral")

plotbelgicaPseudotriakisFA <- ggplot(summarybelgicaPseudotriakisFA, aes(x=CoralNoCoral, y=PseudotriakisFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=PseudotriakisFA-se, ymax=PseudotriakisFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("Coral framework") +
                  ylab("") +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Plot RajaFA for Belgica Mound
summarybelgicaRajaFA <- summarySE(subbelgica, measurevar="RajaFA", groupvars="CoralNoCoral")

plotbelgicaRajaFA <- ggplot(summarybelgicaRajaFA, aes(x=CoralNoCoral, y=RajaFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=RajaFA-se, ymax=RajaFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("Coral framework") +
                  ylab("") +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map


#Hatton Bank

#Subset data for Hatton Bank
subhatton <- subset(data, Area == "Hatton Bank")

#Kruskall-Wallis Test, non-paramteric rank test
kw_LepidionFA_hatton<-kruskal.test(LepidionFA~CoralNoCoral, data=subhatton)
kw_LepidionFA_hatton$star <- "" 
kw_LepidionFA_hatton$star[kw_LepidionFA_hatton$p.value <= .05]  <- "*" 
kw_LepidionFA_hatton$star[kw_LepidionFA_hatton$p.value <= .01]  <- "**" 
kw_LepidionFA_hatton$star[kw_LepidionFA_hatton$p.value <= .001] <- "***"

#Plot LepidionFA for Hatton Bank
summaryhattonLepidionFA <- summarySE(subhatton, measurevar="LepidionFA", groupvars="CoralNoCoral")

plothattonLepidionFA <- ggplot(summaryhattonLepidionFA, aes(x=CoralNoCoral, y=LepidionFA)) +
    geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
    geom_errorbar(aes(ymin=LepidionFA-se, ymax=LepidionFA+se),
                  width=.1,                    # Width of the error bars
                  position=position_dodge(.9)) +      
    opts(title="Hatton Bank") +
    xlab("Coral framework") +
     ylab("") +
     ylim(0,400) +
     geom_text(aes(label=kw_LepidionFA_hatton$star), colour="black", vjust=-1, size=10) +
    #scale_y_continuous(breaks=-1:20000*1000) +
    #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
    geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
    theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_SynaphobranchusFA_hatton<-kruskal.test(SynaphobranchusFA~CoralNoCoral, data=subhatton)
kw_SynaphobranchusFA_hatton$star <- "" 
kw_SynaphobranchusFA_hatton$star[kw_SynaphobranchusFA_hatton$p.value <= .05]  <- "*" 
kw_SynaphobranchusFA_hatton$star[kw_SynaphobranchusFA_hatton$p.value <= .01]  <- "**" 
kw_SynaphobranchusFA_hatton$star[kw_SynaphobranchusFA_hatton$p.value <= .001] <- "***"

#Plot SynaphobranchusFA for Hatton Bank
summaryhattonSynaphobranchusFA <- summarySE(subhatton, measurevar="SynaphobranchusFA", groupvars="CoralNoCoral")

plothattonSynaphobranchusFA <- ggplot(summaryhattonSynaphobranchusFA, aes(x=CoralNoCoral, y=SynaphobranchusFA)) +
    geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
    geom_errorbar(aes(ymin=SynaphobranchusFA-se, ymax=SynaphobranchusFA+se),
                  width=.1,                    # Width of the error bars
                  position=position_dodge(.9)) +
                    opts(title="Hatton Bank") +
    xlab("Coral framework") +
     ylab("") +
     ylim(0,120) +
     geom_text(aes(label=kw_SynaphobranchusFA_hatton$star), colour="black", vjust=-1, size=10) +
    #scale_y_continuous(breaks=-1:20000*1000) +
    #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
    geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
    theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_SigmopsFA_hatton<-kruskal.test(SigmopsFA~CoralNoCoral, data=subhatton)
kw_SigmopsFA_hatton$star <- "" 
kw_SigmopsFA_hatton$star[kw_SigmopsFA_hatton$p.value <= .05]  <- "*" 
kw_SigmopsFA_hatton$star[kw_SigmopsFA_hatton$p.value <= .01]  <- "**" 
kw_SigmopsFA_hatton$star[kw_SigmopsFA_hatton$p.value <= .001] <- "***"

#Plot SigmopsFA for Hatton Bank
summaryhattonSigmopsFA <- summarySE(subhatton, measurevar="SigmopsFA", groupvars="CoralNoCoral")

plothattonSigmopsFA <- ggplot(summaryhattonSigmopsFA, aes(x=CoralNoCoral, y=SigmopsFA)) +
    geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
    geom_errorbar(aes(ymin=SigmopsFA-se, ymax=SigmopsFA+se),
                  width=.1,                    # Width of the error bars
                  position=position_dodge(.9)) +
                    opts(title="Hatton Bank") +
    xlab("Coral framework") +
     ylab("") +
     ylim(0,100) +
     geom_text(aes(label=kw_SigmopsFA_hatton$star), colour="black", vjust=-1, size=10) +
    #scale_y_continuous(breaks=-1:20000*1000) +
    #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
    geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
    theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_MoraFA_hatton<-kruskal.test(MoraFA~CoralNoCoral, data=subhatton)
kw_MoraFA_hatton$star <- "" 
kw_MoraFA_hatton$star[kw_MoraFA_hatton$p.value <= .05]  <- "*" 
kw_MoraFA_hatton$star[kw_MoraFA_hatton$p.value <= .01]  <- "**" 
kw_MoraFA_hatton$star[kw_MoraFA_hatton$p.value <= .001] <- "***"

#Plot MoraFA for Hatton Bank
summaryhattonMoraFA <- summarySE(subhatton, measurevar="MoraFA", groupvars="CoralNoCoral")

plothattonMoraFA <- ggplot(summaryhattonMoraFA, aes(x=CoralNoCoral, y=MoraFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MoraFA-se, ymax=MoraFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="Hatton Bank") +
                  xlab("Coral framework") +
                  ylab("") +
                  ylim(0,25) +
                  geom_text(aes(label=kw_MoraFA_hatton$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_ChimaeraFA_hatton<-kruskal.test(ChimaeraFA~CoralNoCoral, data=subhatton)
kw_ChimaeraFA_hatton$star <- "" 
kw_ChimaeraFA_hatton$star[kw_ChimaeraFA_hatton$p.value <= .05]  <- "*" 
kw_ChimaeraFA_hatton$star[kw_ChimaeraFA_hatton$p.value <= .01]  <- "**" 
kw_ChimaeraFA_hatton$star[kw_ChimaeraFA_hatton$p.value <= .001] <- "***"

#Plot ChimaeraFA for Hatton Bank
summaryhattonChimaeraFA <- summarySE(subhatton, measurevar="ChimaeraFA", groupvars="CoralNoCoral")

plothattonChimaeraFA <- ggplot(summaryhattonChimaeraFA, aes(x=CoralNoCoral, y=ChimaeraFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=ChimaeraFA-se, ymax=ChimaeraFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="Hatton Bank") +
                  xlab("Coral framework") +
                  ylab("") +
                  ylim(0,50) +
                  geom_text(aes(label=kw_ChimaeraFA_hatton$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_HelicolenusFA_hatton<-kruskal.test(HelicolenusFA~CoralNoCoral, data=subhatton)
kw_HelicolenusFA_hatton$star <- "" 
kw_HelicolenusFA_hatton$star[kw_HelicolenusFA_hatton$p.value <= .05]  <- "*" 
kw_HelicolenusFA_hatton$star[kw_HelicolenusFA_hatton$p.value <= .01]  <- "**" 
kw_HelicolenusFA_hatton$star[kw_HelicolenusFA_hatton$p.value <= .001] <- "***"

#Plot HelicolenusFA for Hatton Bank
summaryhattonHelicolenusFA <- summarySE(subhatton, measurevar="HelicolenusFA", groupvars="CoralNoCoral")

plothattonHelicolenusFA <- ggplot(summaryhattonHelicolenusFA, aes(x=CoralNoCoral, y=HelicolenusFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=HelicolenusFA-se, ymax=HelicolenusFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="Hatton Bank") +
                  xlab("Coral framework") +
                  ylab("") +
                  ylim(0,200) +
                  geom_text(aes(label=kw_HelicolenusFA_hatton$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_LophiusFA_hatton<-kruskal.test(LophiusFA~CoralNoCoral, data=subhatton)
kw_LophiusFA_hatton$star <- "" 
kw_LophiusFA_hatton$star[kw_LophiusFA_hatton$p.value <= .05]  <- "*" 
kw_LophiusFA_hatton$star[kw_LophiusFA_hatton$p.value <= .01]  <- "**" 
kw_LophiusFA_hatton$star[kw_LophiusFA_hatton$p.value <= .001] <- "***"

#Plot LophiusFA for Hatton Bank
summaryhattonLophiusFA <- summarySE(subhatton, measurevar="LophiusFA", groupvars="CoralNoCoral")

plothattonLophiusFA <- ggplot(summaryhattonLophiusFA, aes(x=CoralNoCoral, y=LophiusFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=LophiusFA-se, ymax=LophiusFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="Hatton Bank") +
                  xlab("Coral framework") +
                  ylab("") +
                  ylim(0,10) +
                  geom_text(aes(label=kw_LophiusFA_hatton$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_MolvaFA_hatton<-kruskal.test(MolvaFA~CoralNoCoral, data=subhatton)
kw_MolvaFA_hatton$star <- "" 
kw_MolvaFA_hatton$star[kw_MolvaFA_hatton$p.value <= .05]  <- "*" 
kw_MolvaFA_hatton$star[kw_MolvaFA_hatton$p.value <= .01]  <- "**" 
kw_MolvaFA_hatton$star[kw_MolvaFA_hatton$p.value <= .001] <- "***"

#Plot MolvaFA for Hatton Bank
summaryhattonMolvaFA <- summarySE(subhatton, measurevar="MolvaFA", groupvars="CoralNoCoral")

plothattonMolvaFA <- ggplot(summaryhattonMolvaFA, aes(x=CoralNoCoral, y=MolvaFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MolvaFA-se, ymax=MolvaFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="Hatton Bank") +
                  xlab("Coral framework") +
                  ylab("") +
                  ylim(0,5) +
                  geom_text(aes(label=kw_MolvaFA_hatton$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Plot BrosmeFA for Hatton Bank
summaryhattonBrosmeFA <- summarySE(subhatton, measurevar="BrosmeFA", groupvars="CoralNoCoral")

plothattonBrosmeFA <- ggplot(summaryhattonBrosmeFA, aes(x=CoralNoCoral, y=BrosmeFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=BrosmeFA-se, ymax=BrosmeFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("Coral framework") +
                  ylab("") +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Plot HoplostethusFA for Hatton Bank
summaryhattonHoplostethusFA <- summarySE(subhatton, measurevar="HoplostethusFA", groupvars="CoralNoCoral")

plothattonHoplostethusFA <- ggplot(summaryhattonHoplostethusFA, aes(x=CoralNoCoral, y=HoplostethusFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=HoplostethusFA-se, ymax=HoplostethusFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("Coral framework") +
                  ylab("") +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Plot PhycisFA for Hatton Bank
summaryhattonPhycisFA <- summarySE(subhatton, measurevar="PhycisFA", groupvars="CoralNoCoral")

plothattonPhycisFA <- ggplot(summaryhattonPhycisFA, aes(x=CoralNoCoral, y=PhycisFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=PhycisFA-se, ymax=PhycisFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("Coral framework") +
                  ylab("") +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Plot PseudotriakisFA for Hatton Bank
summaryhattonPseudotriakisFA <- summarySE(subhatton, measurevar="PseudotriakisFA", groupvars="CoralNoCoral")

plothattonPseudotriakisFA <- ggplot(summaryhattonPseudotriakisFA, aes(x=CoralNoCoral, y=PseudotriakisFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=PseudotriakisFA-se, ymax=PseudotriakisFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("Coral framework") +
                  ylab("") +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Plot RajaFA for Hatton Bank
summaryhattonRajaFA <- summarySE(subhatton, measurevar="RajaFA", groupvars="CoralNoCoral")

plothattonRajaFA <- ggplot(summaryhattonRajaFA, aes(x=CoralNoCoral, y=RajaFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=RajaFA-se, ymax=RajaFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("Coral framework") +
                  ylab("") +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map


#Rockall Bank

#Subset data for Rockall Bank
subrockall <- subset(data, Area == "Rockall Bank")

#Kruskall-Wallis Test, non-paramteric rank test
kw_LepidionFA_rockall<-kruskal.test(LepidionFA~CoralNoCoral, data=subrockall)
kw_LepidionFA_rockall$star <- "" 
kw_LepidionFA_rockall$star[kw_LepidionFA_rockall$p.value <= .05]  <- "*" 
kw_LepidionFA_rockall$star[kw_LepidionFA_rockall$p.value <= .01]  <- "**" 
kw_LepidionFA_rockall$star[kw_LepidionFA_rockall$p.value <= .001] <- "***"

#Plot LepidionFA for Rockall Bank
summaryrockallLepidionFA <- summarySE(subrockall, measurevar="LepidionFA", groupvars="CoralNoCoral")

plotrockallLepidionFA <- ggplot(summaryrockallLepidionFA, aes(x=CoralNoCoral, y=LepidionFA)) +
    geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
    geom_errorbar(aes(ymin=LepidionFA-se, ymax=LepidionFA+se),
                  width=.1,                    # Width of the error bars
                  position=position_dodge(.9)) +
                    opts(title="Rockall Bank") +
    xlab("Coral framework")+
    ylab("") +
    ylim(0,400) +
    geom_text(aes(label=kw_LepidionFA_rockall$star), colour="black", vjust=-1, size=10) +
    #scale_y_continuous(breaks=-1:20000*1000) +
    #scale_x_discrete(limits=c("non-coral","coral")) + #Reorders x factors on the graph
    geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
    theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_SynaphobranchusFA_rockall<-kruskal.test(SynaphobranchusFA~CoralNoCoral, data=subrockall)
kw_SynaphobranchusFA_rockall$star <- "" 
kw_SynaphobranchusFA_rockall$star[kw_SynaphobranchusFA_rockall$p.value <= .05]  <- "*" 
kw_SynaphobranchusFA_rockall$star[kw_SynaphobranchusFA_rockall$p.value <= .01]  <- "**" 
kw_SynaphobranchusFA_rockall$star[kw_SynaphobranchusFA_rockall$p.value <= .001] <- "***"

#Plot SynaphobranchusFA for Rockall Bank
summaryrockallSynaphobranchusFA <- summarySE(subrockall, measurevar="SynaphobranchusFA", groupvars="CoralNoCoral")

plotrockallSynaphobranchusFA <- ggplot(summaryrockallSynaphobranchusFA, aes(x=CoralNoCoral, y=SynaphobranchusFA)) +
    geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
    geom_errorbar(aes(ymin=SynaphobranchusFA-se, ymax=SynaphobranchusFA+se),
                  width=.1,                    # Width of the error bars
                  position=position_dodge(.9)) +
                    opts(title="Rockall Bank") +
    xlab("Coral framework") +
     ylab("") +
     ylim(0,120) +
     geom_text(aes(label=kw_SynaphobranchusFA_rockall$star), colour="black", vjust=-1, size=10) +
    #scale_y_continuous(breaks=-1:20000*1000) +
    #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
    geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
    theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_SigmopsFA_rockall<-kruskal.test(SigmopsFA~CoralNoCoral, data=subrockall)
kw_SigmopsFA_rockall$star <- "" 
kw_SigmopsFA_rockall$star[kw_SigmopsFA_rockall$p.value <= .05]  <- "*" 
kw_SigmopsFA_rockall$star[kw_SigmopsFA_rockall$p.value <= .01]  <- "**" 
kw_SigmopsFA_rockall$star[kw_SigmopsFA_rockall$p.value <= .001] <- "***"

#Plot SigmopsFA for Rockall Bank
summaryrockallSigmopsFA <- summarySE(subrockall, measurevar="SigmopsFA", groupvars="CoralNoCoral")

plotrockallSigmopsFA <- ggplot(summaryrockallSigmopsFA, aes(x=CoralNoCoral, y=SigmopsFA)) +
    geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
    geom_errorbar(aes(ymin=SigmopsFA-se, ymax=SigmopsFA+se),
                  width=.1,                    # Width of the error bars
                  position=position_dodge(.9)) +
                    opts(title="Rockall Bank") +
    xlab("Coral framework") +
     ylab("") +
     ylim(0,100) +
     geom_text(aes(label=kw_SigmopsFA_rockall$star), colour="black", vjust=-1, size=10) +
    #scale_y_continuous(breaks=-1:20000*1000) +
    #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
    geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
    theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_MoraFA_rockall<-kruskal.test(MoraFA~CoralNoCoral, data=subrockall)
kw_MoraFA_rockall$star <- "" 
kw_MoraFA_rockall$star[kw_MoraFA_rockall$p.value <= .05]  <- "*" 
kw_MoraFA_rockall$star[kw_MoraFA_rockall$p.value <= .01]  <- "**" 
kw_MoraFA_rockall$star[kw_MoraFA_rockall$p.value <= .001] <- "***"

#Plot MoraFA for Rockall Bank
summaryrockallMoraFA <- summarySE(subrockall, measurevar="MoraFA", groupvars="CoralNoCoral")

plotrockallMoraFA <- ggplot(summaryrockallMoraFA, aes(x=CoralNoCoral, y=MoraFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MoraFA-se, ymax=MoraFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="Rockall Bank") +
                  xlab("Coral framework") +
                  ylab("") +
                  ylim(0,25) +
                  geom_text(aes(label=kw_MoraFA_rockall$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_ChimaeraFA_rockall<-kruskal.test(ChimaeraFA~CoralNoCoral, data=subrockall)
kw_ChimaeraFA_rockall$star <- "" 
kw_ChimaeraFA_rockall$star[kw_ChimaeraFA_rockall$p.value <= .05]  <- "*" 
kw_ChimaeraFA_rockall$star[kw_ChimaeraFA_rockall$p.value <= .01]  <- "**" 
kw_ChimaeraFA_rockall$star[kw_ChimaeraFA_rockall$p.value <= .001] <- "***"

#Plot ChimaeraFA for Rockall Bank
summaryrockallChimaeraFA <- summarySE(subrockall, measurevar="ChimaeraFA", groupvars="CoralNoCoral")

plotrockallChimaeraFA <- ggplot(summaryrockallChimaeraFA, aes(x=CoralNoCoral, y=ChimaeraFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=ChimaeraFA-se, ymax=ChimaeraFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="Rockall Bank") +
                  xlab("Coral framework") +
                  ylab("") +
                  ylim(0,50) +
                  geom_text(aes(label=kw_ChimaeraFA_rockall$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_HelicolenusFA_rockall<-kruskal.test(HelicolenusFA~CoralNoCoral, data=subrockall)
kw_HelicolenusFA_rockall$star <- "" 
kw_HelicolenusFA_rockall$star[kw_HelicolenusFA_rockall$p.value <= .05]  <- "*" 
kw_HelicolenusFA_rockall$star[kw_HelicolenusFA_rockall$p.value <= .01]  <- "**" 
kw_HelicolenusFA_rockall$star[kw_HelicolenusFA_rockall$p.value <= .001] <- "***"

#Plot HelicolenusFA for Rockall Bank
summaryrockallHelicolenusFA <- summarySE(subrockall, measurevar="HelicolenusFA", groupvars="CoralNoCoral")

plotrockallHelicolenusFA <- ggplot(summaryrockallHelicolenusFA, aes(x=CoralNoCoral, y=HelicolenusFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=HelicolenusFA-se, ymax=HelicolenusFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="Rockall Bank") +
                  xlab("Coral framework") +
                  ylab("") +
                  ylim(0,200) +
                  geom_text(aes(label=kw_HelicolenusFA_rockall$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_LophiusFA_rockall<-kruskal.test(LophiusFA~CoralNoCoral, data=subrockall)
kw_LophiusFA_rockall$star <- "" 
kw_LophiusFA_rockall$star[kw_LophiusFA_rockall$p.value <= .05]  <- "*" 
kw_LophiusFA_rockall$star[kw_LophiusFA_rockall$p.value <= .01]  <- "**" 
kw_LophiusFA_rockall$star[kw_LophiusFA_rockall$p.value <= .001] <- "***"

#Plot LophiusFA for Rockall Bank
summaryrockallLophiusFA <- summarySE(subrockall, measurevar="LophiusFA", groupvars="CoralNoCoral")

plotrockallLophiusFA <- ggplot(summaryrockallLophiusFA, aes(x=CoralNoCoral, y=LophiusFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=LophiusFA-se, ymax=LophiusFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="Rockall Bank") +
                  xlab("Coral framework") +
                  ylab("") +
                  ylim(0,10) +
                  geom_text(aes(label=kw_LophiusFA_rockall$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_MolvaFA_rockall<-kruskal.test(MolvaFA~CoralNoCoral, data=subrockall)
kw_MolvaFA_rockall$star <- "" 
kw_MolvaFA_rockall$star[kw_MolvaFA_rockall$p.value <= .05]  <- "*" 
kw_MolvaFA_rockall$star[kw_MolvaFA_rockall$p.value <= .01]  <- "**" 
kw_MolvaFA_rockall$star[kw_MolvaFA_rockall$p.value <= .001] <- "***"

#Plot MolvaFA for Rockall Bank
summaryrockallMolvaFA <- summarySE(subrockall, measurevar="MolvaFA", groupvars="CoralNoCoral")

plotrockallMolvaFA <- ggplot(summaryrockallMolvaFA, aes(x=CoralNoCoral, y=MolvaFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MolvaFA-se, ymax=MolvaFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="Rockall Bank") +
                  xlab("Coral framework") +
                  ylab("") +
                  ylim(0,5) +
                  geom_text(aes(label=kw_MolvaFA_rockall$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Plot BrosmeFA for Rockall Bank
summaryrockallBrosmeFA <- summarySE(subrockall, measurevar="BrosmeFA", groupvars="CoralNoCoral")

plotrockallBrosmeFA <- ggplot(summaryrockallBrosmeFA, aes(x=CoralNoCoral, y=BrosmeFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=BrosmeFA-se, ymax=BrosmeFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("Coral framework") +
                  ylab("") +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Plot HoplostethusFA for Rockall Bank
summaryrockallHoplostethusFA <- summarySE(subrockall, measurevar="HoplostethusFA", groupvars="CoralNoCoral")

plotrockallHoplostethusFA <- ggplot(summaryrockallHoplostethusFA, aes(x=CoralNoCoral, y=HoplostethusFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=HoplostethusFA-se, ymax=HoplostethusFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("Coral framework") +
                  ylab("") +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Plot PhycisFA for Rockall Bank
summaryrockallPhycisFA <- summarySE(subrockall, measurevar="PhycisFA", groupvars="CoralNoCoral")

plotrockallPhycisFA <- ggplot(summaryrockallPhycisFA, aes(x=CoralNoCoral, y=PhycisFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=PhycisFA-se, ymax=PhycisFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("Coral framework") +
                  ylab("") +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Plot PseudotriakisFA for Rockall Bank
summaryrockallPseudotriakisFA <- summarySE(subrockall, measurevar="PseudotriakisFA", groupvars="CoralNoCoral")

plotrockallPseudotriakisFA <- ggplot(summaryrockallPseudotriakisFA, aes(x=CoralNoCoral, y=PseudotriakisFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=PseudotriakisFA-se, ymax=PseudotriakisFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("Coral framework") +
                  ylab("") +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Plot RajaFA for Rockall Bank
summaryrockallRajaFA <- summarySE(subrockall, measurevar="RajaFA", groupvars="CoralNoCoral")

plotrockallRajaFA <- ggplot(summaryrockallRajaFA, aes(x=CoralNoCoral, y=RajaFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=RajaFA-se, ymax=RajaFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("Coral framework") +
                  ylab("") +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#All Areas

#Kruskall-Wallis Test, non-paramteric rank test
kw_LepidionFA_data<-kruskal.test(LepidionFA~CoralNoCoral, data=data)
kw_LepidionFA_data$star <- "" 
kw_LepidionFA_data$star[kw_LepidionFA_data$p.value <= .05]  <- "*" 
kw_LepidionFA_data$star[kw_LepidionFA_data$p.value <= .01]  <- "**" 
kw_LepidionFA_data$star[kw_LepidionFA_data$p.value <= .001] <- "***"

#Summary LepidionFA for All Areas
summarydataLepidionFA <- summarySE(data, measurevar="LepidionFA", groupvars="CoralNoCoral")

#Plot LepidionFA for All Areas
plotdataLepidionFA <- ggplot(summarydataLepidionFA, aes(x=CoralNoCoral, y=LepidionFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=LepidionFA-se, ymax=LepidionFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="All Areas") +
                  xlab("Coral framework") +
                  ylab("") +
                  ylim(0,400) +
                  geom_text(aes(label=kw_LepidionFA_data$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_SynaphobranchusFA_data<-kruskal.test(SynaphobranchusFA~CoralNoCoral, data=data)
kw_SynaphobranchusFA_data$star <- "" 
kw_SynaphobranchusFA_data$star[kw_SynaphobranchusFA_data$p.value <= .05]  <- "*" 
kw_SynaphobranchusFA_data$star[kw_SynaphobranchusFA_data$p.value <= .01]  <- "**" 
kw_SynaphobranchusFA_data$star[kw_SynaphobranchusFA_data$p.value <= .001] <- "***"

#Plot SynaphobranchusFA for All Areas
summarydataSynaphobranchusFA <- summarySE(data, measurevar="SynaphobranchusFA", groupvars="CoralNoCoral")

plotdataSynaphobranchusFA <- ggplot(summarydataSynaphobranchusFA, aes(x=CoralNoCoral, y=SynaphobranchusFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=SynaphobranchusFA-se, ymax=SynaphobranchusFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="All Areas") +
                  xlab("Coral framework") +
                  ylab("") +
                  ylim(0,120) +
                  geom_text(aes(label=kw_SynaphobranchusFA_data$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_SigmopsFA_data<-kruskal.test(SigmopsFA~CoralNoCoral, data=data)
kw_SigmopsFA_data$star <- "" 
kw_SigmopsFA_data$star[kw_SigmopsFA_data$p.value <= .05]  <- "*" 
kw_SigmopsFA_data$star[kw_SigmopsFA_data$p.value <= .01]  <- "**" 
kw_SigmopsFA_data$star[kw_SigmopsFA_data$p.value <= .001] <- "***"

#Plot SigmopsFA for All Areas
summarydataSigmopsFA <- summarySE(data, measurevar="SigmopsFA", groupvars="CoralNoCoral")

plotdataSigmopsFA <- ggplot(summarydataSigmopsFA, aes(x=CoralNoCoral, y=SigmopsFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=SigmopsFA-se, ymax=SigmopsFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="All Areas") +
                  xlab("Coral framework") +
                  ylab("") +
                  ylim(0,100) +
                  geom_text(aes(label=kw_SigmopsFA_data$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_MoraFA_data<-kruskal.test(MoraFA~CoralNoCoral, data=data)
kw_MoraFA_data$star <- "" 
kw_MoraFA_data$star[kw_MoraFA_data$p.value <= .05]  <- "*" 
kw_MoraFA_data$star[kw_MoraFA_data$p.value <= .01]  <- "**" 
kw_MoraFA_data$star[kw_MoraFA_data$p.value <= .001] <- "***"

#Plot MoraFA for All Areas
summarydataMoraFA <- summarySE(data, measurevar="MoraFA", groupvars="CoralNoCoral")

plotdataMoraFA <- ggplot(summarydataMoraFA, aes(x=CoralNoCoral, y=MoraFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MoraFA-se, ymax=MoraFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="All Areas") +
                  xlab("Coral framework") +
                  ylab("") +
                  ylim(0,25) +
                  geom_text(aes(label=kw_MoraFA_data$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_ChimaeraFA_data<-kruskal.test(ChimaeraFA~CoralNoCoral, data=data)
kw_ChimaeraFA_data$star <- "" 
kw_ChimaeraFA_data$star[kw_ChimaeraFA_data$p.value <= .05]  <- "*" 
kw_ChimaeraFA_data$star[kw_ChimaeraFA_data$p.value <= .01]  <- "**" 
kw_ChimaeraFA_data$star[kw_ChimaeraFA_data$p.value <= .001] <- "***"


#Plot ChimaeraFA for All Areas
summarydataChimaeraFA <- summarySE(data, measurevar="ChimaeraFA", groupvars="CoralNoCoral")

plotdataChimaeraFA <- ggplot(summarydataChimaeraFA, aes(x=CoralNoCoral, y=ChimaeraFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=ChimaeraFA-se, ymax=ChimaeraFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="All Areas") +
                  xlab("Coral framework") +
                  ylab("") +
                  ylim(0,50) +
                  geom_text(aes(label=kw_ChimaeraFA_data$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_HelicolenusFA_data<-kruskal.test(HelicolenusFA~CoralNoCoral, data=data)
kw_HelicolenusFA_data$star <- "" 
kw_HelicolenusFA_data$star[kw_HelicolenusFA_data$p.value <= .05]  <- "*" 
kw_HelicolenusFA_data$star[kw_HelicolenusFA_data$p.value <= .01]  <- "**" 
kw_HelicolenusFA_data$star[kw_HelicolenusFA_data$p.value <= .001] <- "***"

#Plot HelicolenusFA for All Areas
summarydataHelicolenusFA <- summarySE(data, measurevar="HelicolenusFA", groupvars="CoralNoCoral")

plotdataHelicolenusFA <- ggplot(summarydataHelicolenusFA, aes(x=CoralNoCoral, y=HelicolenusFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=HelicolenusFA-se, ymax=HelicolenusFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="All Areas") +
                  xlab("Coral framework") +
                  ylab("") +
                  ylim(0,200) +
                  geom_text(aes(label=kw_HelicolenusFA_data$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_LophiusFA_data<-kruskal.test(LophiusFA~CoralNoCoral, data=data)
kw_LophiusFA_data$star <- "" 
kw_LophiusFA_data$star[kw_LophiusFA_data$p.value <= .05]  <- "*" 
kw_LophiusFA_data$star[kw_LophiusFA_data$p.value <= .01]  <- "**" 
kw_LophiusFA_data$star[kw_LophiusFA_data$p.value <= .001] <- "***"

#Plot LophiusFA for All Areas
summarydataLophiusFA <- summarySE(data, measurevar="LophiusFA", groupvars="CoralNoCoral")

plotdataLophiusFA <- ggplot(summarydataLophiusFA, aes(x=CoralNoCoral, y=LophiusFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=LophiusFA-se, ymax=LophiusFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="All Areas") +
                  xlab("Coral framework") +
                  ylab("") +
                  ylim(0,10) +
                  geom_text(aes(label=kw_LophiusFA_data$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_MolvaFA_data<-kruskal.test(MolvaFA~CoralNoCoral, data=data)
kw_MolvaFA_data$star <- "" 
kw_MolvaFA_data$star[kw_MolvaFA_data$p.value <= .05]  <- "*" 
kw_MolvaFA_data$star[kw_MolvaFA_data$p.value <= .01]  <- "**" 
kw_MolvaFA_data$star[kw_MolvaFA_data$p.value <= .001] <- "***"

#Plot MolvaFA for All Areas
summarydataMolvaFA <- summarySE(data, measurevar="MolvaFA", groupvars="CoralNoCoral")

plotdataMolvaFA <- ggplot(summarydataMolvaFA, aes(x=CoralNoCoral, y=MolvaFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MolvaFA-se, ymax=MolvaFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="All Areas") +
                  xlab("Coral framework") +
                  ylab("") +
                  ylim(0,5) +
                  geom_text(aes(label=kw_MolvaFA_data$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map


# Produce plot with grid layout
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 4)))

#Belgica Mound
print(plotbelgicaLepidionFA, vp = vplayout(1, 1))
print(plotbelgicaSynaphobranchusFA, vp = vplayout(2, 1))
print(plotbelgicaSigmopsFA, vp = vplayout(3, 1))
print(plotbelgicaMoraFA, vp = vplayout(4, 1))
#print(plotbelgicaChimaeraFA, vp = vplayout(1, 1))
#print(plotbelgicaHelicolenusFA, vp = vplayout(2, 1))
#print(plotbelgicaLophiusFA, vp = vplayout(3, 1))
#print(plotbelgicaMolvaFA, vp = vplayout(4, 1))


# Hatton Bank
print(plothattonLepidionFA, vp = vplayout(1, 2))
print(plothattonSynaphobranchusFA, vp = vplayout(2, 2))
print(plothattonSigmopsFA, vp = vplayout(3, 2))
print(plothattonMoraFA, vp = vplayout(4, 2))
#print(plothattonChimaeraFA, vp = vplayout(1, 2))
#print(plothattonHelicolenusFA, vp = vplayout(2, 2))
#print(plothattonLophiusFA, vp = vplayout(3, 2))
#print(plothattonMolvaFA, vp = vplayout(4, 2))


#Rockall Bank
print(plotrockallLepidionFA, vp = vplayout(1, 3))
print(plotrockallSynaphobranchusFA, vp = vplayout(2, 3))
print(plotrockallSigmopsFA, vp = vplayout(3, 3))
print(plotrockallMoraFA, vp = vplayout(4, 3))
#print(plotrockallChimaeraFA, vp = vplayout(1, 3))
#print(plotrockallHelicolenusFA, vp = vplayout(2, 3))
#print(plotrockallLophiusFA, vp = vplayout(3, 3))
#print(plotrockallMolvaFA, vp = vplayout(4, 3))

#All Areas
print(plotdataLepidionFA, vp = vplayout(1, 4))
print(plotdataSynaphobranchusFA, vp = vplayout(2, 4))
print(plotdataSigmopsFA, vp = vplayout(3, 4))
print(plotdataMoraFA, vp = vplayout(4, 4))
#print(plotdataChimaeraFA, vp = vplayout(1, 4))
#print(plotdataHelicolenusFA, vp = vplayout(2, 4))
#print(plotdataLophiusFA, vp = vplayout(3, 4))
#print(plotdataMolvaFA, vp = vplayout(4, 4))
