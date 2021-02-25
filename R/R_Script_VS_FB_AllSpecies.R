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

#setwd("C:/Users/MBiber/Documents/Analysis_R")
data <- read.delim("VideoData_FA_FB.csv", sep=",",header=T)


# Belgica Mound

#Subset data for Belgica Mound
subbelgica <- subset(data, Area == "Belgica Mound")

#Kruskall-Wallis Test, non-paramteric rank test
kw_LepidionFB_belgica<-kruskal.test(LepidionFB~CoralNoCoral, data=subbelgica)
kw_LepidionFB_belgica$star <- "" 
kw_LepidionFB_belgica$star[kw_LepidionFB_belgica$p.value <= .05]  <- "*" 
kw_LepidionFB_belgica$star[kw_LepidionFB_belgica$p.value <= .01]  <- "**" 
kw_LepidionFB_belgica$star[kw_LepidionFB_belgica$p.value <= .001] <- "***"

#Plot LepidionFB for Belgica Mound
summarybelgicaLepidionFB <- summarySE(subbelgica, measurevar="LepidionFB", groupvars="CoralNoCoral")

plotbelgicaLepidionFB <- ggplot(summarybelgicaLepidionFB, aes(x=CoralNoCoral, y=LepidionFB)) +
    geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
    geom_errorbar(aes(ymin=LepidionFB-se, ymax=LepidionFB+se),
                  width=.1,                    # Width of the error bars
                  position=position_dodge(.9)) +   
                    opts(title="Belgica Mound") +
    xlab("Coral framework") +
    ylab(expression(paste(italic("Lepidion eques")))) +
    ylim(0, 40) + 
    geom_text(aes(label=kw_LepidionFB_belgica$star), colour="black", vjust=-1, size=10) +
    #scale_y_continuous(breaks=-1:20000*1000) +
    #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
    geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
    theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_SynaphobranchusFB_belgica<-kruskal.test(SynaphobranchusFB~CoralNoCoral, data=subbelgica)
kw_SynaphobranchusFB_belgica$star <- "" 
kw_SynaphobranchusFB_belgica$star[kw_SynaphobranchusFB_belgica$p.value <= .05]  <- "*" 
kw_SynaphobranchusFB_belgica$star[kw_SynaphobranchusFB_belgica$p.value <= .01]  <- "**" 
kw_SynaphobranchusFB_belgica$star[kw_SynaphobranchusFB_belgica$p.value <= .001] <- "***"

#Plot SynaphobranchusFB for Belgica Mound
summarybelgicaSynaphobranchusFB <- summarySE(subbelgica, measurevar="SynaphobranchusFB", groupvars="CoralNoCoral")

plotbelgicaSynaphobranchusFB <- ggplot(summarybelgicaSynaphobranchusFB, aes(x=CoralNoCoral, y=SynaphobranchusFB)) +
    geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
    geom_errorbar(aes(ymin=SynaphobranchusFB-se, ymax=SynaphobranchusFB+se),
                  width=.1,                    # Width of the error bars
                  position=position_dodge(.9)) +
                    opts(title="Belgica Mound") +
    xlab("Coral framework") +
    ylab(expression(paste(italic("Synaphobranchus kaupii")))) +
    ylim(0, 0.8) +
    geom_text(aes(label=kw_SynaphobranchusFB_belgica$star), colour="black", vjust=-1, size=10) +
    #scale_y_continuous(breaks=-1:20000*1000) +
    #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
    geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
    theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_MoraFB_belgica<-kruskal.test(MoraFB~CoralNoCoral, data=subbelgica)
kw_MoraFB_belgica$star <- "" 
kw_MoraFB_belgica$star[kw_MoraFB_belgica$p.value <= .05]  <- "*" 
kw_MoraFB_belgica$star[kw_MoraFB_belgica$p.value <= .01]  <- "**" 
kw_MoraFB_belgica$star[kw_MoraFB_belgica$p.value <= .001] <- "***"

#Plot MoraFA for Belgica Mound
summarybelgicaMoraFB <- summarySE(subbelgica, measurevar="MoraFB", groupvars="CoralNoCoral")

plotbelgicaMoraFB <- ggplot(summarybelgicaMoraFB, aes(x=CoralNoCoral, y=MoraFB)) +
    geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
    geom_errorbar(aes(ymin=MoraFB-se, ymax=MoraFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="Belgica Mound") +
    xlab("Coral framework") +
      ylab(expression(paste(italic("Mora moro")))) +
      ylim(0, 100) +
      geom_text(aes(label=kw_MolvaFB_belgica$star), colour="black", vjust=-1, size=10) +
    #scale_y_continuous(breaks=-1:20000*1000) +
    #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
    geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
    theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_ChimaeraFB_belgica<-kruskal.test(ChimaeraFB~CoralNoCoral, data=subbelgica)
kw_ChimaeraFB_belgica$star <- "" 
kw_ChimaeraFB_belgica$star[kw_ChimaeraFB_belgica$p.value <= .05]  <- "*" 
kw_ChimaeraFB_belgica$star[kw_ChimaeraFB_belgica$p.value <= .01]  <- "**" 
kw_ChimaeraFB_belgica$star[kw_ChimaeraFB_belgica$p.value <= .001] <- "***"

#Plot ChimaeraFB for Belgica Mound
summarybelgicaChimaeraFB <- summarySE(subbelgica, measurevar="ChimaeraFB", groupvars="CoralNoCoral")

plotbelgicaChimaeraFB <- ggplot(summarybelgicaChimaeraFB, aes(x=CoralNoCoral, y=ChimaeraFB)) +
    geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
    geom_errorbar(aes(ymin=ChimaeraFB-se, ymax=ChimaeraFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="Belgica Mound") +
    xlab("Coral framework") +
     ylab(expression(paste(italic("Chimaera monstrosa")))) +
     ylim(0, 40) +
     geom_text(aes(label=kw_ChimaeraFB_belgica$star), colour="black", vjust=-1, size=10) +
    #scale_y_continuous(breaks=-1:20000*1000) +
    #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
    geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
    theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_HelicolenusFB_belgica<-kruskal.test(HelicolenusFB~CoralNoCoral, data=subbelgica)
kw_HelicolenusFB_belgica$star <- "" 
kw_HelicolenusFB_belgica$star[kw_HelicolenusFB_belgica$p.value <= .05]  <- "*" 
kw_HelicolenusFB_belgica$star[kw_HelicolenusFB_belgica$p.value <= .01]  <- "**" 
kw_HelicolenusFB_belgica$star[kw_HelicolenusFB_belgica$p.value <= .001] <- "***"

#Plot HelicolenusFB for Belgica Mound
summarybelgicaHelicolenusFB <- summarySE(subbelgica, measurevar="HelicolenusFB", groupvars="CoralNoCoral")

plotbelgicaHelicolenusFB <- ggplot(summarybelgicaHelicolenusFB, aes(x=CoralNoCoral, y=HelicolenusFB)) +
    geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
    geom_errorbar(aes(ymin=HelicolenusFB-se, ymax=HelicolenusFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="Belgica Mound") +
    xlab("Coral framework") +
     ylab(expression(paste(italic("Helicolenus dactylopterus")))) +
     ylim(0, 50) +
     geom_text(aes(label=kw_HelicolenusFB_belgica$star), colour="black", vjust=-1, size=10) +
    #scale_y_continuous(breaks=-1:20000*1000) +
    #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
    geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
    theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_LophiusFB_belgica<-kruskal.test(LophiusFB~CoralNoCoral, data=subbelgica)
kw_LophiusFB_belgica$star <- "" 
kw_LophiusFB_belgica$star[kw_LophiusFB_belgica$p.value <= .05]  <- "*" 
kw_LophiusFB_belgica$star[kw_LophiusFB_belgica$p.value <= .01]  <- "**" 
kw_LophiusFB_belgica$star[kw_LophiusFB_belgica$p.value <= .001] <- "***"

#Plot LophiusFB for Belgica Mound
summarybelgicaLophiusFB <- summarySE(subbelgica, measurevar="LophiusFB", groupvars="CoralNoCoral")

plotbelgicaLophiusFB <- ggplot(summarybelgicaLophiusFB, aes(x=CoralNoCoral, y=LophiusFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=LophiusFB-se, ymax=LophiusFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="Belgica Mound") +
                  xlab("Coral framework") +
                  ylab(expression(paste(italic("Lophius piscatorius")))) +
                  ylim(0, 150) +
                  geom_text(aes(label=kw_LophiusFB_belgica$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_MolvaFB_belgica<-kruskal.test(MolvaFB~CoralNoCoral, data=subbelgica)
kw_MolvaFB_belgica$star <- "" 
kw_MolvaFB_belgica$star[kw_MolvaFB_belgica$p.value <= .05]  <- "*" 
kw_MolvaFB_belgica$star[kw_MolvaFB_belgica$p.value <= .01]  <- "**" 
kw_MolvaFB_belgica$star[kw_MolvaFB_belgica$p.value <= .001] <- "***"

#Plot MolvaFB for Belgica Mound
summarybelgicaMolvaFB <- summarySE(subbelgica, measurevar="MolvaFB", groupvars="CoralNoCoral")

plotbelgicaMolvaFB <- ggplot(summarybelgicaMolvaFB, aes(x=CoralNoCoral, y=MolvaFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MolvaFB-se, ymax=MolvaFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="Belgica Mound") +
                  xlab("Coral framework") +
                  ylab(expression(paste(italic("Molva dypterygia")))) +
                  ylim(0, 10) +
                  geom_text(aes(label=kw_MolvaFB_belgica$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map


#Hatton Bank

#Subset data for Hatton Bank
subhatton <- subset(data, Area == "Hatton Bank")

#Kruskall-Wallis Test, non-paramteric rank test
kw_LepidionFB_hatton<-kruskal.test(LepidionFB~CoralNoCoral, data=subhatton)
kw_LepidionFB_hatton$star <- "" 
kw_LepidionFB_hatton$star[kw_LepidionFB_hatton$p.value <= .05]  <- "*" 
kw_LepidionFB_hatton$star[kw_LepidionFB_hatton$p.value <= .01]  <- "**" 
kw_LepidionFB_hatton$star[kw_LepidionFB_hatton$p.value <= .001] <- "***"

#Plot LepidionFB for Hatton Bank
summaryhattonLepidionFB <- summarySE(subhatton, measurevar="LepidionFB", groupvars="CoralNoCoral")

plothattonLepidionFB <- ggplot(summaryhattonLepidionFB, aes(x=CoralNoCoral, y=LepidionFB)) +
    geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
    geom_errorbar(aes(ymin=LepidionFB-se, ymax=LepidionFB+se),
                  width=.1,                    # Width of the error bars
                  position=position_dodge(.9)) +     
                    opts(title="Hatton Bank") +
    xlab("Coral framework") +
     ylab("") +
     ylim(0, 40) + 
     geom_text(aes(label=kw_LepidionFB_hatton$star), colour="black", vjust=-1, size=10) +
    #scale_y_continuous(breaks=-1:20000*1000) +
    #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
    geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
    theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_SynaphobranchusFB_hatton<-kruskal.test(SynaphobranchusFB~CoralNoCoral, data=subhatton)
kw_SynaphobranchusFB_hatton$star <- "" 
kw_SynaphobranchusFB_hatton$star[kw_SynaphobranchusFB_hatton$p.value <= .05]  <- "*" 
kw_SynaphobranchusFB_hatton$star[kw_SynaphobranchusFB_hatton$p.value <= .01]  <- "**" 
kw_SynaphobranchusFB_hatton$star[kw_SynaphobranchusFB_hatton$p.value <= .001] <- "***"

#Plot SynaphobranchusFB for Hatton Bank
summaryhattonSynaphobranchusFB <- summarySE(subhatton, measurevar="SynaphobranchusFB", groupvars="CoralNoCoral")

plothattonSynaphobranchusFB <- ggplot(summaryhattonSynaphobranchusFB, aes(x=CoralNoCoral, y=SynaphobranchusFB)) +
    geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
    geom_errorbar(aes(ymin=SynaphobranchusFB-se, ymax=SynaphobranchusFB+se),
                  width=.1,                    # Width of the error bars
                  position=position_dodge(.9)) +
                    opts(title="Hatton Bank") +
    xlab("Coral framework") +
     ylab("") +
     ylim(0, 0.8) +
     geom_text(aes(label=kw_SynaphobranchusFB_hatton$star), colour="black", vjust=-1, size=10) +
    #scale_y_continuous(breaks=-1:20000*1000) +
    #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
    geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
    theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_MoraFB_hatton<-kruskal.test(MoraFB~CoralNoCoral, data=subhatton)
kw_MoraFB_hatton$star <- "" 
kw_MoraFB_hatton$star[kw_MoraFB_hatton$p.value <= .05]  <- "*" 
kw_MoraFB_hatton$star[kw_MoraFB_hatton$p.value <= .01]  <- "**" 
kw_MoraFB_hatton$star[kw_MoraFB_hatton$p.value <= .001] <- "***"

#Plot MoraFB for Hatton Bank
summaryhattonMoraFB <- summarySE(subhatton, measurevar="MoraFB", groupvars="CoralNoCoral")

plothattonMoraFB <- ggplot(summaryhattonMoraFB, aes(x=CoralNoCoral, y=MoraFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MoraFB-se, ymax=MoraFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="Hatton Bank") +
                  xlab("Coral framework") +
                  ylab("") +
                  ylim(0, 100) +
                  geom_text(aes(label=kw_MoraFB_hatton$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_ChimaeraFB_hatton<-kruskal.test(ChimaeraFB~CoralNoCoral, data=subhatton)
kw_ChimaeraFB_hatton$star <- "" 
kw_ChimaeraFB_hatton$star[kw_ChimaeraFB_hatton$p.value <= .05]  <- "*" 
kw_ChimaeraFB_hatton$star[kw_ChimaeraFB_hatton$p.value <= .01]  <- "**" 
kw_ChimaeraFB_hatton$star[kw_ChimaeraFB_hatton$p.value <= .001] <- "***"

#Plot ChimaeraFB for Hatton Bank
summaryhattonChimaeraFB <- summarySE(subhatton, measurevar="ChimaeraFB", groupvars="CoralNoCoral")

plothattonChimaeraFB <- ggplot(summaryhattonChimaeraFB, aes(x=CoralNoCoral, y=ChimaeraFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=ChimaeraFB-se, ymax=ChimaeraFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="Hatton Bank") +
                  xlab("Coral framework") +
                  ylab("") +
                  ylim(0, 40) +
                  geom_text(aes(label=kw_ChimaeraFB_hatton$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_HelicolenusFB_hatton<-kruskal.test(HelicolenusFB~CoralNoCoral, data=subhatton)
kw_HelicolenusFB_hatton$star <- "" 
kw_HelicolenusFB_hatton$star[kw_HelicolenusFB_hatton$p.value <= .05]  <- "*" 
kw_HelicolenusFB_hatton$star[kw_HelicolenusFB_hatton$p.value <= .01]  <- "**" 
kw_HelicolenusFB_hatton$star[kw_HelicolenusFB_hatton$p.value <= .001] <- "***"

#Plot HelicolenusFB for Hatton Bank
summaryhattonHelicolenusFB <- summarySE(subhatton, measurevar="HelicolenusFB", groupvars="CoralNoCoral")

plothattonHelicolenusFB <- ggplot(summaryhattonHelicolenusFB, aes(x=CoralNoCoral, y=HelicolenusFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=HelicolenusFB-se, ymax=HelicolenusFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="Hatton Bank") +
                  xlab("Coral framework") +
                  ylab("") +
                  ylim(0, 50) +
                  geom_text(aes(label=kw_HelicolenusFB_hatton$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_LophiusFB_hatton<-kruskal.test(LophiusFB~CoralNoCoral, data=subhatton)
kw_LophiusFB_hatton$star <- "" 
kw_LophiusFB_hatton$star[kw_LophiusFB_hatton$p.value <= .05]  <- "*" 
kw_LophiusFB_hatton$star[kw_LophiusFB_hatton$p.value <= .01]  <- "**" 
kw_LophiusFB_hatton$star[kw_LophiusFB_hatton$p.value <= .001] <- "***"

#Plot LophiusFB for Hatton Bank
summaryhattonLophiusFB <- summarySE(subhatton, measurevar="LophiusFB", groupvars="CoralNoCoral")

plothattonLophiusFB <- ggplot(summaryhattonLophiusFB, aes(x=CoralNoCoral, y=LophiusFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=LophiusFB-se, ymax=LophiusFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="Hatton Bank") +
                  xlab("Coral framework") +
                  ylab("") +
                  ylim(0, 150) +
                  geom_text(aes(label=kw_LophiusFB_hatton$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_MolvaFB_hatton<-kruskal.test(MolvaFB~CoralNoCoral, data=subhatton)
kw_MolvaFB_hatton$star <- "" 
kw_MolvaFB_hatton$star[kw_MolvaFB_hatton$p.value <= .05]  <- "*" 
kw_MolvaFB_hatton$star[kw_MolvaFB_hatton$p.value <= .01]  <- "**" 
kw_MolvaFB_hatton$star[kw_MolvaFB_hatton$p.value <= .001] <- "***"

#Plot MolvaFB for Hatton Bank
summaryhattonMolvaFB <- summarySE(subhatton, measurevar="MolvaFB", groupvars="CoralNoCoral")

plothattonMolvaFB <- ggplot(summaryhattonMolvaFB, aes(x=CoralNoCoral, y=MolvaFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MolvaFB-se, ymax=MolvaFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="Hatton Bank") +
                  xlab("Coral framework") +
                  ylab("") +
                  ylim(0, 10) +
                  geom_text(aes(label=kw_MolvaFB_hatton$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map


#Rockall Bank

#Subset data for Rockall Bank
subrockall <- subset(data, Area == "Rockall Bank")

#Kruskall-Wallis Test, non-paramteric rank test
kw_LepidionFB_rockall<-kruskal.test(LepidionFB~CoralNoCoral, data=subrockall)
kw_LepidionFB_rockall$star <- "" 
kw_LepidionFB_rockall$star[kw_LepidionFB_rockall$p.value <= .05]  <- "*" 
kw_LepidionFB_rockall$star[kw_LepidionFB_rockall$p.value <= .01]  <- "**" 
kw_LepidionFB_rockall$star[kw_LepidionFB_rockall$p.value <= .001] <- "***"

#Plot LepidionFB for Rockall Bank
summaryrockallLepidionFB <- summarySE(subrockall, measurevar="LepidionFB", groupvars="CoralNoCoral")

plotrockallLepidionFB <- ggplot(summaryrockallLepidionFB, aes(x=CoralNoCoral, y=LepidionFB)) +
    geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
    geom_errorbar(aes(ymin=LepidionFB-se, ymax=LepidionFB+se),
                  width=.1,                    # Width of the error bars
                  position=position_dodge(.9)) +
                    opts(title="Rockall Bank") +
    xlab("Coral framework")+
    ylab("") +
    ylim(0, 40) +
    geom_text(aes(label=kw_LepidionFB_rockall$star), colour="black", vjust=-1, size=10) +
    #scale_y_continuous(breaks=-1:20000*1000) +
    #scale_x_discrete(limits=c("non-coral","coral")) + #Reorders x factors on the graph
    geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
    theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_SynaphobranchusFB_rockall<-kruskal.test(SynaphobranchusFB~CoralNoCoral, data=subrockall)
kw_SynaphobranchusFB_rockall$star <- "" 
kw_SynaphobranchusFB_rockall$star[kw_SynaphobranchusFB_rockall$p.value <= .05]  <- "*" 
kw_SynaphobranchusFB_rockall$star[kw_SynaphobranchusFB_rockall$p.value <= .01]  <- "**" 
kw_SynaphobranchusFB_rockall$star[kw_SynaphobranchusFB_rockall$p.value <= .001] <- "***"

#Plot SynaphobranchusFB for Rockall Bank
summaryrockallSynaphobranchusFB <- summarySE(subrockall, measurevar="SynaphobranchusFB", groupvars="CoralNoCoral")

plotrockallSynaphobranchusFB <- ggplot(summaryrockallSynaphobranchusFB, aes(x=CoralNoCoral, y=SynaphobranchusFB)) +
    geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
    geom_errorbar(aes(ymin=SynaphobranchusFB-se, ymax=SynaphobranchusFB+se),
                  width=.1,                    # Width of the error bars
                  position=position_dodge(.9)) +
                    opts(title="Rockall Bank") +
    xlab("Coral framework") +
     ylab("") +
     ylim(0, 0.8) +
     geom_text(aes(label=kw_SynaphobranchusFB_rockall$star), colour="black", vjust=-1, size=10) +
    #scale_y_continuous(breaks=-1:20000*1000) +
    #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
    geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
    theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_MoraFB_rockall<-kruskal.test(MoraFB~CoralNoCoral, data=subrockall)
kw_MoraFB_rockall$star <- "" 
kw_MoraFB_rockall$star[kw_MoraFB_rockall$p.value <= .05]  <- "*" 
kw_MoraFB_rockall$star[kw_MoraFB_rockall$p.value <= .01]  <- "**" 
kw_MoraFB_rockall$star[kw_MoraFB_rockall$p.value <= .001] <- "***"

#Plot MoraFB for Rockall Bank
summaryrockallMoraFB <- summarySE(subrockall, measurevar="MoraFB", groupvars="CoralNoCoral")

plotrockallMoraFB <- ggplot(summaryrockallMoraFB, aes(x=CoralNoCoral, y=MoraFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MoraFB-se, ymax=MoraFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="Rockall Bank") +
                  xlab("Coral framework") +
                  ylab("") +
                  ylim(0, 100) +
                  geom_text(aes(label=kw_MoraFB_rockall$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_ChimaeraFB_rockall<-kruskal.test(ChimaeraFA~CoralNoCoral, data=subrockall)
kw_ChimaeraFB_rockall$star <- "" 
kw_ChimaeraFB_rockall$star[kw_ChimaeraFB_rockall$p.value <= .05]  <- "*" 
kw_ChimaeraFB_rockall$star[kw_ChimaeraFB_rockall$p.value <= .01]  <- "**" 
kw_ChimaeraFB_rockall$star[kw_ChimaeraFB_rockall$p.value <= .001] <- "***"

#Plot ChimaeraFB for Rockall Bank
summaryrockallChimaeraFB <- summarySE(subrockall, measurevar="ChimaeraFB", groupvars="CoralNoCoral")

plotrockallChimaeraFB <- ggplot(summaryrockallChimaeraFB, aes(x=CoralNoCoral, y=ChimaeraFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=ChimaeraFB-se, ymax=ChimaeraFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="Rockall Bank") +
                  xlab("Coral framework") +
                  ylab("") +
                  ylim(0, 40) +
                  geom_text(aes(label=kw_ChimaeraFB_rockall$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_HelicolenusFB_rockall<-kruskal.test(HelicolenusFB~CoralNoCoral, data=subrockall)
kw_HelicolenusFB_rockall$star <- "" 
kw_HelicolenusFB_rockall$star[kw_HelicolenusFB_rockall$p.value <= .05]  <- "*" 
kw_HelicolenusFB_rockall$star[kw_HelicolenusFB_rockall$p.value <= .01]  <- "**" 
kw_HelicolenusFB_rockall$star[kw_HelicolenusFB_rockall$p.value <= .001] <- "***"

#Plot HelicolenusFB for Rockall Bank
summaryrockallHelicolenusFB <- summarySE(subrockall, measurevar="HelicolenusFB", groupvars="CoralNoCoral")

plotrockallHelicolenusFB <- ggplot(summaryrockallHelicolenusFB, aes(x=CoralNoCoral, y=HelicolenusFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=HelicolenusFB-se, ymax=HelicolenusFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="Rockall Bank") +
                  xlab("Coral framework") +
                  ylab("") +
                  ylim(0, 50) +
                  geom_text(aes(label=kw_HelicolenusFB_rockall$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_LophiusFB_rockall<-kruskal.test(LophiusFB~CoralNoCoral, data=subrockall)
kw_LophiusFB_rockall$star <- "" 
kw_LophiusFB_rockall$star[kw_LophiusFB_rockall$p.value <= .05]  <- "*" 
kw_LophiusFB_rockall$star[kw_LophiusFB_rockall$p.value <= .01]  <- "**" 
kw_LophiusFB_rockall$star[kw_LophiusFB_rockall$p.value <= .001] <- "***"

#Plot LophiusFB for Rockall Bank
summaryrockallLophiusFB <- summarySE(subrockall, measurevar="LophiusFB", groupvars="CoralNoCoral")

plotrockallLophiusFB <- ggplot(summaryrockallLophiusFB, aes(x=CoralNoCoral, y=LophiusFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=LophiusFB-se, ymax=LophiusFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="Rockall Bank") +
                  xlab("Coral framework") +
                  ylab("") +
                  ylim(0, 150) +
                  geom_text(aes(label=kw_LophiusFB_rockall$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_MolvaFB_rockall<-kruskal.test(MolvaFB~CoralNoCoral, data=subrockall)
kw_MolvaFB_rockall$star <- "" 
kw_MolvaFB_rockall$star[kw_MolvaFB_rockall$p.value <= .05]  <- "*" 
kw_MolvaFB_rockall$star[kw_MolvaFB_rockall$p.value <= .01]  <- "**" 
kw_MolvaFB_rockall$star[kw_MolvaFB_rockall$p.value <= .001] <- "***"

#Plot MolvaFB for Rockall Bank
summaryrockallMolvaFB <- summarySE(subrockall, measurevar="MolvaFB", groupvars="CoralNoCoral")

plotrockallMolvaFB <- ggplot(summaryrockallMolvaFB, aes(x=CoralNoCoral, y=MolvaFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MolvaFB-se, ymax=MolvaFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="Rockall Bank") +
                  xlab("Coral framework") +
                  ylab("") +
                  ylim(0, 10) +
                  geom_text(aes(label=kw_MolvaFB_rockall$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#All Areas

#Kruskall-Wallis Test, non-paramteric rank test
kw_LepidionFB_data<-kruskal.test(LepidionFB~CoralNoCoral, data=data)
kw_LepidionFB_data$star <- "" 
kw_LepidionFB_data$star[kw_LepidionFB_data$p.value <= .05]  <- "*" 
kw_LepidionFB_data$star[kw_LepidionFB_data$p.value <= .01]  <- "**" 
kw_LepidionFB_data$star[kw_LepidionFB_data$p.value <= .001] <- "***"

#Summary LepidionFB for All Areas
summarydataLepidionFB <- summarySE(data, measurevar="LepidionFB", groupvars="CoralNoCoral")

#Plot LepidionFB for All Areas
plotdataLepidionFB <- ggplot(summarydataLepidionFB, aes(x=CoralNoCoral, y=LepidionFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=LepidionFB-se, ymax=LepidionFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="All Areas") +
                  xlab("Coral framework") +
                  ylab("") +
                  ylim(0,40) +
                  geom_text(aes(label=kw_LepidionFB_data$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_SynaphobranchusFB_data<-kruskal.test(SynaphobranchusFB~CoralNoCoral, data=data)
kw_SynaphobranchusFB_data$star <- "" 
kw_SynaphobranchusFB_data$star[kw_SynaphobranchusFB_data$p.value <= .05]  <- "*" 
kw_SynaphobranchusFB_data$star[kw_SynaphobranchusFB_data$p.value <= .01]  <- "**" 
kw_SynaphobranchusFB_data$star[kw_SynaphobranchusFB_data$p.value <= .001] <- "***"

#Plot SynaphobranchusFB for All Areas
summarydataSynaphobranchusFB <- summarySE(data, measurevar="SynaphobranchusFB", groupvars="CoralNoCoral")

plotdataSynaphobranchusFB <- ggplot(summarydataSynaphobranchusFB, aes(x=CoralNoCoral, y=SynaphobranchusFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=SynaphobranchusFB-se, ymax=SynaphobranchusFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="All Areas") +
                  xlab("") +
                  ylab("") +
                  ylim(0,0.8) +
                  geom_text(aes(label=kw_SynaphobranchusFB_data$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_MoraFB_data<-kruskal.test(MoraFB~CoralNoCoral, data=data)
kw_MoraFB_data$star <- "" 
kw_MoraFB_data$star[kw_MoraFB_data$p.value <= .05]  <- "*" 
kw_MoraFB_data$star[kw_MoraFB_data$p.value <= .01]  <- "**" 
kw_MoraFB_data$star[kw_MoraFB_data$p.value <= .001] <- "***"

#Plot MoraFB for All Areas
summarydataMoraFB <- summarySE(data, measurevar="MoraFB", groupvars="CoralNoCoral")

plotdataMoraFB <- ggplot(summarydataMoraFB, aes(x=CoralNoCoral, y=MoraFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MoraFB-se, ymax=MoraFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="All Areas") +
                  xlab("Coral framework") +
                  ylab("") +
                  ylim(0,100) +
                  geom_text(aes(label=kw_MoraFB_data$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_ChimaeraFB_data<-kruskal.test(ChimaeraFB~CoralNoCoral, data=data)
kw_ChimaeraFB_data$star <- "" 
kw_ChimaeraFB_data$star[kw_ChimaeraFB_data$p.value <= .05]  <- "*" 
kw_ChimaeraFB_data$star[kw_ChimaeraFB_data$p.value <= .01]  <- "**" 
kw_ChimaeraFB_data$star[kw_ChimaeraFB_data$p.value <= .001] <- "***"


#Plot ChimaeraFB for All Areas
summarydataChimaeraFB <- summarySE(data, measurevar="ChimaeraFB", groupvars="CoralNoCoral")

plotdataChimaeraFB <- ggplot(summarydataChimaeraFB, aes(x=CoralNoCoral, y=ChimaeraFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=ChimaeraFB-se, ymax=ChimaeraFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="All Areas") +
                  xlab("Coral framework") +
                  ylab("") +
                  ylim(0,40) +
                  geom_text(aes(label=kw_ChimaeraFB_data$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_HelicolenusFB_data<-kruskal.test(HelicolenusFB~CoralNoCoral, data=data)
kw_HelicolenusFB_data$star <- "" 
kw_HelicolenusFB_data$star[kw_HelicolenusFB_data$p.value <= .05]  <- "*" 
kw_HelicolenusFB_data$star[kw_HelicolenusFB_data$p.value <= .01]  <- "**" 
kw_HelicolenusFB_data$star[kw_HelicolenusFB_data$p.value <= .001] <- "***"

#Plot HelicolenusFB for All Areas
summarydataHelicolenusFB <- summarySE(data, measurevar="HelicolenusFB", groupvars="CoralNoCoral")

plotdataHelicolenusFB <- ggplot(summarydataHelicolenusFB, aes(x=CoralNoCoral, y=HelicolenusFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=HelicolenusFB-se, ymax=HelicolenusFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="All Areas") +
                  xlab("Coral framework") +
                  ylab("") +
                  ylim(0,50) +
                  geom_text(aes(label=kw_HelicolenusFB_data$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_LophiusFB_data<-kruskal.test(LophiusFB~CoralNoCoral, data=data)
kw_LophiusFB_data$star <- "" 
kw_LophiusFB_data$star[kw_LophiusFB_data$p.value <= .05]  <- "*" 
kw_LophiusFB_data$star[kw_LophiusFB_data$p.value <= .01]  <- "**" 
kw_LophiusFB_data$star[kw_LophiusFB_data$p.value <= .001] <- "***"

#Plot LophiusFB for All Areas
summarydataLophiusFB <- summarySE(data, measurevar="LophiusFB", groupvars="CoralNoCoral")

plotdataLophiusFB <- ggplot(summarydataLophiusFB, aes(x=CoralNoCoral, y=LophiusFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=LophiusFB-se, ymax=LophiusFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="All Areas") +
                  xlab("Coral framework") +
                  ylab("") +
                  ylim(0,150) +
                  geom_text(aes(label=kw_LophiusFB_data$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_MolvaFB_data<-kruskal.test(MolvaFB~CoralNoCoral, data=data)
kw_MolvaFB_data$star <- "" 
kw_MolvaFB_data$star[kw_MolvaFB_data$p.value <= .05]  <- "*" 
kw_MolvaFB_data$star[kw_MolvaFB_data$p.value <= .01]  <- "**" 
kw_MolvaFB_data$star[kw_MolvaFB_data$p.value <= .001] <- "***"

#Plot MolvaFB for All Areas
summarydataMolvaFB <- summarySE(data, measurevar="MolvaFB", groupvars="CoralNoCoral")

plotdataMolvaFB <- ggplot(summarydataMolvaFB, aes(x=CoralNoCoral, y=MolvaFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MolvaFB-se, ymax=MolvaFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="All Areas") +
                  xlab("Coral framework") +
                  ylab("") +
                  ylim(0,10) +
                  geom_text(aes(label=kw_MolvaFB_data$star), colour="black", vjust=-1, size=10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Make plot with grid layout
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 4)))

#print(plotbelgicaLepidionFB, vp = vplayout(1, 1))
#print(plotbelgicaSynaphobranchusFB, vp = vplayout(2, 1))
#print(plotbelgicaMoraFB, vp = vplayout(3, 1))
#print(plotbelgicaChimaeraFB, vp = vplayout(4, 1))
print(plotbelgicaHelicolenusFB, vp = vplayout(1, 1))
print(plotbelgicaLophiusFB, vp = vplayout(2, 1))
print(plotbelgicaMolvaFB, vp = vplayout(3, 1))

#print(plothattonLepidionFB, vp = vplayout(1, 2))
#print(plothattonSynaphobranchusFB, vp = vplayout(2, 2))
#print(plothattonMoraFB, vp = vplayout(3, 2))
#print(plothattonChimaeraFB, vp = vplayout(4, 2))
print(plothattonHelicolenusFB, vp = vplayout(1, 2))
print(plothattonLophiusFB, vp = vplayout(2, 2))
print(plothattonMolvaFB, vp = vplayout(3, 2))

#print(plotrockallLepidionFB, vp = vplayout(1, 3))
#print(plotrockallSynaphobranchusFB, vp = vplayout(2, 3))
#print(plotrockallMoraFB, vp = vplayout(3, 3))
#print(plotrockallChimaeraFB, vp = vplayout(4, 3))
print(plotrockallHelicolenusFB, vp = vplayout(1, 3))
print(plotrockallLophiusFB, vp = vplayout(2, 3))
print(plotrockallMolvaFB, vp = vplayout(3, 3))

#print(plotdataLepidionFB, vp = vplayout(1, 4))
#print(plotdataSynaphobranchusFB, vp = vplayout(2, 4))
#print(plotdataMoraFB, vp = vplayout(3, 4))
#print(plotdataChimaeraFB, vp = vplayout(4, 4))
print(plotdataHelicolenusFB, vp = vplayout(1, 4))
print(plotdataLophiusFB, vp = vplayout(2, 4))
print(plotdataMolvaFB, vp = vplayout(3, 4))
